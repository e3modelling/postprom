#' Process and Aggregate Secondary Energy Data
#'
#' This function processes and aggregates secondary energy data.
#' It maps fuel categories, aggregates data by reporting categories, and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated secondary energy data.
#'
#' @examples
#' \dontrun{
#' result <- reportSE(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind
#' @importFrom quitte as.quitte
#' @importFrom dplyr mutate %>%
#' @importFrom madrat toolAggregate
#' @importFrom tidyr separate_rows
#' @export
reportSE <- function(path, regions, years) {
  MtoeToTWh <- 11630 * 1e-3
  shares <- readGDX(path, c("i09ShareFuel", "i04ShareFuels"), field = "l")
  # ===================== Electricity ==========================
  # --------------------- Non CHP -----------------------------
  TECHtoEF <- readGDX(path, "PGALLtoEF") %>%
    rename(TECH = PGALL, EF = EFS)
  CCS <- readGDX(path, "CCS")
  NOCCS <- c(readGDX(path, "NOCCS"), "ATHOIL")
  prodElec <- readGDX(path, "VmProdElec", field = "l")[regions, years, ]
  sharesTech <- shares$i04ShareFuels[regions, , ]
  prodElec <- getSecondaryEnergy(TECHtoEF, prodElec, CCS, NOCCS, sharesTech)
  prodElec <- add_columns(prodElec, addnm = "Heat", fill = 0)
  # ---------------------- CHP --------------------------------
  prodElecCHP <- readGDX(path,
    c("V04ProdElecEstCHP", "V04ProdElecCHP"),
    field = "l", format = "first_found"
  )[regions, years, ]
  TECHtoEF <- readGDX(path, "TSTEAMtoEF") %>%
    filter(TSTEAM %in% getItems(prodElecCHP, 3.1)) %>%
    rename(TECH = TSTEAM)
  CCS <- c("")
  NOCCS <- c("TSTE1AD", "TSTE1AL", "TSTE1AG", "TSTE1AH", "TSTE1AB")
  sharesTech <- shares$i09ShareFuel[regions, years, ]
  prodElecCHP <- getSecondaryEnergy(TECHtoEF, prodElecCHP, CCS, NOCCS, sharesTech)
  # ------------------------------ Electricity ------------------------------
  prodElecAll <- add_columns(prodElec,
    setdiff(getItems(prodElecCHP, 3), getItems(prodElec, 3)),
    fill = 0
  ) +
    add_columns(prodElecCHP,
      setdiff(getItems(prodElec, 3), getItems(prodElecCHP, 3)),
      fill = 0
    )
  getItems(prodElecAll, 3) <- paste0("Secondary Energy|Electricity|", getItems(prodElecAll, 3))
  # =========================== H2 =======================================
  TECHtoEF <- readGDX(path, "H2TECHEFtoEF") %>%
    rename(TECH = H2TECH)
  CCS <- readGDX(path, "H2CCS")
  NOCCS <- readGDX(path, "H2NOCCS")
  prodH2 <- readGDX(path, "VmProdH2", field = "l")[regions, years, ] * MtoeToTWh
  prodH2 <- getSecondaryEnergy(TECHtoEF, prodH2, CCS, NOCCS)
  getItems(prodH2, 3) <- paste0("Secondary Energy|Hydrogen|", getItems(prodH2, 3))
  # =========================== Heat =====================================
  prodHeat <- readGDX(path, "VmProdSte", field = "l")[regions, years, ] * MtoeToTWh
  TECHtoEF <- readGDX(path, "TSTEAMtoEF") %>%
    filter(TSTEAM %in% getItems(prodHeat, 3.1)) %>%
    rename(TECH = TSTEAM)
  CCS <- c("")
  NOCCS <- c(
    "TSTE1AD", "TSTE1AL", "TSTE1AG", "TSTE1AH", "TSTE1AB",
    "TSTE2LGN", "TSTE2OSL", "TSTE2GDO", "TSTE2NGS", "TSTE2BMS"
  )
  sharesTech <- shares$i09ShareFuel[regions, years, ]

  prodHeat <- getSecondaryEnergy(TECHtoEF, prodHeat, CCS, NOCCS, sharesTech)
  getItems(prodHeat, 3) <- paste0("Secondary Energy|Heat|", getItems(prodHeat, 3))
  # ========================== Total ========================================
  magpie_object <- mbind(prodElecAll, prodH2, prodHeat)
  magpie_object <- helperAggregateLevel(magpie_object, level = 2, recursive = TRUE)
  # ========================== Elc Demand =======================================
  elcDemand <- readGDX(path, "V04DemElecTot", field = "l")[regions, years, ]
  getItems(elcDemand, 3) <- paste0("Secondary Energy|Electricity|Demand")

  magpie_object <- mbind(magpie_object, elcDemand)
  units <- sub(".*\\((.*)\\).*", "\\1", prodElec@description)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  return(magpie_object)
}

# Helpers -----------------------------------------------------------------------
getSecondaryEnergy <- function(TECHtoEF, prod, CCS, NOCCS, sharesTech = NULL) {
  BALEFtoEF <- read.csv(
    system.file("mappings", "BALEFtoEF.csv", package = "postprom")
  ) %>%
    separate_rows(EF) %>%
    filter(BALEF %in% c(
      "Coal", "Gas", "Nuclear", "Biofuels", "Oil", "Electricity",
      "Solar", "Wind", "Geothermal and other renewable sources", "Hydrogen", "Hydro"
    ))
  if (!is.null(sharesTech)) {
    sharesTech <- sharesTech[, , paste0(TECHtoEF$TECH, ".", TECHtoEF$EF)]
  }
  TECHtoEF[, 2] <- paste0(TECHtoEF$TECH, ".", TECHtoEF$EF)

  prod <- toolAggregate(prod,
    weight = sharesTech, dim = 3,
    rel = TECHtoEF, from = "TECH", to = "EF"
  )

  prefix <- sub("\\..*", "", getItems(prod, 3))
  withCCS <- case_when(
    prefix %in% CCS ~ "w/ CCS",
    prefix %in% NOCCS ~ "w/o CCS",
    TRUE ~ ""
  )

  prod <- add_dimension(prod, dim = 3.3, add = "CCS", nm = withCCS, expand = FALSE)
  getItems(prod, 3.2) <- BALEFtoEF$BALEF[match(getItems(prod, 3.2), BALEFtoEF$EF)]
  prod <- dimSums(prod, 3.1)

  name <- gsub("\\.", "|", sub("\\.NA$", "", getItems(prod, dim = 3)))
  getItems(prod, 3) <- name
  return(prod)
}
