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
#' @export
reportSE <- function(path, regions, years) {
  rename_EF <- c(
    "LGN" = "Lignite",
    "HCL" = "Hard coal",
    "GDO" = "Oil",
    "NGS" = "Gas",
    "BMSWAS" = "Biomass",
    "NUC" = "Nuclear",
    "HYD" = "Hydro",
    "WND" = "Wind",
    "SOL" = "Solar",
    "GEO" = "Geothermal",
    "H2F" = "Hydrogen"
  )
  mapCCS <- readGDX("blabla.gdx", "CCS_NOCCS") %>% as.data.frame() %>%
    rename(CCS = PGALL, NOCCS = PGALL1)

  PGALLtoEF <- readGDX(path, "PGALLtoEF") %>%
    rename(Tech = PGALL, EF = PGEF)
  CCStoEF <- PGALLtoEF %>%
    filter(Tech %in% mapCCS$CCS)
  PGALLtoEF <- PGALLtoEF %>%
    anti_join(mapCCS, by = c("Tech" = "CCS"))
  CHPtoEF <- readGDX(path, "CHPtoEF") %>%
    rename(Tech = EF, EF = EF1) %>%
    filter(EF %in% PGALLtoEF$EF)

  PGALLtoEF$EF <- str_replace_all(PGALLtoEF$EF, rename_EF)
  CHPtoEF$EF <- str_replace_all(CHPtoEF$EF, rename_EF)
  CCStoEF$EF <- str_replace_all(CCStoEF$EF, rename_EF)
  mapping <- bind_rows(PGALLtoEF, CHPtoEF, CCStoEF)
  CHPs <- filter(PGALLtoEF,!EF %in% setdiff(PGALLtoEF$EF, CHPtoEF$EF))
  CCSs <- filter(PGALLtoEF,!EF %in% setdiff(PGALLtoEF$EF, CCStoEF$EF))

  tempProdNonCHP <- readGDX(path, "VmProdElec", field = "l")[regions, years, ]
  ProdCHP <- readGDX(path, c("V04ProdElecEstCHP", "V04ProdElecCHP"), field = "l", format = "first_found")[regions, years, ]
  ProdCCS <- tempProdNonCHP[,,mapCCS$CCS]
  Prod <- mbind(tempProdNonCHP, ProdCHP)
  Prod <- helperPrepareProd(Prod, mapping)

  ProdNonCHP <- helperPrepareProd(tempProdNonCHP[,,CHPs$Tech], CHPs, "Non-CHP")
  ProdNonCHP <- mbind(ProdNonCHP, helperPrepareProd(tempProdNonCHP[,,CCSs$Tech], CCSs, "Non-CHP|Non-CCS"))

  ProdCHP <- helperPrepareProd(ProdCHP, CHPtoEF, "CHP")
  ProdCCS <- helperPrepareProd(ProdCCS, CCStoEF, "Non-CHP|CCS")

  Total <- dimSums(Prod, 3.1, na.rm = TRUE)
  getItems(Total, 3.1) <- "Secondary Energy|Electricity"

  TotalCHP <- dimSums(ProdCHP, 3.1, na.rm = TRUE)
  getItems(TotalCHP, 3.1) <- "Secondary Energy|Electricity|CHP"

  TotalNonCHP <- Total - TotalCHP
  TotalNonCHP <- collapseDim(TotalNonCHP, 3.2)
  getItems(TotalNonCHP, 3.1) <- "Secondary Energy|Electricity|Non-CHP"

  totalDemand <- readGDX(path, "V04DemElecTot", field = "l")[regions, years, ]
  getItems(totalDemand, 3.1) <- "Secondary Energy|Electricity|Demand"

  ProdNonCHP <- mbind(ProdNonCHP, helperAggregateCoalProd(ProdNonCHP, name = "Non-CHP"))
  #ProdNonCCS <- mbind(ProdNonCCS, helperAggregateCoalProd(ProdNonCCS, name = "Non-CCS"))
  ProdCHP <- mbind(ProdCHP, helperAggregateCoalProd(ProdCHP, name = "CHP"))
  #ProdCCS <- mbind(ProdCCS, helperAggregateCoalProd(ProdCCS, name = "CCS"))


  Total <- dimSums(Prod, 3.1, na.rm = TRUE)
  getItems(Total, 3.1) <- "Secondary Energy|Electricity"
  Prod <- mbind(Prod, helperAggregateCoalProd(Prod))

  magpie_object <- mbind(Prod, ProdNonCHP, ProdCHP, ProdCCS, Total,
                         TotalCHP, TotalNonCHP, totalDemand)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit",nm = "TWh")
  return(magpie_object)
}

# Helper -----------------------------------------------------------------------
helperPrepareProd <- function(magpie, mapping, name = NULL) {
  magpie <- toolAggregate(
    magpie, dim = 3, rel = mapping, from = "Tech", to = "EF"
  )

  title <- paste0("Secondary Energy|Electricity|", getItems(magpie, 3))
  title <- if (!is.null(name)) paste(title, name, sep = "|") else title
  getItems(magpie, 3) <- title
  return(magpie)
}

helperAggregateCoalProd <- function(magpie_object, name = NULL) {
  temp <- magpie_object[,,c("Lignite", "Hard coal"), pmatch = TRUE]
  temp <- dimSums(temp, 3.1, na.rm = TRUE)
  title <- "Secondary Energy|Electricity|Coal"
  title <- if (!is.null(name)) paste(title, name, sep = "|") else title
  getItems(temp, 3) <- title
  return(temp)
}
