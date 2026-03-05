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

  BALEFtoEF <- read.csv(
    system.file("mappings", "BALEFtoEF.csv", package = "postprom")
  ) %>%
    separate_rows(EF) %>%
    filter(BALEF %in% c(
      "Coal", "Gas", "Nuclear", "Biofuels", "Oil",
      "Solar", "Wind", "Geothermal", "Hydrogen", "Hydro"
    ))

  PGALLtoEF <- readGDX(path, "PGALLtoEF")
  TSTEAMtoEF <- readGDX(path, "TSTEAMtoEF")

  shares <- readGDX(path, c("i09ShareFuel", "i04ShareFuels"), field = "l")

  prodElec <- readGDX(path, "VmProdElec", field = "l")[regions, years, ]
  i04ShareFuels <- shares$i04ShareFuels[regions, , ]
  i04ShareFuels <- i04ShareFuels[, , paste0(PGALLtoEF$PGALL, ".", PGALLtoEF$EFS)]
  PGALLtoEF[, 2] <- paste0(PGALLtoEF$PGALL, ".", PGALLtoEF$EFS)

  prodElec <- toolAggregate(prodElec,
    weight = i04ShareFuels, dim = 3,
    rel = PGALLtoEF, from = "PGALL", to = "EFS"
  )

  prefix <- sub("\\..*", "", getItems(prodElec, 3))
  withCCS <- case_when(
    prefix %in% readGDX(path, "CCS") ~ "w/ CCS",
    prefix %in% c(readGDX(path, "NOCCS"), "ATHOIL") ~ "w/o CCS",
    TRUE ~ ""
  )

  prodElec <- add_dimension(prodElec, dim = 3.3, add = "CCS", nm = withCCS, expand = FALSE)
  getItems(prodElec, 3.2) <- BALEFtoEF$BALEF[match(getItems(prodElec, 3.2), BALEFtoEF$EF)]


  prodElec <- dimSums(prodElec, 3.1)

  name <- gsub("\\.", "|", sub("\\.NA$", "", getItems(prodElec, dim = 3)))
  getItems(prodElec, 3) <- name























































  BALEFtoEF <- read.csv(
    system.file("mappings", "BALEFtoEF.csv", package = "postprom")
  ) %>%
    separate_rows(EF) %>%
    filter(BALEF %in% c(
      "Coal", "Gas", "Nuclear", "Biofuels", "Oil",
      "Solar", "Wind", "Geothermal", "Hydrogen", "Hydro"
    ))

  PGALLtoEF <- readGDX(path, "PGALLtoEF")
  TSTEAMtoEF <- readGDX(path, "TSTEAMtoEF")

  shares <- readGDX(path, c("i09ShareFuel", "i04ShareFuels"), field = "l")

  prodElecCHP <- readGDX(path, c("V04ProdElecEstCHP", "V04ProdElecCHP"), field = "l", format = "first_found")[regions, years, ]
  TSTEAMtoEF <- filter(TSTEAMtoEF, TSTEAM %in% getItems(prodElecCHP, 3.1))
  i09ShareFuels <- shares$i09ShareFuel[regions, years, ]
  i09ShareFuels <- i09ShareFuels[, , paste0(TSTEAMtoEF$TSTEAM, ".", TSTEAMtoEF$EF)]
  TSTEAMtoEF[, 2] <- paste0(TSTEAMtoEF$TSTEAM, ".", TSTEAMtoEF$EF)
  prodElecCHP <- toolAggregate(prodElecCHP,
    weight = i09ShareFuels, dim = 3,
    rel = TSTEAMtoEF, from = "TSTEAM", to = "EF"
  )
  prodElecCHP <- dimSums(prodElecCHP, 3.1)

  prodElec <- readGDX(path, "VmProdElec", field = "l")[regions, years, ]
  i04ShareFuels <- shares$i04ShareFuels[regions, , ]
  i04ShareFuels <- i04ShareFuels[, , paste0(PGALLtoEF$PGALL, ".", PGALLtoEF$EFS)]
  PGALLtoEF[, 2] <- paste0(PGALLtoEF$PGALL, ".", PGALLtoEF$EFS)

  prodElec <- toolAggregate(prodElec,
    weight = i04ShareFuels, dim = 3,
    rel = PGALLtoEF, from = "PGALL", to = "EFS"
  )


  prefix <- sub("\\..*", "", getItems(prodElec, 3))
  withCCS <- case_when(
    prefix %in% readGDX(path, "CCS") ~ "w/ CCS",
    prefix %in% c(readGDX(path, "NOCCS"), "ATHOIL") ~ "w/o CCS",
    TRUE ~ ""
  )


  # test <- toolAggregate(prodElec, rel = BALEFtoEF, dim = 3.2, from = "EF", to = "BALEF", partrel = TRUE)

  prodElec <- add_dimension(prodElec, dim = 3.3, add = "CCS", nm = withCCS, expand = FALSE)
  getItems(prodElec, 3.2) <- BALEFtoEF$BALEF[match(getItems(prodElec, 3.2), BALEFtoEF$EF)]


  prodElec <- dimSums(prodElec, 3.1)

  name <- gsub("\\.", "|", sub("\\.NA$", "", getItems(prodElec, dim = 3)))
  getItems(prodElec, 3) <- name


  CCS <- readGDX(path, "CCS", field = "l")

  PGALLtoEF <- readGDX(path, "PGALLtoEF")

  TSTEAMtoEF <- readGDX(path, "TSTEAMtoEF")

  shares <- readGDX(path, c("i09ShareFuel", "i04ShareFuels"), field = "l")

  prodElecCHP <- readGDX(path, c("V04ProdElecEstCHP", "V04ProdElecCHP"), field = "l", format = "first_found")[regions, years, ]
  TSTEAMtoEF <- filter(TSTEAMtoEF, TSTEAM %in% getItems(prodElecCHP, 3.1))
  i09ShareFuels <- shares$i09ShareFuel[regions, years, ]
  i09ShareFuels <- i09ShareFuels[, , paste0(TSTEAMtoEF$TSTEAM, ".", TSTEAMtoEF$EF)]
  TSTEAMtoEF[, 2] <- paste0(TSTEAMtoEF$TSTEAM, ".", TSTEAMtoEF$EF)
  prodElecCHP <- toolAggregate(prodElecCHP,
    weight = i09ShareFuels, dim = 3,
    rel = TSTEAMtoEF, from = "TSTEAM", to = "EF"
  )

  prefix <- sub("\\..*", "", getItems(prodElecCHP, 3))
  withCCS <- case_when(
    getItems(prodElecCHP, 3.1) %in% readGDX(path, "CCS") ~ "w/ CCS",
    getItems(prodElecCHP, 3.1) %in% c("TSTE1AD", "TSTE1AL", "TSTE1AH", "TSTE1AB") ~ "w/o CCS",
    TRUE ~ ""
  )


  # test <- toolAggregate(prodElec, rel = BALEFtoEF, dim = 3.2, from = "EF", to = "BALEF", partrel = TRUE)

  prodElecCHP <- add_dimension(prodElec, dim = 3.3, add = "CCS", nm = withCCS, expand = FALSE)

  getItems(prodElecCHP, 3.2) <- BALEFtoEF$BALEF[match(getItems(prodElecCHP, 3.2), BALEFtoEF$EF)]
  prodElecCHP <- dimSums(prodElecCHP, 3.1)


  prodElec <- add_dimension(prodElec, dim = 3.3, add = "CCS", nm = withCCS, expand = FALSE)


  prodAll <- add_columns(prodElec, setdiff(getItems(prodElecCHP, 3), getItems(prodElec, 3)), fill = 0) +
    0 # add_columns(prodElecCHP, setdiff(getItems(prodElec, 3), getItems(prodElecCHP, 3)), fill = 0)

  # EFSTable <- rgdx.set(path, "EFS", te = TRUE)

  # getItems(prodAll, 3) <- EFSTable$.te[match(getItems(prodAll, 3), EFSTable$EF)]


  test <- toolAggregate(prodAll, rel = BALEFtoEF, dim = 3.1, from = "EF", to = "BALEF", partrel = TRUE)

  getItems(test, 3.1) <- paste0("Secondary Energy|Electricity|", getItems(test, 3.1))

  TSTEAMtoEF <- readGDX(path, "TSTEAMtoEF") %>%
    rename(Tech = TSTEAM, EF = EF) %>%
    filter(Tech %in% getItems(prodElecCHP, 3.1))


  mapping <- bind_rows(PGALLtoEF, TSTEAMtoEF) %>%
    filter(Tech %in% getItems(prodElec, 3.1))

  prodElec <- mbind(prodElec, prodElecCHP)


  EFSTable <- rgdx.set(path, "EFS", te = TRUE)

  BALEF2EFS <- rgdx.set(path, "BALEF2EFS") %>%
    # left_join(EFSTable, by = c("EFS" = "EF")) %>%
    filter(BALEF %in% c(
      "Solids", "Fossil Liquids", "Gases", "Heat",
      "Electricity", "Hydrogen", "Biofuels",
      "Nuclear", "Hydro", "Wind", "Solar energy",
      "Geothermal heat", "Nuclear heat", "Other Fuels"
    )) %>%
    rename(EF = EFS)

  mapCCS <- readGDX(path, "CCS_NOCCS") %>%
    as.data.frame() %>%
    rename(CCS = PGALL, NOCCS = PGALL1)

  TSTEAMtoEF <- readGDX(path, "TSTEAMtoEF") %>%
    rename(Tech = TSTEAM, EF = EF)

  PGALLtoEF <- readGDX(path, "PGALLtoEF") %>%
    rename(Tech = PGALL, EF = EFS)

  mapping <- bind_rows(PGALLtoEF, TSTEAMtoEF) %>%
    filter(Tech %in% getItems(prodElec, 3)) %>%
    left_join(BALEF2EFS, by = "EF") %>%
    select(-EF)


  CCStoEF <- mapping %>%
    filter(Tech %in% mapCCS$CCS)

  CCS <- CCStoEF$Tech


  # Create a mapping for naming (e.g., ATHLGN->Lignite|w/o CCS, etc.)
  mapping2 <- mapping %>%
    mutate(
      BALEF = ifelse(Tech %in% CCS, paste0(BALEF, "|w/ CCS"), as.character(BALEF)),
      BALEF = ifelse(BALEF %in% CCStoEF$BALEF & !(Tech %in% CCS), paste0(BALEF, "|w/o CCS"), as.character(BALEF))
    ) %>%
    filter(Tech %in% getItems(prodElec, 3))

  prodElec <- toolAggregate(prodElec, dim = 3.1, rel = mapping, from = "Tech", to = "EF")
  getItems(prodElec, 3) <- paste0("Secondary Energy|Electricity|", getItems(prodElec, 3))

  prodAll <- helperAggregateLevel(prodElec, level = 2, recursive = TRUE)

  totalDemand <- readGDX(path, "V04DemElecTot", field = "l")[regions, years, ]
  getItems(totalDemand, 3.1) <- "Secondary Energy|Electricity|Demand"
  prodAll <- mbind(prodAll, totalDemand)

  prodAll <- add_dimension(prodAll, dim = 3.2, add = "unit", nm = "TWh")
  return(prodAll)
}
