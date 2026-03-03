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
  EFSTable <- rgdx.set(path, "EFS", te = TRUE)

  BALEF2EFS <- rgdx.set(path, "BALEF2EFS") %>%
    left_join(EFSTable, by = c("EFS" = "EF")) %>%
    filter(BALEF %in% c(
      "Solids", "Fossil Liquids", "Gases", "Heat",
      "Electricity", "Hydrogen", "Biofuels",
      "Nuclear", "Hydro", "Wind", "Solar energy",
      "Geothermal heat", "Nuclear heat", "Other Fuels"
    )) %>%
    rename(EF = EFS) %>%
    select(BALEF, EF)

  mapCCS <- readGDX(path, "CCS_NOCCS") %>%
    as.data.frame() %>%
    rename(CCS = PGALL, NOCCS = PGALL1)

  TSTEAMtoEF <- readGDX(path, "TSTEAMtoEF") %>%
    rename(Tech = TSTEAM, EF = EF)

  PGALLtoEF <- readGDX(path, "PGALLtoEF") %>%
    rename(Tech = PGALL, EF = EFS)

  mapping <- bind_rows(PGALLtoEF, TSTEAMtoEF) %>%
    left_join(BALEF2EFS, by = "EF") %>%
    select(-EF)

  CCStoEF <- mapping %>%
    filter(Tech %in% mapCCS$CCS)

  CCS <- CCStoEF$Tech

  prodElecCHP <- readGDX(path, c("V04ProdElecEstCHP", "V04ProdElecCHP"), field = "l", format = "first_found")[regions, years, ]
  prodElec <- readGDX(path, "VmProdElec", field = "l")[regions, years, ]
  prodElec <- mbind(prodElec, prodElecCHP)

  # Create a mapping for naming (e.g., ATHLGN->Lignite|w/o CCS, etc.)
  mapping2 <- mapping %>%
    mutate(
      BALEF = ifelse(Tech %in% CCS, paste0(BALEF, "|w/ CCS"), as.character(BALEF)),
      BALEF = ifelse(BALEF %in% CCStoEF$BALEF & !(Tech %in% CCS), paste0(BALEF, "|w/o CCS"), as.character(BALEF))
    ) %>%
    filter(Tech %in% getItems(prodElec, 3))

  prodElec <- toolAggregate(prodElec, dim = 3.1, rel = mapping2, from = "Tech", to = "BALEF")
  getItems(prodElec, 3) <- paste0("Secondary Energy|Electricity|", getItems(prodElec, 3))

  prodAll <- helperAggregateLevel(prodElec, level = 2, recursive = TRUE)

  totalDemand <- readGDX(path, "V04DemElecTot", field = "l")[regions, years, ]
  getItems(totalDemand, 3.1) <- "Secondary Energy|Electricity|Demand"
  prodAll <- mbind(prodAll, totalDemand)

  prodAll <- add_dimension(prodAll, dim = 3.2, add = "unit", nm = "TWh")
  return(prodAll)
}
