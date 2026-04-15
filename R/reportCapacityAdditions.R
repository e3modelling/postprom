#' Process and Aggregate Electricity Capacity Additions
#'
#' This function reads electricity capacity addition data from a GDX file and
#' computes both gross and net capacity additions by region and year. It maps
#' technologies to energy forms, applies technology fuel shares, distinguishes
#' between CCS and non-CCS technologies, and aggregates results to reporting
#' categories following the BALEF classification. The output is formatted as a
#' magpie object with appropriate reporting names and units.
#'
#' @param path Character string specifying the file path to the GDX data file.
#' @param regions Character vector of region names used to subset the data.
#' @param years Numeric or character vector of years to include in the report.
#'
#' @return A magpie object containing aggregated gross and net electricity
#'   capacity additions by region, year, technology category, and CCS status,
#'   with units expressed in GW/yr.
#'
#' @examples
#' \dontrun{
#' result <- reportCapacityAdditions(
#'   system.file("extdata", "blabla.gdx", package = "postprom"),
#'   regions = c("MEA"),
#'   years   = c(2030, 2040, 2050)
#' )
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind
#' @importFrom madrat toolAggregate
#' @importFrom stringr str_replace_all
#' @importFrom dplyr %>% case_when
#' @export
reportCapacityAdditions <- function(path, regions, years) {
  VNewCapElec <- readGDX(path, "V04NewCapElec", field = "l")[regions, years, ]
  VNetNewCapElec <- readGDX(path, "V04NetNewCapElec", field = "l")[regions, years, ]
  sharesTech <- readGDX(path, "i04ShareFuels", field = "l")[regions, , ]
  TECHtoEF <- readGDX(path, "PGALLtoEF") %>%
    rename(TECH = PGALL, EF = EFS)
  CCS <- readGDX(path, "CCS")
  NOCCS <- c(readGDX(path, "NOCCS"), "ATHOIL")
  capacityAdditions <- getCapacityAddditions(TECHtoEF, VNewCapElec, CCS, NOCCS, sharesTech)
  getItems(capacityAdditions, 3) <- paste0("Capacity Additions|Electricity|", getItems(capacityAdditions, 3))
  netCapacityAdditions <- getCapacityAddditions(TECHtoEF, VNetNewCapElec, CCS, NOCCS, sharesTech)
  getItems(netCapacityAdditions, 3) <- paste0("Net Capacity Additions|Electricity|", getItems(netCapacityAdditions, 3))
  
  magpie_object <- mbind(capacityAdditions, netCapacityAdditions)
  magpie_object <- helperAggregateLevel(magpie_object, level = 2, recursive = TRUE)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = "GW/yr")
  return(magpie_object)
}

# Helper ----------------------------------------------------------------------------------------
getCapacityAddditions <- function(TECHtoEF, prod, CCS, NOCCS, sharesTech = NULL) {
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
