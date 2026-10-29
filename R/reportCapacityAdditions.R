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
  V09GapShareSte <- readGDX(path, "V09GapShareSte", field = "l")[regions, years, ]
  V09DemGapSte <- readGDX(path, "V09DemGapSte", field = "l")[regions, years, ]
  i09CostInvCostSteProd <- readGDX(path, "i09CostInvCostSteProd", field = "l")[, years, ] #(US$2015/(KWe or KWThrm)
  TSTEAM <- rgdx.set(path, "TSTEAM", te = TRUE)
  
  TSTEAM$.te <- c(
    "Lignite powered advanced CHP",
    "Hard Coal, Coke and Other Solids powered advanced CHP",
    "Diesel Oil powered advanced CHP",
    "Natural Gas powered advanced CHP",
    "Biomass and Waste powered advanced CHP",
    "Hydrogen powered fuel cell CHP",
    "Lignite powered DH",
    "Hard Coal, Coke and Other Solids powered DH",
    "Diesel Oil powered DH",
    "Natural Gas powered DH",
    "Biomass and Waste powered DH",
    "Geothermal and other renewable sources powered DH",
    "Other powered DH"
  )
  
  TECHtoEF <- readGDX(path, "PGALLtoEF") %>%
    rename(TECH = PGALL, EF = EFS)
  CCS <- readGDX(path, "CCS")
  NOCCS <- c(readGDX(path, "NOCCS"), "ATHOIL")
  capacityAdditions <- getCapacityAddditions(TECHtoEF, VNewCapElec, CCS, NOCCS, sharesTech)
  getItems(capacityAdditions, 3) <- paste0("Capacity Additions|Electricity|", getItems(capacityAdditions, 3))
  netCapacityAdditions <- getCapacityAddditions(TECHtoEF, VNetNewCapElec, CCS, NOCCS, sharesTech)
  getItems(netCapacityAdditions, 3) <- paste0("Net Capacity Additions|Electricity|", getItems(netCapacityAdditions, 3))
  
  capacityAdditionselec <- mbind(capacityAdditions, netCapacityAdditions)
  capacityAdditionselec <- helperAggregateLevel(capacityAdditionselec, level = 2, recursive = TRUE)
  capacityAdditionselec <- add_dimension(capacityAdditionselec, dim = 3.2, add = "unit", nm = "GW/yr")
  
  #### HEAT
  capacityAdditionsSterow <- V09DemGapSte * V09GapShareSte
  capacityAdditionsSte <- toolAggregate(capacityAdditionsSterow, dim = 3, rel = TSTEAM, from = "i", to = ".te")
  getItems(capacityAdditionsSte, 3) <- paste0("Capacity Additions|Heat|", getItems(capacityAdditionsSte, 3))
  
  
  #  Investment|Energy Supply|Heat
  investmentHEAT <- capacityAdditionsSterow * i09CostInvCostSteProd
  i09AvailRateSteProd <- readGDX(path, "i09AvailRateSteProd", field = "l")[, years, ]#"Availability rate of STEAM Plants ()"
  smGwToTwhPerYear <- readGDX(path, "smGwToTwhPerYear", field = "l")[, years, ]#convert GW mean power into TWh/y, depending on whether it's a leap year
  smTWhToMtoe <- readGDX(path, "smTWhToMtoe", field = "l")#TWh to Mtoe conversion factor
  investmentHEAT <- investmentHEAT / (i09AvailRateSteProd * smGwToTwhPerYear * smTWhToMtoe)#million US$2015/yr
  investmentHEAT <- toolAggregate(investmentHEAT, dim = 3, rel = TSTEAM, from = "i", to = ".te")
  getItems(investmentHEAT, 3) <- paste0("Investment|Energy Supply|Heat|", getItems(investmentHEAT, 3))
  investmentHEAT <- add_dimension(investmentHEAT, dim = 3.2, add = "unit", nm = "million US$2015/yr")
  #############
  
  capacityAdditionsSte <- helperAggregateLevel(capacityAdditionsSte, level = 2, recursive = TRUE)
  capacityAdditionsSte <- add_dimension(capacityAdditionsSte, dim = 3.2, add = "unit", nm = "Mtoe/yr")
  
  magpie_object <- mbind(capacityAdditionselec, capacityAdditionsSte, investmentHEAT)
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
