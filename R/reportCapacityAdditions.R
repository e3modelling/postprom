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
  
  ############### TRANSPORT
  V01ShareTechTr <- readGDX(path, "V01ShareTechTr", field = "l")[regions, years, ]
  V01GapTranspActiv <- readGDX(path, "V01GapTranspActiv", field = "l")[regions, years, ]
  capacityAdditionsTRNS <- V01ShareTechTr *  V01GapTranspActiv
  TECH <- rgdx.set(path, "TECH", te = TRUE)
  # capacityAdditionsTRNS <- as.quitte(capacityAdditionsTRNS) %>%
  #   dplyr::filter(value != 0)
  # capacityAdditionsTRNS <- as.magpie(capacityAdditionsTRNS)
  
  TECH <- TECH %>% dplyr::filter(i %in% getItems(capacityAdditionsTRNS,3.2))
  TECH <- TECH %>% mutate(.te = coalesce(.te, i))
  capacityAdditionsTRNS <- toolAggregate(capacityAdditionsTRNS, dim = 3.2, rel = TECH, from = "i", to = ".te")
  
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE) %>% dplyr::filter(SBS %in% getItems(capacityAdditionsTRNS,3.1))
  DSBSTable <- DSBSTable  %>% dplyr::filter(!SBS %in%  c("BMAR", "BAV"))
  capacityAdditionsTRNS <- toolAggregate(capacityAdditionsTRNS[,,DSBSTable[,"SBS"]], dim = 3.1, rel = DSBSTable, from = "SBS", to = ".te")
  
  # Replace sep in dimensions and prepend the sector
  name <- gsub("\\.", "|", getItems(capacityAdditionsTRNS, dim = 3)) # e.g., IS.HCL --> IS|HCL

  getItems(capacityAdditionsTRNS, 3) <- paste0("Capacity Additions|", name)
  capacityAdditionsTRNS <- helperAggregateLevel(capacityAdditionsTRNS, level = 2, recursive = TRUE)
  
  actv_units <- list(
    "Passenger Transport - Cars" = "vehicles",
    "Passenger Transport - Rail" = "GPKM",
    "Passenger Transport - Aviation" = "million passengers carried",
    "Passenger Transport - Busses" = "billion pKm/yr",
    "Passenger Transport - Inland Navigation" = "billion pKm/yr",
    "Goods Transport - Trucks" = "GTKM",
    "Goods Transport - Rail" = "GTKM",
    "Goods Transport - Inland Navigation" = "GTKM"
  )
  
  capacityAdditionsTRNS <- add_dimension(capacityAdditionsTRNS, dim = 3.2, add = "unit")
  
  # Extract transport category from dimension 3.1
  transport_category <- sub(
    "^Capacity Additions\\|([^|.]+).*",
    "\\1",
    getItems(capacityAdditionsTRNS, 3.1)
  )
  
  getItems(capacityAdditionsTRNS, 3.2) <- unname(actv_units[transport_category])
  
  ############################ V01CapacityTransport
  V01CapacityTransport <- readGDX(path, "V01CapacityTransport", field = "l")[regions, years, ]
  V01CapacityTransport <- toolAggregate(V01CapacityTransport[,,DSBSTable[,"SBS"]], dim = 3.1, rel = DSBSTable, from = "SBS", to = ".te")
  
  TECHCapacity <- dplyr::filter(TECH, i %in% getItems(V01CapacityTransport,3.2))
  V01CapacityTransport <- toolAggregate(V01CapacityTransport, dim = 3.2, rel = TECHCapacity, from = "i", to = ".te")
  
  # Replace sep in dimensions and prepend the sector
  name <- gsub("\\.", "|", getItems(V01CapacityTransport, dim = 3)) # e.g., IS.HCL --> IS|HCL
  
  getItems(V01CapacityTransport, 3) <- paste0("Capacity|", name)
  V01CapacityTransport <- helperAggregateLevel(V01CapacityTransport, level = 2, recursive = TRUE)
  
  V01CapacityTransport <- add_dimension(V01CapacityTransport, dim = 3.2, add = "unit")
  
  # Extract transport category from dimension 3.1
  transport_category <- sub(
    "^Capacity\\|([^|.]+).*",
    "\\1",
    getItems(V01CapacityTransport, 3.1)
  )
  
  getItems(V01CapacityTransport, 3.2) <- unname(actv_units[transport_category])
  
  ############################ 
  
  capacityAdditionsSte <- helperAggregateLevel(capacityAdditionsSte, level = 2, recursive = TRUE)
  capacityAdditionsSte <- add_dimension(capacityAdditionsSte, dim = 3.2, add = "unit", nm = "Mtoe/yr")
  
  magpie_object <- mbind(capacityAdditionselec, capacityAdditionsSte, investmentHEAT,
                         capacityAdditionsTRNS, V01CapacityTransport)
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
