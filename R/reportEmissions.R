#' Process and Aggregate CO2 Emissions Data
#'
#' This function processes and aggregates CO2 emissions data from a GDX file.
#' It combines multiple components, including final consumption, energy sector inputs,
#' transportation, and carbon capture, to provide a comprehensive emissions report.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated CO2 emissions data.
#'
#' @examples
#' \dontrun{
#' result <- reportEmissions(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#' 
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind collapseDim
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter left_join mutate select group_by %>%

#' @export
reportEmissions <- function(path, regions, years) {
  
  magpie_object <- NULL
  
  ########## SBS
  desc_map <- c(
    IS = "Iron and Steel",
    NF = "Non Ferrous Metals",
    CH = "Chemicals",
    BM = "Non Metallic Minerals",
    PP = "Paper and Pulp",
    FD = "Food Drink and Tobacco",
    EN = "Engineering",
    TX = "Textiles",
    OE = "Ore Extraction",
    OI = "Other Industrial sectors",
    SE = "Services and Trade",
    AG = "Agriculture, Fishing, Forestry etc.",
    HOU = "Households",
    PC = "Passenger Transport - Cars",
    PB = "Passenger Transport - Busses",
    PT = "Passenger Transport - Rail",
    PN = "Passenger Transport - Inland Navigation",
    PA = "Passenger Transport - Aviation",
    GU = "Goods Transport - Trucks",
    GT = "Goods Transport - Rail",
    GN = "Goods Transport - Inland Navigation",
    BU = "Bunkers",
    PCH = "Petrochemicals Industry",
    NEN = "Other Non Energy Uses",
    PG = "Power and Steam Generation",
    H2P = "Hydrogen Production",
    H2INFR = "Hydrogen storage and delivery"
  )
  
  # Convert to a data frame
  df_SBS <- data.frame(
    Code = names(desc_map),
    Description = unname(desc_map),
    stringsAsFactors = FALSE
  )
  #################
  
  fscenario <- readGDX(path, "fscenario")
  # Get supplementary emissions from NAVIGATE through mrprom
  Navigate_Emissions <- calcOutput("NavigateEmissions", aggregate = TRUE, regionmapping = "regionmappingOPDEV3.csv")
  
  if (fscenario %in% c(0, 1)) {
    Navigate_Emissions <- Navigate_Emissions[, , "SUP_NPi_Default"][regions, years, ]
  } else if (fscenario == 2) {
    Navigate_Emissions <- Navigate_Emissions[, , "SUP_1p5C_Default"][regions, years, ]
  } else if (fscenario == 3) {
    Navigate_Emissions <- Navigate_Emissions[, , "SUP_2C_Default"][regions, years, ]
  }

  Navigate_Emissions <- collapseDim(Navigate_Emissions, 3.1)
  Navigate_Emissions <- collapseDim(Navigate_Emissions, 3.1)

  remind_AFOLU_Industrial_Processes <- Navigate_Emissions[, , c("Emissions|CO2|AFOLU", "Emissions|CO2|Industrial Processes")]
  remind <- dimSums(remind_AFOLU_Industrial_Processes, 3, na.rm = TRUE)

  iCo2EmiFac <- readGDX(path, "imCo2EmiFac")[regions, years, ]
  VConsFuel <- readGDX(path, "VmConsFuel", field = 'l')[regions, years, ]
  VInpTransfTherm <- readGDX(path, "VmInpTransfTherm", field = 'l')[regions, years, ]
  VTransfInputDHPlants <- readGDX(path, "VmTransfInputDHPlants", field = 'l')[regions, years, ]
  VConsFiEneSec <- readGDX(path, "VmConsFiEneSec", field = 'l')[regions, years, ]
  VDemFinEneTranspPerFuel <- readGDX(path, "VmDemFinEneTranspPerFuel", field = 'l')[regions, years, ]
  VProdElec <- readGDX(path, "VmProdElec", field = 'l')[regions, years, ]
  iPlantEffByType <- readGDX(path, "imPlantEffByType")[regions, years, ]
  iCO2CaptRate <- readGDX(path, "imCO2CaptRate")[regions, years, ]
  VConsFuelTechH2Prod <- readGDX(path, "VmConsFuelTechH2Prod", field = 'l')[regions, years, ]
  # Link between Model Subsectors and Fuels

  sets4 <- readGDX(path, "SECtoEF")

  EFtoEFS <- readGDX(path, "EFtoEFS")

  IND <- readGDX(path, "INDDOM")
  IND <- as.data.frame(IND)

  map_INDDOM <- sets4 %>% filter(SBS %in% IND[, 1], EF!= "")

  qINDDOM <- map_INDDOM %>%
    left_join(EFtoEFS, by = "EF") %>%
    select(-c("EF")) %>%
    unique()
  names(qINDDOM) <- sub("EFS", "SECtoEF", names(qINDDOM))

  qINDDOM <- paste0(qINDDOM[["SBS"]], ".", qINDDOM[["SECtoEF"]])
  INDDOM <- as.data.frame(qINDDOM)

  PGEF <- readGDX(path, "PGEF") %>% as.data.frame()

  # final consumption
  sum1 <- iCo2EmiFac[, , INDDOM[, 1]] * VConsFuel[, , INDDOM[, 1]]
  sum1 <- dimSums(sum1, 3, na.rm = TRUE)
  # input to power generation sector
  sum2 <- VInpTransfTherm[, , PGEF[, 1]] * iCo2EmiFac[, , "PG"][, , PGEF[, 1]]
  sum2 <- dimSums(sum2, 3, na.rm = TRUE)
  # hydrogen sector
  H2TECHEFtoEF <- readGDX(path, "H2TECHEFtoEF")
  hydrogen <- VConsFuelTechH2Prod[, , paste(H2TECHEFtoEF[[1]],H2TECHEFtoEF[[2]],sep=".")] * iCo2EmiFac[, , "H2P"][, , unique(H2TECHEFtoEF$EF)]
  hydrogen <- dimSums(hydrogen, 3, na.rm = TRUE)
  # input to district heating plants
  sum3 <- VTransfInputDHPlants * iCo2EmiFac[, , "PG"][, , getItems(VTransfInputDHPlants, 3)]
  sum3 <- dimSums(sum3, 3, na.rm = TRUE)
  # consumption of energy branch
  sum4 <- VConsFiEneSec * iCo2EmiFac[, , "PG"][, , getItems(VConsFiEneSec, 3)]
  sum4 <- dimSums(sum4, 3, na.rm = TRUE)

  TRANSE <- readGDX(path, "TRANSE") %>% as.data.frame()

  map_TRANSECTOR <- sets4 %>% filter(SBS %in% TRANSE[, 1])
  map_TRANSECTOR <- paste0(map_TRANSECTOR[["SBS"]], ".", map_TRANSECTOR[["EF"]])
  map_TRANSECTOR <- as.data.frame(map_TRANSECTOR)

  sum5 <- VDemFinEneTranspPerFuel[, , map_TRANSECTOR[, 1]] * iCo2EmiFac[, , map_TRANSECTOR[, 1]]
  
  ############## transport by sector
  TRANSE_by_sector <- dimSums(sum5, 3.2, na.rm = TRUE)
  
  items_TRANSE <- df_SBS[df_SBS[,"Code"] %in% getItems(TRANSE_by_sector, 3),]
  TRANSE_by_sector <- toolAggregate(TRANSE_by_sector[,,items_TRANSE[,"Code"]], dim = 3, rel = items_TRANSE, from = "Code", to = "Description")
  
  getItems(TRANSE_by_sector, 3) <- paste0("Emissions|CO2|Energy|Demand|Transportation|",getItems(TRANSE_by_sector, 3))
  
  TRANSE_by_sector <- add_dimension(TRANSE_by_sector, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, TRANSE_by_sector)
  ##############
  
  # transport
  sum5 <- dimSums(sum5, 3, na.rm = TRUE)

  PGALLtoEF <- readGDX(path, "PGALLtoEF") %>% as.data.frame()
  names(PGALLtoEF) <- c("PGALL", "EF")

  CCS <- readGDX(path, "CCS")
  CCS <- as.data.frame(CCS)

  CCS <- PGALLtoEF[PGALLtoEF$PGALL %in% CCS$CCS, ]

  # CO2 captured by CCS plants in power generation
  var_16 <- VProdElec[, , CCS[, 1]] * 0.086 / iPlantEffByType[, , CCS[, 1]] * iCo2EmiFac[, , "PG"][, , CCS[, 2]] * iCO2CaptRate[, , CCS[, 1]]
  emi_factor_ATHBMSCCS <- 4.1868
  ATHBMSCCS <- VProdElec[, , "ATHBMSCCS"] * 0.086 / iPlantEffByType[, ,  "ATHBMSCCS"] * emi_factor_ATHBMSCCS * iCO2CaptRate[, ,  "ATHBMSCCS"]
  # CO2 captured by CCS plants
  sum6 <- dimSums(var_16, dim = 3, na.rm = TRUE) + ATHBMSCCS

  ###################### SUPPLY  BY FUEL       #######################
  SUPPLY2 <- VInpTransfTherm[, , PGEF[, 1]] * iCo2EmiFac[, , "PG"][, , PGEF[, 1]]
  SUPPLY2 <- dimSums(SUPPLY2, dim = 3.2, na.rm = TRUE)
  SUPPLY3 <- VTransfInputDHPlants * iCo2EmiFac[, , "PG"][, , getItems(VTransfInputDHPlants, 3)]
  SUPPLY3 <- dimSums(SUPPLY3, dim = 3.2, na.rm = TRUE)
  SUPPLY4 <- VConsFiEneSec * iCo2EmiFac[, , "PG"][, , getItems(VConsFiEneSec, 3)]
  SUPPLY4 <- dimSums(SUPPLY4, dim = 3.2, na.rm = TRUE)
  SUPPLYa <- dimSums(var_16, dim = 3.1, na.rm = TRUE)
  SUPPLYa <- dimSums(SUPPLYa, dim = 3.1, na.rm = TRUE)
  SUPPLYa <- SUPPLYa * (- 1)
  SUPPLY_ATHBMSCCS <- ATHBMSCCS * (- 1)
  
  dn <- dimnames(SUPPLYa)
  names(dn)[names(dn) == "EF"] <- "EFS"
  dimnames(SUPPLYa) <- dn
  
  dn <- dimnames(SUPPLY_ATHBMSCCS)
  names(dn)[names(dn) == "PGALL"] <- "EFS"
  dimnames(SUPPLY_ATHBMSCCS) <- dn
  getItems(SUPPLY_ATHBMSCCS,3) <- "BMSWAS"
  
  SUPPLY_by_fuel <- mbind(SUPPLY2,SUPPLY3,SUPPLY4,SUPPLYa,SUPPLY_ATHBMSCCS)
  
  all_fuels <- getItems(SUPPLY_by_fuel, dim = 3)
  
  # Identify unique fuels
  unique_fuels <- unique(all_fuels)
  
  # For each unique fuel, find matching slices and sum them
  fuel_sums <- lapply(unique_fuels, function(fuel) {
    idx <- which(all_fuels == fuel)
    summed <- dimSums(SUPPLY_by_fuel[,,idx], dim = 3)
    getItems(summed, dim = 3) <- fuel
    return(summed)
  })
  
  # Combine all summed fuels into a single magpie object
  SUPPLY_by_fuel_summed <- do.call(mbind, fuel_sums)
  
  getItems(SUPPLY_by_fuel_summed, 3) <- paste0("Emissions|CO2|Energy|Supply|",getItems(SUPPLY_by_fuel_summed, 3))
  
  SUPPLY_by_fuel_summed <- add_dimension(SUPPLY_by_fuel_summed, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  
  magpie_object <- mbind(magpie_object, SUPPLY_by_fuel_summed)
   ###########################
  
  # input hydrogen_CCS
  H2CCS <- readGDX(path, "H2CCS")
  iCo2EmiFac[, , "H2P"][, , "BMSWAS"] <- emi_factor_ATHBMSCCS
  hydrogen_CCS <- VConsFuelTechH2Prod[, , PGEF[, 1]][,,H2CCS] * iCo2EmiFac[, , "H2P"][, , PGEF[, 1]]
  iCo2EmiFac[, , "H2P"][, , "BMSWAS"] <- 0
  hydrogen_CCS <- dimSums(hydrogen_CCS, 3, na.rm = TRUE)

  SECTTECH2 <- sets4 %>% filter(SBS %in% c("BU"))
  SECTTECH2 <- paste0(SECTTECH2[["SBS"]], ".", SECTTECH2[["EF"]])
  SECTTECH2 <- as.data.frame(SECTTECH2)
  # Bunkers
  sum7 <- iCo2EmiFac[, , SECTTECH2[, 1]] * VConsFuel[, , SECTTECH2[, 1]]
  
  ############## Bunkers by sector
  Bunkers_by_sector <- dimSums(sum7, 3.2, na.rm = TRUE)
  
  items_Bunkers <- df_SBS[df_SBS[,"Code"] %in% getItems(Bunkers_by_sector, 3),]
  Bunkers_by_sector <- toolAggregate(Bunkers_by_sector[,,items_Bunkers[,"Code"]], dim = 3, rel = items_Bunkers, from = "Code", to = "Description")
  
  getItems(Bunkers_by_sector, 3) <- paste0("Emissions|CO2|Energy|Demand|Bunkers|",getItems(Bunkers_by_sector, 3))
  
  Bunkers_by_sector <- add_dimension(Bunkers_by_sector, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, Bunkers_by_sector)
  ##############
  
  sum7 <- dimSums(sum7, dim = 3, na.rm = TRUE)

  total_CO2 <- sum1 + sum2 + sum3 + sum4 + sum5 - sum6 + sum7 + remind + hydrogen - hydrogen_CCS

  getItems(total_CO2, 3) <- "Emissions|CO2"

  total_CO2 <- add_dimension(total_CO2, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, total_CO2, Navigate_Emissions)

  # Hydrogen
  Hydrogen_total <- hydrogen - hydrogen_CCS

  getItems(Hydrogen_total, 3) <- "Emissions|CO2|Energy|Supply|Hydrogen"
  
  Emissions_Supply_Hydrogen <- Hydrogen_total

  Hydrogen_total <- add_dimension(Hydrogen_total, dim = 3.2, add = "unit", nm = "Mt CO2/yr")

  magpie_object <- mbind(magpie_object, Hydrogen_total)

  # Extra Emissions
  # Emissions|CO2|Energy|Demand|Industry

  INDSE <- readGDX(path, "INDSE")
  INDSE <- as.data.frame(INDSE)

  map_INDSE <- sets4 %>% filter(SBS %in% INDSE[, 1], EF != "")

  qINDSE <- map_INDSE %>%
    left_join(EFtoEFS, by = "EF") %>%
    select(-c("EF")) %>%
    unique()
  names(qINDSE) <- sub("EFS", "SECtoEF", names(qINDSE))

  qINDSE <- paste0(qINDSE[["SBS"]], ".", qINDSE[["SECtoEF"]])
  INDSE <- as.data.frame(qINDSE)

  # final consumption
  sum_INDSE <- iCo2EmiFac[, , INDSE[, 1]] * VConsFuel[, , INDSE[, 1]]
  
  ############## Industry by sector
  INDSE_by_sector <- dimSums(sum_INDSE, 3.2, na.rm = TRUE)
  
  items_INDSE <- df_SBS[df_SBS[,"Code"] %in% getItems(INDSE_by_sector, 3),]
  INDSE_by_sector <- toolAggregate(INDSE_by_sector[,,items_INDSE[,"Code"]], dim = 3, rel = items_INDSE, from = "Code", to = "Description")
  
  getItems(INDSE_by_sector, 3) <- paste0("Emissions|CO2|Energy|Demand|Industry|",getItems(INDSE_by_sector, 3))
  
  INDSE_by_sector <- add_dimension(INDSE_by_sector, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, INDSE_by_sector)
  ##############
  
  sum_INDSE <- dimSums(sum_INDSE, 3, na.rm = TRUE)

  getItems(sum_INDSE, 3) <- "Emissions|CO2|Energy|Demand|Industry"

  sum_INDSE <- add_dimension(sum_INDSE, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, sum_INDSE)

  # Emissions|CO2|Energy|Demand|Residential and Commercial

  DOMSE <- readGDX(path, "DOMSE")
  DOMSE <- as.data.frame(DOMSE)

  map_DOMSE <- sets4 %>% filter(SBS %in% DOMSE[, 1], EF != "")

  qDOMSE <- map_DOMSE %>%
    left_join(EFtoEFS, by = "EF") %>%
    select(-c("EF")) %>%
    unique()
  names(qDOMSE) <- sub("EFS", "SECtoEF", names(qDOMSE))

  qDOMSE <- paste0(qDOMSE[["SBS"]], ".", qDOMSE[["SECtoEF"]])
  DOMSE <- as.data.frame(qDOMSE)

  # final consumption
  sum_DOMSE <- iCo2EmiFac[, , DOMSE[, 1]] * VConsFuel[, , DOMSE[, 1]]
  
  ############## DOMSE by sector
  DOMSE_by_sector <- dimSums(sum_DOMSE, 3.2, na.rm = TRUE)
  
  items_DOMSE <- df_SBS[df_SBS[,"Code"] %in% getItems(DOMSE_by_sector, 3),]
  DOMSE_by_sector <- toolAggregate(DOMSE_by_sector[,,items_DOMSE[,"Code"]], dim = 3, rel = items_DOMSE, from = "Code", to = "Description")
  
  getItems(DOMSE_by_sector, 3) <- paste0("Emissions|CO2|Energy|Demand|Residential and Commercial|",getItems(DOMSE_by_sector, 3))
  
  DOMSE_by_sector <- add_dimension(DOMSE_by_sector, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, DOMSE_by_sector)
  ##############
  
  sum_DOMSE <- dimSums(sum_DOMSE, 3, na.rm = TRUE)

  getItems(sum_DOMSE, 3) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"

  sum_DOMSE <- add_dimension(sum_DOMSE, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, sum_DOMSE)

  # Emissions|CO2|Energy|Demand|Transportation
  sum_TRANSE <- sum5 # transport

  getItems(sum_TRANSE, 3) <- "Emissions|CO2|Energy|Demand|Transportation"

  sum_TRANSE <- add_dimension(sum_TRANSE, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, sum_TRANSE)

  # Emissions|CO2|Energy|Demand|Bunkers
  sum_Bunkers <- sum7 # Bunkers

  getItems(sum_Bunkers, 3) <- "Emissions|CO2|Energy|Demand|Bunkers"

  sum_Bunkers <- add_dimension(sum_Bunkers, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, sum_Bunkers)

  # Emissions|CO2|Energy|Demand
  # Emissions|CO2|Energy|Demand|Bunkers, sum_Bunkers
  # Emissions|CO2|Energy|Demand|Transportation, sum_TRANSE
  # Emissions|CO2|Energy|Demand|Residential and Commercial, sum_DOMSE
  # Emissions|CO2|Energy|Demand|Industry, sum_INDSE
  sum_Demand <- sum_Bunkers + sum_TRANSE + sum_DOMSE + sum_INDSE

  getItems(sum_Demand, 3) <- "Emissions|CO2|Energy|Demand"

  sum_Demand <- add_dimension(sum_Demand, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, sum_Demand)

  # Emissions|CO2|Energy|Supply
  # input to power generation sector, sum2
  # input to district heating plants, sum3
  # consumption of energy branch, sum4
  # CO2 captured by CCS plants in power generation, sum6
  # Emissions|CO2|Energy|Supply|Hydrogen
  sum_Supply <- sum2 + sum3 + sum4 - sum6 + Emissions_Supply_Hydrogen

  getItems(sum_Supply, 3) <- "Emissions|CO2|Energy|Supply"

  sum_Supply <- add_dimension(sum_Supply, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, sum_Supply)
  
  Emissions_Supply_Electricity <- sum2 + sum4 - sum6
  
  getItems(Emissions_Supply_Electricity, 3) <- "Gross Emissions|CO2|Energy|Supply|Electricity"
  
  Emissions_Supply_Electricity <- add_dimension(Emissions_Supply_Electricity, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, Emissions_Supply_Electricity)

  # Emissions|CO2|Energy
  # Emissions|CO2|Energy|Demand, sum_Demand
  # Emissions|CO2|Energy|Supply, sum_Supply

  sum_Energy <- sum_Demand + sum_Supply

  getItems(sum_Energy, 3) <- "Emissions|CO2|Energy"

  sum_Energy <- add_dimension(sum_Energy, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, sum_Energy)

  # Emissions|CO2|Cumulated

  Cumulated <- as.quitte(total_CO2)

  Cumulated <- Cumulated %>%
    group_by(region) %>%
    mutate(value = cumsum(value)) %>%
    as.data.frame() %>%
    as.quitte() %>%
    as.magpie()
  getItems(Cumulated, 3) <- "Emissions|CO2|Cumulated"

  Cumulated <- Cumulated / 1000
  Cumulated <- add_dimension(Cumulated, dim = 3.2, add = "unit", nm = "Gt CO2")
  magpie_object <- mbind(magpie_object, Cumulated)
  return(magpie_object)
}
