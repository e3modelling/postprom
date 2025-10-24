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
    Land_Use <- readSource("Navigate", subtype = "SUP_NPi_Default")
  } else if (fscenario == 2) {
    Navigate_Emissions <- Navigate_Emissions[, , "SUP_1p5C_Default"][regions, years, ]
    Land_Use <- readSource("Navigate", subtype = "SUP_1p5C_Default")
  } else if (fscenario == 3) {
    Navigate_Emissions <- Navigate_Emissions[, , "SUP_2C_Default"][regions, years, ]
    Land_Use <- readSource("Navigate", subtype = "SUP_2C_Default")
  }

  ####################  Carbon Capture
  Land_Use <- Land_Use[,,c("Carbon Removal|Land Use")]
  Land_Use <- Land_Use[,,"REMIND-MAgPIE 3_2-4_6"]
  
  mapping <- toolGetMapping("regionmappingOPDEV3.csv",
                            type = "regional",
                            where = "mrprom")
  
  Land_Use[is.na(Land_Use)] <- 10^-6
  Land_Use <- toolAggregate(Land_Use,rel  = mapping,from = "ISO3.Code",to   = "Region.Code")
  
  Land_Use <- as.quitte(Land_Use)

  Land_Use <- interpolate_missing_periods(Land_Use, 2010:2100, expand.values = TRUE)
  
  Land_Use <- as.magpie(Land_Use)
  
  Land_Use <- Land_Use[,years,]
  
  # set NA to 0
  Land_Use[is.na(Land_Use)] <- 10^-6
  
  Land_Use <- Land_Use[regions, years, ]
  
  Navigate_Emissions <- mbind(Navigate_Emissions, Land_Use)
  
  Navigate_Emissions <- extractAggregatedData(fscenario, Navigate_Emissions, years)

  Navigate_Emissions <- collapseDim(Navigate_Emissions, 3.1)
  Navigate_Emissions <- collapseDim(Navigate_Emissions, 3.1)
  
  Carbon_Removal_Land_Use <- Navigate_Emissions[,,"Carbon Removal|Land Use.Mt CO2/yr"]
  getItems(Carbon_Removal_Land_Use, 3) <-NULL
  
  l <- getItems(Navigate_Emissions,3.1) == "Carbon Removal|Land Use"
  
  getItems(Navigate_Emissions,3.1)[l] <- "Carbon Capture|Land Use"
  
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
  v04CO2CaptRate <- readGDX(path, name = c("V04CO2CaptRate", "imCO2CaptRate"),
                            field = "l", format = "first_found")[regions, years, ]
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
  district_heating <- VTransfInputDHPlants * iCo2EmiFac[, , "PG"][, , getItems(VTransfInputDHPlants, 3)]
  district_heating <- dimSums(district_heating, 3, na.rm = TRUE)
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
  
  iCo2EmiFac_PG <- iCo2EmiFac[, , "PG"][, , CCS[, 2]]
  iCo2EmiFac_PG <- collapseDim(iCo2EmiFac_PG, 3.1)
  getItems(iCo2EmiFac_PG, 3) <- getItems(VProdElec[, , CCS[, 1]], 3)

  # CO2 captured by CCS plants in power generation
  var_16 <- VProdElec[, , CCS[, 1]] * 0.086 / iPlantEffByType[, , CCS[, 1]] * iCo2EmiFac_PG * v04CO2CaptRate[, , CCS[, 1]]
  emi_factor_ATHBMSCCS <- 4.1868
  ATHBMSCCS <- VProdElec[, , "ATHBMSCCS"] * 0.086 / iPlantEffByType[, ,  "ATHBMSCCS"] * emi_factor_ATHBMSCCS * v04CO2CaptRate[, ,  "ATHBMSCCS"]
  
  car_capt_other_sources <- dimSums(var_16[,,c("ATHCOALCCS","ATHLGNCCS","ATHGASCCS")], dim = 3, na.rm = TRUE)
  
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
  V05CaptRateH2 <- readGDX(path, "V05CaptRateH2", field = 'l')[,,H2CCS][regions, years, ]
  
  if (is.null(V05CaptRateH2)) {
    message("V05CaptRateH2 not found – creating empty V05CaptRateH2 placeholder")
    # Zero placeholder for Carbon Capture
    V05CaptRateH2     <- new.magpie(regions, years, "V05CaptRateH2", fill = 0)
  }
  
  iCo2EmiFac[, , "H2P"][, , "BMSWAS"] <- emi_factor_ATHBMSCCS
  iCo2EmiFac_hydrogen <- collapseDim(iCo2EmiFac[, , "H2P"][, , PGEF[, 1]],3.1)
  tech_hydrogen <- dimSums(VConsFuelTechH2Prod[, , PGEF[, 1]][,,H2CCS] * iCo2EmiFac_hydrogen,3.2)
  hydrogen_CCS <- tech_hydrogen * V05CaptRateH2
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
  
  ################# INDUSTRY SECTOR ##################
  
  # CO2 captured by CCS plants in INDUSTRY SECTOR
  Industry_CCS <- readGDX(path, "V06CapCO2ElecHydr", field = 'l')[regions, years, ]
  
  if (!is.null(getItems(Industry_CCS,3)) & ("IND" %in% getItems(Industry_CCS,3))) {
    Industry_CCS <- Industry_CCS[,,"IND"]
    Industry_CCS <- Industry_CCS
    Industry_CCS <- dimSums(Industry_CCS, 3, na.rm = TRUE)
    
    Industry_CCS_plot_in_mif <- Industry_CCS
    getItems(Industry_CCS_plot_in_mif, 3) <- "Carbon Capture|Industry"
    Industry_CCS_plot_in_mif <- add_dimension(Industry_CCS_plot_in_mif, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    magpie_object <- mbind(magpie_object, Industry_CCS_plot_in_mif)
    
  } else {
    Industry_CCS <- new.magpie(regions, years, fill = 0)
  }
  
  #########       DAC     #################
  
  DAC <- NULL
  VCapDAC <- readGDX(path, "V06CapDAC", field = "l")[regions, years, ]
  
  if (is.null(VCapDAC)) {
    message("V06CapDAC not found – creating empty DAC placeholder")
    
    # Zero placeholder for Carbon Capture
    VCapDAC_total     <- new.magpie(regions, years, "Carbon Capture", fill = 0)
    Enhanced_Weathering <- new.magpie(regions, years, "Carbon Capture|Enhanced Weathering", fill = 0)
    LTDAC             <- new.magpie(regions, years, "Carbon Capture|Direct Air Capture|LTDAC", fill = 0)
    HTDAC             <- new.magpie(regions, years, "Carbon Capture|Direct Air Capture|HTDAC", fill = 0)
    H2DAC             <- new.magpie(regions, years, "Carbon Capture|Direct Air Capture|H2DAC", fill = 0)
    Direct_Air_Capture<- new.magpie(regions, years, "Carbon Capture|Direct Air Capture", fill = 0)
    
    DAC <- mbind(VCapDAC_total, Enhanced_Weathering, LTDAC, HTDAC, Direct_Air_Capture, H2DAC)
    DAC <- add_dimension(DAC, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    
    CDR<- new.magpie(regions, years, "Emissions|CO2|Carbon Dioxide Removal", fill = 0)
    CDR <- add_dimension(CDR, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    magpie_object <- mbind(magpie_object, CDR)
    
    ###############################################
    
  } else {
    VCapDAC <- VCapDAC[regions, years, ]
    
    VCapDAC_total <- dimSums(VCapDAC, dim = 3)
    VCap_total_plot <- VCapDAC_total / 1e6 + Industry_CCS + sum6 + hydrogen_CCS + Carbon_Removal_Land_Use
    getItems(VCap_total_plot, 3) <- "Carbon Capture"
    
    Carbon_Geological_Storage <- dimSums(VCapDAC[,,c("HTDAC","H2DAC","LTDAC")], dim = 3) / 1e6 + Industry_CCS + sum6 + hydrogen_CCS
    getItems(Carbon_Geological_Storage, 3) <- "Carbon Capture|Geological Storage"
    
    other_sources <- car_capt_other_sources + Industry_CCS + hydrogen_CCS
    getItems(other_sources, 3) <- "Carbon Capture|Geological Storage|Other Sources"
    
    biom_car_capture <- ATHBMSCCS
    getItems(biom_car_capture, 3) <- "Carbon Capture|Geological Storage|Biomass"
      
    Enhanced_Weathering <- VCapDAC[,,"EWDAC"]
    getItems(Enhanced_Weathering, 3) <- "Carbon Capture|Enhanced Weathering"
    
    LTDAC <- VCapDAC[,,"LTDAC"]
    getItems(LTDAC, 3) <- "Carbon Capture|Geological Storage|Direct Air Capture|LTDAC"
    
    HTDAC <- VCapDAC[,,"HTDAC"]
    getItems(HTDAC, 3) <- "Carbon Capture|Geological Storage|Direct Air Capture|HTDAC"
    
    
    H2DAC <- VCapDAC[,,"H2DAC"]
    getItems(H2DAC, 3) <- "Carbon Capture|Geological Storage|Direct Air Capture|H2DAC"
    
    Direct_Air_Capture <- dimSums(VCapDAC[,,c("HTDAC","LTDAC","H2DAC")], dim = 3)
    getItems(Direct_Air_Capture, 3) <- "Carbon Capture|Geological Storage|Direct Air Capture"
    
    DAC <- mbind(Enhanced_Weathering, LTDAC, HTDAC, Direct_Air_Capture, H2DAC)
    DAC <- DAC / 1e6
    DAC <- add_dimension(DAC, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    
    VCap_total_plot <- add_dimension(VCap_total_plot, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    magpie_object <- mbind(magpie_object, VCap_total_plot)
    
    Carbon_Geological_Storage <- add_dimension(Carbon_Geological_Storage, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    other_sources <- add_dimension(other_sources, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    biom_car_capture <- add_dimension(biom_car_capture, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    
    magpie_object <- mbind(magpie_object, Carbon_Geological_Storage,other_sources,biom_car_capture)
    
    #######################################
    #cdr for plotting
    CDR <- - dimSums(VCapDAC, dim = 3) / 10^6
    CDR <- dimSums(CDR, dim = 3)
    getItems(CDR, 3) <- "Emissions|CO2|Carbon Dioxide Removal"
    CDR <- add_dimension(CDR, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    magpie_object <- mbind(magpie_object, CDR)
    
    ###############################################
  }
  
  magpie_object <- mbind(magpie_object, DAC)
  
  getItems(VCapDAC_total, 3) <- NULL
  
  VCapDAC_total <- VCapDAC_total * (-1)
  
  #################################################
  
  ############## TOTAL  ##############
  
  # add VmTransfInputCHPlants to Emissions|CO2|Energy|Supply|Heat
  VmTransfInputCHPlants <- readGDX(path, "VmTransfInputCHPlants", field = 'l')[regions, years, ]
  
  district_heating <- dimSums(district_heating,3)
  
  if (!is.null(VmTransfInputCHPlants)) {
    VmTransfInputCHPlants <- VmTransfInputCHPlants * iCo2EmiFac[, , "PG"][, , getItems(VmTransfInputCHPlants, 3)]
    VmTransfInputCHPlants <- dimSums(VmTransfInputCHPlants, 3, na.rm = TRUE)
    supply_Heat <- VmTransfInputCHPlants + district_heating
    
    getItems(VmTransfInputCHPlants, 3) <- "Emissions|CO2|Energy|Supply|Heat|CHPlants"
    VmTransfInputCHPlants <- add_dimension(VmTransfInputCHPlants, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    magpie_object <- mbind(magpie_object, VmTransfInputCHPlants)
    
  } else {
    supply_Heat <- district_heating
  }

  total_CO2 <- sum1 + sum2 + supply_Heat + sum4 + sum5 - sum6 + sum7 + remind + hydrogen - hydrogen_CCS + (VCapDAC_total / 10^6) - Industry_CCS

  getItems(total_CO2, 3) <- "Emissions|CO2"

  total_CO2 <- add_dimension(total_CO2, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, total_CO2, Navigate_Emissions)
  
  ####################################
  
  ########for plotting total co2
  
  plotting_total_co2 <- total_CO2
  getItems(plotting_total_co2, 3.1) <- "Emissions|CO2|"
  magpie_object <- mbind(magpie_object, plotting_total_co2)
  
  ########################

  # Hydrogen
  Hydrogen_total <- hydrogen - hydrogen_CCS

  getItems(Hydrogen_total, 3) <- "Emissions|CO2|Energy|Supply|Hydrogen"
  
  Emissions_Supply_Hydrogen <- Hydrogen_total

  Hydrogen_total <- add_dimension(Hydrogen_total, dim = 3.2, add = "unit", nm = "Mt CO2/yr")

  magpie_object <- mbind(magpie_object, Hydrogen_total)
  
  # hydrogen_CCS for plotting
  hydrogen_CCS_plot <- hydrogen_CCS
  
  getItems(hydrogen_CCS_plot, 3) <- "Carbon Capture|Hydrogen"
  
  hydrogen_CCS_plot <- add_dimension(hydrogen_CCS_plot, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  
  magpie_object <- mbind(magpie_object, hydrogen_CCS_plot)
  ##############

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

  sum_INDSE <- sum_INDSE - Industry_CCS
  
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
  # input to district heating plants + VmTransfInputCHPlants = supply_Heat
  # consumption of energy branch, sum4
  # CO2 captured by CCS plants in power generation, sum6
  # Emissions|CO2|Energy|Supply|Hydrogen
  sum_Supply <- sum2 + supply_Heat + sum4 - sum6 + Emissions_Supply_Hydrogen

  getItems(sum_Supply, 3) <- "Emissions|CO2|Energy|Supply"

  sum_Supply <- add_dimension(sum_Supply, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, sum_Supply)
  
  Emissions_Supply_Electricity <- sum2 + sum4 - sum6
  
  getItems(Emissions_Supply_Electricity, 3) <- "Emissions|CO2|Energy|Supply|Electricity"
  
  Emissions_Supply_Electricity <- add_dimension(Emissions_Supply_Electricity, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, Emissions_Supply_Electricity)
  
  carbon_capture_electricity <- sum6

  getItems(carbon_capture_electricity, 3) <- "Carbon Capture|Electricity"
  carbon_capture_electricity <- add_dimension(carbon_capture_electricity, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, carbon_capture_electricity)
  
  getItems(district_heating, 3) <- "Emissions|CO2|Energy|Supply|Heat|District Heating"
  district_heating <- add_dimension(district_heating, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, district_heating)
  
  own_consumption <- sum4
  
  getItems(own_consumption, 3) <- "Emissions|CO2|Energy|Supply|Own Consumption"
  own_consumption <- add_dimension(own_consumption, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, own_consumption)
  
  # Combine all summed fuels into a single magpie object for supply category
  supply_category <- do.call(mbind, fuel_sums)
  
  BALEF2EFS <- readGDX(path, "BALEF2EFS")
  names(BALEF2EFS) <- c("BAL", "EF")
  BALEF2EFS[["BAL"]] <- gsub("Gas fuels", "Gases", BALEF2EFS[["BAL"]])
  BALEF2EFS[["BAL"]] <- gsub("Steam", "Heat", BALEF2EFS[["BAL"]])
  
  Liquids <- BALEF2EFS[BALEF2EFS[["BAL"]] == "Liquids", ]
  Solids <- BALEF2EFS[BALEF2EFS[["BAL"]] == "Solids", ]
  Gases <- BALEF2EFS[BALEF2EFS[["BAL"]] == "Gases", ]
  Heat <- BALEF2EFS[BALEF2EFS[["BAL"]] == "Heat", ]
  
  supply_Liquids <- supply_category[,,Liquids[["EF"]]]
  supply_Solids <- supply_category[,,Solids[["EF"]]]
  supply_Gases <- supply_category[,,Gases[["EF"]]]
  #supply_Heat <- supply_category[,,Heat[["EF"]]]
  
  supply_Liquids <- dimSums(supply_Liquids, 3, na.rm = TRUE)
  supply_Solids <- dimSums(supply_Solids, 3, na.rm = TRUE)
  supply_Gases <- dimSums(supply_Gases, 3, na.rm = TRUE)
  #supply_Heat <- dimSums(supply_Heat, 3, na.rm = TRUE)
  
  getItems(supply_Liquids, 3) <- paste0("Emissions|CO2|Energy|Supply|Liquids")
  getItems(supply_Solids, 3) <- paste0("Emissions|CO2|Energy|Supply|Solids")
  getItems(supply_Gases, 3) <- paste0("Emissions|CO2|Energy|Supply|Gases")
  getItems(supply_Heat, 3) <- paste0("Emissions|CO2|Energy|Supply|Heat")
  
  supply_per_category <- mbind(supply_Liquids, supply_Solids, supply_Gases, supply_Heat)
  
  supply_per_category <- add_dimension(supply_per_category, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  
  magpie_object <- mbind(magpie_object, supply_per_category)

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
# Helpers -------------------------------------------------------------
extractAggregatedData <- function(scenario,x,years, ...) {
  
  map <- toolGetMapping(name = "NavigateEmissions.csv",
                      type = "sectoral",
                      where = "mrprom")
  
  new_row <- data.frame(
    Emissions = "Carbon Removal|Land Use",
    stringsAsFactors = FALSE
  )
  
  # Combine with the existing map
  map <- rbind(map, new_row)
  
  # Get the aggregated data for World, LAM etc
  if (scenario %in% c(0, 1)) {
    xa <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)
  } else if (scenario == 2) {
    xa <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = FALSE)
  } else if (scenario == 3) {
    xa <- readSource("Navigate", subtype = "SUP_2C_Default", convert = FALSE)
  }
 
  xa <- xa[,,map[,"Emissions"]]
  xa <- xa[,years,"REMIND-MAgPIE 3_2-4_6"]
  xa <- as.quitte(xa)
  xa <- interpolate_missing_periods(xa, 2010:2100, expand.values = TRUE)
  xa <- as.magpie(xa)
  xa <- xa[,years,"REMIND-MAgPIE 3_2-4_6"]

  desiredRegions <- c(
  "REMIND 3_2|Canada, Australia, New Zealand",
  "REMIND 3_2|Latin America and the Caribbean",
  "REMIND 3_2|Middle East and North Africa",
  "REMIND 3_2|Non-EU28 Europe",
  "REMIND 3_2|Other Asia",
  "REMIND 3_2|Russia and Reforming Economies",
  "REMIND 3_2|Sub-Saharan Africa"
  )

  # If only RWO is needed (e.g., DEV mode), then substract the regions (e.g., USA) from the world.

    xa<- xa[desiredRegions,,]
    # Mapping
    RegionMap <- c(
    "REMIND 3_2|Canada, Australia, New Zealand" = "CAZ",
    "REMIND 3_2|EU 28"                           = "ELL",
    "REMIND 3_2|Latin America and the Caribbean" = "LAM",
    "REMIND 3_2|Middle East and North Africa"   = "MEA",
    "REMIND 3_2|Non-EU28 Europe"                = "NEU",
    "REMIND 3_2|Other Asia"                      = "OAS",
    "REMIND 3_2|Russia and Reforming Economies"  = "REF",
    "REMIND 3_2|Sub-Saharan Africa"             = "SSA"
    )

    commonRegions <- intersect(RegionMap[getRegions(xa)], getRegions(x))
    xaRegions <- names(RegionMap)[RegionMap %in% commonRegions]
    matchRegions <- RegionMap[xaRegions]

    for (i in seq_along(matchRegions)) {
      x[matchRegions[i], , ] <- xa[xaRegions[i], , ]
    }

    # Check if 'RWO' exists as a region and then compute RWO as World - sum of countries
    if ("RWO" %in% getItems(x,1)) {
      
      extracted <- xa['World',,]
      withoutRWO <- x[!getItems(x,1) %in% "RWO", , ]
      mainCountriesSum <- dimSums(withoutRWO, dim = 1)
      newRWO <- extracted - mainCountriesSum
      getItems(newRWO,1) <- "RWO"
      
      x <- mbind(withoutRWO, newRWO)
      
    } else {
      print("Rest of the world (RWO) is missing from magpie object!")
    }


  # set NA to 0
  x[is.na(x)] <- 10^-6

  return(x)
}
