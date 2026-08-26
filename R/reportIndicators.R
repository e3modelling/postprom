#' Energy Efficiency (GDP/TFC) 
#' Energy intensity (TFC/GDP)
#'
#' @param reports Magpie object created from postprom.
#' @return A magpie object containing Indicators
#'
#' @examples
#' \dontrun{
#' result <- reportIndicators(reports)
#' }
#'
#' @importFrom magclass getItems dimSums add_dimension mbind collapseDim
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter left_join mutate select group_by %>% arrange ungroup rename
#' @export
reportIndicators <- function(reports, path, regions, years, blabla_regions) {
  
  items <- getItems(reports,3)
  
  # ============ Energy demand/ activity====================
  
  IFullACTV <- calcOutput("IFullACTV", aggregate = TRUE, regionmapping = "regionmappingOPDEV5.csv")
  IFullACTV <- IFullACTV[blabla_regions, years, ]
  
  unitsPassenger <- getItems(IFullACTV,3)
  
  IFullACTV <- collapseDim(IFullACTV, dim = 3.2)
  
  if ("World" %in% regions) {
    # Calculate the sum, World
    add_region_GLO_IFullACTV <- dimSums(IFullACTV, 1, na.rm = TRUE)
    getItems(add_region_GLO_IFullACTV, 1) <- "World"
    IFullACTV <- mbind(IFullACTV, add_region_GLO_IFullACTV)
  }
  
  if ("EU" %in% regions) {
    # --- Calculate EU-27 Aggregation ---
    regionMapping <- toolGetMapping(name = "EU28.csv", type = "regional", where = "mrprom")
    regionsEu27 <- regionMapping$ISO3.Code[regionMapping$ISO3.Code != "GBR"]
    regionsEu27 <- regionsEu27[regionsEu27 %in% blabla_regions] # Ensure only regions present in the data are included
    
    if (length(regionsEu27) != 0) {
      add_region_EU_IFullACTV <- dimSums(IFullACTV[regionsEu27,,], 1, na.rm = TRUE)
      getItems(add_region_EU_IFullACTV, 1) <- "EU"
      IFullACTV <- mbind(IFullACTV, add_region_EU_IFullACTV)
    }
  }
  
  mappingACTV <- tribble(
    ~variable, ~code,
    "Final Energy|Industry|Iron and Steel", "IS",
    "Final Energy|Industry|Non Ferrous Metals", "NF",
    "Final Energy|Industry|Chemicals", "CH",
    "Final Energy|Industry|Non Metallic Minerals", "BM",
    "Final Energy|Industry|Paper and Pulp", "PP",
    "Final Energy|Industry|Food Drink and Tobacco", "FD",
    "Final Energy|Industry|Engineering", "EN",
    "Final Energy|Industry|Textiles", "TX",
    "Final Energy|Industry|Ore Extraction", "OE",
    "Final Energy|Industry|Other Industrial sectors", "OI",
    "Final Energy|Commercial|Services", "SE",
    "Final Energy|Agriculture, Fishing, Forestry", "AG",
    "Final Energy|Residential", "HOU",
    "Final Energy|Bunkers", "BU",
    "Final Energy|Non-Energy Use|Petrochemicals Industry", "PCH",
    "Final Energy|Non-Energy Use|Other Non Energy Uses", "NEN")
  
  # -------------------------- Commercial +  Residential -------
  Com_Res <- reports[, , c("Final Energy|Residential", "Final Energy|Commercial|Services")]
  
  Com_Res <- dimSums(Com_Res, 3)
  getItems(Com_Res, 3.1) <- "Final Energy|Residential and Commercial"
  getItems(Com_Res, 3.2) <- "Mtoe"
  
  Com_Res_ACTV <- IFullACTV[, , c("SE", "HOU")]
  
  Com_Res_ACTV <- dimSums(Com_Res_ACTV, 3)
  getItems(Com_Res_ACTV, 3.1) <- "ACTV|Residential and Commercial"
  
  Energy_Intensity_Com_Res <- Com_Res / Com_Res_ACTV
  
  Energy_Intensity_Com_Res <- collapseDim(Energy_Intensity_Com_Res, dim = 3.3)
  Energy_Intensity_Com_Res <- collapseDim(Energy_Intensity_Com_Res, dim = 3.2)
  
  getItems(Energy_Intensity_Com_Res, dim = 3) <- paste0("Energy Intensity|Residential and Commercial")
  
  Energy_Intensity_Com_Res <- add_dimension(Energy_Intensity_Com_Res, dim = 3.2, add = "unit", nm = "Mtoe/billion $")
  Energy_Intensity_Com_Res[is.na(Energy_Intensity_Com_Res)] <- 0
  ################
  
  FEACTV <- NULL
  
  for (i in seq_len(nrow(mappingACTV))) {
    
    tmp <- reports[, , mappingACTV$variable[i]] /
      IFullACTV[, , mappingACTV$code[i]]
    
    FEACTV <- mbind(FEACTV, tmp)
  }
  
  FEACTV <- collapseDim(FEACTV, dim = 3.3)
  FEACTV <- collapseDim(FEACTV, dim = 3.2)
  
  getItems(FEACTV, dim = 3) <- sub(
    "^Final Energy\\|",
    "",
    getItems(FEACTV, dim = 3)
  )
  
  getItems(FEACTV, dim = 3) <- paste0("Energy Intensity|",getItems(FEACTV, dim = 3))
  
  FEACTV <- add_dimension(FEACTV, dim = 3.2, add = "unit", nm = "Mtoe/billion $")
  FEACTV[is.na(FEACTV)] <- 0
  
  FEACTV <- mbind(FEACTV, Energy_Intensity_Com_Res)
  
  transitionIndicators <- calculateTransitionIndicators(reports)
  EnergyEfficiency <- transitionIndicators$energyEfficiency
  EnergyIntensity <- transitionIndicators$finalEnergyIntensity
  EmissionsIntensity <- transitionIndicators$economyWideCo2Intensity
  PrimaryEnergyEfficiency <- transitionIndicators$primaryEnergyEfficiency
  PrimaryEnergyIntensity <- transitionIndicators$primaryEnergyIntensity
  PrimaryEnergyCarbonIntensity <- transitionIndicators$primaryEnergyCarbonIntensity
  EnergyCo2GdpIntensity <- transitionIndicators$energyCo2GdpIntensity
  PrimaryEnergyFossilShare <- transitionIndicators$primaryEnergyFossilShare
  ElectricityshareFE <- transitionIndicators$electricityShare
  CO2Intensityindicators <- transitionIndicators$secondaryEnergyCarbonIntensity
  
  # ============ Energy intensity (TES/GDP) =============================
  Energy <- reports[,,c("GDP|PPP.billion US$2015/yr", "Primary Energy.Mtoe",
                        "Trade|Import|Primary Energy.Mtoe", "Trade|Export|Primary Energy.Mtoe",
                        "Trade|Import|Secondary Energy.Mtoe", "Trade|Export|Secondary Energy.Mtoe",
                        "Final Energy|Bunkers.Mtoe")]
  Energy <- collapseDim(Energy, dim = 3.2)
  imports <- Energy[,,"Trade|Import|Primary Energy"] + Energy[,,"Trade|Import|Secondary Energy"]
  exports <- Energy[,,"Trade|Export|Primary Energy"] + Energy[,,"Trade|Export|Secondary Energy"]
  TES <- Energy[,,"Primary Energy"] + imports - exports - Energy[,,"Final Energy|Bunkers"]
  TESEnergyIntensity  <- TES / Energy[,,"GDP|PPP"]
  getItems(TESEnergyIntensity, 3) <- "Energy Intensity"
  names(dimnames(TESEnergyIntensity))[3] <- "TESEnergyIntensity"
  TESEnergyIntensity <- add_dimension(TESEnergyIntensity, dim = 3.2, add = "unit", nm = "Mtoe/billion US$2015")
  # ============ RES share in power generation missing (RES/TOTAL) =============================
  RESSec  <-  reports[,,"Secondary Energy|Electricity|Renewables"]
  RESSec <- collapseDim(RESSec, 3)
  SecTotal <- reports[,,"Secondary Energy|Electricity"]
  SecTotal <- collapseDim(SecTotal, 3)
  RESSecShare <- RESSec / SecTotal
  getItems(RESSecShare, 3.1) <- "Secondary Energy|Electricity|Renewables Share"
  getItems(RESSecShare, 3.2) <- "1"
  names(dimnames(RESSecShare))[3] <- "SecondaryElectricityRenewables"
  # ============ CO2 intensity of energy demand by sector============
  emi_demand_level5_same <- c("Emissions|CO2|Energy|Demand|Industry.Mt CO2/yr",
                              "Emissions|CO2|Energy|Demand|Commercial.Mt CO2/yr",
                              "Emissions|CO2|Energy|Demand|Agriculture, Fishing, Forestry.Mt CO2/yr",
                              "Emissions|CO2|Energy|Demand|Residential.Mt CO2/yr",
                              "Emissions|CO2|Energy|Demand|Transportation.Mt CO2/yr",
                              "Emissions|CO2|Energy|Demand|Bunkers.Mt CO2/yr")
  
  FE_demand_level2 <-  c("Final Energy|Industry.Mtoe",
                         "Final Energy|Commercial.Mtoe",
                         "Final Energy|Agriculture, Fishing, Forestry.Mtoe",
                         "Final Energy|Residential.Mtoe",
                         "Final Energy|Transportation.Mtoe",
                         "Final Energy|Bunkers.Mtoe")
  
  CO2FEIntensity <- reports[,,c(emi_demand_level5_same, FE_demand_level2)]
  CO2FEIntensity <- collapseDim(CO2FEIntensity, dim = 3.2)
  emi_demand_level5_same <- sub("\\.[^.]+$", "", emi_demand_level5_same)
  FE_demand_level2 <- sub("\\.[^.]+$", "", FE_demand_level2)
  
  # -------------------------- Commercial +  Residential -------
  Com_Res <- reports[, , c("Final Energy|Residential and Commercial", "Final Energy|Commercial|Services")]
  Com_Res <- dimSums(Com_Res, 3)
  getItems(Com_Res, 3.1) <- "Final Energy|Residential and Commercial"
  
  Com_ResEmissions <- reports[, , c("Emissions|CO2|Energy|Demand|Residential and Commercial",
                                    "Emissions|CO2|Energy|Demand|Commercial")]
  
  Com_ResEmissions <- dimSums(Com_ResEmissions, 3)
  getItems(Com_ResEmissions, 3.1) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
  ##########
  CO2FEIntensity <- mbind(CO2FEIntensity, Com_ResEmissions, Com_Res)
  
  CO2FEIntensity <- CO2FEIntensity[, , c(emi_demand_level5_same, "Emissions|CO2|Energy|Demand|Residential and Commercial")] / CO2FEIntensity[, , c(FE_demand_level2, "Final Energy|Residential and Commercial")]
  
  items_z <- getItems(CO2FEIntensity, 3)
  
  emi_catDemand <- sub("^Emissions\\|CO2\\|Energy\\|Demand\\|([^.]*)\\..*$", "\\1", items_z)
  FE_cat <- sub("^.*\\.Final Energy\\|", "", items_z)
  
  same_items_FE <- items_z[emi_catDemand == FE_cat]
  
  CO2FEIntensityindicators <- CO2FEIntensity[, , same_items_FE]
  
  catsFE <- sub("^.*\\.Final Energy\\|", "", getItems(CO2FEIntensityindicators, 3))
  
  getItems(CO2FEIntensityindicators, 3) <- paste0(
    "Carbon Intensity|Final Energy|",
    catsFE
  )
  
  names(dimnames(CO2FEIntensityindicators))[3] <- "CO2FEIntensityindicators"
  CO2FEIntensityindicators <- add_dimension(CO2FEIntensityindicators, dim = 3.2, add = "unit", nm = "Mt CO2/Mtoe")
  # ============ CO2 intensity of INDUSTRY (Emissions/Useful Energy)============
  CO2DemandIndustry <- reports[,,"Emissions|CO2|Energy|Demand|Industry.Mt CO2/yr"]
  CO2DemandIndustry <- collapseDim(CO2DemandIndustry, dim = 3.2)
  variables <- readGDX(
    path,
    c(
      "V02DemSubUsefulSubsec", "V02UsefulElecNonSubIndTert"
    ),
    field = "l"
  )
  
  V02DemSubUsefulSubsec <- variables$V02DemSubUsefulSubsec[blabla_regions, years,]
  V02UsefulElecNonSubIndTert <- variables$V02UsefulElecNonSubIndTert[blabla_regions, years,]
  
  UsefulEnergy  <- V02DemSubUsefulSubsec + V02UsefulElecNonSubIndTert
  DSBSIndustry <- readGDX(path, "INDSE")
  UsefulEnergyIndustry <- UsefulEnergy[,,DSBSIndustry]
  UsefulEnergyIndustry <- dimSums(UsefulEnergyIndustry, dim = 3)
  
  if ("World" %in% regions) {
    # Calculate the sum, World
    add_region_GLO <- dimSums(UsefulEnergyIndustry, 1, na.rm = TRUE)
    getItems(add_region_GLO, 1) <- "World"
    UsefulEnergyIndustry <- mbind(UsefulEnergyIndustry, add_region_GLO)
  }
  
  if ("EU" %in% regions) {
    # --- Calculate EU-27 Aggregation ---
    regionMapping <- toolGetMapping(name = "EU28.csv", type = "regional", where = "mrprom")
    regionsEu27 <- regionMapping$ISO3.Code[regionMapping$ISO3.Code != "GBR"]
    regionsEu27 <- regionsEu27[regionsEu27 %in% blabla_regions] # Ensure only regions present in the data are included
    
    if (length(regionsEu27) != 0) {
      add_region_EU <- dimSums(UsefulEnergyIndustry[regionsEu27,,], 1, na.rm = TRUE)
      getItems(add_region_EU, 1) <- "EU"
      UsefulEnergyIndustry <- mbind(UsefulEnergyIndustry, add_region_EU)
    }
  }
  
  CO2IntensityofIndustry <- CO2DemandIndustry / UsefulEnergyIndustry
  getItems(CO2IntensityofIndustry, 3) <- "Carbon Intensity|Energy|Demand|Industry"
  names(dimnames(CO2IntensityofIndustry))[3] <- "CO2IntensityofIndustry"
  CO2IntensityofIndustry <- add_dimension(CO2IntensityofIndustry, dim = 3.2, add = "unit", nm = "Mt CO2/Mtoe")
  
  # ============Energy intensity of industry (TFC industry/Useful Energy)============
  FEIndustry <- reports[,,"Final Energy|Industry.Mtoe"]
  FEIndustry <- collapseDim(FEIndustry, dim = 3.2)
  
  EnergyIntensityofIndustry <- FEIndustry / UsefulEnergyIndustry
  getItems(EnergyIntensityofIndustry, 3) <- "Energy Intensity|Industry"
  names(dimnames(EnergyIntensityofIndustry))[3] <- "EnergyIntensityofIndustry"
  EnergyIntensityofIndustry <- add_dimension(EnergyIntensityofIndustry, dim = 3.2, add = "unit", nm = "1")
  
  # ============Energy intensity of Transportation============
  variablesACTVTransport <- readGDX(
    path,
    c(
      "v01ActivPassTrnsp", "V01ActivGoodsTransp"
    ),
    field = "l"
  )
  v01ActivPassTrnsp <- variablesACTVTransport$V01ActivPassTrnsp[blabla_regions,years,]
  V01ActivGoodsTransp <- variablesACTVTransport$V01ActivGoodsTransp[blabla_regions,years,]
  if ("World" %in% regions) {
    # Calculate the sum, World
    add_region_GLO_v01ActivPassTrnsp <- dimSums(v01ActivPassTrnsp, 1, na.rm = TRUE)
    getItems(add_region_GLO_v01ActivPassTrnsp, 1) <- "World"
    v01ActivPassTrnsp <- mbind(v01ActivPassTrnsp, add_region_GLO_v01ActivPassTrnsp)
    add_region_GLO_V01ActivGoodsTransp <- dimSums(V01ActivGoodsTransp, 1, na.rm = TRUE)
    getItems(add_region_GLO_V01ActivGoodsTransp, 1) <- "World"
    V01ActivGoodsTransp <- mbind(V01ActivGoodsTransp, add_region_GLO_V01ActivGoodsTransp)
  }
  
  if ("EU" %in% regions) {
    # --- Calculate EU-27 Aggregation ---
    regionMapping <- toolGetMapping(name = "EU28.csv", type = "regional", where = "mrprom")
    regionsEu27 <- regionMapping$ISO3.Code[regionMapping$ISO3.Code != "GBR"]
    regionsEu27 <- regionsEu27[regionsEu27 %in% blabla_regions] # Ensure only regions present in the data are included
    
    if (length(regionsEu27) != 0) {
      add_region_EU_v01ActivPassTrnsp <- dimSums(v01ActivPassTrnsp[regionsEu27,,], 1, na.rm = TRUE)
      getItems(add_region_EU_v01ActivPassTrnsp, 1) <- "EU"
      v01ActivPassTrnsp <- mbind(v01ActivPassTrnsp, add_region_EU_v01ActivPassTrnsp)
      add_region_EU_V01ActivGoodsTransp <- dimSums(V01ActivGoodsTransp[regionsEu27,,], 1, na.rm = TRUE)
      getItems(add_region_EU_V01ActivGoodsTransp, 1) <- "EU"
      V01ActivGoodsTransp <- mbind(V01ActivGoodsTransp, add_region_EU_V01ActivGoodsTransp)
    }
  }
  
  # -------------------------- Transport Passenger -------
  mappingTransport <- tribble(
    ~variable, ~code,
    "Final Energy|Transportation|Passenger Transport - Cars", "PC",
    "Final Energy|Transportation|Passenger Transport - Busses", "PB",
    "Final Energy|Transportation|Passenger Transport - Rail", "PT",
    "Final Energy|Transportation|Passenger Transport - Inland Navigation", "PN",
    "Final Energy|Transportation|Passenger Transport - Aviation", "PA",
    "Final Energy|Transportation|Goods Transport - Trucks", "GU",
    "Final Energy|Transportation|Goods Transport - Rail", "GT",
    "Final Energy|Transportation|Goods Transport - Inland Navigation", "GN")
  
  TRANP <- reports[, , c("Final Energy|Transportation|Passenger Transport - Cars",
                         "Final Energy|Transportation|Passenger Transport - Busses",
                         "Final Energy|Transportation|Passenger Transport - Rail",
                         "Final Energy|Transportation|Passenger Transport - Inland Navigation",
                         "Final Energy|Transportation|Passenger Transport - Aviation")]
  
  TRANP <- collapseDim(TRANP, 3.2)
  
  PassengerFE <- dimSums(TRANP, 3)
  getItems(PassengerFE, 3.1) <- "Final Energy|Transportation|Passenger"
  
  v01ActivPassTrnsp <- toolAggregate(v01ActivPassTrnsp, weight = NULL, dim = 3,
                                     rel = mappingTransport,from = "code",to = "variable")
  v01ActivPassTrnsp <- v01ActivPassTrnsp[,,getItems(TRANP, 3)]
  # -------------------------- Transport Freight -------
  TRANG <- reports[, , c("Final Energy|Transportation|Goods Transport - Trucks",
                         "Final Energy|Transportation|Goods Transport - Rail",
                         "Final Energy|Transportation|Goods Transport - Inland Navigation")]
  TRANG <- collapseDim(TRANG, 3.2)
  
  FreightFE <- dimSums(TRANG, 3)
  getItems(FreightFE, 3.1) <- "Final Energy|Transportation|Freight"
  
  V01ActivGoodsTransp <- toolAggregate(V01ActivGoodsTransp, weight = NULL, dim = 3,
                                       rel = mappingTransport,from = "code",to = "variable")
  V01ActivGoodsTransp <- V01ActivGoodsTransp[,,getItems(TRANG, 3)]
  # -------------------------- 
  
  ActivPassTrnsp <- TRANP / v01ActivPassTrnsp
  ActivGoodsTransp <-  TRANG / V01ActivGoodsTransp
  
  unitsActivPassTrnsp <- paste0("Mtoe/",sub("^[^.]+\\.","",unitsPassenger[
    match(c("PC", "PB", "PT", "PN", "PA"),sub("\\..*", "", unitsPassenger))]))
  
  ActivPassTrnsp <- mbind(
    lapply(seq_along(unitsActivPassTrnsp), function(i) {
      add_dimension(ActivPassTrnsp[, , i],dim = 3.2,add = "unit",nm = unitsActivPassTrnsp[i])}))
  
  ActivGoodsTransp <- add_dimension(ActivGoodsTransp, dim = 3.2, add = "unit", nm = "Mtoe/Gtkm")
  
  ActivTrnsp <- mbind(ActivPassTrnsp, ActivGoodsTransp)
  
  getItems(ActivTrnsp, dim = 3.1) <- sub(
    "^Final Energy\\|",
    "",
    getItems(ActivTrnsp, dim = 3.1)
  )
  
  getItems(ActivTrnsp, dim = 3.1) <- paste0("Energy Intensity|",getItems(ActivTrnsp, dim = 3.1))
  
  getItems(PassengerFE, 3.2) <- "Mtoe"
  getItems(FreightFE, 3.2) <- "Mtoe"
  
  ActivTrnsp <- mbind(ActivTrnsp, PassengerFE, FreightFE)
  
  # ==================== Combine all indicators into a single magpie object ============================
  magpie_object <- mbind(
    EnergyEfficiency,
    EnergyIntensity,
    PrimaryEnergyEfficiency,
    PrimaryEnergyIntensity,
    PrimaryEnergyCarbonIntensity,
    EnergyCo2GdpIntensity,
    PrimaryEnergyFossilShare,
    CO2Intensityindicators,
    CO2IntensityofIndustry,
    EnergyIntensityofIndustry,
    FEACTV,
    ActivTrnsp,
    EmissionsIntensity,
    RESSecShare,
    ElectricityshareFE,
    CO2FEIntensityindicators,
    TESEnergyIntensity
  )
  
  magpie_object[is.na(magpie_object) | is.infinite(magpie_object)] <- 0
  
  return(magpie_object)
}

# Calculate the transition indicators used by result validation. This helper is
# deliberately called only by reportIndicators(); validation reads its outputs.
calculateTransitionIndicators <- function(reports) {
  energy <- reports[, , c(
    "GDP|PPP.billion US$2015/yr",
    "Final Energy.Mtoe",
    "Final Energy|Electricity.Mtoe",
    "Emissions|CO2.Mt CO2/yr",
    "Emissions|CO2|Energy.Mt CO2/yr",
    "Primary Energy.Mtoe",
    "Primary Energy|Coal.Mtoe",
    "Primary Energy|Gas.Mtoe",
    "Primary Energy|Oil.Mtoe"
  )]
  energy <- collapseDim(energy, dim = 3.2)
  
  addIndicatorMetadata <- function(x, variable, dimension, unit) {
    getItems(x, 3) <- variable
    names(dimnames(x))[3] <- dimension
    add_dimension(x, dim = 3.2, add = "unit", nm = unit)
  }
  
  energyEfficiency <- addIndicatorMetadata(
    energy[, , "GDP|PPP"] / energy[, , "Final Energy"],
    "Efficiency|Final Energy", "EnergyEfficiency",
    "billion US$2015/Mtoe"
  )
  finalEnergyIntensity <- addIndicatorMetadata(
    energy[, , "Final Energy"] / energy[, , "GDP|PPP"],
    "Intensity|Final Energy", "EnergyIntensity",
    "Mtoe/billion US$2015"
  )
  economyWideCo2Intensity <- addIndicatorMetadata(
    energy[, , "Emissions|CO2"] / energy[, , "GDP|PPP"],
    "Carbon Intensity|GDP", "EnergyIntensityCO2",
    "Mt CO2/billion US$2015"
  )
  primaryEnergyEfficiency <- addIndicatorMetadata(
    energy[, , "GDP|PPP"] / energy[, , "Primary Energy"],
    "Efficiency|Primary Energy", "PrimaryEnergyEfficiency",
    "billion US$2015/Mtoe"
  )
  primaryEnergyIntensity <- addIndicatorMetadata(
    energy[, , "Primary Energy"] / energy[, , "GDP|PPP"],
    "Intensity|Primary Energy", "PrimaryEnergyIntensity",
    "Mtoe/billion US$2015"
  )
  primaryEnergyCarbonIntensity <- addIndicatorMetadata(
    energy[, , "Emissions|CO2|Energy"] / energy[, , "Primary Energy"],
    "Carbon Intensity|Primary Energy", "PrimaryEnergyCarbonIntensity",
    "Mt CO2/Mtoe"
  )
  energyCo2GdpIntensity <- addIndicatorMetadata(
    energy[, , "Emissions|CO2|Energy"] / energy[, , "GDP|PPP"],
    "Carbon Intensity|GDP|Energy", "EnergyCo2GdpIntensity",
    "Mt CO2/billion US$2015"
  )
  fossilPrimaryEnergy <- energy[, , "Primary Energy|Coal"] +
    energy[, , "Primary Energy|Gas"] +
    energy[, , "Primary Energy|Oil"]
  primaryEnergyFossilShare <- addIndicatorMetadata(
    fossilPrimaryEnergy / energy[, , "Primary Energy"],
    "Primary Energy|Fossil Share", "PrimaryEnergyFossilShare", "1"
  )
  electricityShare <- addIndicatorMetadata(
    energy[, , "Final Energy|Electricity"] / energy[, , "Final Energy"],
    "Final Energy|Electricity Share", "ElectricityshareFE", "1"
  )
  
  emissionVariables <- c(
    "Emissions|CO2|Energy|Supply|Electricity.Mt CO2/yr",
    "Emissions|CO2|Energy|Supply|Hydrogen.Mt CO2/yr",
    "Emissions|CO2|Energy|Supply|Heat.Mt CO2/yr",
    "Emissions|CO2|Energy|Supply|Liquids.Mt CO2/yr",
    "Emissions|CO2|Energy|Supply|Gases.Mt CO2/yr",
    "Emissions|CO2|Energy|Supply|Solids.Mt CO2/yr"
  )
  secondaryEnergyVariables <- c(
    "Secondary Energy|Electricity.TWh",
    "Secondary Energy|Hydrogen.TWh",
    "Secondary Energy|Heat.TWh",
    "Secondary Energy|Liquids.TWh",
    "Secondary Energy|Gases.TWh",
    "Secondary Energy|Solids.TWh"
  )
  secondaryEnergy <- reports[, , c(emissionVariables, secondaryEnergyVariables)]
  secondaryEnergy <- collapseDim(secondaryEnergy, dim = 3.2)
  emissionNames <- sub("\\.[^.]+$", "", emissionVariables)
  secondaryNames <- sub("\\.[^.]+$", "", secondaryEnergyVariables)
  intensities <- secondaryEnergy[, , emissionNames] /
    secondaryEnergy[, , secondaryNames]
  intensityItems <- getItems(intensities, 3)
  emissionCategory <- sub(
    "^Emissions\\|CO2\\|Energy\\|Supply\\|([^.]*)\\..*$",
    "\\1", intensityItems
  )
  secondaryCategory <- sub(
    "^.*\\.Secondary Energy\\|", "", intensityItems
  )
  matchingItems <- intensityItems[emissionCategory == secondaryCategory]
  secondaryEnergyCarbonIntensity <- intensities[, , matchingItems]
  categories <- sub(
    "^.*\\.Secondary Energy\\|", "",
    getItems(secondaryEnergyCarbonIntensity, 3)
  )
  getItems(secondaryEnergyCarbonIntensity, 3) <- paste0(
    "Carbon Intensity|Secondary Energy|", categories
  )
  names(dimnames(secondaryEnergyCarbonIntensity))[3] <-
    "CO2Intensityindicators"
  secondaryEnergyCarbonIntensity <- add_dimension(
    secondaryEnergyCarbonIntensity,
    dim = 3.2, add = "unit", nm = "Mt CO2/TWh"
  )
  
  list(
    energyEfficiency = energyEfficiency,
    finalEnergyIntensity = finalEnergyIntensity,
    economyWideCo2Intensity = economyWideCo2Intensity,
    primaryEnergyEfficiency = primaryEnergyEfficiency,
    primaryEnergyIntensity = primaryEnergyIntensity,
    primaryEnergyCarbonIntensity = primaryEnergyCarbonIntensity,
    energyCo2GdpIntensity = energyCo2GdpIntensity,
    primaryEnergyFossilShare = primaryEnergyFossilShare,
    electricityShare = electricityShare,
    secondaryEnergyCarbonIntensity = secondaryEnergyCarbonIntensity
  )
}