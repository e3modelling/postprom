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
    "Final Energy|Commercial|Services.Mtoe", "SE",
    "Final Energy|Agriculture, Fishing, Forestry", "AG",
    "Final Energy|Residential", "HOU",
    "Final Energy|Bunkers", "BU",
    "Final Energy|Non-Energy Use|Petrochemicals Industry", "PCH",
    "Final Energy|Non-Energy Use|Other Non Energy Uses", "NEN")
  
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
  
  FEACTV <- add_dimension(FEACTV, dim = 3.2, add = "unit", nm = "Mtoe/ACTV")
  FEACTV[is.na(FEACTV)] <- 0
  
  # ============ Energy Efficiency (GDP/TFC),====================
  Energy <- reports[,,c("GDP|PPP.billion US$2015/yr", "Final Energy.Mtoe",
                        "Emissions|CO2.Mt CO2/yr", "Primary Energy.Mtoe",
                        "Trade|Import|Primary Energy.Mtoe", "Trade|Export|Primary Energy.Mtoe",
                        "Trade|Import|Secondary Energy.Mtoe", "Trade|Export|Secondary Energy.Mtoe",
                        "Final Energy|Bunkers.Mtoe")]
  Energy <- collapseDim(Energy, dim = 3.2)
  EnergyEfficiency <- Energy[,,"GDP|PPP"] / Energy[,,"Final Energy"]
  getItems(EnergyEfficiency, 3) <- "Efficiency|Final Energy"
  names(dimnames(EnergyEfficiency))[3] <- "EnergyEfficiency"
  EnergyEfficiency <- add_dimension(EnergyEfficiency, dim = 3.2, add = "unit", nm = "billion US$2015/Mtoe")
  # ============ Energy intensity (TFC/GDP) =============================
  EnergyIntensity  <- Energy[,,"Final Energy"] / Energy[,,"GDP|PPP"]
  getItems(EnergyIntensity, 3) <- "Intensity|Final Energy"
  names(dimnames(EnergyIntensity))[3] <- "EnergyIntensity"
  EnergyIntensity <- add_dimension(EnergyIntensity, dim = 3.2, add = "unit", nm = "Mtoe/billion US$2015")
  # ============ Emissions intensity (CO2/GDP) =============================
  EmissionsIntensity  <- Energy[,,"Emissions|CO2"] / Energy[,,"GDP|PPP"]
  getItems(EmissionsIntensity, 3) <- "Carbon Intensity|GDP"
  names(dimnames(EmissionsIntensity))[3] <- "EnergyIntensityCO2"
  EmissionsIntensity <- add_dimension(EmissionsIntensity, dim = 3.2, add = "unit", nm = "Mt CO2/billion US$2015")
  # ============ Primary Energy Efficiency (GDP/TES),====================
  PrimaryEnergyEfficiency <- Energy[,,"GDP|PPP"] / Energy[,,"Primary Energy"]
  getItems(PrimaryEnergyEfficiency, 3) <- "Efficiency|Primary Energy"
  names(dimnames(PrimaryEnergyEfficiency))[3] <- "PrimaryEnergyEfficiency"
  PrimaryEnergyEfficiency <- add_dimension(PrimaryEnergyEfficiency, dim = 3.2, add = "unit", nm = "billion US$2015/Mtoe")
  # ============ Primary Energy intensity (TES/GDP) =============================
  PrimaryEnergyIntensity  <- Energy[,,"Primary Energy"] / Energy[,,"GDP|PPP"]
  getItems(PrimaryEnergyIntensity, 3) <- "Intensity|Primary Energy"
  names(dimnames(PrimaryEnergyIntensity))[3] <- "PrimaryEnergyIntensity"
  PrimaryEnergyIntensity <- add_dimension(PrimaryEnergyIntensity, dim = 3.2, add = "unit", nm = "Mtoe/billion US$2015")
  # ============ Energy intensity (TES/GDP) =============================
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
  CO2FEIntensity <- CO2FEIntensity[, , emi_demand_level5_same] / CO2FEIntensity[, , FE_demand_level2]
  
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
  # ============ Electricity share in final energy demand =============================
  FEELEC  <-  reports[,,"Final Energy|Electricity"]
  FEELEC <- collapseDim(FEELEC, 3)
  FE  <-  reports[,,"Final Energy"]
  FE <- collapseDim(FE, 3)
  ElectricityshareFE <- FEELEC / FE
  getItems(ElectricityshareFE, 3.1) <- "Final Energy|Electricity Share"
  getItems(ElectricityshareFE, 3.2) <- "1"
  names(dimnames(ElectricityshareFE))[3] <- "ElectricityshareFE"
  # ============ CO2 intensity of electricity generation (Emissions / Electricity production)============
  emi_supply_level5_same <- c("Emissions|CO2|Energy|Supply|Electricity.Mt CO2/yr",
                                "Emissions|CO2|Energy|Supply|Hydrogen.Mt CO2/yr",
                                "Emissions|CO2|Energy|Supply|Heat.Mt CO2/yr",
                                "Emissions|CO2|Energy|Supply|Liquids.Mt CO2/yr",
                                "Emissions|CO2|Energy|Supply|Gases.Mt CO2/yr",
                                "Emissions|CO2|Energy|Supply|Solids.Mt CO2/yr")
  
  sec_level2 <- c("Secondary Energy|Electricity.TWh","Secondary Energy|Hydrogen.TWh",
                  "Secondary Energy|Heat.TWh","Secondary Energy|Liquids.TWh",
                  "Secondary Energy|Gases.TWh","Secondary Energy|Solids.TWh")  
  
  CO2Intensity <- reports[,,c(emi_supply_level5_same, sec_level2)]
  CO2Intensity <- collapseDim(CO2Intensity, dim = 3.2)
  emi_supply_level5_same <- sub("\\.[^.]+$", "", emi_supply_level5_same)
  sec_level2 <- sub("\\.[^.]+$", "", sec_level2)
  CO2Intensity <- CO2Intensity[, , emi_supply_level5_same] / CO2Intensity[, , sec_level2]
  
  items_y <- getItems(CO2Intensity, 3)
  
  emi_cat <- sub("^Emissions\\|CO2\\|Energy\\|Supply\\|([^.]*)\\..*$", "\\1", items_y)
  sec_cat <- sub("^.*\\.Secondary Energy\\|", "", items_y)
  
  same_items <- items_y[emi_cat == sec_cat]
  
  CO2Intensityindicators <- CO2Intensity[, , same_items]
  
  cats <- sub("^.*\\.Secondary Energy\\|", "", getItems(CO2Intensityindicators, 3))
  
  getItems(CO2Intensityindicators, 3) <- paste0(
    "Carbon Intensity|Secondary Energy|",
    cats
  )
  
  names(dimnames(CO2Intensityindicators))[3] <- "CO2Intensityindicators"
  CO2Intensityindicators <- add_dimension(CO2Intensityindicators, dim = 3.2, add = "unit", nm = "Mt CO2/TWh")
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
  v01ActivPassTrnsp <- variablesACTVTransport$V01ActivGoodsTransp[blabla_regions,years,]
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
  
  v01ActivPassTrnsp <- toolAggregate(v01ActivPassTrnsp, weight = NULL, dim = 3,
                     rel = mappingTransport,from = "code",to = "variable")
  v01ActivPassTrnsp <- v01ActivPassTrnsp[,,getItems(TRANP, 3)]
  # -------------------------- Transport Freight -------
  TRANG <- reports[, , c("Final Energy|Transportation|Goods Transport - Trucks",
                              "Final Energy|Transportation|Goods Transport - Rail",
                              "Final Energy|Transportation|Goods Transport - Inland Navigation")]
  TRANG <- collapseDim(TRANG, 3.2)
  
  V01ActivGoodsTransp <- toolAggregate(V01ActivGoodsTransp, weight = NULL, dim = 3,
                                     rel = mappingTransport,from = "code",to = "variable")
  V01ActivGoodsTransp <- V01ActivGoodsTransp[,,getItems(TRANG, 3)]
  # -------------------------- 
  
  ActivPassTrnsp <- TRANP / v01ActivPassTrnsp
  ActivGoodsTransp <-  TRANG / V01ActivGoodsTransp
  
  ActivPassTrnsp <- add_dimension(ActivPassTrnsp, dim = 3.2, add = "unit", nm = "Mtoe/ACTV")
  ActivGoodsTransp <- add_dimension(ActivGoodsTransp, dim = 3.2, add = "unit", nm = "Mtoe/Gtkm")
  
  ActivTrnsp <- mbind(ActivPassTrnsp, ActivGoodsTransp)
  
  getItems(ActivTrnsp, dim = 3.1) <- sub(
    "^Final Energy\\|",
    "",
    getItems(ActivTrnsp, dim = 3.1)
  )
  
  getItems(ActivTrnsp, dim = 3.1) <- paste0("Energy Intensity|",getItems(Activitybind, dim = 3))
  
  ActivTrnsp <- mbind(ActivTrnsp)
    # ==================== Combine all indicators into a single magpie object ============================
  magpie_object <- mbind(
    EnergyEfficiency,
    EnergyIntensity,
    PrimaryEnergyEfficiency,
    PrimaryEnergyIntensity,
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
  
  return(magpie_object)
}