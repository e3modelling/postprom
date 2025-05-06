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
#' result <- reportEmissions(system.file("extdata", "blabla.gdx", package = "openprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind collapseDim
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter left_join mutate select group_by %>%
#' @export
reportEmissions <- function(path, regions, years) {
  fscenario <- readGDX(path, "fscenario")

  Navigate_Emissions <- read.csv(file.path(dirname(path), "data", "NavigateEmissions.csv"))
  Navigate_Emissions <- as.magpie(Navigate_Emissions)

  if (fscenario == 2) {
    Navigate_Emissions <- Navigate_Emissions[, , "SUP_2C_Default"][regions, years, ]
  } else if (fscenario == 1) {
    Navigate_Emissions <- Navigate_Emissions[, , "SUP_1p5C_Default"][regions, years, ]
  } else if (fscenario == 0) {
    Navigate_Emissions <- Navigate_Emissions[, , "SUP_NPi_Default"][regions, years, ]
  }

  Navigate_Emissions <- collapseDim(Navigate_Emissions, 3.1)
  Navigate_Emissions <- collapseDim(Navigate_Emissions, 3.1)

  remind_AFOLU_Industrial_Processes <- Navigate_Emissions[, , c("Emissions|CO2|AFOLU", "Emissions|CO2|Industrial Processes")]
  remind <- dimSums(remind_AFOLU_Industrial_Processes, 3, na.rm = TRUE)


  iCo2EmiFac <- readGDX(path, "iCo2EmiFac")[regions, years, ]
  VConsFuel <- readGDX(path, "VConsFuel", field = 'l')[regions, years, ]
  VInpTransfTherm <- readGDX(path, "VInpTransfTherm", field = 'l')[regions, years, ]
  VTransfInputDHPlants <- readGDX(path, "VTransfInputDHPlants", field = 'l')[regions, years, ]
  VConsFiEneSec <- readGDX(path, "VConsFiEneSec", field = 'l')[regions, years, ]
  VDemFinEneTranspPerFuel <- readGDX(path, "VDemFinEneTranspPerFuel", field = 'l')[regions, years, ]
  VProdElec <- readGDX(path, "VProdElec", field = 'l')[regions, years, ]
  iPlantEffByType <- readGDX(path, "iPlantEffByType")[regions, years, ]
  iCO2CaptRate <- readGDX(path, "iCO2CaptRate")[regions, years, ]
  VConsFuelTechH2Prod <- readGDX(path, "VConsFuelTechH2Prod", field = 'l')[regions, years, ]
  # Link between Model Subsectors and Fuels

  sets4 <- readGDX(path, "SECTTECH")

  EFtoEFS <- readGDX(path, "EFtoEFS")

  IND <- readGDX(path, "INDDOM")
  IND <- as.data.frame(IND)

  map_INDDOM <- sets4 %>% filter(SBS %in% IND[, 1], EF!= "")

  qINDDOM <- map_INDDOM %>%
    left_join(EFtoEFS, by = "EF") %>%
    select(-c("EF")) %>%
    unique()
  names(qINDDOM) <- sub("EFS", "SECTTECH", names(qINDDOM))

  qINDDOM <- paste0(qINDDOM[["SBS"]], ".", qINDDOM[["SECTTECH"]])
  INDDOM <- as.data.frame(qINDDOM)

  PGEF <- readGDX(path, "PGEF") %>% as.data.frame()

  # final consumption
  sum1 <- iCo2EmiFac[, , INDDOM[, 1]] * VConsFuel[, , INDDOM[, 1]]
  sum1 <- dimSums(sum1, 3, na.rm = TRUE)
  # input to power generation sector
  sum2 <- VInpTransfTherm[, , PGEF[, 1]] * iCo2EmiFac[, , "PG"][, , PGEF[, 1]]
  sum2 <- dimSums(sum2, 3, na.rm = TRUE)
  # input to power hydrogen sector
  hydrogen <- VConsFuelTechH2Prod[, , PGEF[, 1]][,,c("gsr","bgfl")] * iCo2EmiFac[, , "H2P"][, , PGEF[, 1]]
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
  # transport
  sum5 <- dimSums(sum5, 3, na.rm = TRUE)

  PGALLtoEF <- readGDX(path, "PGALLtoEF") %>% as.data.frame()
  names(PGALLtoEF) <- c("PGALL", "EF")

  CCS <- readGDX(path, "CCS")
  CCS <- as.data.frame(CCS)

  CCS <- PGALLtoEF[PGALLtoEF$PGALL %in% CCS$CCS, ]

  var_16 <- VProdElec[, , CCS[, 1]] * 0.086 / iPlantEffByType[, , CCS[, 1]] * iCo2EmiFac[, , "PG"][, , CCS[, 2]] * iCO2CaptRate[, , CCS[, 1]]
  # CO2 captured by CCS plants in power generation
  sum6 <- dimSums(var_16, dim = 3, na.rm = TRUE)
  
  # input hydrogen_CCS
  hydrogen_CCS <- VConsFuelTechH2Prod[, , PGEF[, 1]][,,c("gss","bgfls")] * iCo2EmiFac[, , "H2P"][, , PGEF[, 1]]
  hydrogen_CCS <- dimSums(hydrogen_CCS, 3, na.rm = TRUE)

  SECTTECH2 <- sets4 %>% filter(SBS %in% c("BU"))
  SECTTECH2 <- paste0(SECTTECH2[["SBS"]], ".", SECTTECH2[["EF"]])
  SECTTECH2 <- as.data.frame(SECTTECH2)
  # Bunkers
  sum7 <- iCo2EmiFac[, , SECTTECH2[, 1]] * VConsFuel[, , SECTTECH2[, 1]]
  sum7 <- dimSums(sum7, dim = 3, na.rm = TRUE)

  total_CO2 <- sum1 + sum2 + sum3 + sum4 + sum5 - sum6 + sum7 + remind + hydrogen + hydrogen_CCS

  getItems(total_CO2, 3) <- "Emissions|CO2"

  magpie_object <- NULL
  total_CO2 <- add_dimension(total_CO2, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, total_CO2, Navigate_Emissions)
  
  # Hydrogen
  Hydrogen_total <- hydrogen + hydrogen_CCS
  
  getItems(Hydrogen_total, 3) <- "Emissions|CO2|Hydrogen"
  
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
  names(qINDSE) <- sub("EFS", "SECTTECH", names(qINDSE))

  qINDSE <- paste0(qINDSE[["SBS"]], ".", qINDSE[["SECTTECH"]])
  INDSE <- as.data.frame(qINDSE)

  # final consumption
  sum_INDSE <- iCo2EmiFac[, , INDSE[, 1]] * VConsFuel[, , INDSE[, 1]]
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
  names(qDOMSE) <- sub("EFS", "SECTTECH", names(qDOMSE))

  qDOMSE <- paste0(qDOMSE[["SBS"]], ".", qDOMSE[["SECTTECH"]])
  DOMSE <- as.data.frame(qDOMSE)

  # final consumption
  sum_DOMSE <- iCo2EmiFac[, , DOMSE[, 1]] * VConsFuel[, , DOMSE[, 1]]
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
  sum_Supply <- sum2 + sum3 + sum4 - sum6

  getItems(sum_Supply, 3) <- "Emissions|CO2|Energy|Supply"

  sum_Supply <- add_dimension(sum_Supply, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  magpie_object <- mbind(magpie_object, sum_Supply)

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
