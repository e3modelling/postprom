#' Energy Efficiency (GDP/TFC) 
#' Energy intensity (TFC/GDP)
#'
#' @param reports Magpie object created from postprom.
#' @return A magpie object containing Efficiency.
#'
#' @examples
#' \dontrun{
#' result <- reportEfficiency(reports)
#' }
#'
#' @importFrom magclass getItems dimSums add_dimension mbind collapseDim
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter left_join mutate select group_by %>% arrange ungroup rename
#' @export
reportEfficiency <- function(reports, path, regions, years, blabla_regions) {
  
  items <- getItems(reports,3)
  
  # ============ Energy Efficiency (GDP/TFC),====================
  Energy <- reports[,,c("GDP|PPP.billion US$2015/yr", "Final Energy.Mtoe", "Primary Energy.Mtoe")]
  Energy <- collapseDim(Energy, dim = 3.2)
  EnergyEfficiency <- Energy[,,"GDP|PPP"] / Energy[,,"Final Energy"]
  getItems(EnergyEfficiency, 3) <- "Energy Efficiency GDP/TFC"
  names(dimnames(EnergyEfficiency))[3] <- "EnergyEfficiency"
  EnergyEfficiency <- add_dimension(EnergyEfficiency, dim = 3.2, add = "unit", nm = "billion US$2015/Mtoe")
  # ============ Energy intensity (TFC/GDP) =============================
  EnergyIntensity  <- Energy[,,"Final Energy"] / Energy[,,"GDP|PPP"]
  getItems(EnergyIntensity, 3) <- "Energy Intensity TFC/GDP"
  names(dimnames(EnergyIntensity))[3] <- "EnergyIntensity"
  EnergyIntensity <- add_dimension(EnergyIntensity, dim = 3.2, add = "unit", nm = "Mtoe/billion US$2015")
  # ============ Primary Energy Efficiency (GDP/TES),====================
  PrimaryEnergyEfficiency <- Energy[,,"GDP|PPP"] / Energy[,,"Primary Energy"]
  getItems(PrimaryEnergyEfficiency, 3) <- "Primary Energy Efficiency GDP/TES"
  names(dimnames(PrimaryEnergyEfficiency))[3] <- "PrimaryEnergyEfficiency"
  PrimaryEnergyEfficiency <- add_dimension(PrimaryEnergyEfficiency, dim = 3.2, add = "unit", nm = "billion US$2015/Mtoe")
  # ============ Primary Energy intensity (TES/GDP) =============================
  PrimaryEnergyIntensity  <- Energy[,,"Primary Energy"] / Energy[,,"GDP|PPP"]
  getItems(PrimaryEnergyIntensity, 3) <- "Primary Energy Intensity TES/GDP"
  names(dimnames(PrimaryEnergyIntensity))[3] <- "PrimaryEnergyIntensity"
  PrimaryEnergyIntensity <- add_dimension(PrimaryEnergyIntensity, dim = 3.2, add = "unit", nm = "Mtoe/billion US$2015")
  
  # ============ CO2 intensity of electricity generation (Emissions / Electricity production)============
  CO2Intensity <- reports[,,c("Emissions|CO2.Mt CO2/yr", "Secondary Energy|Electricity.TWh")]
  CO2Intensity <- collapseDim(CO2Intensity, dim = 3.2)
  CO2IntensityPower <- CO2Intensity[,,"Emissions|CO2"] / CO2Intensity[,,"Secondary Energy|Electricity"]
  getItems(CO2IntensityPower, 3) <- "CO2 Intensity of Electricity Generation Emissions / Electricity production"
  names(dimnames(CO2IntensityPower))[3] <- "CO2IntensityPower"
  CO2IntensityPower <- add_dimension(CO2IntensityPower, dim = 3.2, add = "unit", nm = "Mt CO2/TWh")
  # ============ CO2 intensity of INDUSTRY (Emissions/Useful Energy)============
  CO2Intensity <- reports[,,"Emissions|CO2|Energy|Demand|Industry.Mt CO2/yr"]
  CO2Intensity <- collapseDim(CO2Intensity, dim = 3.2)
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
    # Calculate the sum
    add_region_GLO <- dimSums(UsefulEnergyIndustry, 1, na.rm = TRUE)
    getItems(add_region_GLO, 1) <- "World"
    UsefulEnergyIndustry <- mbind(UsefulEnergyIndustry, add_region_GLO)
  }
  
  CO2IntensityofIndustry <- CO2Intensity / UsefulEnergyIndustry
  getItems(CO2IntensityofIndustry, 3) <- "CO2 Intensity of Industry Emissions/Useful Energy"
  names(dimnames(CO2IntensityofIndustry))[3] <- "CO2IntensityofIndustry"
  CO2IntensityofIndustry <- add_dimension(CO2IntensityofIndustry, dim = 3.2, add = "unit", nm = "Mt CO2/Mtoe")
  
  # ============Energy intensity of industry (TFC industry/Useful Energy)============
  FEIndustry <- reports[,,"Final Energy|Industry.Mtoe"]
  FEIndustry <- collapseDim(FEIndustry, dim = 3.2)
  
  EnergyIntensityofIndustry <- FEIndustry / UsefulEnergyIndustry
  getItems(EnergyIntensityofIndustry, 3) <- "CO2 Intensity of Industry TFC Industry/Useful Energy"
  names(dimnames(EnergyIntensityofIndustry))[3] <- "EnergyIntensityofIndustry"
  EnergyIntensityofIndustry <- add_dimension(EnergyIntensityofIndustry, dim = 3.2, add = "unit", nm = "1")
  # ==================== Combine all indicators into a single magpie object ============================
  magpie_object <- mbind(
    EnergyEfficiency,
    EnergyIntensity,
    PrimaryEnergyEfficiency,
    PrimaryEnergyIntensity,
    CO2IntensityPower,
    CO2IntensityofIndustry,
    EnergyIntensityofIndustry
  )
  
  return(magpie_object)
}