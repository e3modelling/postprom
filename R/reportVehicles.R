#' Process and Aggregate Population Data
#'
#' This function processes and aggregates population data from a GDX file,
#' converting it to a standardized format and attaching proper units.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed population data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportPOP(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind
#' @export
reportVehicles <- function(path, regions, years) {
  rename_TTECH <- c(
    "TGSL" = "ICE Gasoline",
    "TLPG" = "ICE LPG",
    "TGDO" = "ICE Diesel",
    "TNGS" = "ICE Natural Gas",
    "TELC" = "EVs",
    "TKRS" = "Gas Turbine Kerosene",
    "TETH" = "ICE Ethanol",
    "TPHEVGSL" = "Plug-in Hybrid Gasoline",
    "TPHEVGDO" = "Plug-in Hybrid Diesel",
    "TH2F" = "Fuel Cells - Hydrogen",
    "TCHEVGSL" = "Conventional Hybrid Gasoline",
    "TCHEVGDO" = "Conventional Hybrid Diesel"
  )
  vars <- c("V01StockPcYearlyTech", "V01NewRegPcTechYearly")
  values <- readGDX(path, vars, field = 'l')
  
  StockPcYearlyTech <- values$V01StockPcYearlyTech[regions, years, ]
  NewRegPcTechYearly <- values$V01NewRegPcTechYearly[regions, years, ]

  TechNames <- str_replace_all(getItems(NewRegPcTechYearly, 3.1), rename_TTECH)

  getItems(NewRegPcTechYearly, 3.1) <- paste0("Capacity Additions|Passenger Cars|", TechNames)
  getItems(StockPcYearlyTech, 3.1) <- paste0("Capacity|Passenger Cars|", TechNames)

  NewRegPcTechYearly <- add_dimension(NewRegPcTechYearly, dim = 3.2, add = "unit", nm = "million vehicles")
  StockPcYearlyTech <- add_dimension(StockPcYearlyTech, dim = 3.2, add = "unit", nm = "million vehicles")
  
  return(mbind(NewRegPcTechYearly, StockPcYearlyTech))
}
