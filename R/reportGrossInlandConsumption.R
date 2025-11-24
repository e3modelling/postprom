#' Process and Aggregate Gross Inland Consumption|Biomass and Waste
#'
#' This function processes and aggregates Gross Inland Consumption|Biomass and Waste data from a GDX file,
#' converting it to a standardized format and attaching proper units.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed carbon price data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportGrossInlandConsumption(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind
#' @export
reportGrossInlandConsumption <- function(path, regions, years) {

  energy_codes <- data.frame(
    Code = c(
      "HCL","LGN","CRO","LPG","GSL","KRS","GDO","RFO","OLQ","NGS","OGS","NUC","STE",
      "HYD","WND","SOL","BMSWAS","GEO","MET","ETH","BGDO","H2F","ELC",
      "STE1CL","STE1CH","STE1CD","STE1CR","STE1CG","STE1CB",
      "STE1AL","STE1AH","STE1AD","STE1AR","STE1AG","STE1AB","STE1AH2F",
      "STE2LGN","STE2OSL","STE2GDO","STE2RFO","STE2OLQ","STE2NGS","STE2OGS","STE2BMS",
      "PHEVGSL","PHEVGDO","CHEVGSL","CHEVGDO",
      "HTDAC","H2DAC","LTDAC","EWDAC"
    ),
    Description = c(
      "Hard Coal, Coke and Other Solids",
      "Lignite",
      "Crude Oil and Feedstocks",
      "Liquefied Petroleum Gas",
      "Gasoline",
      "Kerosene",
      "Diesel Oil",
      "Residual Fuel Oil",
      "Other Liquids",
      "Natural Gas",
      "Other Gases",
      "Nuclear",
      "Steam",
      "Hydro",
      "Wind",
      "Solar",
      "Biomass and Waste",
      "Geothermal and other renewable sources eg. Tidal, etc.",
      "Methanol",
      "Ethanol",
      "Biodiesel",
      "Hydrogen",
      "Electricity",
      "Steam coming from CHP plants conventional lgn",
      "Steam produced from CHP conventional hcl",
      "CHP conventional gdo",
      "Steam produced from CHP conventional rfo",
      "Steam produced from CHP conventional ngs",
      "Steam produced from CHP conventional bmswas",
      "Steam produced from CHP advanced lgn",
      "Steam produced from CHP advanced hcl",
      "Steam produced from CHP advanced gdo",
      "Steam produced from CHP advanced rfo",
      "Steam produced from CHP advanced ngs",
      "Steam produced from CHP advanced bmswas",
      "Steam produced from Hydrogen powered CHP fuel cells",
      "Steam coming from district heating plants burning lgn",
      "Steam produced from district heating plants burning hcl",
      "Steam produced from district heating plants burning gdo",
      "Steam produced from district heating plants burning rfo",
      "Steam produced from district heating plants burning olq",
      "Steam produced from district heating plants burning ngs",
      "Steam produced from district heating plants burning ogs",
      "Steam produced from district heating plants burning bmswas",
      "Plug in Hybrid engine - gasoline",
      "Plug in Hybrid engine - diesel",
      "Conventional Hybrid engine - gasoline",
      "Conventional Hybrid engine - diesel",
      "High-Temperature DAC",
      "High-Temperature H2-fueled DAC",
      "Low-Temperature DAC",
      "Enhanced-Weathering DAC"
    ),
    stringsAsFactors = FALSE
  )

  V03ConsGrssInl <- readGDX(path, "V03ConsGrssInl", field = "l")[regions, years, ]

  #filter biomass
  V03ConsGrssInl_BMSWAS <- V03ConsGrssInl[,2020 : gsub("y", "", max(years)), "BMSWAS"]

  #per year
  V03ConsGrssInl_BMSWAS <- dimSums(V03ConsGrssInl_BMSWAS, 1, na.rm = TRUE)

  #Mtoe to EJ
  V03ConsGrssInl_BMSWAS <- V03ConsGrssInl_BMSWAS * 0.041868

  getItems(V03ConsGrssInl_BMSWAS, 3) <- paste0("Gross Inland Consumption", "|", energy_codes[energy_codes[,"Code"]==getItems(V03ConsGrssInl_BMSWAS, 3),"Description"])


  V03ConsGrssInl_BMSWAS <- add_dimension(V03ConsGrssInl_BMSWAS, dim = 3.2, add = "unit", nm = "EJ")
  getItems(V03ConsGrssInl_BMSWAS, 1) <- "World"

  return(V03ConsGrssInl_BMSWAS)
}
