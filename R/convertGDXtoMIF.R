#' Convert GDX to MIF Format
#'
#' This function processes a GDX file for specified regions and generates a MIF
#' report. The output combines several metrics into a single MIF file.
#'
#' @param path_gdx Character. Path to the input GDX file.
#' @param regions Character vector. A list of regions to include in the analysis.
#' @param mif_name Character. Name of the output MIF file to be created. If NULL the formatted content is returned
#' @param scenario_name Character. Name of the scenario to associate with the generated MIF file.
#'
#' @details
#' This function aggregates various metrics such as final energy, emissions,
#' socio-economic data, and more. The data is read from the provided GDX file
#' and written to a MIF file in a format compatible with the OPEN-PROM model.
#'
#' The generated MIF file includes the following metrics:
#' \itemize{
#'   \item Final Energy
#'   \item Emissions
#'   \item Secondary Energy (SE)
#'   \item Primary Energy (PE)
#'   \item Gross Domestic Product (GDP)
#'   \item Population (POP)
#'   \item Carbon Prices
#'   \item Price Data
#'   \item Electricity Capacity
#' }
#'
#' @return This function either creates a MIF file or implicitly returns the formatted content
#'
#' @examples
#' \dontrun{
#' convertGDXtoMIF(
#'   path_gdx = "path/to/data.gdx",
#'   regions = c("MEA", "USA"),
#'   mif_name = "output.mif",
#'   scenario_name = "Scenario_name"
#' )
#' }
#' @seealso \code{\link[magclass]{write.report}}
#' @importFrom magclass mbind write.report
#' @export
convertGDXtoMIF <- function(path_gdx, regions, mif_name, scenario_name) {
  FE <- reportFinalEnergy(path_gdx, regions)
  EMI <- reportEmissions(path_gdx, regions)
  SE <- reportSE(path_gdx, regions)
  PE <- reportPE(path_gdx, regions)
  GDP <- reportGDP(path_gdx, regions)
  POP <- reportPOP(path_gdx, regions)
  PCar <- reportPriceCarbon(path_gdx, regions)
  Price <- reportPrice(path_gdx, regions)
  CapElec <- reportCapacityElectricity(path_gdx, regions)

  magpie_reporting <- mbind(FE, EMI, SE, PE, GDP, POP, PCar, Price, CapElec)
  write.report(
    magpie_reporting,
    file=mif_name,
    model="OPEN-PROM",
    scenario=scenario_name
  )
}
