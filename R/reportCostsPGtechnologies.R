#' Process and Aggregate CAPEX and fixed costs of any power generation technology.
#'
#' This function processes and aggregates cost data from a GDX file,
#' converting it to a standardized format and attaching proper units.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed carbon price data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportCostsPGtechnologies(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind
#' @export
reportCostsPGtechnologies <- function(path, regions, years) {
  
  V04CapexFixCostPG <- readGDX(path, "V04CapexFixCostPG", field = "l")[regions, years, ]
  
  # complete names
  getItems(V04CapexFixCostPG, 3) <- paste0("CAPEX and fixed costs of power generation technology|", getItems(V04CapexFixCostPG, 3))
  V04CapexFixCostPG <- add_dimension(V04CapexFixCostPG, dim = 3.2, add = "unit", nm = "US$2015/kW")
  
  return(V04CapexFixCostPG)
}
