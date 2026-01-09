#' Process and Aggregate Learning curve cost multiplier and Global cumulative capacity
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
#' result <- reportLearningCurve(system.file("extdata", "blabla.gdx", package = "postprom"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind
#' @export
reportLearningCurve <- function(path, regions, years) {
  
  VmCostLC <- readGDX(path, "VmCostLC", field = "l")
  V10CumCapGlobal <- readGDX(path, "V10CumCapGlobal", field = "l")
  
  # complete names
  getItems(VmCostLC, 3) <- paste0("Learning curve cost multiplier|", getItems(VmCostLC, 3))
  VmCostLC <- add_dimension(VmCostLC, dim = 3.2, add = "unit", nm = "1")
  
  # complete names
  getItems(V10CumCapGlobal, 3) <- paste0("Global cumulative capacity|", getItems(V10CumCapGlobal, 3))
  V10CumCapGlobal <- add_dimension(V10CumCapGlobal, dim = 3.2, add = "unit", nm = "GW")
  
  variables <- mbind(VmCostLC, V10CumCapGlobal)
  getItems(variables, 1) <- "World"
  
  return(variables)
}
