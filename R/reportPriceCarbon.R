#' Process and Aggregate Carbon Price Data
#'
#' This function processes and aggregates carbon price data from a GDX file,
#' converting it to a standardized format and attaching proper units.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed carbon price data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportPriceCarbon(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind
#' @export
reportPriceCarbon <- function(path, regions, years) {
  VCarVal <- readGDX(path, "VmCarVal", field = "l")[regions, years, ][,,"TRADE"]

  # complete names
  getItems(VCarVal, 3) <- "Price|Carbon"

  VCarVal <- add_dimension(VCarVal, dim = 3.2, add = "unit", nm = "US$2015/tn CO2")
  return(VCarVal)
}
