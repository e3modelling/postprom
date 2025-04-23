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
#' result <- reportPOP(system.file("extdata", "blabla.gdx", package = "openprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind
#' @export
reportPOP <- function(path, regions) {
  iPop <- readGDX(path, "iPop")[regions,,]
  getItems(iPop, 3) <- "Population"

  iPop <- add_dimension(iPop, dim = 3.2, add = "unit", nm = "billion")
  return(iPop)
}
