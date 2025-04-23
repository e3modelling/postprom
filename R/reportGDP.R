#' Process and Aggregate GDP Data
#'
#' This function processes and aggregates GDP data from a GDX file,
#' converting it to a standardized format and attaching proper units.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed GDP data in PPP terms with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportGDP(path_gdx, c("MEA"))
#' }
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension
#' @export
reportGDP <- function(path, regions) {
  iGDP <- readGDX(path, "iGDP")[regions,,]
  getItems(iGDP, 3) <- "GDP|PPP"
  iGDP <- add_dimension(iGDP, dim = 3.2, add = "unit", nm = "billion US$2015/yr")
  return(iGDP)
}
