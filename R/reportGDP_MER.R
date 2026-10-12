#' Process and Aggregate GDP_MER Data
#'
#' "Country-level GDP at Market Exchange Rates (MER), derived from",
#' "SSP GDP projections (mrdrivers) expressed in Purchasing Power Parity (PPP).",
#' "PPP GDP is converted to MER using the World Bank Price Level Index.",
#' "Values are expressed in billion constant US$2015 per year,",
#' "with annual interpolation for 2010–2100."
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed GDP data in PPP terms with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportGDP_MER(path_gdx, c("MEA"))
#' }
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension
#' @export
reportGDP_MER <- function(path, regions, years) {
  GDP_MER <- calcOutput("iGDP_MER", aggregate = TRUE,  regionmapping = "regionmappingOPDEV5.csv")
  getItems(GDP_MER, 3) <- "GDP|MER"
  GDP_MER <- add_dimension(GDP_MER, dim = 3.2, add = "unit", nm = "billion US$2015/yr")
  GDP_MER <-  GDP_MER[regions, years,]
  return(GDP_MER)
}
