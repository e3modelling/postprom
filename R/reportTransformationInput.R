#' Process and Aggregate Transformation Input Data
#'
#' This function processes and aggregates Transformation Input data.
#' It maps fuel categories, aggregates data by reporting categories, and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated Transformation Input data.
#'
#' @examples
#' \dontrun{
#' result <- reportTransformationInput(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind getItems
#' @importFrom quitte as.quitte
#' @importFrom dplyr mutate %>% case_when
#' @importFrom madrat toolAggregate
#' @importFrom tidyr separate_rows
#' @importFrom dplyr filter %>%
#' @export
reportTransformationInput <- function(path, regions, years) {
  V03InpTotTransf <- readGDX(path, c("V03InpTotTransf"), field = "l")[regions, years, ]
  units <- sub(".*\\((.*)\\).*", "\\1", V03InpTotTransf@description)
  
  SSBSTable <- rgdx.set(path, "SSBS", te = TRUE)
  SSBSTable$.te[SSBSTable$.te == "Hydrogen storage and delivery"] <- "Hydrogen"
  EFS <- rgdx.set(path, "EFS", te = TRUE)
  
  V03InpTotTransf <- toolAggregate(V03InpTotTransf, dim = 3.1, rel = SSBSTable, from = "SBS", to = ".te")
  V03InpTotTransf <- toolAggregate(V03InpTotTransf, dim = 3.2, rel = EFS, from = "EF", to = ".te")
  
  getItems(V03InpTotTransf, 3) <- gsub("\\.", "|", getItems(V03InpTotTransf, dim = 3))
  getItems(V03InpTotTransf, 3) <- paste0("Transformation Input|", getItems(V03InpTotTransf, 3))
  
  V03InpTotTransf <- helperAggregateLevel(V03InpTotTransf, level = 1, recursive = TRUE)
  
  magpie_object <- mbind(V03InpTotTransf)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  
  return(magpie_object)
}
