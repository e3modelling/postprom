#' Process and Aggregate Energy Industry Own Use Data
#'
#' This function processes and aggregates Energy Industry Own Use data.
#' It maps fuel categories, aggregates data by reporting categories, and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated Energy Industry Own Use data.
#'
#' @examples
#' \dontrun{
#' result <- reportEnergyIndustryOwnUse(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
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
reportEnergyIndustryOwnUse <- function(path, regions, years) {
  VmConsFiEneSec <- readGDX(path, c("VmConsFiEneSec"), field = "l")[regions, years, ]
  units <- sub(".*\\((.*)\\).*", "\\1", VmConsFiEneSec@description)
  
  SSBSTable <- rgdx.set(path, "SSBS", te = TRUE)
  SSBSTable$.te[SSBSTable$.te == "Hydrogen storage and delivery"] <- "Hydrogen"
  EFS <- rgdx.set(path, "EFS", te = TRUE)
  
  VmConsFiEneSec <- toolAggregate(VmConsFiEneSec, dim = 3.1, rel = SSBSTable, from = "SBS", to = ".te")
  VmConsFiEneSec <- toolAggregate(VmConsFiEneSec, dim = 3.2, rel = EFS, from = "EF", to = ".te")
  
  getItems(VmConsFiEneSec, 3) <- gsub("\\.", "|", getItems(VmConsFiEneSec, dim = 3))
  getItems(VmConsFiEneSec, 3) <- paste0("Energy Industry Own Use|", getItems(VmConsFiEneSec, 3))
  
  VmConsFiEneSec <- helperAggregateLevel(VmConsFiEneSec, level = 1, recursive = TRUE)
  
  magpie_object <- mbind(VmConsFiEneSec)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  
  return(magpie_object)
}
