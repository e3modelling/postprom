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
  BALEF2EFS <- rgdx.set(path, "BALEF2EFS") %>%
    
    filter(BALEF %in% c("Total", "Other Fuels", "Renewables", "Solids",
                        "Liquids", "Fossil Liquids", "Fossil",
                        "Gases", "Biofuels"
    ))
  
  VmConsFiEneSec <- toolAggregate(VmConsFiEneSec, dim = 3.1, rel = SSBSTable, from = "SBS", to = ".te")
  VmConsFiEneSec <- toolAggregate(VmConsFiEneSec, dim = 3.2, rel = BALEF2EFS, from = "EFS", to = "BALEF")
  
  getItems(VmConsFiEneSec, 3) <- gsub("\\.", "|", getItems(VmConsFiEneSec, dim = 3))
  getItems(VmConsFiEneSec, 3) <- paste0("Energy Industry Own Use|", getItems(VmConsFiEneSec, 3))
  
  VmConsFiEneSecagr <- helperAggregateLevel(VmConsFiEneSec[,,grep("\\|Total$", getItems(VmConsFiEneSec, 3))], level = 1, recursive = TRUE)
  VmConsFiEneSecagr <- VmConsFiEneSecagr[, ,!grepl("\\|Total$", getItems(VmConsFiEneSecagr, 3))]
  
  magpie_object <- mbind(VmConsFiEneSec, VmConsFiEneSecagr)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  
  return(magpie_object)
}
