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
  BALEF2EFS <- rgdx.set(path, "BALEF2EFS") %>%
    
    filter(BALEF %in% c("Total", "Other Fuels", "Renewables", "Solids",
                        "Liquids", "Fossil Liquids", "Fossil",
                        "Gases", "Biofuels"
  ))
  
  V03InpTotTransf <- toolAggregate(V03InpTotTransf, dim = 3.1, rel = SSBSTable, from = "SBS", to = ".te")
  V03InpTotTransf <- toolAggregate(V03InpTotTransf, dim = 3.2, rel = BALEF2EFS, from = "EFS", to = "BALEF")
  
  getItems(V03InpTotTransf, 3) <- gsub("\\.", "|", getItems(V03InpTotTransf, dim = 3))
  getItems(V03InpTotTransf, 3) <- paste0("Transformation Input|", getItems(V03InpTotTransf, 3))
  
  V03InpTotTransfagr <- helperAggregateLevel(V03InpTotTransf[,,grep("\\|Total$", getItems(V03InpTotTransf, 3))], level = 1, recursive = TRUE)
  V03InpTotTransfagr <- V03InpTotTransfagr[, ,!grepl("\\|Total$", getItems(V03InpTotTransfagr, 3))]
  
  magpie_object <- mbind(V03InpTotTransf, V03InpTotTransfagr)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  
  return(magpie_object)
}
