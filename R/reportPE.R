#' Process and Aggregate Primary Energy Data
#'
#' A function to process and aggregate primary energy data from a GDX file.
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated primary energy data.
#'
#' @examples
#' \dontrun{
#' result <- reportPE(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom madrat toolAggregate
#' @importFrom magclass getItems getNames add_dimension mbind
#' @export
reportPE <- function(path, regions, years) {
  BALEFtoEF <- read.csv(
    system.file("mappings", "BALEFtoEF.csv", package = "postprom")
  ) %>%
    separate_rows(EF) %>%
    filter(BALEF %in% c(
      "Coal", "Gas", "Nuclear", "Biofuels", "Oil", "Electricity", "Heat",
      "Other fuels", "Solar", "Wind", "Geothermal and other renewable sources",
      "Hydrogen", "Hydro"
    ))
  #MtoeToEJ <- 0.041868 units to be Mtoe
  V03ConsGrssInl <- readGDX(path, "V03ConsGrssInl", field = "l")[regions, years, ]

  V03ConsGrssInl <- toolAggregate(V03ConsGrssInl,
    weight = NULL, dim = 3,
    rel = BALEFtoEF, from = "BALEF", to = "EF"
  )
  getItems(V03ConsGrssInl, 3) <- paste0("Primary Energy|", getItems(V03ConsGrssInl, 3))

  magpie_object <- helperAggregateLevel(V03ConsGrssInl, level = 1, recursive = TRUE)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = "Mtoe")
  return(magpie_object)
}
