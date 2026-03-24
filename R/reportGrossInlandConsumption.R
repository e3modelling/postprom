#' Process and Aggregate Gross Inland Consumption|Biomass and Waste
#'
#' This function processes and aggregates Gross Inland Consumption|Biomass and Waste data from a GDX file,
#' converting it to a standardized format and attaching proper units.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed carbon price data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportGrossInlandConsumption(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind
#' @export
reportGrossInlandConsumption <- function(path, regions, years) {
  BALEFtoEF <- read.csv(
    system.file("mappings", "BALEFtoEF.csv", package = "postprom")
  ) %>%
    separate_rows(EF) %>%
    filter(BALEF %in% c(
      "Coal", "Gas", "Nuclear", "Biofuels", "Oil", "Electricity", "Heat",
      "Other fuels", "Solar", "Wind", "Geothermal and other renewable sources",
      "Hydrogen", "Hydro"
    ))
  MtoeToEJ <- 0.041868
  V03ConsGrssInl <- readGDX(path, "V03ConsGrssInl", field = "l")[regions, years, ] * MtoeToEJ

  V03ConsGrssInl <- toolAggregate(V03ConsGrssInl,
    weight = NULL, dim = 3,
    rel = BALEFtoEF, from = "BALEF", to = "EF"
  )
  getItems(V03ConsGrssInl, 3) <- paste0("Gross Inland Consumption|", getItems(V03ConsGrssInl, 3))

  magpie_object <- helperAggregateLevel(V03ConsGrssInl, level = 1, recursive = TRUE)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = "EJ")
  return(magpie_object)
}
