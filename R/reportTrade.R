#' Process and Aggregate Energy Trade Data
#'
#' This function processes gross imports, gross exports and net exports of all fuels
#' from a GDX file, in the model's native units, and classifies them into a Primary
#' Energy / Secondary Energy structure (with a Fossil aggregate).
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @param years Years to report.
#' @return A magpie object containing processed and aggregated trade data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportTrade(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom madrat toolAggregate
#' @importFrom magclass getItems add_dimension mbind
#' @importFrom dplyr filter %>%
#' @export
reportTrade <- function(path, regions, years) {
  variables <- readGDX(
    path,
    c("V03Imp", "V03Exp", "VmImpNetEneBrnch"),
    field = "l"
  )
  units <- sub(".*\\((.*)\\).*", "\\1", variables$VmImpNetEneBrnch@description)

  # Primary / Secondary classification. A STATIC per-carrier mapping: primary vs secondary
  # is a property of the energy carrier, not of the country. i03RatioPrimaryFuels is
  # deliberately NOT used - it is the domestic production mix, PrimProd/(PrimProd+OutTransf),
  # and carries no information about the composition of the traded stream (it reads 0 for any
  # pure importer, e.g. it would call 100% of Germany's hard-coal imports secondary).
  #
  # The node names match the common definitions (Coal, Oil, Gas, Fossil, Biomass / Liquids,
  # Gases, Electricity, Heat, Hydrogen), so the project mapping is a thin rename. BALEFtoEF is
  # intentionally not reused here: its categories overlap primary and secondary (Liquids
  # includes crude, Fossil includes refined products). Fossil and the Primary/Secondary Energy
  # totals overlap their children by design. Non-traded primaries (nuclear, hydro, wind, solar,
  # geothermal) are omitted - they are 0 in the model's trade variables.
  EFtoPrimarySecondary <- read.csv(
    system.file("mappings", "EFtoPrimarySecondary.csv", package = "postprom")
  ) %>%
    filter(EF %in% getItems(variables$VmImpNetEneBrnch, 3))

  # Imports and exports are gross flows; net imports are negated, so that Net Export is
  # positive for a net exporter.
  flows <- list(
    "Import" = variables$V03Imp,
    "Export" = variables$V03Exp,
    "Net Export" = -variables$VmImpNetEneBrnch
  )

  magpie_object <- NULL
  for (flow in names(flows)) {
    v <- flows[[flow]][regions, years, unique(EFtoPrimarySecondary$EF)]
    v <- toolAggregate(v,
      weight = NULL, dim = 3,
      rel = EFtoPrimarySecondary, from = "node", to = "EF"
    )
    getItems(v, 3) <- paste0("Trade|", flow, "|", getItems(v, 3))
    magpie_object <- mbind(magpie_object, v)
  }

  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  return(magpie_object)
}
