#' Process and Aggregate Energy Trade Data
#'
#' This function processes gross imports, gross exports and net exports of all fuels
#' from a GDX file and aggregates them to every fuel category of the BALEFtoEF mapping.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated trade data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportTrade(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom madrat toolAggregate
#' @importFrom magclass getItems dimSums add_dimension mbind
#' @importFrom dplyr filter %>%
#' @importFrom tidyr separate_rows
#' @export
reportTrade <- function(path, regions, years) {
  variables <- readGDX(
    path,
    c("V03Imp", "V03Exp", "VmImpNetEneBrnch"),
    field = "l"
  )
  units <- sub(".*\\((.*)\\).*", "\\1", variables$VmImpNetEneBrnch@description)

  # Every category of the mapping is reported. The fuels a run contains depend on the
  # model version (e.g. BGAS), so the mapping is restricted to the fuels actually present.
  # Categories overlap by design (Total, Fossil, Coal, Hard coal, ...), so they must not
  # be summed into a parent.
  BALEFtoEF <- read.csv(
    system.file("mappings", "BALEFtoEF.csv", package = "postprom")
  ) %>%
    separate_rows(EF) %>%
    filter(EF %in% getItems(variables$VmImpNetEneBrnch, 3))

  # Everything the model computes. Imports and exports are gross flows; net imports are
  # negated, so that Net Export is positive for a net exporter.
  flows <- list(
    "Import" = variables$V03Imp,
    "Export" = variables$V03Exp,
    "Net Export" = -variables$VmImpNetEneBrnch
  )

  magpie_object <- NULL
  for (flow in names(flows)) {
    v <- flows[[flow]][regions, years, ]
    v <- toolAggregate(v,
      weight = NULL, dim = 3,
      rel = BALEFtoEF, from = "BALEF", to = "EF"
    )
    getItems(v, 3) <- paste0("Trade|", flow, "|", getItems(v, 3))
    magpie_object <- mbind(magpie_object, v)
  }

  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  return(magpie_object)
}
