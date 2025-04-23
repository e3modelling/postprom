#' Process and Aggregate Primary Energy Data
#'
#' A function to process and aggregate primary energy data from a GDX file.
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated primary energy data.
#'
#' @examples
#' \dontrun{
#' result <- reportPE(system.file("extdata", "blabla.gdx", package = "openprom"), c("MEA"))
#' }
#' @importFrom gdx readGDX
#' @importFrom madrat toolAggregate
#' @importFrom magclass getItems getNames add_dimension mbind
#' @export
reportPE <- function(path, regions) {
  vars = c("VProdPrimary", "BALEF2EFS")
  values <- readGDX(path, vars, field = 'l')

  VProdPrimary <- values$VProdPrimary[regions, , ]
  sets <- values$BALEF2EFS
  names(sets) <- c("BAL", "EF")

  replacements <- c(
    "Gas fuels" = "Gases",
    "Solids" = "Coal",
    "Crude oil and Feedstocks" = "Oil",
    "Nuclear heat" = "Nuclear",
    "Solar energy" = "Solar",
    "Geothermal heat" = "Geothermal",
    "Steam" = "Heat"
  )
  sets$BAL <- Reduce(function(x, pattern) {
    gsub(pattern, replacements[pattern], x)
  }, names(replacements), init = sets$BAL)

    VProdPrimary <- toolAggregate(
    VProdPrimary[ , , unique(sets$EF)],
    dim = 3,
    rel = sets,
    from = "EF",
    to = "BAL"
  )

  # Update item names
  getItems(VProdPrimary, 3) <- paste0("Primary Energy|", getItems(VProdPrimary, 3))
  getNames(VProdPrimary)[getNames(VProdPrimary) == "Primary Energy|Total"] <- "Primary Energy"

  # Add dimensions and bind to magpie object
  VProdPrimary <- add_dimension(VProdPrimary, dim = 3.2, add = "unit", nm = "Mtoe")
  return(VProdPrimary)
}
