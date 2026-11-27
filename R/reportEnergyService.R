#' Generate Energy services report from GDX Data
#'
#' This function reads activity data from a GDX file,
#' assigns appropriate unit mappings, and returns a structured `magclass` object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#'
#' @return A magclass object containing filtered energy service data with assigned units.
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' reportEnergyService(path_gdx, c("MEA", "USA"))
#' }
#'
#' @importFrom magclass add_dimension getItems
#' @importFrom gdx readGDX
#' @importFrom tools toTitleCase
#' @export
reportEnergyService <- function(path, regions, years) {
  EnergyService <- readGDX(path, c("V12EnergyServices"), field = "l")[regions, years, ]
  if (is.null(EnergyService)) {
    return(NULL)
  }

  getItems(EnergyService, 3) <- paste0("Energy Service|AFOFI|", toTitleCase(gsub("_", " ", tolower(getItems(EnergyService, 3)))))

  units <- c("1e9 ha", "1e9 ha", "1e9 ha", "1e6 ha", "1e9 ha", "1e9 LU", "1e6 m^3", "1e6 tonnes")
  EnergyService <- add_dimension(EnergyService, dim = 3.2, add = "unit", nm = units, expand = FALSE)
  return(EnergyService)
}
