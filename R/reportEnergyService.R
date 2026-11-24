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
  EnergyService <- readGDX(path, c("V12EnergyService", "V02EnergyService"), field = "l")
  if (is.null(EnergyService)) {
    return(NULL)
  }
  agriculture <- EnergyService$V12EnergyService[regions, years, ]
  names(dimnames(agriculture))[3] <- "sector"
  getItems(agriculture, 3) <- paste0("Energy Service|AFOFI|", toTitleCase(gsub("_", " ", tolower(getItems(agriculture, 3)))))

  # Iron and Steel
  INDDOMtoServs <- rgdx.set(path, "INDDOMtoSERVS")
  IND_SERVS <- rgdx.set(path, "IND_SERVS", te = TRUE)
  IS <- EnergyService$V02EnergyService[regions, years, paste0(INDDOMtoServs$INDDOM, ".", INDDOMtoServs$IND_SERVS)]
  IS <- collapseDim(
    IS[, , "IS"],
    dim = 3.1
  )
  names(dimnames(IS))[3] <- "sector"
  getItems(IS, 3) <- paste0("Energy Service|Iron & Steel|", IND_SERVS$.te[match(getItems(IS, 3), IND_SERVS$i)])

  # Units
  ag_units <- c("1e9 ha", "1e9 ha", "1e9 ha", "1e6 ha", "1e9 ha", "1e9 LU", "1e6 m^3", "1e6 tonnes")
  is_units <- c("Mtonnes", "Mtonnes", "Mtonnes")
  
  IS <- helperAggregateLevel(IS, level = 2, recursive = TRUE)
  
  agriculture <- add_dimension(agriculture, dim = 3.2, add = "unit", nm = ag_units, expand = FALSE)
  IS <- add_dimension(IS, dim = 3.2, add = "unit", nm = is_units, expand = FALSE)

  magpie_object <- mbind(IS, agriculture)
  return(magpie_object)
}
