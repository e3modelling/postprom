#' Generate an Activity Report from GDX Data
#'
#' This function reads activity data from a GDX file,
#' assigns appropriate unit mappings, and returns a structured `magclass` object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#'
#' @return A magclass object containing filtered activity data with assigned units.
#' @details
#' - Reads `iActv` from the specified GDX file.
#' - Removes unwanted activity categories (`PG`, `H2P`, `H2INFR`).
#' - Maps corresponding unit labels to activity variables.
#' - Returns a magclass object with an added unit dimension.
#'
#' @examples
#' \dontrun{
#' reportACTV(path_gdx, c("MEA", "EU"))
#' }
#'
#' @importFrom magclass add_dimension getItems
#' @importFrom gdx readGDX
#' @export
reportACTV <- function(path, regions, years) {
  vector <- readGDX(path, c("imActv", "TRANSE"))
  iActv <- vector$imActv
  iActv <- iActv[, , c("DAC", "EW"), invert = TRUE]
  iActv <- vector$imActv[regions, years, setdiff(getItems(iActv, 3), c("PG", "H2P", "H2INFR"))]
  getItems(iActv, 3.1) <- paste0("Activity growth rate|", getItems(iActv, 3.1))

  transport <- as.character(vector$TRANSE)
  pred_years <- years[years > "y2020"]
  VActv_Pass <- readGDX(path, "V01ActivPassTrnsp", field = "l")[regions, pred_years, c("PC", "PB", "PT", "PN", "PA")]
  getItems(VActv_Pass, 3.1) <- paste0("Activity growth rate|", getItems(VActv_Pass, 3.1))

  pred_years <- years[years > "y2020"]
  VActv_Goods <- readGDX(path, "V01ActivGoodsTransp", field = "l")[regions, pred_years, c("GU", "GT", "GN")]
  getItems(VActv_Goods, 3.1) <- paste0("Activity growth rate|", getItems(VActv_Goods, 3.1))

  VActv <- mbind(VActv_Pass, VActv_Goods)

  iActv[regions, pred_years, paste0("Activity growth rate|", transport)] <- VActv
  old_names <- paste0("Activity growth rate|", transport)
  new_names <- paste0("Activity|", transport)

  # Find the positions of the old items
  item_pos <- match(old_names, getItems(iActv, 3))

  # Rename directly in the full object
  getItems(iActv, 3)[item_pos] <- new_names

  actv_units <- list(
    "Activity growth rate|IS" = "1",
    "Activity growth rate|NF" = "1",
    "Activity growth rate|CH" = "1",
    "Activity growth rate|BM" = "1",
    "Activity growth rate|PP" = "1",
    "Activity growth rate|FD" = "1",
    "Activity growth rate|EN" = "1",
    "Activity growth rate|TX" = "1",
    "Activity growth rate|OE" = "1",
    "Activity growth rate|OI" = "1",
    "Activity growth rate|SE" = "1",
    "Activity growth rate|AG" = "1",
    "Activity growth rate|HOU" = "1",
    "Activity|PC" = "km/vehicle",
    "Activity|PT" = "GPKM",
    "Activity|PA" = "million passengers carried",
    "Activity|PB" = "billion pKm/yr",
    "Activity|PN" = "billion pKm/yr",
    "Activity|GU" = "GTKM",
    "Activity|GT" = "GTKM",
    "Activity|GN" = "GTKM",
    "Activity growth rate|BU" = "1",
    "Activity growth rate|PCH" = "1",
    "Activity growth rate|NEN" = "1"
  )

  iActv <- add_dimension(iActv, dim = 3.2, add = "unit")
  getItems(iActv, 3.2) <- unname(actv_units[getItems(iActv, 3.1)])

  return(iActv)
}
