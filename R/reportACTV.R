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
  vector <- readGDX(path, c("iActv", "TRANSE"))
  iActv <- vector$iActv
  iActv <- vector$iActv[regions, years, setdiff(getItems(iActv, 3), c("PG", "H2P", "H2INFR"))]

  transport <- as.character(vector$TRANSE)
  pred_years <- years[years>"y2020"]
  VActv <- readGDX(path, "VActivPassTrnsp", field= "l")[regions, pred_years, transport]

  iActv[regions, pred_years, transport] <- VActv

  actv_units <- list(
    IS  = "billion US$2014",
    NF  = "billion US$2014",
    CH  = "billion US$2014",
    BM  = "billion US$2014",
    PP  = "billion US$2014",
    FD  = "billion US$2014",
    EN  = "billion US$2014",
    TX  = "billion US$2014",
    OE  = "billion US$2014",
    OI  = "billion US$2014",
    SE  = "billion US$2014",
    AG  = "billion US$2014",
    HOU = "billion US$2014",
    PC  = "km/vehicle",
    PT  = "GPKM",
    PA  = "million passengers carried",
    PB  = "billion pKm/yr",
    PN  = "billion pKm/yr",
    GU  = "GTKM",
    GT  = "GTKM",
    GN  = "GTKM",
    BU  = "billion US$2014",
    PCH = "billion US$2014",
    NEN = "billion US$2014"
  )

  iActv <- add_dimension(iActv, dim = 3.2, add = "unit")
  getItems(iActv, 3.2) <- unname(actv_units[getItems(iActv, 3.1)])
  getItems(iActv, 3.1) <- paste0("Activity|", getItems(iActv, 3.1))
  return(iActv)
}
