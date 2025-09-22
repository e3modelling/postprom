#' Generate a DAC Report from GDX Data
#'
#' This function reads DAC data from a GDX file,
#' assigns appropriate unit mappings, and returns a structured `magclass` object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#'
#' @return A magclass object containing filtered activity data with assigned units.
#' @details
#' - Reads `V06CapDAC` from the specified GDX file.
#' - Maps corresponding unit labels to DAC technologies.
#' - Returns a magclass object with an added unit dimension.
#'
#' @examples
#' \dontrun{
#' reportDAC(path_gdx, c("MEA", "EU"))
#' }
#'
#' @importFrom magclass add_dimension getItems dimSums mbind
#' @importFrom gdx readGDX
#' @export
reportDAC <- function(path, regions, years) {
    VCapDAC <- readGDX(path, c("V06CapDAC"), field = "l")[regions, years, ]
    magpie <- dimSums(VCapDAC, dim = 3)
    getItems(magpie, 3) <- "Direct Air Capture"

    getItems(VCapDAC, 3.1) <- paste0("Direct Air Capture|", getItems(VCapDAC, 3.1))
    VCapDAC <- mbind(VCapDAC, magpie)

    VCapDAC <- add_dimension(VCapDAC, dim = 3.2, add = "unit")
    getItems(VCapDAC, 3.2) <- "tCO2"

    return(VCapDAC)
}
