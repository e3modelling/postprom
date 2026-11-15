.landCoverMapping <- function() {
  utils::read.csv(system.file("extdata", "magpie-land-cover-variables.csv",
                              package = "postprom", mustWork = TRUE))
}

#' Prepare MAgPIE land-cover areas for OPEN-PROM reporting
#'
#' @param reportMifPath The selected MAgPIE round's `report.mif`. Its directory
#'   must contain the matching spatial outputs and cluster map.
#' @param outMifPath Destination for the OPEN-PROM `iLandCover_magpie.mif`.
#' @param scenario Scenario label for the exchange file.
#' @return The written path, invisibly.
#' @export
writeMagpieLandCover <- function(reportMifPath, outMifPath, scenario) {
  mapping <- .landCoverMapping()
  report <- magclass::collapseNames(magclass::read.report(reportMifPath, as.list = FALSE))
  available <- magclass::getNames(report)
  sources <- vapply(seq_len(nrow(mapping)), function(i) {
    aliases <- c(mapping$source[i], mapping$alternate_source[i])
    matches <- intersect(paste0(aliases[nzchar(aliases)], " (million ha)"), available)
    if (length(matches) != 1L) {
      stop("Expected one MAgPIE source in million ha for ", mapping$variable[i],
           " in ", reportMifPath, "; found ", length(matches))
    }
    matches
  }, character(1))

  regions <- madrat::toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")
  resCy <- unique(regions[["Region.Code"]])
  h12For <- ifelse(resCy %in% .EU28, "EUR", resCy)
  areas <- report[unique(h12For), , sources]
  rm(report)
  if (any(!is.finite(areas) | areas < 0)) stop("Invalid MAgPIE land cover in ", reportMifPath)
  magclass::getNames(areas) <- mapping$variable
  areas <- magclass::time_interpolate(areas, interpolated_year = 2010:2100,
                                      extrapolation_type = "constant")

  weights <- .buildCellLevelWeights(dirname(reportMifPath), .EU28,
                                    unique(mapping$weight_source))
  # Each area is an independent stock. In particular, Energy Crops is only
  # one component of Cropland, whose total comes directly from MAgPIE.
  mapping$magpie_emisco2_type <- ""
  landCover <- .disaggregateToResCy(areas, resCy, h12For, mapping, weights,
                                   cwWeights = NULL, requireAreaWeights = TRUE)
  magclass::getNames(landCover) <- paste0(mapping$variable, " (million ha)")

  staged <- tempfile(pattern = ".land-cover-", tmpdir = dirname(outMifPath), fileext = ".mif")
  on.exit(unlink(staged), add = TRUE)
  magclass::write.report(landCover, file = staged, model = "OPENPROM",
                         scenario = scenario, ndigit = 9)
  if (!file.rename(staged, outMifPath)) stop("Could not publish ", outMifPath)
  invisible(outMifPath)
}

reportLandCover <- function(path, regions, years) {
  exchange <- file.path(dirname(path), "iLandCover_magpie.mif")
  if (!file.exists(exchange)) return(NULL)  # pre-existing runs have no sidecar

  mode <- gdx::readGDX(path, "sLandEmiMode", react = "silent")
  if (!identical(as.character(mode), "softmif")) return(NULL)

  report <- magclass::collapseNames(magclass::read.report(exchange, as.list = FALSE))
  variables <- .landCoverMapping()$variable
  # Sidecars written before natural forest was added still contain four areas.
  natural <- "Land Cover|Forest|Planted|Natural|Re/Afforestation"
  if (!paste0(natural, " (million ha)") %in% magclass::getNames(report)) {
    variables <- setdiff(variables, natural)
  }
  report <- report[regions, years, paste0(variables, " (million ha)")]
  magclass::getNames(report) <- variables
  magclass::add_dimension(report, dim = 3.2, add = "unit", nm = "million ha")
}
