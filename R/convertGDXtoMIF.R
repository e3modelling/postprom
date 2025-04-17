#' Convert GDX Data to MIF Format
#'
#' This function processes GDX files for specified regions and generates
#' a MIF (Model Intercomparison Format) file. It supports handling multiple
#' scenarios and includes options for appending validation data and performing
#' regions aggregation.
#'
#' @param .path Character vector. Paths to scenario directories.
#' @param regions Character vector. A list of regions to include in the analysis.
#' @param mif_name Character. Name of the output MIF file to be created.
#' @param fullValidation Logical. Whether to append validation data to the output (default: TRUE).
#' @param scenario_name Character vector. Names of the scenarios being processed. Defaults to the basename of `.path`.
#' @param aggregate Logical. Whether to perform region aggregation (default: TRUE).
#'
#' @details
#' This function processes GDX files by calling the helper function `convertGDXtoMIF_single()`
#' for each scenario. If multiple paths are provided, results are combined into a
#' timestamped comparison MIF file. If validation is enabled, it appends validation
#' data using `appendValidationMIF()`.
#'
#' @return Generates a MIF file with the processed data. Returns NULL.
#'
#' @examples
#' \dontrun{
#' convertGDXtoMIF(
#'   .path = c("path/to/scenario1", "path/to/scenario2"),
#'   regions = c("MEA", "USA"),
#'   mif_name = "output.mif",
#'   fullValidation = TRUE,
#'   scenario_name = c("Scenario1", "Scenario2"),
#'   aggregate = TRUE
#' )
#' }
#'
#' @seealso \code{\link[magclass]{write.report}}, \code{\link[magclass]{mbind}}
#' @importFrom magclass mbind dimSums getItems getRegions write.report read.report
#' @importFrom gdx readGDX
#' @export
convertGDXtoMIF <- function(.path, regions, mif_name, fullValidation = TRUE,
                            scenario_name = NULL, aggregate = TRUE,
                            save = TRUE) {
  if (is.null(scenario_name)) scenario_name <- basename(.path)
  current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  append <- length(.path) > 1
  path_mif <- file.path(
    if (length(.path) > 1) dirname(.path[1]) else .path[1],
    if (length(.path) > 1) paste0("comparison_", current_time, "_", mif_name) else mif_name
  )

  scenarios_reports <- mapply(function(path, scenario) {
    convertGDXtoMIF_single(
      .path = path,
      regions = regions,
      path_mif = path_mif,
      scenario_name = scenario,
      aggregate = aggregate,
      append = append,
      save = save
    )
  },
  .path, scenario_name,
  SIMPLIFY = FALSE
  )

  if (fullValidation == TRUE) appendValidationMIF(.path[1], path_mif)
  return(scenarios_reports)
}
# Helpers -----------------------------------------------------------------
convertGDXtoMIF_single <- function(.path, regions, path_mif, append,
                                   scenario_name = NULL, aggregate = TRUE,
                                   save = TRUE) {
  print(paste0("Region aggregation: ", aggregate))
  print(paste0("Processing path: ", .path))
  path_gdx <- file.path(.path, "blabla.gdx")

  reports <- reportFinalEnergy(path_gdx, regions)
  reports <- mbind(reports, reportEmissions(path_gdx, regions))
  reports <- mbind(reports, reportSE(path_gdx, regions))
  reports <- mbind(reports, reportPE(path_gdx, regions))
  reports <- mbind(reports, reportGDP(path_gdx, regions))
  reports <- mbind(reports, reportPOP(path_gdx, regions))
  reports <- mbind(reports, reportPriceCarbon(path_gdx, regions))
  reports <- mbind(reports, reportPrice(path_gdx, regions))
  reports <- mbind(reports, reportCapacityElectricity(path_gdx, regions))

  if (aggregate == TRUE) reports <- aggregateMIF(report = reports)

  if(save == TRUE) {
    write.report(
      reports,
      file = path_mif,
      model = "OPEN-PROM",
      scenario = scenario_name,
      append = append
    )
    print(paste0("Saved mif file in ", path_mif))
  }
  return(reports)
}

aggregateMIF <- function(report) {
  report_data <- report
  items <- getItems(report_data, 3)
  get_items <- items[grep("^Price", items)]
  get_items_not <- items[!grepl("^Price", items)]

  # Calculate the sum for the 'not Price' items
  add_region_GLO <- dimSums(report_data[, , get_items_not], 1, na.rm = TRUE)
  getItems(add_region_GLO, 1) <- "World"

  # Extract GDP data and region map
  gdp <- report_data[, , "GDP|PPP.billion US$2015/yr"]
  rmap_world <- as.data.frame(getRegions(report_data))
  names(rmap_world) <- "Region.Code"
  rmap_world$V2 <- "World" # V2 is just a placeholder

  # Aggregate the data for "Price" items using the GDP as weights
  add_region_GLO_mean <- toolAggregate(
    report_data[, , get_items],
    weight = gdp, rel = rmap_world, from = "Region.Code", to = "V2"
  )
  getItems(add_region_GLO_mean, 1) <- "World"

  # Combine the calculated regions
  world_reg <- mbind(add_region_GLO, add_region_GLO_mean)

  # Bind the new aggregated data with the original report
  report <- mbind(report_data, world_reg)
  return(report)
}

appendValidationMIF <- function(runpath, path_mif) {
  base_dir <- dirname(dirname(runpath[1]))
  validation_path <- file.path(base_dir, "fullValidation.mif")

  if (file.exists(validation_path)) {
    reporting_fullVALIDATION <- read.report(validation_path)
    write.report(reporting_fullVALIDATION,
      file = path_mif,
      append = TRUE
    )
  } else {
    message(paste0(
      "Skipping validation: 'fullValidation.mif' not found in ",
      base_dir
    ))
  }
}
