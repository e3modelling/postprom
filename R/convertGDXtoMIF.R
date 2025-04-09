#' Convert GDX to MIF Format
#'
#' This function processes GDX files for specified regions and generates a MIF
#' report. It supports both single and multiple scenario paths, with the option
#' to create a comparison mif.
#'
#' @param .path Character vector. Path(s) to the input GDX file(s). If multiple paths
#'   are provided, results are appended to the same MIF file.
#' @param regions Character vector. A list of regions to include in the analysis.
#' @param mif_name Character. Name of the output MIF file to be created. If NULL,
#'   the function will return the formatted content instead of writing it to a file.
#' @param scenario_name Character. Name of the scenario to associate with the
#'   generated MIF file. If NULL, the base name of the path is used.
#'
#' @details
#' This function reads data from GDX files and aggregates metrics such as:
#' - Final Energy
#' - Emissions
#' - Secondary Energy (SE)
#' - Primary Energy (PE)
#' - Gross Domestic Product (GDP)
#' - Population (POP)
#' - Carbon Prices
#' - Price Data
#' - Electricity Capacity
#'
#' These metrics are combined into a single data structure and written to a MIF file
#' in a format compatible with the OPEN-PROM model.
#'
#' If multiple paths are provided via `.path`, results are appended to the same
#' output file specified by `mif_name`.
#'
#' @return If `mif_name` is NULL, the function returns the aggregated data content.
#' Otherwise, it writes the MIF file and returns NULL.
#'
#' @examples
#' \dontrun{
#' convertGDXtoMIF(
#'   .path = c("path/to/scenario1", "path/to/scenario2"),
#'   regions = c("MEA", "USA"),
#'   mif_name = "output.mif",
#'   scenario_name = "MyScenario"
#' )
#' }
#'
#' @seealso \code{\link[magclass]{write.report}}, \code{\link[magclass]{mbind}}
#' @importFrom magclass mbind write.report
#' @importFrom gdx readGDX
#' @export
convertGDXtoMIF <- function(.path, regions, mif_name,
                            scenario_name = NULL) {
  if (length(.path) == 1) {
    if (is.null(scenario_name)) scenario_name <- basename(.path)
    return(convertGDXtoMIF_single(
      .path, regions, mif_name, .path,
      scenario_name = scenario_name
    ))
  }

  path_mif <- file.path(.path[1], "..") # save comparison mif in base dir
  if (is.null(scenario_name)) scenario_name <- basename(.path)

  mapply(function(path, scenario) {
    convertGDXtoMIF_single(
      .path = path,
      regions = regions,
      mif_name = mif_name,
      path_mif = path_mif,
      scenario_name = scenario,
      append = TRUE
    )
  }, .path, scenario_name)
}
# Helper -----------------------------------------------------------------
convertGDXtoMIF_single <- function(.path, regions, mif_name, path_mif,
                                   scenario_name = NULL, append = FALSE) {
  # path is path to scenario
  print(paste("Processing path:", .path))
  path_gdx <- file.path(.path, "blabla.gdx")
  path_mif <- if (is.null(path_mif)) mif_name else file.path(path_mif, mif_name)

  FE <- reportFinalEnergy(path_gdx, regions)
  EMI <- reportEmissions(path_gdx, regions)
  SE <- reportSE(path_gdx, regions)
  PE <- reportPE(path_gdx, regions)
  GDP <- reportGDP(path_gdx, regions)
  POP <- reportPOP(path_gdx, regions)
  PCar <- reportPriceCarbon(path_gdx, regions)
  Price <- reportPrice(path_gdx, regions)
  CapElec <- reportCapacityElectricity(path_gdx, regions)

  magpie_reporting <- mbind(FE, EMI, SE, PE, GDP, POP, PCar, Price, CapElec)
  write.report(
    magpie_reporting,
    file = path_mif,
    model = "OPEN-PROM",
    scenario = scenario_name,
    append = append
  )
  print(paste0('Saved mif file in ', path_mif))
}

#' @export
aggregateMIF <- function(path_report, path_save = NULL) {
  report <- read.report(path_report)

  for (i in seq_along(report)) {
    report_data <- report[[i]][[1]]
    items <- getItems(report_data, 3)
    get_items <- items[grep("^Price", items)]
    get_items_not <- items[!grepl("^Price", items)]

    # Calculate the sum for the 'not Price' items
    add_region_GLO <- dimSums(report_data[,,get_items_not], 1, na.rm = TRUE)
    getItems(add_region_GLO, 1) <- "World"

    # Extract GDP data and region map
    gdp <- report_data[, , "GDP|PPP (billion US$2015/yr)"]
    rmap_world <- as.data.frame(getRegions(report_data))
    names(rmap_world) <- "Region.Code"
    rmap_world$V2 <- "World" # V2 is just a placeholder

    # Aggregate the data for "Price" items using the GDP as weights
    add_region_GLO_mean <- toolAggregate(
      report_data[,,get_items], weight = gdp, rel = rmap_world, from = "Region.Code", to = "V2")
    getItems(add_region_GLO_mean, 1) <- "World"

    # Combine the calculated regions
    world_reg <- mbind(add_region_GLO, add_region_GLO_mean)

    # Bind the new aggregated data with the original report
    report[[i]][[1]] <- mbind(report_data, world_reg)
  }

    write.report(
      report,
      file=path_save
    )
}

