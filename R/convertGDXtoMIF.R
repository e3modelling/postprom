#' Convert GDX Files to MIF Format
#'
#' This function processes GDX files and converts them to MIF format, with options for validation, aggregation, and saving the output.
#'
#' @param .path A character vector specifying the path(s) to the GDX files.
#' @param mif_name A string specifying the name of the MIF file to create.
#' @param regions Optional; a character vector specifying the regions to include. Defaults to `NULL`, in which case regions are read from the GDX file.
#' @param fullValidation Optional; a logical value indicating whether full validation should be performed. Defaults to `TRUE`.
#' @param scenario_name Optional; a character vector specifying the scenario names. Defaults to the basename of `.path`.
#' @param aggregate Optional; a logical value indicating whether to aggregate data. Defaults to `TRUE`.
#' @param save Optional; a logical value indicating whether to save the output. Defaults to `TRUE`.
#' @param emissions Optional; a logical value indicating whether to generate a separate emissions csv file for running climate assessment. Defaults to `TRUE`.
#' @param htmlReport Optional; a logical value indicating whether to generate a HTML report file with piamValidation. Defaults to `FALSE`.

#' @return A list of scenario reports generated from the provided GDX files.
#' @details
#' - If `regions` is not provided, it is inferred from the GDX file using the `readGDX` function.
#' - The function supports multiple paths and scenarios, and appends reports when more than one path is provided.
#' - Full validation is optionally applied by calling `appendValidationMIF`.
#'
#' @examples
#' \dontrun{
#' convertGDXtoMIF(
#'   .path = c("path/to/gdx1", "path/to/gdx2"),
#'   mif_name = "output.mif",
#'   regions = c("runCYL", "World"),
#'   fullValidation = TRUE,
#'   scenario_name = c("Scenario1", "Scenario2"),
#'   aggregate = TRUE,
#'   emissions = TRUE,
#'   save = TRUE,
#'   htmlReport = FALSE,
#'   projectReport = FALSE
#' )
#' }
#' @importFrom magclass mbind dimSums getItems getRegions write.report read.report
#' @importFrom gdx readGDX
#' @export
convertGDXtoMIF <- function(.path, mif_name, regions = NULL, years = NULL,
                            fullValidation = TRUE, scenario_name = NULL,
                            aggregate = TRUE, emissions = TRUE, save = TRUE,
                            htmlReport = FALSE, projectReport = FALSE) {
  if (is.null(scenario_name)) scenario_name <- basename(.path)
  current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  append <- length(.path) > 1
  path_mif <- file.path(
    if (length(.path) > 1) dirname(.path[1]) else .path[1],
    if (length(.path) > 1) paste0("comparison_", current_time, "_", mif_name) else mif_name
  )

  scenarios_reports <- mapply(
    function(path, scenario) {
      convertGDXtoMIF_single(
        .path = path,
        regions = regions,
        years = years,
        path_mif = path_mif,
        scenario_name = scenario,
        aggregate = aggregate,
        append = append,
        save = save,
        emissions = emissions,
        htmlReport = htmlReport,
        projectReport = projectReport
      )
    },
    .path, scenario_name,
    SIMPLIFY = FALSE
  )

  if (fullValidation == TRUE) appendValidationMIF(.path[1], path_mif)

  return(scenarios_reports)
}
# Helpers -----------------------------------------------------------------
convertGDXtoMIF_single <- function(.path, path_mif, append, regions = NULL,
                                   years = NULL, scenario_name = NULL,
                                   aggregate = TRUE, emissions = TRUE, save = TRUE,
                                   htmlReport = TRUE, projectReport = TRUE) {
  print(paste0("Region aggregation: ", aggregate))
  print(paste0("Processing path: ", .path))
  path_gdx <- file.path(.path, "blabla.gdx")

  if (is.null(regions)) regions <- readGDX(path_gdx, "runCYL")
  if (is.null(years)) {
    years <- as.character(readGDX(path_gdx, "datay"))
    years <- c(years, as.character(readGDX(path_gdx, "an")))
    years <- paste0("y", years)
  }

  reports <- reportFinalEnergy(path_gdx, regions, years)
  reports <- mbind(reports, reportEmissions(path_gdx, regions, years))
  reports <- mbind(reports, reportSE(path_gdx, regions, years))
  reports <- mbind(reports, reportPE(path_gdx, regions, years))
  reports <- mbind(reports, reportGDP(path_gdx, regions, years))
  reports <- mbind(reports, reportPOP(path_gdx, regions, years))
  reports <- mbind(reports, reportPriceCarbon(path_gdx, regions, years))
  reports <- mbind(reports, reportPrice(path_gdx, regions, years))
  reports <- mbind(reports, reportCapacityElectricity(path_gdx, regions, years))
  reports <- mbind(reports, reportACTV(path_gdx, regions, years))
  reports <- mbind(reports, reportCapacityAdditions(path_gdx, regions, years))
  reports <- mbind(reports, reportCostsPGtechnologies(path_gdx, regions, years))
  reports <- mbind(reports, reportVehicles(path_gdx, regions, years))
  reports <- mbind(reports, reportGrossInlandConsumption(path_gdx, regions, years))
  reports <- mbind(reports, reportEquipmentCapacityShare(path_gdx, regions, years))


  # reportLearningCurve <- reportLearningCurve(path_gdx, regions, years)


  if (aggregate == TRUE) reports <- aggregateMIF(report = reports)

  # reports <- mbind(reports, reportLearningCurve)
  reports <- mbind(reports, reportBudget(
    magpieObject = reports, aggregate, budgetBaseYear = 2019,
    budget1p5 = 400, budget2c = 1150, probLabel = "67%"
  ))
  # reports <- mbind(reports, reportGrowthRates(reports))

  if (emissions == TRUE) generateEmissionsFile(.path, reports, years, scenario_name)

  if (save == TRUE) {
    write.report(
      reports,
      file = path_mif,
      model = "OPEN-PROM",
      scenario = scenario_name,
      append = append
    )
    print(paste0("Saving mif file in ", path_mif))

    if (htmlReport == TRUE) htmlReportValidation(.path, path_mif)

    if (projectReport == TRUE) projectReport(.path, path_mif)
  }

  return(reports)
}

aggregateMIF <- function(report) {
  reportData <- report
  items <- getItems(reportData, 3)
  worldCodes <- getRegions(reportData)

  # --- Define Item Categories ---
  # Exclude Price|Final Energy, Price|Carbon, Activity growth rate
  itemsToSum <- items[!grepl("^Price|^Activity growth rate", items)]

  # Take Price|Final Energy, Activity growth rate (excluding Carbon)
  itemsWeightedGdp <- items[grep("^(Price|Activity growth rate)(?!.*Carbon)", items, perl = TRUE)]
  itemPriceCarbon <- "Price|Carbon"

  # --- Extract Weights ---
  gdp <- reportData[, , "GDP|PPP.billion US$2015/yr"]
  emissions <- reportData[, , "Emissions|CO2"]

  # --- Helper Function for region aggregation ---
  aggregateRegion <- function(regionName, regionCodes) {
    # Subset data and weights for the target region
    regionData <- reportData[regionCodes, , ]
    regionGdp <- gdp[regionCodes, , ]
    regionEmissions <- emissions[regionCodes, , ]

    # Create mapping data frame for toolAggregate
    rmap <- data.frame(regionCode = regionCodes, targetRegion = regionName)

    # Sum aggregation
    sumAgg <- dimSums(regionData[, , itemsToSum], 1, na.rm = TRUE)
    getItems(sumAgg, 1) <- regionName

    # Weighted mean aggregation (GDP)
    gdpMeanAgg <- toolAggregate(
      regionData[, , itemsWeightedGdp],
      weight = regionGdp,
      rel = rmap,
      from = "regionCode",
      to = "targetRegion"
    )
    getItems(gdpMeanAgg, 1) <- regionName

    # Weighted mean aggregation (Emissions) for Price|Carbon
    carbonPriceMeanAgg <- toolAggregate(
      regionData[, , itemPriceCarbon],
      weight = regionEmissions,
      rel = rmap,
      from = "regionCode",
      to = "targetRegion"
    )
    getItems(carbonPriceMeanAgg, 1) <- regionName

    # Combine and return the aggregated parts
    return(mbind(sumAgg, gdpMeanAgg, carbonPriceMeanAgg))
  }
  # --- Calculate World Aggregation ---
  worldRegion <- aggregateRegion(regionName = "World", regionCodes = worldCodes)

  # --- Calculate EU-27 Aggregation ---
  regionMapping <- toolGetMapping(name = "EU28.csv", type = "regional", where = "mrprom")
  regionsEu27 <- regionMapping$ISO3.Code[regionMapping$ISO3.Code != "GBR"]
  eu27Region <- aggregateRegion(regionName = "EU", regionCodes = regionsEu27)

  reportOut <- mbind(reportData, worldRegion, eu27Region)

  return(reportOut)
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
