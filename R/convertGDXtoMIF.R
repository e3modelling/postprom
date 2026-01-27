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
#' convertGDXtoMIF(.path = c("path/to/gdx1", "path/to/gdx2"),
#'                 mif_name = "output.mif",
#'                 regions = c("runCYL", "World"),
#'                 fullValidation = TRUE,
#'                 scenario_name = c("Scenario1", "Scenario2"),
#'                 aggregate = TRUE,
#'                 emissions = TRUE,
#'                 save = TRUE,
#'                 htmlReport = FALSE,
#'                 projectReport = FALSE)
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

  scenarios_reports <- mapply(function(path, scenario) {
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
                                   aggregate = TRUE, emissions=TRUE, save = TRUE,
                                   htmlReport = TRUE, projectReport = TRUE) {
  print(paste0("Region aggregation: ", aggregate))
  print(paste0("Processing path: ", .path))
  path_gdx <- file.path(.path, "blabla.gdx")

  if (is.null(regions)) regions <- readGDX(path_gdx, "runCYL")
  if (is.null(years)) {
    years <- as.character(c(2010:2020))
    years <- c(years, as.character(readGDX(path_gdx, "an")))
    years <- paste0("y", years)
  }

  reports <- reportFinalEnergy(path_gdx, regions, years)
  
  sLink2MAgPIE <- readGDX(path_gdx, "sLink2MAgPIE")
  
  #if (is.null(sLink2MAgPIE)) {
    sLink2MAgPIE <- 1
  #}
  
  if (sLink2MAgPIE == 1) {
    print("coupled with MAgPIE")
    reportEmissionsMagpie <- reportEmissionsMagpie(path_gdx, regions, years)
    AFOLU <- reportEmissionsMagpie[,,c("Emissions|CO2|AFOLU|Agriculture", "Emissions|CO2|Land")]
    AFOLU <- dimSums(AFOLU, 3)
    getItems(AFOLU, 3) <- "Emissions|CO2|AFOLU"
    AFOLU_unit <- add_dimension(AFOLU, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
    reportEmissionsMagpie <- mbind(reportEmissionsMagpie, AFOLU_unit)
    reportEmissions <- reportEmissions(path_gdx, regions, years, AFOLU, sLink2MAgPIE)
    reportEmissions <- reportEmissions[,,setdiff(getItems(reportEmissions,3),getItems(reportEmissionsMagpie,3))]
    # map <- toolGetMapping(name = "NavigateEmissions.csv",
    #                       type = "sectoral",
    #                       where = "mrprom")
    # 
    new_row <- data.frame(
      Emissions = "Emissions|CO2|AFOLU",
      stringsAsFactors = FALSE
    )
    # # Combine with the existing map
    # map <- rbind(map, new_row)
    # 
    reportEmissions <- reportEmissions[,,setdiff(getItems(reportEmissions,3.1), new_row[,"Emissions"])]
    
    reportEmissions <- mbind(reportEmissions, reportEmissionsMagpie)
    
  } else {
    reportEmissions <- reportEmissions(path_gdx, regions, years)
  }
  
  reports <- mbind(reports, reportEmissions)
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
  
  reportLearningCurve <- reportLearningCurve(path_gdx, regions, years)
  GrossInlandConsumption <- reportGrossInlandConsumption(path_gdx, regions, years)

  #add dummy NA values for all regions and years
  GrossInlandConsumption <- add_columns(GrossInlandConsumption, addnm = setdiff(getRegions(reports),getRegions(GrossInlandConsumption)), dim = 1, fill = NA)
  GrossInlandConsumption <- add_columns(GrossInlandConsumption, addnm = setdiff(getYears(reports),getYears(GrossInlandConsumption)), dim = 2, fill = NA)
  reportLearningCurve <- add_columns(reportLearningCurve, addnm = setdiff(getRegions(reports),getRegions(reportLearningCurve)), dim = 1, fill = NA)
  
  if (aggregate == TRUE) {
      reports <- aggregateMIF(report = reports)
  } else {
    reports <- add_columns(reports, addnm = setdiff(getRegions(GrossInlandConsumption),getRegions(reports)), dim = 1, fill = NA)
  }
  reports <- mbind(reports, GrossInlandConsumption)
  reports <- mbind(reports, reportLearningCurve)
  reports <- mbind(reports, reportBudget(magpieObject=reports,aggregate, budgetBaseYear = 2019,
                       budget1p5= 400, budget2c= 1150, probLabel= "67%"))

  if (emissions == TRUE) generateEmissionsFile(.path, reports, years, scenario_name)

  if(save == TRUE) {
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
  report_data <- report
  items <- getItems(report_data, 3)
  get_items <- items[grep("^Price|^Activity growth rate", items)]
  get_items_not <- items[!grepl("^Price|^Activity growth rate", items)]

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
