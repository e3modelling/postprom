#' Create a project-ready report from OPEN-PROM results
#'
#' Renames OPEN-PROM variables to match a project template, checks and converts
#' units, keeps five-year periods, removes EU aggregate regions, and writes one
#' concatenated project report for all supplied scenarios.
#'
#' @param .path A run path used to locate the project template and write the
#'   output files.
#' @param openPromVariables A MAgPIE object, a named list of MAgPIE objects, a
#'   `read.report()`-style nested list (`scenario -> model -> magpie`), or a MIF
#'   path for backwards-compatible file input.
#' @param openPromFile Optional MIF file path to check and use as fallback input
#'   when `openPromVariables` is `NULL`.
#' @param scenario_name Optional scenario name(s). Required for unnamed single
#'   MAgPIE object input.
#' @param model Model name to use when it cannot be detected from nested input.
#' @param output_basename Base name for the written `.mif` and `.xlsx` files.
#' @param project_template Project template filename or path. If `NULL`, the
#'   project report is skipped.
#' @param stripScenarioTimestamp Logical; remove trailing timestamps like
#'   `_2026-05-11_16-57-15` from project-report scenario names.
#' @return Invisibly returns a list with output paths and the prepared tables.
#'
#' @examples
#' \dontrun{
#' projectReport(.path = "path/to/run", openPromVariables = reports,
#'               scenario_name = "Scenario1")
#' projectReport(.path = "path/to/run", openPromFile = "path/to/reporting.mif")
#' }
#'
#' @importFrom magclass getItems getRegions getYears is.magpie mbind read.report write.report
#' @importFrom dplyr bind_rows
#' @importFrom utils read.csv
#' @export
projectReport <- function(.path, openPromVariables = NULL, openPromFile = NULL,
                          scenario_name = NULL, model = "OPEN-PROM",
                          output_basename = "outputForProject",
                          project_template = "project-template.csv",
                          stripScenarioTimestamp = TRUE) {

  if (is.null(project_template)) {
    message("Skipping project report: 'project_template' is NULL.")
    return(invisible(NULL))
  }

  if (is.character(openPromVariables) && length(openPromVariables) == 1 &&
      is.null(openPromFile) && is.null(scenario_name)) {
    openPromFile <- openPromVariables
    openPromVariables <- NULL
  }

  if (!is.null(openPromFile) && !file.exists(openPromFile)) {
    stop("Error: The data file '", openPromFile, "' does not exist.\n",
         "Current working directory: ", getwd())
  }

  if (is.null(openPromVariables)) {
    if (is.null(openPromFile)) {
      stop("Either 'openPromVariables' or 'openPromFile' must be provided.")
    }
    openPromVariables <- read.report(openPromFile)
  }

  templatePath <- findProjectTemplatePath(.path, project_template)

  project <- read.csv(templatePath)
  map <- toolGetMapping(name = "prom-project-template.csv", where = "postprom")
  reports <- normalizeProjectReportInput(openPromVariables, scenario_name, model)
  if (stripScenarioTimestamp) {
    reports <- stripProjectReportScenarioTimestamps(reports)
  }

  outputMif <- file.path(.path, paste0(output_basename, ".mif"))
  outputXlsx <- file.path(.path, paste0(output_basename, ".xlsx"))
  xlsxTables <- list()

  for (i in seq_along(reports)) {
    reportInfo <- reports[[i]]
    finalDataMagpie <- prepareProjectReportScenario(reportInfo$object, project, map)

    write.report(
      finalDataMagpie,
      file = outputMif,
      model = reportInfo$model,
      scenario = reportInfo$scenario,
      append = i > 1
    )

    xlsxTables[[i]] <- write.report(
      finalDataMagpie,
      file = NULL,
      model = reportInfo$model,
      scenario = reportInfo$scenario
    )
  }

  xlsxTable <- bind_rows(xlsxTables)
  writexl::write_xlsx(xlsxTable, outputXlsx)

  message(paste0("Saving project mif file in ", outputMif))
  message(paste0("Saving project xlsx file in ", outputXlsx))

  invisible(list(mif = outputMif, xlsx = outputXlsx, table = xlsxTable))
}

normalizeProjectReportInput <- function(openPromVariables, scenario_name = NULL,
                                        model = "OPEN-PROM") {
  if (is.magpie(openPromVariables)) {
    if (is.null(scenario_name) || length(scenario_name) != 1) {
      stop("'scenario_name' must be supplied for a single MAgPIE object.")
    }
    return(list(list(
      scenario = scenario_name,
      model = model,
      object = openPromVariables
    )))
  }

  if (!is.list(openPromVariables)) {
    stop("'openPromVariables' must be a MAgPIE object or a list of MAgPIE objects.")
  }

  reports <- list()

  for (scenarioIndex in seq_along(openPromVariables)) {
    scenarioObject <- openPromVariables[[scenarioIndex]]
    scenario <- names(openPromVariables)[scenarioIndex]

    if (is.null(scenario) || !nzchar(scenario)) {
      if (!is.null(scenario_name) && length(scenario_name) >= scenarioIndex) {
        scenario <- scenario_name[scenarioIndex]
      } else {
        scenario <- paste0("scenario", scenarioIndex)
      }
    }

    if (is.magpie(scenarioObject)) {
      reports[[length(reports) + 1]] <- list(
        scenario = scenario,
        model = model,
        object = scenarioObject
      )
    } else if (is.list(scenarioObject)) {
      for (modelName in names(scenarioObject)) {
        magpieObject <- scenarioObject[[modelName]]
        if (!is.magpie(magpieObject)) next

        reports[[length(reports) + 1]] <- list(
          scenario = scenario,
          model = if (nzchar(modelName)) modelName else model,
          object = magpieObject
        )
      }
    }
  }

  if (!length(reports)) {
    stop("No MAgPIE reports found in 'openPromVariables'.")
  }

  return(reports)
}

stripProjectReportScenarioTimestamps <- function(reports) {
  for (i in seq_along(reports)) {
    reports[[i]]$scenario <- sub(
      "_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}$",
      "",
      reports[[i]]$scenario
    )
  }

  return(reports)
}

findProjectTemplatePath <- function(.path, project_template = "project-template.csv") {
  candidates <- unique(c(
    project_template,
    file.path(.path, project_template),
    file.path(dirname(.path), project_template)
  ))
  existing <- candidates[file.exists(candidates)]

  if (length(existing)) {
    return(existing[1])
  }

  stop(
    "Error: Could not find project template '", project_template, "'. Checked: ",
    paste(normalizePath(candidates, mustWork = FALSE), collapse = "; ")
  )
}

prepareProjectReportScenario <- function(dataMagpie, project, map) {
  dataMagpie <- flattenProjectReportNames(dataMagpie)
  dataMagpie <- mapProjectReportVariables(dataMagpie, map)

  renamedClean <- cleanProjectReportVariable(getItems(dataMagpie, dim = 3))
  commonVars <- intersect(project$variable, renamedClean)

  if (!length(commonVars)) {
    stop("No project-template variables were found in the OPEN-PROM report.")
  }

  currentVarNames <- getItems(dataMagpie, dim = 3)
  currentClean <- cleanProjectReportVariable(currentVarNames)
  currentUnits <- extractProjectReportUnit(currentVarNames)
  namesByVariable <- setNames(currentVarNames, currentClean)
  unitsByVariable <- setNames(currentUnits, currentClean)
  keepNames <- unname(namesByVariable[commonVars])

  expectedUnits <- project$unit[match(commonVars, project$variable)]
  commonUnits <- unname(unitsByVariable[commonVars])
  unitMatch <- commonUnits == expectedUnits
  unitMatch[is.na(unitMatch)] <- FALSE

  checkUnits <- data.frame(
    variable = commonVars,
    magpieUnit = commonUnits,
    expectedUnit = expectedUnits,
    unitMatches = unitMatch,
    stringsAsFactors = FALSE
  )

  filteredMagpie <- dataMagpie[, , keepNames]

  usd2015to2010 <- 0.9253
  finalResults <- convertUnitsToExpected(
    magpieObj = filteredMagpie,
    unitTable = checkUnits,
    usd2015to2010 = usd2015to2010,
    allowUnrecognized = FALSE,
    quiet = TRUE
  )

  finalDataMagpie <- finalResults$object
  finalDataMagpie <- filterProjectReportRegions(finalDataMagpie)
  finalDataMagpie <- filterProjectReportYears(finalDataMagpie)

  return(mbind(finalDataMagpie))
}

mapProjectReportVariables <- function(dataMagpie, map) {
  varNames <- getItems(dataMagpie, dim = 3)
  varsClean <- cleanProjectReportVariable(varNames)
  units <- extractProjectReportUnit(varNames)

  map$OPEN_PROM <- trimws(map$OPEN_PROM)
  map$project_template <- trimws(map$project_template)
  map <- map[map$OPEN_PROM %in% varsClean, , drop = FALSE]

  if (!nrow(map)) {
    return(dataMagpie)
  }

  mappedIndex <- match(map$OPEN_PROM, varsClean)
  passthroughIndex <- setdiff(seq_along(varNames), mappedIndex)

  mappedMagpie <- dataMagpie[, , mappedIndex]
  mappedNames <- map$project_template
  mappedHasUnits <- !is.na(units[mappedIndex])
  mappedNames[mappedHasUnits] <- paste0(
    mappedNames[mappedHasUnits],
    " (",
    units[mappedIndex][mappedHasUnits],
    ")"
  )
  getItems(mappedMagpie, dim = 3) <- mappedNames

  mappedSourceClean <- map$OPEN_PROM

  if (length(passthroughIndex)) {
    passthroughMagpie <- dataMagpie[, , passthroughIndex]
    mappedMagpie <- mbind(passthroughMagpie, mappedMagpie)
    mappedSourceClean <- c(varsClean[passthroughIndex], mappedSourceClean)
  }

  return(collapseProjectReportDuplicates(mappedMagpie, dataMagpie, mappedSourceClean))
}

collapseProjectReportDuplicates <- function(dataMagpie, sourceMagpie, sourceClean) {
  itemNames <- getItems(dataMagpie, dim = 3)
  targetClean <- cleanProjectReportVariable(itemNames)
  targetUnits <- extractProjectReportUnit(itemNames)
  out <- NULL

  for (target in unique(targetClean)) {
    idx <- which(targetClean == target)
    unit <- unique(targetUnits[idx][!is.na(targetUnits[idx])])
    itemName <- target
    if (length(unit)) itemName <- paste0(target, " (", unit[1], ")")

    if (length(idx) == 1) {
      item <- dataMagpie[, , idx]
    } else if (grepl("^Price\\|", target)) {
      item <- weightedProjectReportPrice(
        prices = dataMagpie[, , idx],
        sourceMagpie = sourceMagpie,
        sourcePriceClean = sourceClean[idx],
        target = target
      )
    } else {
      item <- dimSums(dataMagpie[, , idx], dim = 3, na.rm = TRUE)
    }

    getItems(item, dim = 3) <- itemName
    out <- mbind(out, item)
  }

  return(out)
}

weightedProjectReportPrice <- function(prices, sourceMagpie, sourcePriceClean, target) {
  sourceWeightClean <- sub("^Price\\|", "", sourcePriceClean)
  sourceNames <- getItems(sourceMagpie, dim = 3)
  sourceClean <- cleanProjectReportVariable(sourceNames)
  weightIndex <- match(sourceWeightClean, sourceClean)

  if (anyNA(weightIndex)) {
    missingWeights <- sourceWeightClean[is.na(weightIndex)]
    keepWeighted <- !is.na(weightIndex)

    if (any(keepWeighted)) {
      warning(
        "Ignoring unweighted price source(s) for '", target,
        "' because matching final-energy weights were not found for: ",
        paste(missingWeights, collapse = "; "),
        call. = FALSE
      )
      prices <- prices[, , keepWeighted]
      sourcePriceClean <- sourcePriceClean[keepWeighted]
      sourceWeightClean <- sourceWeightClean[keepWeighted]
      weightIndex <- weightIndex[keepWeighted]
    } else {
      warning(
        "Using arithmetic mean for '", target,
        "' because matching final-energy weights were not found for: ",
        paste(missingWeights, collapse = "; "),
        call. = FALSE
      )
      return(dimSums(prices, dim = 3, na.rm = TRUE) / length(sourcePriceClean))
    }
  }

  if (length(sourcePriceClean) == 1) {
    return(prices)
  }

  weights <- sourceMagpie[, , weightIndex]
  getItems(weights, dim = 3) <- getItems(prices, dim = 3)

  denominator <- dimSums(weights, dim = 3, na.rm = TRUE)
  if (any(as.array(denominator) == 0, na.rm = TRUE)) {
    warning(
      "Zero final-energy weights found for '", target,
      "'. Weighted price will be undefined for those cells.",
      call. = FALSE
    )
  }

  numerator <- dimSums(prices * weights, dim = 3, na.rm = TRUE)
  return(numerator / denominator)
}

flattenProjectReportNames <- function(dataMagpie) {
  fullNames <- getItems(dataMagpie, dim = 3)

  if (any(grepl("\\(.*\\)$", fullNames))) {
    return(dataMagpie)
  }

  splitNames <- strsplit(fullNames, "\\.")
  flattenedNames <- vapply(splitNames, function(parts) {
    if (length(parts) < 2) return(parts[1])

    variable <- paste(parts[-length(parts)], collapse = ".")
    unit <- parts[length(parts)]
    paste0(variable, " (", unit, ")")
  }, character(1))

  getItems(dataMagpie, dim = 3) <- flattenedNames
  return(dataMagpie)
}

cleanProjectReportVariable <- function(x) {
  trimws(gsub("\\s*\\(.*\\)$", "", x))
}

extractProjectReportUnit <- function(x) {
  out <- rep(NA_character_, length(x))
  hasUnits <- grepl("\\(.*\\)$", x)
  out[hasUnits] <- gsub(".*\\((.*)\\)$", "\\1", x[hasUnits])
  return(out)
}

filterProjectReportRegions <- function(dataMagpie) {
  euAggregates <- c("EU", "EU27", "EU28", "Europe")
  keepRegions <- setdiff(getRegions(dataMagpie), euAggregates)
  return(dataMagpie[keepRegions, , ])
}

filterProjectReportYears <- function(dataMagpie) {
  yearsInt <- getYears(dataMagpie, as.integer = TRUE)
  keepYears <- getYears(dataMagpie)[yearsInt %% 5 == 0]
  return(dataMagpie[, keepYears, ])
}
