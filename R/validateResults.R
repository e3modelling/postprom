#' Default indicator validation checks
#'
#' Loads the lightweight bounds and published benchmark checks shipped with
#' postprom. The returned data frame can be filtered, edited, or extended and
#' passed to \code{\link{validateResults}}.
#'
#' @return A data frame describing validation checks.
#' @export
defaultValidationChecks <- function() {
  path <- system.file(
    "extdata", "result-validation-checks.csv",
    package = "postprom"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "result-validation-checks.csv")
  }
  if (!file.exists(path)) {
    stop("Could not find the packaged result-validation checks.")
  }

  checks <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  checks$enabled <- tolower(as.character(checks$enabled)) %in%
    c("true", "t", "1", "yes")
  checks
}

#' Validate reported Postprom indicators
#'
#' Applies physical bounds and published benchmark checks to indicators already
#' calculated by `reportIndicators()`. This function never recalculates an
#' indicator formula.
#'
#' @param results A completed MAgPIE report, or a uniquely named list of
#'   completed MAgPIE reports.
#' @param checks A validation-check data frame, normally returned by
#'   \code{\link{defaultValidationChecks}}.
#'
#' @return An object of class `postprom_validation` with `findings`, `summary`,
#'   and the extracted `indicator_values`.
#' @export
validateResults <- function(results, checks = defaultValidationChecks()) {
  reports <- normalizeValidationReports(results)
  checks <- validateCheckTable(checks)
  registry <- validationIndicatorRegistry()

  findings <- list()
  indicatorValues <- list()
  findingIndex <- 0L
  valueIndex <- 0L

  for (scenario in names(reports)) {
    report <- reports[[scenario]]
    tidy <- tidyValidationReport(report)
    scenarioValues <- extractValidationIndicatorValues(
      tidy, scenario, registry
    )
    valueIndex <- valueIndex + 1L
    indicatorValues[[valueIndex]] <- scenarioValues

    quality <- inspectValidationInputs(tidy, scenario, registry)
    for (finding in quality$findings) {
      findingIndex <- findingIndex + 1L
      findings[[findingIndex]] <- finding
    }

    activeChecks <- checks[checks$enabled, , drop = FALSE]
    for (i in seq_len(nrow(activeChecks))) {
      check <- activeChecks[i, , drop = FALSE]
      checkFindings <- evaluateValidationCheck(
        check = check,
        values = scenarioValues,
        scenario = scenario,
        registry = registry,
        invalidKeys = quality$invalidKeys
      )
      for (finding in checkFindings) {
        findingIndex <- findingIndex + 1L
        findings[[findingIndex]] <- finding
      }
    }
  }

  findingTable <- bindValidationRows(findings, emptyValidationFindings())
  valueTable <- bindValidationRows(
    indicatorValues,
    emptyValidationIndicatorValues()
  )
  summaryTable <- summarizeValidationFindings(
    findingTable, names(reports)
  )

  structure(
    list(
      findings = findingTable,
      summary = summaryTable,
      indicator_values = valueTable
    ),
    class = "postprom_validation"
  )
}

#' @export
print.postprom_validation <- function(x, ...) {
  cat("Postprom indicator validation\n")
  print(x$summary, row.names = FALSE)
  exceptions <- x$findings[x$findings$status != "pass", , drop = FALSE]
  if (!nrow(exceptions)) {
    cat("No validation exceptions found.\n")
  } else {
    cat(nrow(exceptions), "validation exception(s). Showing up to 10:\n")
    columns <- c(
      "scenario", "check_id", "indicator", "region", "start_year",
      "end_year", "status", "message"
    )
    print(utils::head(exceptions[, columns, drop = FALSE], 10),
          row.names = FALSE)
  }
  invisible(x)
}

validationIndicatorRegistry <- function() {
  list(
    primary_energy_intensity = list(
      label = "Primary-energy intensity of GDP",
      variable = "Intensity|Primary Energy",
      unit = "Mtoe/billion US$2015",
      dependencies = data.frame(
        variable = c("Primary Energy", "GDP|PPP"),
        unit = c("Mtoe", "billion US$2015/yr"),
        denominator = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      )
    ),
    final_energy_intensity = list(
      label = "Final-energy intensity of GDP",
      variable = "Intensity|Final Energy",
      unit = "Mtoe/billion US$2015",
      dependencies = data.frame(
        variable = c("Final Energy", "GDP|PPP"),
        unit = c("Mtoe", "billion US$2015/yr"),
        denominator = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      )
    ),
    primary_energy_carbon_intensity = list(
      label = "Carbon intensity of primary energy",
      variable = "Carbon Intensity|Primary Energy",
      unit = "Mt CO2/Mtoe",
      dependencies = data.frame(
        variable = c("Emissions|CO2|Energy", "Primary Energy"),
        unit = c("Mt CO2/yr", "Mtoe"),
        denominator = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      )
    ),
    energy_co2_gdp_intensity = list(
      label = "Energy CO2 intensity of GDP",
      variable = "Carbon Intensity|GDP|Energy",
      unit = "Mt CO2/billion US$2015",
      dependencies = data.frame(
        variable = c("Emissions|CO2|Energy", "GDP|PPP"),
        unit = c("Mt CO2/yr", "billion US$2015/yr"),
        denominator = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      )
    ),
    electrification_rate = list(
      label = "Electrification rate",
      variable = "Final Energy|Electricity Share",
      unit = "1",
      dependencies = data.frame(
        variable = c("Final Energy|Electricity", "Final Energy"),
        unit = c("Mtoe", "Mtoe"),
        denominator = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      )
    ),
    electricity_carbon_intensity = list(
      label = "Carbon intensity of electricity",
      variable = "Carbon Intensity|Secondary Energy|Electricity",
      unit = "Mt CO2/TWh",
      dependencies = data.frame(
        variable = c(
          "Emissions|CO2|Energy|Supply|Electricity",
          "Secondary Energy|Electricity"
        ),
        unit = c("Mt CO2/yr", "TWh"),
        denominator = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      )
    ),
    fossil_primary_energy_share = list(
      label = "Fossil share of primary energy",
      variable = "Primary Energy|Fossil Share",
      unit = "1",
      dependencies = data.frame(
        variable = c(
          "Primary Energy|Coal",
          "Primary Energy|Gas",
          "Primary Energy|Oil",
          "Primary Energy"
        ),
        unit = rep("Mtoe", 4),
        denominator = c(FALSE, FALSE, FALSE, TRUE),
        stringsAsFactors = FALSE
      )
    )
  )
}

normalizeValidationReports <- function(results) {
  if (magclass::is.magpie(results)) {
    return(list(scenario = results))
  }
  if (!is.list(results) || !length(results)) {
    stop("'results' must be a MAgPIE object or a named list of MAgPIE objects.")
  }
  reportNames <- names(results)
  if (is.null(reportNames) || any(!nzchar(reportNames)) ||
      anyDuplicated(reportNames)) {
    stop("A list of reports must have unique, non-empty scenario names.")
  }
  if (!all(vapply(results, magclass::is.magpie, logical(1)))) {
    stop("Every element of 'results' must be a MAgPIE object.")
  }
  results
}

tidyValidationReport <- function(report) {
  sets <- magclass::getSets(report)
  variableColumn <- unname(sets["d3.1"])
  unitColumn <- unname(sets["d3.2"])
  if (is.na(variableColumn) || !nzchar(variableColumn) ||
      is.na(unitColumn) || !nzchar(unitColumn)) {
    stop("The report must have variable and unit subdimensions.")
  }

  data <- as.data.frame(quitte::as.quitte(report))
  if (!all(c(variableColumn, unitColumn, "region", "period", "value") %in%
           names(data))) {
    stop("Could not identify variable, unit, region, period, and value columns.")
  }
  data.frame(
    region = as.character(data$region),
    year = as.integer(data$period),
    variable = as.character(data[[variableColumn]]),
    unit = as.character(data[[unitColumn]]),
    value = as.numeric(data$value),
    stringsAsFactors = FALSE
  )
}

extractValidationIndicatorValues <- function(tidy, scenario, registry) {
  output <- list()
  index <- 0L
  for (indicatorId in names(registry)) {
    entry <- registry[[indicatorId]]
    rows <- tidy[tidy$variable == entry$variable, , drop = FALSE]
    if (!nrow(rows)) next
    index <- index + 1L
    output[[index]] <- data.frame(
      scenario = scenario,
      indicator_id = indicatorId,
      indicator = entry$label,
      region = rows$region,
      year = rows$year,
      value = rows$value,
      unit = rows$unit,
      stringsAsFactors = FALSE
    )
  }
  bindValidationRows(output, emptyValidationIndicatorValues())
}

inspectValidationInputs <- function(tidy, scenario, registry) {
  findings <- list()
  invalidKeys <- list()
  index <- 0L

  for (indicatorId in names(registry)) {
    entry <- registry[[indicatorId]]
    indicatorRows <- tidy[tidy$variable == entry$variable, , drop = FALSE]
    keyName <- paste(scenario, indicatorId, sep = "::")
    invalid <- character()

    if (!nrow(indicatorRows)) {
      index <- index + 1L
      findings[[index]] <- newValidationFinding(
        scenario, paste0("availability_", indicatorId), indicatorId,
        entry$label, "availability", "All", NA, NA, NA, NA, NA,
        entry$unit, "fail",
        paste0("Reported indicator '", entry$variable, "' is missing."),
        "Postprom report"
      )
      invalidKeys[[keyName]] <- "*"
      next
    }

    actualUnits <- unique(indicatorRows$unit)
    unitOk <- length(actualUnits) == 1L &&
      identical(actualUnits, entry$unit)
    duplicateRows <- duplicated(
      paste(indicatorRows$region, indicatorRows$year, sep = "::")
    )
    status <- if (unitOk && !any(duplicateRows)) "pass" else "fail"
    messages <- character()
    if (!unitOk) {
      messages <- c(
        messages,
        paste0(
          "Expected unit '", entry$unit, "'; found '",
          paste(actualUnits, collapse = ", "), "'."
        )
      )
      invalid <- "*"
    }
    if (any(duplicateRows)) {
      messages <- c(messages, "Duplicate region-year indicator values found.")
      invalid <- "*"
    }
    if (!length(messages)) messages <- "Indicator and unit are available."
    index <- index + 1L
    findings[[index]] <- newValidationFinding(
      scenario, paste0("availability_", indicatorId), indicatorId,
      entry$label, "availability", "All",
      min(indicatorRows$year), max(indicatorRows$year),
      NA, NA, NA, entry$unit, status,
      paste(messages, collapse = " "), "Postprom report"
    )

    dependenciesOk <- TRUE
    dependencyMessages <- character()
    for (i in seq_len(nrow(entry$dependencies))) {
      dependency <- entry$dependencies[i, , drop = FALSE]
      dependencyRows <- tidy[
        tidy$variable == dependency$variable &
          tidy$unit == dependency$unit,
        ,
        drop = FALSE
      ]
      anyVariable <- tidy$variable == dependency$variable
      if (!nrow(dependencyRows)) {
        dependenciesOk <- FALSE
        invalid <- "*"
        if (any(anyVariable)) {
          found <- unique(tidy$unit[anyVariable])
          dependencyMessages <- c(
            dependencyMessages,
            paste0(
              dependency$variable, " has unit '",
              paste(found, collapse = ", "), "' instead of '",
              dependency$unit, "'."
            )
          )
        } else {
          dependencyMessages <- c(
            dependencyMessages,
            paste0("Source variable '", dependency$variable, "' is missing.")
          )
        }
        next
      }

      duplicateDependencies <- duplicated(
        paste(dependencyRows$region, dependencyRows$year, sep = "::")
      )
      if (any(duplicateDependencies)) {
        dependenciesOk <- FALSE
        invalid <- "*"
        dependencyMessages <- c(
          dependencyMessages,
          paste0("Source variable '", dependency$variable, "' is duplicated.")
        )
      }

      invalidRows <- !is.finite(dependencyRows$value)
      if (isTRUE(dependency$denominator)) {
        invalidRows <- invalidRows | dependencyRows$value <= 0
      }
      if (any(invalidRows)) {
        dependenciesOk <- FALSE
        rowKeys <- paste(
          dependencyRows$region[invalidRows],
          dependencyRows$year[invalidRows],
          sep = "::"
        )
        invalid <- unique(c(invalid, rowKeys))
        for (rowIndex in which(invalidRows)) {
          reason <- if (!is.finite(dependencyRows$value[rowIndex])) {
            "is not finite"
          } else {
            "is not positive"
          }
          index <- index + 1L
          findings[[index]] <- newValidationFinding(
            scenario, paste0("inputs_", indicatorId), indicatorId,
            entry$label, "input", dependencyRows$region[rowIndex],
            dependencyRows$year[rowIndex], dependencyRows$year[rowIndex],
            dependencyRows$value[rowIndex], NA, NA, dependency$unit,
            "fail",
            paste0(
              "Source variable '", dependency$variable, "' ", reason, "."
            ),
            "Postprom report"
          )
        }
      }
    }

    if (dependenciesOk) {
      index <- index + 1L
      findings[[index]] <- newValidationFinding(
        scenario, paste0("inputs_", indicatorId), indicatorId,
        entry$label, "input", "All",
        min(indicatorRows$year), max(indicatorRows$year),
        NA, NA, NA, entry$unit, "pass",
        "Required source variables and denominators are valid.",
        "Postprom report"
      )
    } else if (length(dependencyMessages)) {
      index <- index + 1L
      findings[[index]] <- newValidationFinding(
        scenario, paste0("inputs_", indicatorId), indicatorId,
        entry$label, "input", "All", NA, NA, NA, NA, NA,
        entry$unit, "fail",
        paste(dependencyMessages, collapse = " "), "Postprom report"
      )
    }

    invalidKeys[[keyName]] <- unique(invalid)
  }

  list(findings = findings, invalidKeys = invalidKeys)
}

evaluateValidationCheck <- function(check, values, scenario, registry,
                                    invalidKeys) {
  indicatorId <- check$indicator_id[[1]]
  entry <- registry[[indicatorId]]
  if (is.null(entry)) {
    return(list(newValidationFinding(
      scenario, check$check_id[[1]], indicatorId, indicatorId,
      check$check_type[[1]], "All", check$start_year[[1]],
      check$end_year[[1]], NA, check$reference_value[[1]], NA,
      check$unit[[1]], "skip", "Unknown indicator ID in check table.",
      check$source[[1]]
    )))
  }

  indicatorRows <- values[
    values$scenario == scenario &
      values$indicator_id == indicatorId,
    ,
    drop = FALSE
  ]
  invalid <- invalidKeys[[paste(scenario, indicatorId, sep = "::")]]
  if (!nrow(indicatorRows)) {
    return(list(newValidationFinding(
      scenario, check$check_id[[1]], indicatorId, entry$label,
      check$check_type[[1]], check$region[[1]],
      check$start_year[[1]], check$end_year[[1]], NA,
      check$reference_value[[1]], NA, entry$unit, "skip",
      "Indicator is not available.", check$source[[1]]
    )))
  }
  if (!identical(unique(indicatorRows$unit), entry$unit)) {
    return(list(newValidationFinding(
      scenario, check$check_id[[1]], indicatorId, entry$label,
      check$check_type[[1]], check$region[[1]],
      check$start_year[[1]], check$end_year[[1]], NA,
      check$reference_value[[1]], NA, entry$unit, "skip",
      "Indicator unit is incompatible with this check.", check$source[[1]]
    )))
  }

  checkType <- check$check_type[[1]]
  if (checkType %in% c("range", "finite")) {
    selected <- indicatorRows
    if (!is.na(check$region[[1]]) && check$region[[1]] != "*") {
      selected <- selected[
        selected$region == check$region[[1]], , drop = FALSE
      ]
    }
    if (!nrow(selected)) {
      return(list(newValidationFinding(
        scenario, check$check_id[[1]], indicatorId, entry$label,
        checkType, check$region[[1]], NA, NA, NA, NA, NA,
        entry$unit, "skip", "No matching indicator values.",
        check$source[[1]]
      )))
    }

    output <- vector("list", nrow(selected))
    for (i in seq_len(nrow(selected))) {
      key <- paste(selected$region[i], selected$year[i], sep = "::")
      if ("*" %in% invalid || key %in% invalid) {
        status <- "skip"
        message <- "Source data or denominator is invalid."
      } else if (checkType == "finite") {
        status <- if (is.finite(selected$value[i])) "pass" else "fail"
        message <- if (status == "pass") {
          "Indicator value is finite."
        } else {
          "Indicator value is not finite."
        }
      } else {
        status <- classifyValidationScore(selected$value[i], check)
        message <- validationRangeMessage(status)
      }
      output[[i]] <- newValidationFinding(
        scenario, check$check_id[[1]], indicatorId, entry$label,
        checkType, selected$region[i], selected$year[i],
        selected$year[i], selected$value[i], NA, NA,
        entry$unit, status, message, check$source[[1]]
      )
    }
    return(output)
  }

  region <- check$region[[1]]
  if (is.na(region) || region == "*") {
    return(list(newValidationFinding(
      scenario, check$check_id[[1]], indicatorId, entry$label,
      checkType, "All", check$start_year[[1]], check$end_year[[1]],
      NA, check$reference_value[[1]], NA, entry$unit, "skip",
      "Benchmark checks require one explicit region.", check$source[[1]]
    )))
  }
  regionRows <- indicatorRows[indicatorRows$region == region, , drop = FALSE]
  if (!nrow(regionRows)) {
    return(list(newValidationFinding(
      scenario, check$check_id[[1]], indicatorId, entry$label,
      checkType, region, check$start_year[[1]], check$end_year[[1]],
      NA, check$reference_value[[1]], NA, entry$unit, "skip",
      "Benchmark region is not available.", check$source[[1]]
    )))
  }

  years <- if (checkType == "level") {
    check$end_year[[1]]
  } else {
    c(check$start_year[[1]], check$end_year[[1]])
  }
  selected <- regionRows[regionRows$year %in% years, , drop = FALSE]
  if (!all(years %in% selected$year)) {
    return(list(newValidationFinding(
      scenario, check$check_id[[1]], indicatorId, entry$label,
      checkType, region, check$start_year[[1]], check$end_year[[1]],
      NA, check$reference_value[[1]], NA, check$unit[[1]], "skip",
      "Exact benchmark endpoint year(s) are not available.",
      check$source[[1]]
    )))
  }
  selectedKeys <- paste(selected$region, selected$year, sep = "::")
  if ("*" %in% invalid || any(selectedKeys %in% invalid)) {
    return(list(newValidationFinding(
      scenario, check$check_id[[1]], indicatorId, entry$label,
      checkType, region, check$start_year[[1]], check$end_year[[1]],
      NA, check$reference_value[[1]], NA, check$unit[[1]], "skip",
      "Source data or denominator is invalid.", check$source[[1]]
    )))
  }

  if (checkType == "level") {
    observed <- selected$value[match(check$end_year[[1]], selected$year)]
  } else if (checkType == "cagr") {
    startValue <- selected$value[match(check$start_year[[1]], selected$year)]
    endValue <- selected$value[match(check$end_year[[1]], selected$year)]
    elapsed <- check$end_year[[1]] - check$start_year[[1]]
    if (!is.finite(startValue) || !is.finite(endValue) ||
        startValue <= 0 || endValue <= 0 || elapsed <= 0) {
      return(list(newValidationFinding(
        scenario, check$check_id[[1]], indicatorId, entry$label,
        checkType, region, check$start_year[[1]], check$end_year[[1]],
        NA, check$reference_value[[1]], NA, check$unit[[1]], "skip",
        "CAGR requires positive finite endpoint values and a positive period.",
        check$source[[1]]
      )))
    }
    observed <- (endValue / startValue)^(1 / elapsed) - 1
  } else {
    return(list(newValidationFinding(
      scenario, check$check_id[[1]], indicatorId, entry$label,
      checkType, region, check$start_year[[1]], check$end_year[[1]],
      NA, check$reference_value[[1]], NA, check$unit[[1]], "skip",
      paste0("Unsupported check type '", checkType, "'."),
      check$source[[1]]
    )))
  }

  reference <- check$reference_value[[1]]
  if (!is.finite(reference) || reference == 0) {
    deviation <- NA_real_
    status <- "skip"
    message <- "Relative benchmark reference must be finite and non-zero."
  } else {
    deviation <- (observed - reference) / abs(reference)
    status <- classifyValidationScore(deviation, check)
    message <- paste0(
      "Relative deviation from the published benchmark is ",
      formatC(100 * deviation, digits = 1, format = "f"), "%."
    )
  }

  list(newValidationFinding(
    scenario, check$check_id[[1]], indicatorId, entry$label,
    checkType, region, check$start_year[[1]], check$end_year[[1]],
    observed, reference, deviation, check$unit[[1]], status,
    message, check$source[[1]]
  ))
}

validateCheckTable <- function(checks) {
  if (!is.data.frame(checks)) {
    stop("'checks' must be a data frame.")
  }
  required <- c(
    "check_id", "indicator_id", "check_type", "region", "start_year",
    "end_year", "reference_value", "unit", "fail_min", "pass_min",
    "pass_max", "fail_max", "source", "notes", "enabled"
  )
  missing <- setdiff(required, names(checks))
  if (length(missing)) {
    stop("Check table is missing columns: ", paste(missing, collapse = ", "))
  }
  checks$enabled <- if (is.logical(checks$enabled)) {
    checks$enabled
  } else {
    tolower(as.character(checks$enabled)) %in% c("true", "t", "1", "yes")
  }
  checks
}

classifyValidationScore <- function(score, check) {
  if (!is.finite(score)) return("fail")
  failMin <- check$fail_min[[1]]
  passMin <- check$pass_min[[1]]
  passMax <- check$pass_max[[1]]
  failMax <- check$fail_max[[1]]
  if ((!is.na(failMin) && score < failMin) ||
      (!is.na(failMax) && score > failMax)) {
    return("fail")
  }
  if ((!is.na(passMin) && score < passMin) ||
      (!is.na(passMax) && score > passMax)) {
    return("warn")
  }
  "pass"
}

validationRangeMessage <- function(status) {
  switch(
    status,
    pass = "Indicator is within the configured bounds.",
    warn = "Indicator is outside the preferred bounds.",
    fail = "Indicator is outside the allowed bounds.",
    skip = "Indicator could not be evaluated."
  )
}

newValidationFinding <- function(scenario, checkId, indicatorId, indicator,
                                 checkType, region, startYear, endYear,
                                 observed, reference, deviation, unit, status,
                                 message, source) {
  data.frame(
    scenario = as.character(scenario),
    check_id = as.character(checkId),
    indicator_id = as.character(indicatorId),
    indicator = as.character(indicator),
    check_type = as.character(checkType),
    region = as.character(region),
    start_year = as.integer(startYear),
    end_year = as.integer(endYear),
    observed = as.numeric(observed),
    reference = as.numeric(reference),
    deviation = as.numeric(deviation),
    unit = as.character(unit),
    status = as.character(status),
    message = as.character(message),
    source = as.character(source),
    stringsAsFactors = FALSE
  )
}

emptyValidationFindings <- function() {
  newValidationFinding(
    character(), character(), character(), character(), character(),
    character(), integer(), integer(), numeric(), numeric(), numeric(),
    character(), character(), character(), character()
  )
}

emptyValidationIndicatorValues <- function() {
  data.frame(
    scenario = character(),
    indicator_id = character(),
    indicator = character(),
    region = character(),
    year = integer(),
    value = numeric(),
    unit = character(),
    stringsAsFactors = FALSE
  )
}

bindValidationRows <- function(rows, empty) {
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) return(empty)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

summarizeValidationFindings <- function(findings, scenarios) {
  statuses <- c("pass", "warn", "fail", "skip")
  grid <- expand.grid(
    scenario = scenarios,
    status = statuses,
    stringsAsFactors = FALSE
  )
  if (nrow(findings)) {
    counts <- stats::aggregate(
      list(count = rep.int(1L, nrow(findings))),
      list(scenario = findings$scenario, status = findings$status),
      sum
    )
    grid <- merge(
      grid, counts, by = c("scenario", "status"),
      all.x = TRUE, sort = FALSE
    )
  } else {
    grid$count <- 0L
  }
  grid$count[is.na(grid$count)] <- 0L
  wide <- stats::reshape(
    grid,
    idvar = "scenario",
    timevar = "status",
    direction = "wide"
  )
  names(wide) <- sub("^count\\.", "", names(wide))
  for (status in statuses) {
    if (!status %in% names(wide)) wide[[status]] <- 0L
  }
  wide[, c("scenario", statuses), drop = FALSE]
}
