#' Historical numerical validation checks
#'
#' Loads 2024 historical benchmark levels derived from
#' \code{validation-research-report.md}. Reference values are stored in native
#' Postprom units and can be inspected or extended before validation.
#'
#' @return A data frame describing historical numerical checks.
#' @export
defaultValidationChecks <- function() {
  readValidationCsv("validation-checks.csv")
}

#' Country policy validation checks
#'
#' Loads country-level targets converted from the Climate Policy Modelling
#' Protocol. Only directly reportable OPEN-PROM regions are retained. The
#' protocol's CHN identifier is mapped explicitly to OPEN-PROM's CHA region;
#' other unavailable jurisdictions are not assigned to model aggregates.
#'
#' @return A data frame describing country policy checks.
#' @export
defaultPolicyValidationChecks <- function() {
  readValidationCsv("policy-validation-checks.csv")
}

#' Long-term trend checks
#'
#' Loads long-term targets derived from
#' \code{quantified_longterm_trends.md}. Checks cover annualized trends,
#' cumulative changes, and endpoint levels using exact configured years.
#' EU-specific checks use the aggregate OPEN-PROM \code{EU} region.
#'
#' @return A data frame describing long-term trend checks.
#' @export
defaultLongTermValidationChecks <- function() {
  readValidationCsv("long-term-targets.csv")
}

#' Validate Postprom results
#'
#' Runs the four maintained check families: historical validation, country
#' policies, reported indicators, and long-term targets.
#' Indicator formulas are consumed from the report and are never recalculated.
#'
#' @param results A completed MAgPIE report, or a uniquely named list of
#'   completed MAgPIE reports.
#' @param validation_checks Historical model-versus-reference checks.
#' @param policy_checks Individual-country policy checks.
#' @param indicators_checks Checks for reported indicators and their trends.
#' @param long_term_checks Long-term target checks.
#'
#' @return A \code{postprom_validation} object containing unified findings,
#'   summaries, indicator values, and the four family-specific results.
#' @export
validateResults <- function(
    results,
    validation_checks = defaultValidationChecks(),
    policy_checks = defaultPolicyValidationChecks(),
    indicators_checks = defaultIndicatorsChecks(),
    long_term_checks = defaultLongTermValidationChecks()) {
  reports <- normalizeValidationReports(results)
  validation <- evaluateChecksByScenario(
    reports, evaluateHistoricalValidation, validation_checks
  )
  policies <- evaluateChecksByScenario(
    reports, evaluatePolicyValidation, policy_checks
  )
  indicators <- validateIndicatorResults(reports, indicators_checks)
  longTerm <- evaluateChecksByScenario(
    reports, evaluateLongTermValidation, long_term_checks
  )

  familyTables <- list(
    "Validation checks" = validation,
    "Policy checks" = policies,
    "Indicators check" = indicatorFindingsForPdf(indicators),
    "Long term checks" = longTerm
  )
  overview <- do.call(rbind, lapply(names(familyTables), function(family) {
    summarizeValidationFamily(familyTables[[family]], family)
  }))
  rownames(overview) <- NULL
  findings <- do.call(rbind, unname(familyTables))
  rownames(findings) <- NULL

  structure(
    list(
      findings = findings,
      summary = overview,
      indicator_values = indicators$indicator_values,
      overview = overview,
      validation = validation,
      validation_summary = summarizeHistoricalValidation(validation),
      policies = policies,
      indicators = indicators,
      long_term = longTerm
    ),
    class = "postprom_validation"
  )
}

#' @export
print.postprom_validation <- function(x, ...) {
  cat("Postprom validation\n")
  print(x$summary, row.names = FALSE)
  exceptions <- x$findings[x$findings$status != "pass", , drop = FALSE]
  if (!nrow(exceptions)) {
    cat("No validation exceptions found.\n")
  } else {
    cat(nrow(exceptions), "validation exception(s). Showing up to 10:\n")
    columns <- c(
      "scenario", "check_id", "variable", "region", "period", "status",
      "message"
    )
    print(utils::head(exceptions[, columns, drop = FALSE], 10),
          row.names = FALSE)
  }
  invisible(x)
}

#' Create a standalone OPEN-PROM validation PDF
#'
#' Builds four check sections: historical validation, country policy targets,
#' indicators and trends, and long-term targets. Indicator
#' formulas are read from the completed report
#' and are never recalculated here.
#'
#' @param report A completed Postprom MAgPIE report.
#' @param metadata Optional scenario metadata displayed in the PDF.
#' @param output_file Target PDF path. Defaults to \code{Validation.pdf}.
#' @param scenario Scenario label used in validation tables.
#' @param validation_checks Historical-number validation configuration.
#' @param policy_checks Country policy target configuration.
#' @param indicators_checks Checks passed to \code{\link{validateResults}}.
#' @param long_term_checks Long-term target configuration.
#'
#' @return Invisibly returns output paths and all prepared validation sections.
#' @export
validationPdfReport <- function(
    report,
    metadata = NULL,
    output_file = "Validation.pdf",
    scenario = "scenario",
    validation_checks = defaultValidationChecks(),
    policy_checks = defaultPolicyValidationChecks(),
    indicators_checks = defaultIndicatorsChecks(),
    long_term_checks = defaultLongTermValidationChecks()) {
  if (!magclass::is.magpie(report)) {
    stop("'report' must be a completed MAgPIE report.")
  }
  outputFile <- normalizePath(output_file, mustWork = FALSE)
  if (tolower(tools::file_ext(outputFile)) != "pdf") {
    stop("'output_file' must end in '.pdf'.")
  }
  outputDirectory <- dirname(outputFile)
  if (!dir.exists(outputDirectory)) {
    stop("Output directory does not exist: ", outputDirectory)
  }

  sections <- validateResults(
    results = stats::setNames(list(report), scenario),
    validation_checks = validation_checks,
    policy_checks = policy_checks,
    indicators_checks = indicators_checks,
    long_term_checks = long_term_checks
  )

  if (!tinytex::is_tinytex()) {
    message("TinyTeX is not installed. Validation checks were evaluated, but the PDF was not created.")
    return(invisible(list(pdf = NULL, tex = NULL, sections = sections)))
  }

  texFile <- paste0(tools::file_path_sans_ext(outputFile), ".tex")
  renderEnvironment <- new.env(parent = environment(validationPdfReport))
  renderEnvironment$validation_sections <- sections
  renderEnvironment$validation_metadata <- data.frame(
    Scenario = as.character(scenario),
    fScenario = if (is.null(metadata)) "" else paste(metadata, collapse = ", "),
    stringsAsFactors = FALSE
  )

  template <- system.file(
    "templates", "validation.Rnw", package = "postprom"
  )
  if (!nzchar(template)) {
    template <- file.path("inst", "templates", "validation.Rnw")
  }
  knitr::knit2pdf(
    input = template,
    output = texFile,
    envir = renderEnvironment,
    quiet = TRUE
  )
  message("Saving validation PDF in ", outputFile)
  invisible(list(pdf = outputFile, tex = texFile, sections = sections))
}

evaluateChecksByScenario <- function(reports, evaluator, checks) {
  output <- lapply(names(reports), function(scenario) {
    findings <- evaluator(reports[[scenario]], checks)
    findings$scenario <- scenario
    findings[, c("scenario", setdiff(names(findings), "scenario")), drop = FALSE]
  })
  result <- do.call(rbind, output)
  rownames(result) <- NULL
  result
}

evaluateHistoricalValidation <- function(report, checks) {
  required <- c(
    "check_id", "variable", "unit", "region", "year", "central",
    "lower", "upper", "source", "notes", "enabled"
  )
  if (!is.data.frame(checks) || !all(required %in% names(checks))) {
    return(validationSectionPlaceholder(
      "historical", "Historical validation configuration is unavailable."
    ))
  }
  active <- checks[validationEnabled(checks$enabled), , drop = FALSE]
  if (!nrow(active)) {
    return(validationSectionPlaceholder(
      "historical", "No historical numerical checks are configured."
    ))
  }

  tidy <- tidyValidationReport(report)
  output <- vector("list", nrow(active))
  for (i in seq_len(nrow(active))) {
    check <- active[i, , drop = FALSE]
    rows <- tidy[
      tidy$variable == check$variable[[1]] &
        tidy$region == check$region[[1]] &
        tidy$year == check$year[[1]],
      ,
      drop = FALSE
    ]
    exactRows <- rows[rows$unit == check$unit[[1]], , drop = FALSE]
    validReference <- all(is.finite(c(
      check$central[[1]], check$lower[[1]], check$upper[[1]]
    ))) && check$lower[[1]] <= check$upper[[1]]

    if (nrow(exactRows) != 1L || !validReference ||
        !is.finite(exactRows$value[[1]])) {
      foundUnits <- unique(rows$unit)
      message <- if (nrow(rows) && !nrow(exactRows)) {
        paste0(
          "Expected unit '", check$unit[[1]], "'; found '",
          paste(foundUnits, collapse = ", "), "'."
        )
      } else {
        "The exact variable, region, year, or finite benchmark is unavailable."
      }
      output[[i]] <- newValidationSectionFinding(
        "historical", check$check_id[[1]], check$variable[[1]],
        check$region[[1]], as.character(check$year[[1]]), NA,
        check$central[[1]], NA, check$unit[[1]], "skip", message,
        check$source[[1]]
      )
      next
    }

    observed <- exactRows$value[[1]]
    central <- check$central[[1]]
    deviation <- if (central == 0) NA_real_ else
      (observed - central) / abs(central)
    withinRange <- observed >= check$lower[[1]] &&
      observed <= check$upper[[1]]
    status <- if (withinRange) {
      "pass"
    } else if (is.finite(deviation) && abs(deviation) <= 0.5) {
      "warn"
    } else {
      "fail"
    }
    message <- paste0(
      "Observed value is ", formatC(observed, digits = 6, format = "g"),
      "; benchmark range is [",
      formatC(check$lower[[1]], digits = 6, format = "g"), ", ",
      formatC(check$upper[[1]], digits = 6, format = "g"), "]."
    )
    output[[i]] <- newValidationSectionFinding(
      "historical", check$check_id[[1]], check$variable[[1]],
      check$region[[1]], as.character(check$year[[1]]), observed,
      central, deviation, check$unit[[1]], status, message,
      check$source[[1]]
    )
  }
  bindValidationRows(output, emptyValidationSectionFindings())
}

evaluatePolicyValidation <- function(report, checks) {
  required <- c(
    "check_id", "country", "variable", "unit", "baseline_year",
    "target_year", "target_type", "target_value", "warn_tolerance",
    "source", "notes", "enabled"
  )
  if (!is.data.frame(checks) || !all(required %in% names(checks))) {
    return(validationSectionPlaceholder(
      "policy", "Country policy configuration is unavailable."
    ))
  }
  active <- checks[validationEnabled(checks$enabled), , drop = FALSE]
  if (!nrow(active)) {
    return(validationSectionPlaceholder(
      "policy",
      "No reviewed country policy targets are configured yet. Add rows to policy-validation-checks.csv."
    ))
  }

  tidy <- tidyValidationReport(report)
  tidy <- addPolicyElectricitySourceShares(tidy, active)
  output <- vector("list", nrow(active))
  for (i in seq_len(nrow(active))) {
    check <- active[i, , drop = FALSE]
    variableRows <- tidy[
      tidy$region == check$country[[1]] &
        tidy$variable == check$variable[[1]],
      ,
      drop = FALSE
    ]
    targetType <- check$target_type[[1]]
    relativeTypes <- c(
      "reduction_from_baseline", "maximum_reduction_from_baseline",
      "increase_from_baseline", "maximum_increase_from_baseline"
    )
    cumulativeTypes <- c("cumulative_minimum", "cumulative_maximum")
    needsBaseline <- targetType %in% relativeTypes
    needsCumulative <- targetType %in% cumulativeTypes
    needsStart <- needsBaseline || needsCumulative
    if (needsBaseline) {
      reportUnits <- unique(variableRows$unit)
      rows <- if (length(reportUnits) == 1L) variableRows else
        variableRows[FALSE, , drop = FALSE]
    } else {
      rows <- variableRows[
        variableRows$unit == check$unit[[1]], , drop = FALSE
      ]
    }
    targetEndpoint <- nearestPolicyEndpoint(
      rows, check$target_year[[1]]
    )
    baselineEndpoint <- if (needsStart) {
      nearestPolicyEndpoint(rows, check$baseline_year[[1]])
    } else {
      list(row = rows[FALSE, , drop = FALSE], year = NA_integer_)
    }
    targetRow <- targetEndpoint$row
    baselineRow <- baselineEndpoint$row
    cumulativeRows <- if (needsCumulative &&
                          is.finite(baselineEndpoint$year) &&
                          is.finite(targetEndpoint$year)) {
      rows[
        rows$year >= baselineEndpoint$year &
          rows$year <= targetEndpoint$year,
        ,
        drop = FALSE
      ]
    } else {
      rows[FALSE, , drop = FALSE]
    }
    invalidCumulative <- needsCumulative && (
      nrow(cumulativeRows) < 2L ||
        any(!is.finite(cumulativeRows$value)) ||
        anyDuplicated(cumulativeRows$year) ||
        baselineEndpoint$year >= targetEndpoint$year
    )
    if (nrow(targetRow) != 1L || !is.finite(targetRow$value[[1]]) ||
        (needsStart && (nrow(baselineRow) != 1L ||
         !is.finite(baselineRow$value[[1]]) ||
         (needsBaseline && baselineRow$value[[1]] == 0))) ||
        invalidCumulative) {
      output[[i]] <- newValidationSectionFinding(
        "policy", check$check_id[[1]], check$variable[[1]],
        check$country[[1]], as.character(check$target_year[[1]]),
        NA, check$target_value[[1]], NA, check$unit[[1]], "skip",
        paste0(
          "The required country, variable, unit, or a model year within ",
          "two years of the policy endpoint is unavailable."
        ),
        check$source[[1]]
      )
      next
    }
    observed <- if (needsCumulative) {
      cumulativePolicyValue(cumulativeRows, baselineRow$value[[1]])
    } else {
      policyObservedValue(
        targetType, targetRow$value[[1]],
        if (needsBaseline) baselineRow$value[[1]] else NA_real_
      )
    }
    target <- check$target_value[[1]]
    tolerance <- check$warn_tolerance[[1]]
    status <- classifyPolicyTarget(observed, target, tolerance, targetType)
    outputUnit <- if (needsBaseline) {
      "1"
    } else if (needsCumulative) {
      sub("/yr$", "", check$unit[[1]])
    } else {
      check$unit[[1]]
    }
    period <- if (needsStart) {
      paste0(baselineEndpoint$year, "--", targetEndpoint$year)
    } else {
      as.character(targetEndpoint$year)
    }
    yearMessage <- policyEndpointMessage(
      targetEndpoint$year, check$target_year[[1]],
      baselineEndpoint$year, check$baseline_year[[1]], needsStart
    )
    output[[i]] <- newValidationSectionFinding(
      "policy", check$check_id[[1]], check$variable[[1]],
      check$country[[1]], period,
      observed, target, observed - target, outputUnit, status,
      paste0(policyValidationMessage(status, targetType), yearMessage),
      check$source[[1]]
    )
  }
  bindValidationRows(output, emptyValidationSectionFindings())
}

# Derive technology-specific electricity shares only for policy checks that
# request them. These values are private to validation and are not appended to
# the report produced by reportIndicators().
addPolicyElectricitySourceShares <- function(tidy, checks) {
  shareToSource <- c(
    "Secondary Energy|Electricity|Solar Share" =
      "Secondary Energy|Electricity|Solar",
    "Secondary Energy|Electricity|Wind Share" =
      "Secondary Energy|Electricity|Wind",
    "Secondary Energy|Electricity|Hydro Share" =
      "Secondary Energy|Electricity|Hydro",
    "Secondary Energy|Electricity|Geothermal and other renewable sources Share" =
      "Secondary Energy|Electricity|Geothermal and other renewable sources",
    "Secondary Energy|Electricity|Biofuels Share" =
      "Secondary Energy|Electricity|Biofuels",
    "Secondary Energy|Electricity|Nuclear Share" =
      "Secondary Energy|Electricity|Nuclear"
  )
  japanChecks <- checks[checks$country == "JPN", , drop = FALSE]
  requested <- intersect(unique(japanChecks$variable), names(shareToSource))
  if (!length(requested)) return(tidy)

  regions <- "JPN"
  total <- tidy[
    tidy$region %in% regions &
      tidy$variable == "Secondary Energy|Electricity" &
      tidy$unit == "TWh",
    c("region", "year", "value"), drop = FALSE
  ]
  names(total)[3] <- "total"
  if (!nrow(total)) return(tidy)

  derived <- lapply(requested, function(shareVariable) {
    source <- tidy[
      tidy$region %in% regions &
        tidy$variable == unname(shareToSource[[shareVariable]]) &
        tidy$unit == "TWh",
      c("region", "year", "value"), drop = FALSE
    ]
    if (!nrow(source)) return(NULL)
    names(source)[3] <- "source"
    values <- merge(source, total, by = c("region", "year"), all = FALSE)
    if (!nrow(values)) return(NULL)
    data.frame(
      region = values$region,
      year = values$year,
      variable = shareVariable,
      unit = "1",
      value = ifelse(
        is.finite(values$source) & is.finite(values$total) & values$total != 0,
        values$source / values$total,
        NA_real_
      ),
      stringsAsFactors = FALSE
    )
  })
  bindValidationRows(c(list(tidy), derived), tidy[FALSE, , drop = FALSE])
}

evaluateLongTermValidation <- function(report, checks) {
  checks <- normalizeLongTermChecks(checks)
  if (is.null(checks)) {
    return(validationSectionPlaceholder(
      "long_term", "Long-term current-policy configuration is unavailable."
    ))
  }
  active <- checks[validationEnabled(checks$enabled), , drop = FALSE]
  if (!nrow(active)) {
    return(validationSectionPlaceholder(
      "long_term",
      "No reviewed long-term targets are configured in long-term-targets.csv."
    ))
  }

  tidy <- tidyValidationReport(report)
  output <- list()
  outputIndex <- 0L
  for (i in seq_len(nrow(active))) {
    check <- active[i, , drop = FALSE]
    availableRegions <- unique(tidy$region[
      tidy$variable == check$variable[[1]] &
        tidy$unit == check$unit[[1]]
    ])
    selector <- check$region[[1]]
    if (identical(selector, "*")) {
      regions <- sort(availableRegions)
    } else if (identical(selector, "countries")) {
      regions <- sort(availableRegions[grepl("^[A-Z]{3}$", availableRegions)])
      regions <- setdiff(regions, eu27Iso3())
    } else {
      regions <- selector
    }
    if (!length(regions) && !selector %in% c("*", "countries")) {
      regions <- selector
    }

    for (region in regions) {
      outputIndex <- outputIndex + 1L
      checkType <- check$check_type[[1]]
      requiredYears <- if (checkType == "level") {
        check$end_year[[1]]
      } else {
        c(check$start_year[[1]], check$end_year[[1]])
      }
      rows <- tidy[
        tidy$region == region &
          tidy$variable == check$variable[[1]] &
          tidy$unit == check$unit[[1]] &
          tidy$year %in% requiredYears,
        ,
        drop = FALSE
      ]
      period <- if (checkType == "level") {
        as.character(check$end_year[[1]])
      } else {
        paste0(check$start_year[[1]], "--", check$end_year[[1]])
      }
      valid <- nrow(rows) == length(requiredYears) &&
        !anyDuplicated(rows$year) && all(is.finite(rows$value))
      if (checkType == "cagr") {
        valid <- valid && all(rows$value > 0) &&
          check$end_year[[1]] > check$start_year[[1]]
      } else if (checkType == "change") {
        startRows <- rows[rows$year == check$start_year[[1]], , drop = FALSE]
        valid <- valid && nrow(startRows) == 1L && startRows$value[[1]] != 0 &&
          check$end_year[[1]] > check$start_year[[1]]
      } else if (checkType != "level") {
        valid <- FALSE
      }
      if (!valid) {
        output[[outputIndex]] <- newValidationSectionFinding(
          "long_term", check$check_id[[1]], check$variable[[1]],
          region, period, NA, check$reference_value[[1]], NA,
          longTermOutputUnit(checkType, check$unit[[1]]), "skip",
          paste0(
            "The ", checkType, " check requires its exact endpoint year(s) ",
            "and finite compatible values."
          ),
          check$source[[1]]
        )
        next
      }
      endValue <- rows$value[match(check$end_year[[1]], rows$year)]
      observed <- if (checkType == "level") {
        endValue
      } else {
        startValue <- rows$value[match(check$start_year[[1]], rows$year)]
        if (checkType == "cagr") {
          (endValue / startValue)^(
            1 / (check$end_year[[1]] - check$start_year[[1]])
          ) - 1
        } else {
          (endValue - startValue) / abs(startValue)
        }
      }
      reference <- check$reference_value[[1]]
      deviation <- if (is.finite(reference) && reference != 0) {
        (observed - reference) / abs(reference)
      } else {
        NA_real_
      }
      status <- classifyLongTermTarget(observed, check)
      output[[outputIndex]] <- newValidationSectionFinding(
        "long_term", check$check_id[[1]], check$variable[[1]],
        region, period, observed, reference, deviation,
        longTermOutputUnit(checkType, check$unit[[1]]), status,
        paste0(
          "Observed ", checkType, " value is ",
          formatC(observed, digits = 4, format = "g"),
          "; preferred range is [",
          formatC(check$pass_min[[1]], digits = 4, format = "g"), ", ",
          formatC(check$pass_max[[1]], digits = 4, format = "g"), "]."
        ),
        check$source[[1]]
      )
    }
  }
  bindValidationRows(output, emptyValidationSectionFindings())
}

normalizeLongTermChecks <- function(checks) {
  if (!is.data.frame(checks)) return(NULL)
  common <- c(
    "check_id", "variable", "unit", "region", "start_year", "end_year",
    "fail_min", "pass_min", "pass_max", "fail_max", "source", "notes",
    "enabled"
  )
  if (!all(common %in% names(checks))) return(NULL)
  if (all(c("check_type", "reference_value") %in% names(checks))) {
    return(checks)
  }
  if (!"reference_cagr" %in% names(checks)) return(NULL)

  reference <- checks$reference_cagr
  scale <- abs(reference)
  checks$check_type <- "cagr"
  checks$reference_value <- reference
  checks$fail_min <- reference + scale * checks$fail_min
  checks$pass_min <- reference + scale * checks$pass_min
  checks$pass_max <- reference + scale * checks$pass_max
  checks$fail_max <- reference + scale * checks$fail_max
  checks
}

classifyLongTermTarget <- function(observed, check) {
  if (!is.finite(observed)) return("skip")
  if ((!is.na(check$fail_min[[1]]) && observed < check$fail_min[[1]]) ||
      (!is.na(check$fail_max[[1]]) && observed > check$fail_max[[1]])) {
    return("fail")
  }
  if ((!is.na(check$pass_min[[1]]) && observed < check$pass_min[[1]]) ||
      (!is.na(check$pass_max[[1]]) && observed > check$pass_max[[1]])) {
    return("warn")
  }
  "pass"
}

longTermOutputUnit <- function(checkType, sourceUnit) {
  switch(checkType, cagr = "1/yr", change = "1", level = sourceUnit, sourceUnit)
}

eu27Iso3 <- function() {
  c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
    "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX",
    "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
  )
}

indicatorFindingsForPdf <- function(validation) {
  findings <- validation$findings
  period <- ifelse(
    is.na(findings$start_year), "",
    ifelse(
      findings$start_year == findings$end_year,
      findings$end_year,
      paste0(findings$start_year, "--", findings$end_year)
    )
  )
  data.frame(
    scenario = findings$scenario,
    family = "indicator",
    check_id = findings$check_id,
    variable = findings$indicator,
    region = findings$region,
    period = period,
    observed = findings$observed,
    reference = findings$reference,
    deviation = findings$deviation,
    unit = findings$unit,
    status = findings$status,
    message = findings$message,
    source = findings$source,
    stringsAsFactors = FALSE
  )
}

summarizeValidationFamily <- function(findings, family) {
  statuses <- c("pass", "warn", "fail", "skip")
  groups <- split(findings, findings$scenario)
  rows <- lapply(names(groups), function(scenario) {
    counts <- table(factor(groups[[scenario]]$status, levels = statuses))
    data.frame(
      Scenario = scenario,
      Check.family = family,
      Pass = unname(counts["pass"]),
      Warn = unname(counts["warn"]),
      Fail = unname(counts["fail"]),
      Skip = unname(counts["skip"]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

summarizeHistoricalValidation <- function(findings) {
  if (!nrow(findings)) {
    return(data.frame(
      Scenario = character(), Variable = character(), Region = character(),
      Status = character(),
      Worst.deviation = character(), stringsAsFactors = FALSE
    ))
  }
  groups <- split(
    findings,
    paste(findings$scenario, findings$variable, findings$region)
  )
  rows <- lapply(groups, function(group) {
    status <- if (any(group$status == "fail")) {
      "Fail"
    } else if (any(group$status == "warn")) {
      "Warn"
    } else if (any(group$status == "pass")) {
      "Pass"
    } else {
      "N/A"
    }
    evaluatedDeviation <- abs(group$deviation[
      group$status != "skip" & is.finite(group$deviation)
    ])
    data.frame(
      Scenario = group$scenario[1],
      Variable = group$variable[1],
      Region = group$region[1],
      Status = status,
      Worst.deviation = if (length(evaluatedDeviation)) {
        paste0(
          formatC(
            100 * max(evaluatedDeviation), digits = 1, format = "f"
          ),
          "%"
        )
      } else {
        ""
      },
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

newValidationSectionFinding <- function(family, checkId, variable, region,
                                        period, observed, reference, deviation,
                                        unit, status, message, source) {
  data.frame(
    family = as.character(family),
    check_id = as.character(checkId),
    variable = as.character(variable),
    region = as.character(region),
    period = as.character(period),
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

emptyValidationSectionFindings <- function() {
  newValidationSectionFinding(
    character(), character(), character(), character(), character(),
    numeric(), numeric(), numeric(), character(), character(), character(),
    character()
  )
}

validationSectionPlaceholder <- function(family, message) {
  newValidationSectionFinding(
    family, paste0(family, "_not_configured"), "", "All", "",
    NA, NA, NA, "", "skip", message, ""
  )
}

validationConfigPath <- function(file) {
  path <- system.file("extdata", file, package = "postprom")
  if (!nzchar(path)) path <- file.path("inst", "extdata", file)
  if (!file.exists(path)) stop("Could not find validation config: ", file)
  path
}

readValidationCsv <- function(file) {
  utils::read.csv(
    validationConfigPath(file), stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
}

validationEnabled <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(as.character(x)) %in% c("true", "t", "1", "yes")
}

nearestPolicyEndpoint <- function(rows, requestedYear, maxGap = 2L) {
  empty <- list(row = rows[FALSE, , drop = FALSE], year = NA_integer_)
  if (!nrow(rows) || !is.finite(requestedYear)) return(empty)
  available <- sort(unique(rows$year[is.finite(rows$year)]))
  if (!length(available)) return(empty)
  gaps <- abs(available - requestedYear)
  if (min(gaps) > maxGap) return(empty)
  selectedYear <- available[which.min(gaps)]
  list(
    row = rows[rows$year == selectedYear, , drop = FALSE],
    year = as.integer(selectedYear)
  )
}

policyEndpointMessage <- function(targetYear, requestedTargetYear,
                                  baselineYear, requestedBaselineYear,
                                  needsBaseline) {
  actual <- if (needsBaseline) c(baselineYear, targetYear) else targetYear
  requested <- if (needsBaseline) {
    c(requestedBaselineYear, requestedTargetYear)
  } else {
    requestedTargetYear
  }
  if (all(actual == requested)) return("")
  paste0(
    " Model year(s) ", paste(actual, collapse = " and "),
    " were used for configured policy year(s) ",
    paste(requested, collapse = " and "), "; no interpolation was applied."
  )
}

policyObservedValue <- function(type, targetValue, baselineValue) {
  switch(
    type,
    maximum = targetValue,
    minimum = targetValue,
    reduction_from_baseline = (baselineValue - targetValue) / abs(baselineValue),
    maximum_reduction_from_baseline =
      (baselineValue - targetValue) / abs(baselineValue),
    increase_from_baseline = (targetValue - baselineValue) / abs(baselineValue),
    maximum_increase_from_baseline =
      (targetValue - baselineValue) / abs(baselineValue),
    NA_real_
  )
}

cumulativePolicyValue <- function(rows, baselineValue) {
  rows <- rows[order(rows$year), , drop = FALSE]
  widths <- diff(rows$year)
  avoided <- baselineValue - rows$value
  sum(widths * (utils::head(avoided, -1L) +
    utils::tail(avoided, -1L)) / 2)
}

classifyPolicyTarget <- function(observed, target, tolerance, type) {
  if (!is.finite(observed) || !is.finite(target)) return("skip")
  if (!is.finite(tolerance)) tolerance <- 0
  tolerance <- abs(tolerance)
  # Generated policy checks use a 10% warning tolerance. Reserve one tenth of
  # it (1% of the target) as a numerical/model-year pass margin so negligible
  # misses caused by reporting precision or nearest-year selection do not warn.
  passTolerance <- 0.1 * tolerance
  if (type %in% c(
    "maximum", "maximum_reduction_from_baseline",
    "maximum_increase_from_baseline", "cumulative_maximum"
  )) {
    if (observed <= target + passTolerance) {
      "pass"
    } else if (observed <= target + tolerance) {
      "warn"
    } else {
      "fail"
    }
  } else if (type %in% c(
    "minimum", "reduction_from_baseline", "increase_from_baseline",
    "cumulative_minimum"
  )) {
    if (observed >= target - passTolerance) {
      "pass"
    } else if (observed >= target - tolerance) {
      "warn"
    } else {
      "fail"
    }
  } else {
    "skip"
  }
}

policyValidationMessage <- function(status, type) {
  if (status == "skip") return(paste0("Unsupported or unevaluable target type '", type, "'."))
  switch(
    status,
    pass = "The country meets the configured policy target within the pass tolerance.",
    warn = "The country is close to, but does not reach, the configured policy target.",
    fail = "The country does not reach the configured policy target."
  )
}
