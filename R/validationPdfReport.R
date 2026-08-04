#' Historical numerical validation checks
#'
#' Loads the existing piamValidation-style configuration used for historical
#' comparisons such as emissions, electricity generation, capacity, and final
#' energy.
#'
#' @return A data frame describing historical numerical checks.
#' @export
defaultValidationChecks <- function() {
  path <- validationConfigPath("validationConfig_OPEN-PROM.csv")
  utils::read.csv(
    path,
    sep = ";",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

#' Country policy validation checks
#'
#' Loads country-level policy targets. The packaged table intentionally starts
#' empty; add reviewed rows before enabling policy checks.
#'
#' @return A data frame describing country policy checks.
#' @export
defaultPolicyValidationChecks <- function() {
  readValidationCsv("policy-validation-checks.csv")
}

#' Long-term current-policy trend checks
#'
#' Loads long-term trend benchmarks. The packaged table intentionally starts
#' empty until the values from the reviewed current-policies document are
#' entered.
#'
#' @return A data frame describing long-term trend checks.
#' @export
defaultLongTermValidationChecks <- function() {
  readValidationCsv("current-policy-trend-checks.csv")
}

#' Validate Postprom results
#'
#' Runs the four maintained check families: historical validation, country
#' policies, reported indicators, and long-term current-policy trends.
#' Indicator formulas are consumed from the report and are never recalculated.
#'
#' @param results A completed MAgPIE report, or a uniquely named list of
#'   completed MAgPIE reports.
#' @param validation_checks Historical model-versus-reference checks.
#' @param policy_checks Individual-country policy checks.
#' @param indicators_checks Checks for reported indicators and their trends.
#' @param long_term_checks Long-term current-policy trend checks.
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
#' indicators and trends, and long-term current-policy trends. Indicator
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
#' @param long_term_checks Long-term current-policy trend configuration.
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
    "metric", "variable", "unit", "region", "period",
    "min_red", "min_yel", "max_yel", "max_red"
  )
  if (!is.data.frame(checks) || !all(required %in% names(checks))) {
    return(validationSectionPlaceholder(
      "historical", "Historical validation configuration is unavailable."
    ))
  }
  active <- checks[
    !is.na(checks$metric) & checks$metric == "relative" &
      !is.na(checks$variable) & nzchar(checks$variable),
    ,
    drop = FALSE
  ]
  if (!nrow(active)) {
    return(validationSectionPlaceholder(
      "historical", "No historical numerical checks are configured."
    ))
  }

  tidy <- tidyValidationReport(report)
  output <- list()
  outputIndex <- 0L
  for (i in seq_len(nrow(active))) {
    check <- active[i, , drop = FALSE]
    variable <- check$variable[[1]]
    referenceVariable <- paste0(variable, "|VAL")
    modelRows <- tidy[tidy$variable == variable, , drop = FALSE]
    referenceRows <- tidy[
      tidy$variable == referenceVariable, , drop = FALSE
    ]
    checkId <- paste0("historical_", make.names(variable))
    source <- historicalCheckSource(check)

    if (!nrow(modelRows) || !nrow(referenceRows)) {
      outputIndex <- outputIndex + 1L
      output[[outputIndex]] <- newValidationSectionFinding(
        "historical", checkId, variable, "All", check$period[[1]],
        NA, NA, NA, check$unit[[1]], "skip",
        paste0(
          "Model variable or historical reference '", referenceVariable,
          "' is not present in the report."
        ),
        source
      )
      next
    }

    regions <- trimws(strsplit(check$region[[1]], ",", fixed = TRUE)[[1]])
    years <- validationPeriodYears(check$period[[1]])
    modelRows <- modelRows[
      modelRows$region %in% regions & modelRows$year %in% years,
      ,
      drop = FALSE
    ]
    referenceRows <- referenceRows[
      referenceRows$region %in% regions & referenceRows$year %in% years,
      ,
      drop = FALSE
    ]
    joined <- merge(
      modelRows, referenceRows,
      by = c("region", "year"), suffixes = c("_model", "_reference")
    )
    if (!nrow(joined)) {
      outputIndex <- outputIndex + 1L
      output[[outputIndex]] <- newValidationSectionFinding(
        "historical", checkId, variable, "All", check$period[[1]],
        NA, NA, NA, check$unit[[1]], "skip",
        "No common configured region-year values are available.", source
      )
      next
    }

    thresholds <- data.frame(
      fail_min = parseValidationPercent(check$min_red[[1]]),
      pass_min = parseValidationPercent(check$min_yel[[1]]),
      pass_max = parseValidationPercent(check$max_yel[[1]]),
      fail_max = parseValidationPercent(check$max_red[[1]])
    )
    for (j in seq_len(nrow(joined))) {
      valid <- is.finite(joined$value_model[j]) &&
        is.finite(joined$value_reference[j]) &&
        joined$value_reference[j] != 0 &&
        identical(joined$unit_model[j], check$unit[[1]]) &&
        identical(joined$unit_reference[j], check$unit[[1]])
      if (valid) {
        deviation <- (joined$value_model[j] - joined$value_reference[j]) /
          abs(joined$value_reference[j])
        status <- classifyValidationScore(deviation, thresholds)
        message <- paste0(
          "Relative deviation is ",
          formatC(100 * deviation, digits = 1, format = "f"), "%."
        )
      } else {
        deviation <- NA_real_
        status <- "skip"
        message <- "Values are non-finite, zero-reference, or use incompatible units."
      }
      outputIndex <- outputIndex + 1L
      output[[outputIndex]] <- newValidationSectionFinding(
        "historical", checkId, variable, joined$region[j],
        as.character(joined$year[j]), joined$value_model[j],
        joined$value_reference[j], deviation, check$unit[[1]], status,
        message, source
      )
    }
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
  output <- vector("list", nrow(active))
  for (i in seq_len(nrow(active))) {
    check <- active[i, , drop = FALSE]
    rows <- tidy[
      tidy$region == check$country[[1]] &
        tidy$variable == check$variable[[1]] &
        tidy$unit == check$unit[[1]],
      ,
      drop = FALSE
    ]
    targetRow <- rows[rows$year == check$target_year[[1]], , drop = FALSE]
    baselineRow <- rows[rows$year == check$baseline_year[[1]], , drop = FALSE]
    targetType <- check$target_type[[1]]
    needsBaseline <- targetType %in% c(
      "reduction_from_baseline", "increase_from_baseline"
    )
    if (nrow(targetRow) != 1L || (needsBaseline && nrow(baselineRow) != 1L)) {
      output[[i]] <- newValidationSectionFinding(
        "policy", check$check_id[[1]], check$variable[[1]],
        check$country[[1]], as.character(check$target_year[[1]]),
        NA, check$target_value[[1]], NA, check$unit[[1]], "skip",
        "The required country, variable, unit, or endpoint year is unavailable.",
        check$source[[1]]
      )
      next
    }
    observed <- policyObservedValue(
      targetType, targetRow$value[[1]],
      if (needsBaseline) baselineRow$value[[1]] else NA_real_
    )
    target <- check$target_value[[1]]
    tolerance <- check$warn_tolerance[[1]]
    status <- classifyPolicyTarget(observed, target, tolerance, targetType)
    outputUnit <- if (needsBaseline) "1" else check$unit[[1]]
    output[[i]] <- newValidationSectionFinding(
      "policy", check$check_id[[1]], check$variable[[1]],
      check$country[[1]], as.character(check$target_year[[1]]),
      observed, target, observed - target, outputUnit, status,
      policyValidationMessage(status, targetType), check$source[[1]]
    )
  }
  bindValidationRows(output, emptyValidationSectionFindings())
}

evaluateLongTermValidation <- function(report, checks) {
  required <- c(
    "check_id", "variable", "unit", "region", "start_year", "end_year",
    "reference_cagr", "fail_min", "pass_min", "pass_max", "fail_max",
    "source", "notes", "enabled"
  )
  if (!is.data.frame(checks) || !all(required %in% names(checks))) {
    return(validationSectionPlaceholder(
      "long_term", "Long-term current-policy configuration is unavailable."
    ))
  }
  active <- checks[validationEnabled(checks$enabled), , drop = FALSE]
  if (!nrow(active)) {
    return(validationSectionPlaceholder(
      "long_term",
      "No reviewed long-term current-policy trends are configured yet. Add rows from Panagiotis's document to current-policy-trend-checks.csv."
    ))
  }

  tidy <- tidyValidationReport(report)
  output <- vector("list", nrow(active))
  for (i in seq_len(nrow(active))) {
    check <- active[i, , drop = FALSE]
    rows <- tidy[
      tidy$region == check$region[[1]] &
        tidy$variable == check$variable[[1]] &
        tidy$unit == check$unit[[1]] &
        tidy$year %in% c(check$start_year[[1]], check$end_year[[1]]),
      ,
      drop = FALSE
    ]
    period <- paste0(check$start_year[[1]], "--", check$end_year[[1]])
    if (nrow(rows) != 2L || any(!is.finite(rows$value)) ||
        any(rows$value <= 0) || check$end_year[[1]] <= check$start_year[[1]]) {
      output[[i]] <- newValidationSectionFinding(
        "long_term", check$check_id[[1]], check$variable[[1]],
        check$region[[1]], period, NA, check$reference_cagr[[1]], NA,
        "1/yr", "skip",
        "CAGR requires both exact years and positive finite endpoint values.",
        check$source[[1]]
      )
      next
    }
    startValue <- rows$value[match(check$start_year[[1]], rows$year)]
    endValue <- rows$value[match(check$end_year[[1]], rows$year)]
    observed <- (endValue / startValue)^(
      1 / (check$end_year[[1]] - check$start_year[[1]])
    ) - 1
    reference <- check$reference_cagr[[1]]
    if (!is.finite(reference) || reference == 0) {
      deviation <- NA_real_
      status <- "skip"
    } else {
      deviation <- (observed - reference) / abs(reference)
      status <- classifyValidationScore(deviation, check)
    }
    output[[i]] <- newValidationSectionFinding(
      "long_term", check$check_id[[1]], check$variable[[1]],
      check$region[[1]], period, observed, reference, deviation,
      "1/yr", status,
      paste0(
        "Annualized trend deviation is ",
        ifelse(is.na(deviation), "unavailable", paste0(
          formatC(100 * deviation, digits = 1, format = "f"), "%"
        )), "."
      ),
      check$source[[1]]
    )
  }
  bindValidationRows(output, emptyValidationSectionFindings())
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
  evaluated <- findings[findings$status != "skip", , drop = FALSE]
  if (!nrow(evaluated)) {
    return(data.frame(
      Scenario = character(), Variable = character(), Region = character(),
      Evaluated = integer(), Pass = integer(), Warn = integer(), Fail = integer(),
      Worst.deviation = character(), stringsAsFactors = FALSE
    ))
  }
  groups <- split(
    evaluated,
    paste(evaluated$scenario, evaluated$variable, evaluated$region)
  )
  rows <- lapply(groups, function(group) {
    data.frame(
      Scenario = group$scenario[1],
      Variable = group$variable[1],
      Region = group$region[1],
      Evaluated = nrow(group),
      Pass = sum(group$status == "pass"),
      Warn = sum(group$status == "warn"),
      Fail = sum(group$status == "fail"),
      Worst.deviation = paste0(
        formatC(100 * max(abs(group$deviation), na.rm = TRUE),
                digits = 1, format = "f"), "%"
      ),
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

parseValidationPercent <- function(x) {
  if (is.na(x)) return(NA_real_)
  as.numeric(sub("%$", "", trimws(as.character(x)))) / 100
}

validationPeriodYears <- function(period) {
  values <- as.integer(strsplit(as.character(period), "-", fixed = TRUE)[[1]])
  if (length(values) == 1L) values else seq(values[1], values[2])
}

historicalCheckSource <- function(check) {
  discussion <- if ("source/link to discussion" %in% names(check)) {
    check[["source/link to discussion"]][[1]]
  } else {
    NA_character_
  }
  if (!is.na(discussion) && nzchar(discussion)) return(discussion)
  referenceModel <- if ("ref_model" %in% names(check)) {
    check$ref_model[[1]]
  } else {
    NA_character_
  }
  referenceScenario <- if ("ref_scenario" %in% names(check)) {
    check$ref_scenario[[1]]
  } else {
    NA_character_
  }
  paste(
    stats::na.omit(c(referenceModel, referenceScenario)), collapse = " / "
  )
}

policyObservedValue <- function(type, targetValue, baselineValue) {
  switch(
    type,
    maximum = targetValue,
    minimum = targetValue,
    reduction_from_baseline = (baselineValue - targetValue) / abs(baselineValue),
    increase_from_baseline = (targetValue - baselineValue) / abs(baselineValue),
    NA_real_
  )
}

classifyPolicyTarget <- function(observed, target, tolerance, type) {
  if (!is.finite(observed) || !is.finite(target)) return("skip")
  if (!is.finite(tolerance)) tolerance <- 0
  if (type == "maximum") {
    if (observed <= target) "pass" else if (observed <= target + tolerance) "warn" else "fail"
  } else if (type %in% c("minimum", "reduction_from_baseline", "increase_from_baseline")) {
    if (observed >= target) "pass" else if (observed >= target - tolerance) "warn" else "fail"
  } else {
    "skip"
  }
}

policyValidationMessage <- function(status, type) {
  if (status == "skip") return(paste0("Unsupported or unevaluable target type '", type, "'."))
  switch(
    status,
    pass = "The country reaches the configured policy target.",
    warn = "The country is close to, but does not reach, the configured policy target.",
    fail = "The country does not reach the configured policy target."
  )
}
