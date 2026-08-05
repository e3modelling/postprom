test_that("validation checks compare report values with packaged benchmarks", {
  report <- makeCompletedValidationReport()
  checks <- data.frame(
    check_id = "world_co2", variable = "Emissions|CO2",
    unit = "Mt CO2/yr", region = "World", year = 2024,
    central = 25, lower = 24, upper = 26,
    source = "validation-research-report.md", notes = "", enabled = TRUE
  )

  result <- validateResults(report, validation_checks = checks)
  expect_true(nrow(result$validation) > 0)
  expect_true(all(result$validation$status == "warn"))
  expect_equal(unique(result$validation$deviation), 0.2)
  expect_named(
    result$validation_summary,
    c("Scenario", "Variable", "Region", "Status", "Worst.deviation")
  )
  expect_equal(result$validation_summary$Status, "Warn")
  expect_setequal(
    unique(result$findings$family),
    c("historical", "policy", "indicator", "long_term")
  )

  report["World", 2024, "Emissions|CO2"] <- 25
  passing <- validateResults(report, validation_checks = checks)
  expect_equal(passing$validation$status, "pass")
  expect_equal(passing$validation_summary$Status, "Pass")
  report["World", 2024, "Emissions|CO2"] <- 50
  failing <- validateResults(report, validation_checks = checks)
  expect_equal(failing$validation$status, "fail")
  expect_equal(failing$validation_summary$Status, "Fail")

  checks$region <- "USA"
  unavailable <- validateResults(report, validation_checks = checks)
  expect_equal(unavailable$validation_summary$Status, "N/A")
  expect_equal(unavailable$validation_summary$Worst.deviation, "")
})

test_that("packaged historical checks use native units and compatible scopes", {
  checks <- defaultValidationChecks()

  expect_equal(nrow(checks), 116)
  expect_equal(anyDuplicated(checks$check_id), 0L)
  expect_true(all(checks$enabled))
  expect_false(any(grepl("GDP", checks$variable, fixed = TRUE)))
  expect_false(any(grepl("Total excl", checks$variable, fixed = TRUE)))

  primary <- checks[
    checks$check_id == "hist_2024_primary_energy_world", , drop = FALSE
  ]
  expect_equal(primary$unit, "Mtoe")
  expect_equal(primary$central, 620 / 0.041868, tolerance = 1e-6)

  population <- checks[
    checks$check_id == "hist_2024_population_cha", , drop = FALSE
  ]
  expect_equal(population$region, "CHA")
  expect_equal(population$unit, "billion")
  expect_equal(population$central, 1.408)
})

test_that("public PDF APIs use the four check-family names", {
  batchArguments <- formals(batchPlotReport)
  reportArguments <- formals(validationPdfReport)

  expect_true("validation_checks" %in% names(batchArguments))
  expect_true("policy_checks" %in% names(batchArguments))
  expect_true("indicators_checks" %in% names(batchArguments))
  expect_true("long_term_checks" %in% names(batchArguments))

  expect_true("validation_checks" %in% names(reportArguments))
  expect_true("policy_checks" %in% names(reportArguments))
  expect_true("indicators_checks" %in% names(reportArguments))
  expect_true("long_term_checks" %in% names(reportArguments))
})

test_that("country policy checks evaluate configured country targets", {
  report <- makeCompletedValidationReport(regions = c("GRC", "DEU"))
  checks <- data.frame(
    check_id = "grc_electrification",
    country = "GRC",
    variable = "Final Energy|Electricity Share",
    unit = "1",
    baseline_year = NA,
    target_year = 2024,
    target_type = "minimum",
    target_value = 0.15,
    warn_tolerance = 0.02,
    source = "Reviewed policy source",
    notes = "",
    enabled = TRUE
  )

  result <- validateResults(report, policy_checks = checks)
  expect_equal(result$policies$region, "GRC")
  expect_equal(result$policies$observed, 0.2)
  expect_equal(result$policies$status, "pass")
})

test_that("long-term current-policy checks evaluate exact endpoint CAGRs", {
  report <- makeCompletedValidationReport()
  checks <- data.frame(
    check_id = "world_pe_intensity_cp",
    variable = "Intensity|Primary Energy",
    unit = "Mtoe/billion US$2015",
    region = "World",
    start_year = 2010,
    end_year = 2024,
    reference_cagr = -0.02,
    fail_min = -0.5,
    pass_min = -0.1,
    pass_max = 0.1,
    fail_max = 0.5,
    source = "Panagiotis current-policies document",
    notes = "",
    enabled = TRUE
  )

  result <- validateResults(report, long_term_checks = checks)
  expect_equal(result$long_term$observed, 0)
  expect_equal(result$long_term$deviation, 1)
  expect_equal(result$long_term$status, "fail")
})

test_that("long-term country selectors expand one check to ISO3 regions", {
  report <- makeCompletedValidationReport(
    regions = c("GRC", "DEU", "EU", "World"),
    years = c(2025, 2050)
  )
  intensity <- report[, , "Intensity|Primary Energy"]
  intensity[, 2025, ] <- 1
  intensity["GRC", 2050, ] <- (1 - 0.01)^25
  intensity["DEU", 2050, ] <- (1 - 0.02)^25
  report <- replaceValidationVariable(
    report, "Intensity|Primary Energy", intensity
  )
  checks <- defaultLongTermValidationChecks()
  checks <- checks[
    checks$check_id == "lt_energy_intensity_gdp_countries",
    ,
    drop = FALSE
  ]

  result <- validateResults(report, long_term_checks = checks)

  expect_setequal(result$long_term$region, c("DEU", "GRC"))
  expect_true(all(result$long_term$status == "pass"))
  expect_equal(result$long_term$observed, c(-0.02, -0.01), tolerance = 1e-10)
})

test_that("the dedicated template contains all four validation sections", {
  report <- makeCompletedValidationReport()
  sections <- validateResults(
    results = list(Scenario_50 = report),
    validation_checks = defaultValidationChecks(),
    policy_checks = defaultPolicyValidationChecks(),
    indicators_checks = defaultIndicatorsChecks(),
    long_term_checks = defaultLongTermValidationChecks()
  )
  outputFile <- tempfile(fileext = ".tex")
  on.exit(unlink(outputFile), add = TRUE)
  environment <- new.env(parent = globalenv())
  environment$validation_sections <- sections
  environment$validation_metadata <- data.frame(
    Scenario = "Scenario_50%", fScenario = "1"
  )

  knitr::knit(
    system.file("templates", "validation.Rnw", package = "postprom"),
    output = outputFile,
    envir = environment,
    quiet = TRUE
  )
  tex <- paste(readLines(outputFile, warn = FALSE), collapse = "\n")

  section_positions <- vapply(
    c("Validation checks", "Policy checks", "Indicators check", "Long term checks"),
    function(section) regexpr(paste0("\\\\section\\{", section, "\\}"), tex)[1],
    numeric(1)
  )
  expect_true(all(section_positions > 0))
  expect_true(all(diff(section_positions) > 0))
  expect_match(tex, "Scenario\\_50\\%", fixed = TRUE)
  expect_match(
    tex,
    "\\usepackage[a4paper, landscape, margin=1.2cm]{geometry}",
    fixed = TRUE
  )
  expect_match(tex, "p{0.080\\linewidth}", fixed = TRUE)
  expect_match(tex, "\\definecolor{statuspass}{HTML}{C8E6C9}", fixed = TRUE)
  expect_match(
    tex,
    "\\colorbox{statusfail}{\\strut\\textbf{Fail}}",
    fixed = TRUE
  )
  expect_false(grepl("Evaluated & Pass & Warn & Fail", tex, fixed = TRUE))
})

test_that("the plot PDF template no longer embeds validation", {
  plotFile <- tempfile(fileext = ".rds")
  outputFile <- tempfile(fileext = ".tex")
  saveRDS(list(), plotFile)
  on.exit(unlink(c(plotFile, outputFile)), add = TRUE)
  environment <- new.env(parent = globalenv())
  environment$plot_rds_path <- plotFile
  environment$pdf_title <- "Plot report"
  environment$fScenario <- 1

  knitr::knit(
    system.file("templates", "pdf.Rnw", package = "postprom"),
    output = outputFile,
    envir = environment,
    quiet = TRUE
  )
  tex <- paste(readLines(outputFile, warn = FALSE), collapse = "\n")

  expect_false(grepl("Indicator Validation", tex, fixed = TRUE))
  expect_false(grepl("Validation checks", tex, fixed = TRUE))
  expect_match(tex, "\\\\tableofcontents")
})
