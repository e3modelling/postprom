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

test_that("policy checks allow a one-percent numerical pass margin", {
  target <- 0.152542372881356
  warningTolerance <- 0.1 * target

  expect_equal(
    postprom:::classifyPolicyTarget(
      observed = 0.1519765,
      target = target,
      tolerance = warningTolerance,
      type = "reduction_from_baseline"
    ),
    "pass"
  )
  expect_equal(
    postprom:::classifyPolicyTarget(
      observed = 0.145,
      target = target,
      tolerance = warningTolerance,
      type = "reduction_from_baseline"
    ),
    "warn"
  )
  expect_equal(
    postprom:::classifyPolicyTarget(
      observed = 0.13,
      target = target,
      tolerance = warningTolerance,
      type = "reduction_from_baseline"
    ),
    "fail"
  )
})

test_that("the near-miss Japan commercial policy is reported as passing", {
  report <- makeCompletedValidationReport(
    regions = "JPN", years = c(2013, 2030)
  )
  commercial <- magclass::new.magpie(
    "JPN", c(2013, 2030), "Final Energy|Commercial", fill = 100
  )
  commercial[, 2030, ] <- 84.80235
  commercial <- magclass::add_dimension(
    commercial, dim = 3.2, add = "unit", nm = "Mtoe"
  )
  report <- magclass::mbind(report, commercial)
  checks <- defaultPolicyValidationChecks()
  checks <- checks[
    checks$check_id == "policy_11c_jpn_bui_eff_30_min", , drop = FALSE
  ]

  result <- validateResults(report, policy_checks = checks)

  expect_equal(result$policies$observed, 0.1519765, tolerance = 1e-8)
  expect_equal(result$policies$status, "pass")
})

test_that("packaged policy checks preserve regions and protocol provenance", {
  checks <- defaultPolicyValidationChecks()

  expect_equal(nrow(checks), 126)
  expect_equal(anyDuplicated(checks$check_id), 0L)
  expect_true(sum(checks$enabled) > 0)
  expect_true(sum(!checks$enabled) > 0)
  expect_true(all(grepl("zenodo.20848687", checks$source, fixed = TRUE)))
  expect_true(all(checks$country %in% c("CHA", "EU", "IND", "JPN", "USA")))
  expect_false("CHN" %in% checks$country)

  hfc <- checks[
    checks$check_id == "policy_9c_chn_gen_fgas_45_min", , drop = FALSE
  ]
  expect_equal(hfc$country, "CHA")
  expect_equal(hfc$target_type, "reduction_from_baseline")
  expect_equal(hfc$target_value, 0.8)
  expect_equal(hfc$unit, "1")
  expect_true(hfc$enabled)

  renewable <- checks[
    checks$check_id == "policy_3c_ind_ene_ren_30_min", , drop = FALSE
  ]
  expect_equal(
    renewable$variable,
    "Secondary Energy|Electricity|Renewables Share"
  )
  expect_equal(renewable$target_value, 0.2344)
  expect_equal(renewable$unit, "1")

  noYear <- checks[
    checks$check_id == "policy_4a_usa_ene_fin_24_min", , drop = FALSE
  ]
  expect_false(noYear$enabled)
  expect_match(noYear$notes, "target year is unavailable")
})

test_that("relative policy checks use the report's native source unit", {
  report <- makeCompletedValidationReport(
    regions = "USA", years = c(2010, 2035)
  )
  hfc <- magclass::new.magpie(
    "USA", c(2010, 2035), "Emissions|HFC", fill = 100
  )
  hfc[, 2010, ] <- 100
  hfc[, 2035, ] <- 10
  hfc <- magclass::add_dimension(
    hfc, dim = 3.2, add = "unit", nm = "kt HFC/yr"
  )
  report <- magclass::mbind(report, hfc)
  checks <- defaultPolicyValidationChecks()
  checks <- checks[
    checks$check_id == "policy_3a_usa_gen_fgas_36_min", , drop = FALSE
  ]

  result <- validateResults(report, policy_checks = checks)

  expect_equal(result$policies$observed, 0.9)
  expect_equal(result$policies$reference, 0.85)
  expect_equal(result$policies$unit, "1")
  expect_equal(result$policies$status, "pass")
  expect_equal(result$policies$period, "2010--2035")
  expect_match(result$policies$message, "2012 and 2036")
})

test_that("reviewed policy aliases and missing carbon-price years are converted", {
  checks <- defaultPolicyValidationChecks()
  getCheck <- function(id) checks[checks$check_id == id, , drop = FALSE]

  expect_equal(
    getCheck("policy_1a_chn_gen_eff_30_min")$variable,
    "Carbon Intensity|GDP"
  )
  expect_equal(
    getCheck("policy_17a_eur_gen_fos_50_max")$variable,
    "Emissions|CO2|Energy|Supply|Solids"
  )
  expect_equal(
    getCheck("policy_14a_eur_alu_ghg_30_min")$variable,
    "Emissions|Kyoto Gases|AFOLU"
  )
  geothermal <- getCheck("policy_3d_jpn_ene_ren_40_min")
  expect_equal(
    geothermal$variable,
    paste0(
      "Secondary Energy|Electricity|Geothermal and other renewable ",
      "sources Share"
    )
  )
  expect_equal(geothermal$unit, "1")
  expect_equal(geothermal$target_value, 0.01)

  chinaPrice <- getCheck("policy_13a_chn_ene_ets_20_min")
  japanPrice <- getCheck("policy_8a_jpn_tra_fin_02_min")
  expect_true(chinaPrice$enabled)
  expect_equal(chinaPrice$target_year, 2025)
  expect_equal(chinaPrice$unit, "US$2015/tn CO2")
  expect_true(japanPrice$enabled)
  expect_equal(japanPrice$target_year, 2025)
  expect_equal(japanPrice$unit, "US$2015/tn CO2")
})

test_that("reviewed China EU and Japan policy mappings are evaluated", {
  regions <- c("CHA", "EU", "JPN")
  years <- c(2025, 2030, 2040, 2050)
  report <- makeCompletedValidationReport(regions = regions, years = years)

  carbonPrice <- magclass::new.magpie(
    regions, years, "Price|Carbon", fill = 20
  )
  carbonPrice["JPN", , ] <- 2
  carbonPrice <- magclass::add_dimension(
    carbonPrice, dim = 3.2, add = "unit", nm = "US$2015/tn CO2"
  )
  afolu <- magclass::new.magpie(
    regions, years, "Emissions|Kyoto Gases|AFOLU", fill = -300
  )
  afolu <- magclass::add_dimension(
    afolu, dim = 3.2, add = "unit", nm = "Mt CO2-equiv/yr"
  )
  report <- magclass::mbind(report, carbonPrice, afolu)

  checkIds <- c(
    "policy_1a_chn_gen_eff_30_min",
    "policy_13a_chn_ene_ets_20_min",
    "policy_17a_eur_gen_fos_50_max",
    "policy_14a_eur_alu_ghg_30_min",
    "policy_3a_jpn_ene_ren_40_min",
    "policy_8a_jpn_tra_fin_02_min",
    "policy_8a_jpn_tra_fin_02_max"
  )
  checks <- defaultPolicyValidationChecks()
  checks <- checks[checks$check_id %in% checkIds, , drop = FALSE]

  result <- validateResults(report, policy_checks = checks)

  expect_false(
    "Secondary Energy|Electricity|Solar Share" %in%
      magclass::getItems(report, 3.1)
  )
  expect_setequal(result$policies$check_id, checkIds)
  expect_false(any(result$policies$status == "skip"))
  expect_equal(
    result$policies$variable[
      result$policies$check_id == "policy_3a_jpn_ene_ren_40_min"
    ],
    "Secondary Energy|Electricity|Solar Share"
  )
  expect_equal(
    result$policies$observed[
      result$policies$check_id == "policy_3a_jpn_ene_ren_40_min"
    ],
    0.1
  )
})

test_that("cumulative policy checks integrate the annual report series", {
  report <- makeCompletedValidationReport(
    regions = "USA", years = c(2010, 2020, 2030, 2040)
  )
  variable <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
  annual <- magclass::new.magpie(
    "USA", c(2010, 2020, 2030, 2040), variable, fill = 1
  )
  annual[, 2010, ] <- 2
  annual <- magclass::add_dimension(
    annual, dim = 3.2, add = "unit", nm = "Mt CO2/yr"
  )
  report <- magclass::mbind(report, annual)
  checks <- defaultPolicyValidationChecks()
  checks <- checks[
    checks$check_id == "policy_7a_usa_bui_eff_40_alt_min", , drop = FALSE
  ]

  result <- validateResults(report, policy_checks = checks)

  expect_true(checks$enabled)
  expect_equal(checks$target_type, "cumulative_minimum")
  expect_equal(result$policies$observed, 25)
  expect_equal(result$policies$unit, "Mt CO2")
  expect_equal(result$policies$period, "2010--2040")
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
  expect_false(grepl(
    "textbf\\{(Pass|Warn|Fail|N/A)\\}\\}\\}[0-9]", tex, perl = TRUE
  ))
  policyText <- sub(
    ".*\\\\section\\{Policy checks\\}", "", tex
  )
  policyText <- sub(
    "\\\\section\\{Indicators check\\}.*", "", policyText
  )
  doiMatches <- gregexpr(
    "10.5281/zenodo.20848687", policyText, fixed = TRUE
  )[[1]]
  expect_equal(sum(doiMatches > 0), 1)
  expect_false(grepl("Message & Source", policyText, fixed = TRUE))
  expect_false(grepl("Evaluated & Pass & Warn & Fail", tex, fixed = TRUE))

  templateText <- paste(readLines(
    system.file("templates", "validation.Rnw", package = "postprom"),
    warn = FALSE
  ), collapse = "\n")
  expect_match(
    templateText,
    'benchmark$status == "pass"',
    fixed = TRUE
  )
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
