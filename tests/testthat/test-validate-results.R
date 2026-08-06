test_that("validateResults returns auditable structured findings", {
  report <- makeCompletedValidationReport()
  result <- validateResults(report)

  expect_s3_class(result, "postprom_validation")
  expect_true(all(c("findings", "summary", "indicator_values") %in% names(result)))
  expect_setequal(
    unique(result$indicator_values$indicator_id),
    c(
      "primary_energy_intensity",
      "final_energy_intensity",
      "primary_energy_carbon_intensity",
      "energy_co2_gdp_intensity",
      "electrification_rate",
      "electricity_carbon_intensity",
      "fossil_primary_energy_share"
    )
  )
  expect_true(all(c("pass", "warn", "fail", "skip") %in%
                    names(result$indicators$summary)))
})

test_that("validator consumes the reported indicator instead of recalculating it", {
  report <- makeCompletedValidationReport()
  replacement <- report[, , "Intensity|Primary Energy"]
  replacement[] <- -1
  report <- replaceValidationVariable(
    report, "Intensity|Primary Energy", replacement
  )

  result <- validateResults(report)
  finding <- result$indicators$findings[
    result$indicators$findings$check_id == "primary_energy_intensity_nonnegative" &
      result$indicators$findings$region == "World" &
      result$indicators$findings$start_year == 2010,
    ,
    drop = FALSE
  ]

  expect_equal(finding$observed, -1)
  expect_equal(finding$status, "fail")
})

test_that("shares, missing indicators, and exact benchmark years are diagnosed", {
  report <- makeCompletedValidationReport()
  replacement <- report[, , "Final Energy|Electricity Share"]
  replacement["World", 2024, ] <- 1.2
  report <- replaceValidationVariable(
    report, "Final Energy|Electricity Share", replacement
  )
  report <- report[
    , , magclass::getItems(report, 3.1) != "Primary Energy|Fossil Share"
  ]

  checks <- defaultIndicatorsChecks()
  checks$enabled[
    checks$check_id == "pe_intensity_world_1990_2010"
  ] <- TRUE
  result <- validateResults(report, indicators_checks = checks)
  findings <- result$indicators$findings

  expect_true(any(
    findings$check_id == "electrification_rate_bounds" &
      findings$region == "World" &
      findings$end_year == 2024 &
      findings$status == "fail"
  ))
  expect_true(any(
    findings$check_id ==
      "availability_fossil_primary_energy_share" &
      findings$status == "fail"
  ))
  expect_true(any(
    findings$check_id == "pe_intensity_world_1990_2010" &
      findings$status == "skip"
  ))
})

test_that("invalid denominators fail input checks and skip dependent checks", {
  report <- makeCompletedValidationReport()
  report["World", 2010, "GDP|PPP"] <- 0
  reported <- report[, , "Intensity|Primary Energy"]
  reported["World", 2010, ] <- 0
  report <- replaceValidationVariable(
    report, "Intensity|Primary Energy", reported
  )

  result <- validateResults(report)
  findings <- result$indicators$findings
  expect_true(any(
    findings$check_id == "inputs_primary_energy_intensity" &
      findings$region == "World" &
      findings$start_year == 2010 &
      findings$status == "fail"
  ))
  expect_true(any(
    findings$check_id == "primary_energy_intensity_nonnegative" &
      findings$region == "World" &
      findings$start_year == 2010 &
      findings$status == "skip"
  ))
})

test_that("negative net carbon intensities pass finite checks", {
  report <- makeCompletedValidationReport(energyEmissions = -10)
  result <- validateResults(report)
  genericCarbon <- result$indicators$findings[
    result$indicators$findings$check_id %in% c(
      "primary_energy_carbon_intensity_finite",
      "energy_co2_gdp_intensity_finite"
    ),
    ,
    drop = FALSE
  ]

  expect_true(all(genericCarbon$status == "pass"))
  expect_true(any(genericCarbon$observed < 0))
})

test_that("benchmark bands use absolute negative references", {
  report <- makeCompletedValidationReport()
  checks <- defaultIndicatorsChecks()
  check <- checks[
    checks$check_id == "pe_intensity_world_2010_2019",
    ,
    drop = FALSE
  ]

  primary <- report[, , "Intensity|Primary Energy"]
  primary["World", 2010, ] <- 1
  primary["World", 2019, ] <- (1 - 0.02)^9
  report <- replaceValidationVariable(
    report, "Intensity|Primary Energy", primary
  )
  result <- validateResults(report, indicators_checks = check)
  expect_equal(result$indicators$findings$status[
    result$indicators$findings$check_id == check$check_id
  ], "pass")

  primary["World", 2019, ] <- (1 - 0.024)^9
  report <- replaceValidationVariable(
    report, "Intensity|Primary Energy", primary
  )
  result <- validateResults(report, indicators_checks = check)
  expect_equal(result$indicators$findings$status[
    result$indicators$findings$check_id == check$check_id
  ], "warn")
})

test_that("wrong units are failures and make configured checks skip", {
  report <- makeCompletedValidationReport()
  replacement <- report[, , "Primary Energy|Fossil Share"]
  magclass::getItems(replacement, 3.2) <- "%"
  report <- replaceValidationVariable(
    report, "Primary Energy|Fossil Share", replacement
  )

  result <- validateResults(report)
  findings <- result$indicators$findings
  expect_true(any(
    findings$check_id ==
      "availability_fossil_primary_energy_share" &
      findings$status == "fail"
  ))
  expect_true(any(
    findings$check_id == "fossil_primary_energy_share_bounds" &
      findings$status == "skip"
  ))
})
