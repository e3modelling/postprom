test_that("the PDF template renders validation and escapes table content", {
  report <- makeCompletedValidationReport()
  validation <- validateResults(
    stats::setNames(list(report), "Scenario_50%")
  )
  plotFile <- tempfile(fileext = ".rds")
  outputFile <- tempfile(fileext = ".tex")
  saveRDS(list(), plotFile)
  on.exit(unlink(c(plotFile, outputFile)), add = TRUE)

  environment <- new.env(parent = globalenv())
  environment$plot_rds_path <- plotFile
  environment$pdf_title <- "Validation test"
  environment$fScenario <- 1
  environment$validation_result <- validation

  knitr::knit(
    system.file("templates", "pdf.Rnw", package = "postprom"),
    output = outputFile,
    envir = environment,
    quiet = TRUE
  )
  tex <- paste(readLines(outputFile, warn = FALSE), collapse = "\n")

  expect_match(tex, "\\\\section\\{Indicator Validation\\}")
  expect_match(tex, "Evaluated benchmarks", fixed = TRUE)
  expect_match(tex, "Scenario\\_50\\%", fixed = TRUE)
})

test_that("the PDF template remains unchanged when validation is disabled", {
  plotFile <- tempfile(fileext = ".rds")
  outputFile <- tempfile(fileext = ".tex")
  saveRDS(list(), plotFile)
  on.exit(unlink(c(plotFile, outputFile)), add = TRUE)

  environment <- new.env(parent = globalenv())
  environment$plot_rds_path <- plotFile
  environment$pdf_title <- "No validation"
  environment$fScenario <- 1
  environment$validation_result <- NULL

  knitr::knit(
    system.file("templates", "pdf.Rnw", package = "postprom"),
    output = outputFile,
    envir = environment,
    quiet = TRUE
  )
  tex <- paste(readLines(outputFile, warn = FALSE), collapse = "\n")

  expect_false(grepl("\\\\section\\{Indicator Validation\\}", tex))
  expect_match(tex, "\\\\tableofcontents")
})
