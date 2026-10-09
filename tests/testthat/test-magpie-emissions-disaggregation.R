test_that("non-EU MAgPIE parent emissions are rebuilt from their children", {
  parent <- "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"
  deforestation <- "Emissions|CO2|Land|Land-use Change|+|Deforestation (Mt CO2/yr)"
  regrowth <- "Emissions|CO2|Land|Land-use Change|+|Regrowth (Mt CO2/yr)"
  variables <- c(parent, deforestation, regrowth)
  values <- array(
    c(900, 100, 200),
    dim = c(1, 1, 3),
    dimnames = list("LAM", "y2015", variables)
  )
  h12 <- magclass::as.magpie(values, spatial = 1, temporal = 2)

  result <- postprom:::.disaggregateToResCy(
    m = h12,
    resCy = "LAM",
    h12For = "LAM",
    emiCsv = data.frame(variable = variables, weight_source = "unused"),
    cellWeights = list(),
    cwWeights = list()
  )

  expect_equal(as.numeric(result["LAM", "y2015", parent]), 300)
  expect_equal(as.numeric(result["LAM", "y2015", deforestation]), 100)
  expect_equal(as.numeric(result["LAM", "y2015", regrowth]), 200)
})