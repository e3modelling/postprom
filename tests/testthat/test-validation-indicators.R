test_that("transition indicators are calculated once with stable names and units", {
  input <- makeValidationInput()
  indicators <- postprom:::calculateTransitionIndicators(input)

  expect_equal(
    as.numeric(indicators$finalEnergyIntensity["World", 2010, ]),
    0.5
  )
  expect_equal(
    as.numeric(indicators$primaryEnergyIntensity["World", 2010, ]),
    0.8
  )
  expect_equal(
    as.numeric(indicators$economyWideCo2Intensity["World", 2010, ]),
    0.3
  )
  expect_equal(
    as.numeric(indicators$primaryEnergyCarbonIntensity["World", 2010, ]),
    0.25
  )
  expect_equal(
    as.numeric(indicators$energyCo2GdpIntensity["World", 2010, ]),
    0.2
  )
  expect_equal(
    as.numeric(indicators$primaryEnergyFossilShare["World", 2010, ]),
    0.75
  )
  expect_equal(
    as.numeric(indicators$electricityShare["World", 2010, ]),
    0.2
  )
  expect_equal(
    as.numeric(indicators$secondaryEnergyCarbonIntensity[
      "World", 2010, "Carbon Intensity|Secondary Energy|Electricity"
    ]),
    0.5
  )

  expect_equal(
    magclass::getItems(indicators$primaryEnergyCarbonIntensity, 3),
    "Carbon Intensity|Primary Energy.Mt CO2/Mtoe"
  )
  expect_equal(
    magclass::getItems(indicators$energyCo2GdpIntensity, 3),
    "Carbon Intensity|GDP|Energy.Mt CO2/billion US$2015"
  )
  expect_equal(
    magclass::getItems(indicators$primaryEnergyFossilShare, 3),
    "Primary Energy|Fossil Share.1"
  )
})

test_that("net carbon intensities retain negative finite values", {
  input <- makeValidationInput(energyEmissions = -10)
  indicators <- postprom:::calculateTransitionIndicators(input)

  expect_equal(
    as.numeric(indicators$primaryEnergyCarbonIntensity["World", 2010, ]),
    -0.125
  )
  expect_equal(
    as.numeric(indicators$energyCo2GdpIntensity["World", 2010, ]),
    -0.1
  )
})
