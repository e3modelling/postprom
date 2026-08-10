makeValidationInput <- function(regions = c("World", "EU"),
                                years = c(2010, 2019, 2020, 2021, 2022, 2024),
                                energyEmissions = 20) {
  makeVariable <- function(variable, unit, value) {
    x <- magclass::new.magpie(regions, years, variable, fill = value)
    magclass::add_dimension(x, dim = 3.2, add = "unit", nm = unit)
  }

  report <- magclass::mbind(
    makeVariable("GDP|PPP", "billion US$2015/yr", 100),
    makeVariable("Final Energy", "Mtoe", 50),
    makeVariable("Final Energy|Electricity", "Mtoe", 10),
    makeVariable("Emissions|CO2", "Mt CO2/yr", 30),
    makeVariable("Emissions|CO2|Energy", "Mt CO2/yr", energyEmissions),
    makeVariable("Primary Energy", "Mtoe", 80),
    makeVariable("Primary Energy|Coal", "Mtoe", 20),
    makeVariable("Primary Energy|Gas", "Mtoe", 20),
    makeVariable("Primary Energy|Oil", "Mtoe", 20)
  )

  for (carrier in c(
    "Electricity", "Hydrogen", "Heat", "Liquids", "Gases", "Solids"
  )) {
    report <- magclass::mbind(
      report,
      makeVariable(
        paste0("Emissions|CO2|Energy|Supply|", carrier),
        "Mt CO2/yr", 2
      ),
      makeVariable(
        paste0("Secondary Energy|", carrier),
        "TWh", 4
      )
    )
  }
  for (source in c(
    "Solar", "Wind", "Hydro",
    "Geothermal and other renewable sources", "Biofuels", "Nuclear"
  )) {
    sourceValue <- c(
      "Solar" = 0.4, "Wind" = 0.8, "Hydro" = 1,
      "Geothermal and other renewable sources" = 0.2,
      "Biofuels" = 0.4, "Nuclear" = 1.2
    )[[source]]
    report <- magclass::mbind(
      report,
      makeVariable(
        paste0("Secondary Energy|Electricity|", source),
        "TWh", sourceValue
      )
    )
  }
  report
}

makeCompletedValidationReport <- function(...) {
  raw <- makeValidationInput(...)
  indicators <- postprom:::calculateTransitionIndicators(raw)
  do.call(magclass::mbind, c(list(raw), unname(indicators)))
}

replaceValidationVariable <- function(report, variable, replacement) {
  keep <- magclass::getItems(report, 3.1) != variable
  magclass::mbind(report[, , keep], replacement)
}
