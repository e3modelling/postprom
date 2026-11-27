test_that("electricity generation keeps PV/CSP, onshore/offshore and oil plant types apart", {
  TECHtoEF <- data.frame(
    TECH = c("PGSOL", "PGCSP", "PGAWND", "PGAWNO", "ATHOIL"),
    EF = c("SOL", "SOL", "WND", "WND", "GDO")
  )
  prod <- magclass::new.magpie("DEU", 2050, TECHtoEF$TECH, fill = c(1, 2, 3, 4, 5))

  result <- getSecondaryEnergy(TECHtoEF, prod,
    CCS = character(), NOCCS = "ATHOIL",
    generationDetail = TRUE
  )

  expected <- c(
    "Solar|PV" = 1, "Solar|CSP" = 2, "Wind|Onshore" = 3, "Wind|Offshore" = 4,
    "Oil|w/o CCS|Gas Turbine" = 5
  )
  expect_setequal(magclass::getItems(result, 3), names(expected))
  expect_equal(as.numeric(result[, , names(expected)]), unname(expected))
})
