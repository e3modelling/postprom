#' Plot Carbon Budget
#'
#' This function adds two variables in the magpie object 
#' in order to plot two vertical lines with the budgets.
#'
#' @param budgetBaseYear The cumulated CO2 emissions in the year before 2020 where the budget begins.
#' @param budget1p5 The carbon budget for CO2 emissions in Gt CO2 reaching 1.5C until 2100.
#' @param budget2c The carbon budget for CO2 emissions in Gt CO2 reaching 2C until 2100.
#' @param probLabel The probability for the carbon budgets.
#' @return A magpie object containing carbon budgets.
#'
#' @examples
#' \dontrun{
#' result <- reportBudget(magpieObject=example_magpie,aggregate= TRUE, budgetBaseYear = 2019,
#'                        budget1p5= 400,budget2c= 1150,probLabel= "67%")}
#'
#' @importFrom magclass mbind
#' @export
reportBudget <- function(magpieObject,
                         aggregate        = TRUE,
                         budgetBaseYear = 2019,
                         budget1p5       = 400,
                         budget2c        = 1150,
                         probLabel       = "67%") {

  # Create two new variables with NA for carbon budgets
  carbonBudget1p5C <- magpieObject[, , "Emissions|CO2|Cumulated", drop = FALSE]
  carbonBudget1p5C[] <- NA
  dimnames(carbonBudget1p5C)[[3]] <- "Emissions|CO2|Budget1p5C.Gt CO2/yr"
  carbonBudget2C <- magpieObject[, , "Emissions|CO2|Cumulated", drop = FALSE]
  carbonBudget2C[] <- NA
  dimnames(carbonBudget2C)[[3]] <- "Emissions|CO2|Budget2C.Gt CO2/yr"  
  
  if (aggregate == TRUE) {
    # Add carbon budget values (1p5C, 2C) to the magpie object and add cumulated emissions before 2020 
    # Carbon budget values for 1p5C and 2C and for probability 67%
    carbonBudget1p5C['World',,] <- budget1p5 + as.numeric(magpieObject["World", budgetBaseYear, "Emissions|CO2|Cumulated.Gt CO2"])
    carbonBudget2C['World',,] <- budget2c + as.numeric(magpieObject["World", budgetBaseYear, "Emissions|CO2|Cumulated.Gt CO2"])
  }
  return(mbind(carbonBudget1p5C, carbonBudget2C))
}
