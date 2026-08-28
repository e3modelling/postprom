#' Report PRISMA-style energy system costs
#'
#' This function reports a first electricity-focused set of energy system cost
#' variables from OPEN-PROM output. Investment is calculated from gross new
#' nominal electricity capacity additions and overnight power generation capital
#' cost. Capital cost reuses the model's annualized CAPEX and fixed-cost variable.
#' Policy cost reports active economy-module subsidy spending.
#'
#' @param path Character string specifying the file path to the GDX data file.
#' @param regions Character vector of region names used to subset the data.
#' @param years Character vector of years to include in the report.
#'
#' @return A magpie object with IAMC-style investment, capital-cost, and policy-cost variables.
#'
#' @examples
#' \dontrun{
#' result <- reportEnergySystemCosts(
#'   system.file("extdata", "blabla.gdx", package = "postprom"),
#'   regions = c("MEA"),
#'   years = c("y2030", "y2040", "y2050")
#' )
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind
#' @importFrom dplyr %>% rename
#' @importFrom gdxrrw rgdx.set
#' @export
reportEnergySystemCosts <- function(path, regions, years) {
  newCapacity <- readGDX(path, "V04NewCapElec", field = "l")[regions, years, ]
  availability <- readGDX(path, "i04AvailRate", field = "l")[regions, years, ]
  overnightCost <- readGDX(path, "i04GrossCapCosSubRen", field = "l")[regions, years, ]
  capitalCost <- readGDX(path, "V04CapexFixCostPG", field = "l")[regions, years, ]

  nominalNewCapacity <- newCapacity / availability
  nominalNewCapacity[!is.finite(nominalNewCapacity)] <- 0

  investment <- nominalNewCapacity * overnightCost
  imCGI <- try(readGDX(path, "imCGI", field = "l")[regions, years, ], silent = TRUE)
  if (!inherits(imCGI, "try-error")) {
    investment <- investment * imCGI
  }
  investment[!is.finite(investment)] <- 0
  investment <- RenametoSTECH(path, investment)
  getItems(investment, 3) <- paste0("Investment|Energy Supply|Electricity|", getItems(investment, 3))
  investment <- add_dimension(investment, dim = 3.2, add = "unit", nm = "billion US$2015/yr")

  capitalCost <- RenametoSTECH(path, capitalCost)
  getItems(capitalCost, 3) <- paste0("Capital Cost|Electricity|", getItems(capitalCost, 3))
  capitalCost <- add_dimension(capitalCost, dim = 3.2, add = "unit", nm = "US$2015/kW")

  subsidyTotal <- readGDX(path, "V11SubsiTot", field = "l")[regions, years, ]
  netSubsidyTax <- readGDX(path, "VmNetSubsiTax", field = "l")[regions, years, ]
  policyCost <- subsidyTotal - netSubsidyTax
  policyCost[!is.finite(policyCost)] <- 0
  getItems(policyCost, 3) <- "Policy Cost|Energy System"
  policyCost <- add_dimension(policyCost, dim = 3.2, add = "unit", nm = "million US$2015/yr")

  return(mbind(investment, capitalCost, policyCost))
}

# Helper ----------------------------------------------------------------------------------------
RenametoSTECH <- function(path, prod) {
  STECH <- rgdx.set(path, "STECH", te = TRUE) %>%
    filter(i %in% getItems(prod,3))
  
  prod <- toolAggregate(prod, dim = 3, rel = STECH, from = "i", to = ".te")
  return(prod)
}

