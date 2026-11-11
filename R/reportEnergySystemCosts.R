#' Report PRISMA-style energy system costs
#'
#' This function reports a first electricity-focused set of energy system cost
#' variables from OPEN-PROM output. Investment is calculated from gross new
#' nominal electricity capacity additions and overnight power generation capital
#' cost. Capital cost reuses the model's annualized CAPEX and fixed-cost variable.
#' Policy cost reports active economy-module subsidy spending.
#'
#' Total energy system cost, step 1: the function also reads every input that
#' the total energy system cost needs (`readEnergySystemCostInputs()`) and the
#' mrprom ex-tax end-use fuel prices (`getFuelPriceExTax()`). Nothing from this
#' step is reported yet.
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
  capitalCost <- readGDX(path, "i04GrossCapCosSubRen", field = "l")[regions, years, ] * 1000 #kUS$2015 to US$2015

  nominalNewCapacity <- newCapacity / availability
  nominalNewCapacity[!is.finite(nominalNewCapacity)] <- 0

  investment <- nominalNewCapacity * overnightCost
  imCGI <- try(readGDX(path, "imCGI", field = "l")[regions, years, ], silent = TRUE)
  if (!inherits(imCGI, "try-error")) {
    investment <- investment * imCGI
  }
  investment[!is.finite(investment)] <- 0
  investment <- RenametoTECH(path, investment)
  getItems(investment, 3) <- paste0("Investment|Energy Supply|Electricity|", getItems(investment, 3))
  investment <- add_dimension(investment, dim = 3.2, add = "unit", nm = "billion US$2015/yr")

  capitalCost <- RenametoTECH(path, capitalCost)
  getItems(capitalCost, 3) <- paste0("Capital Cost|Electricity|", getItems(capitalCost, 3))
  capitalCost <- add_dimension(capitalCost, dim = 3.2, add = "unit", nm = "US$2015/kW")

  subsidyTotal <- readGDX(path, "V11SubsiTot", field = "l")[regions, years, ]
  netSubsidyTax <- readGDX(path, "VmNetSubsiTax", field = "l")[regions, years, ]
  policyCost <- subsidyTotal - netSubsidyTax
  policyCost[!is.finite(policyCost)] <- 0
  getItems(policyCost, 3) <- "Policy Cost|Energy System"
  policyCost <- add_dimension(policyCost, dim = 3.2, add = "unit", nm = "million US$2015/yr")

  # Total energy system cost, step 1: read all inputs. Not reported yet.
  costInputs <- readEnergySystemCostInputs(path, regions, years)
  costInputs$fuel$priceExTax <- getFuelPriceExTax(regions, years)

  return(mbind(investment, capitalCost, policyCost))
}

# Helper ----------------------------------------------------------------------------------------
RenametoTECH <- function(path, prod) {
  mapSEC <- data.frame(
    i = c("ATHLGN", "ATHCOAL", "ATHGAS", "ATHBMSWAS", "ATHBMSCCS", "ATHOIL",
          "PGLHYD", "PGSHYD", "PGAWND", "PGSOL", "PGCSP", "PGOTHREN",
          "PGANUC", "ATHCOALCCS", "ATHLGNCCS", "ATHGASCCS", "PGAWNO", "PGH2F"),
    
    te = c("Lignite|w/o CCS", "Coal|w/o CCS","Gas|w/o CCS", "Biomass|w/o CCS","Biomass|w/ CCS", "Oil",
           "Hydro|Large", "Hydro|Small", "Wind|Onshore", "Solar|PV","Solar|CSP", "Geothermal",
           "Nuclear","Coal|w/ CCS", "Lignite|w/ CCS","Gas|w/ CCS", "Wind|Offshore","Hydrogen"))
  
  prod <- toolAggregate(prod, dim = 3, rel = mapSEC, from = "i", to = "te")
  return(prod)
}

# Total energy system cost: inputs --------------------------------------------------------------
# Reads every GDX symbol the total energy system cost needs, grouped by sector.
# Units are the model's own, derived from the equations (not from declaration comments).
# A symbol that cannot be read comes back as NULL with a warning, so one bad name does not
# stop the MIF conversion.
readEnergySystemCostInputs <- function(path, regions, years) {
  reg <- function(name) readCostSymbol(path, name, regions = regions, years = years) # region x year
  glo <- function(name) readCostSymbol(path, name, years = years) # no region dimension
  raw <- function(name, field = "l") readCostSymbol(path, name, field = field) # no region, no year

  list(
    shared = list(
      imDisc           = reg("imDisc"),           # discount rate by sector; uses "STEAMP" and "H2P"
      imCGI            = reg("imCGI"),            # capital goods index (1 in current runs)
      smGwToTwhPerYear = glo("smGwToTwhPerYear"), # TWh per GW-year (8.76; 8.784 in leap years)
      smTWhToMtoe      = raw("smTWhToMtoe")       # 0.086 Mtoe per TWh
    ),
    power = list(
      V04CapElecNominal = reg("V04CapElecNominal"), # GW nameplate
      V04CapexFixCostPG = reg("V04CapexFixCostPG"), # US$2015/kW/yr = CRF x capex + FOM, per nameplate kW
      V04CapexRESRate   = reg("V04CapexRESRate"),   # wind/solar grid & storage markup; 1 for other plants
      i04FixOandMCost   = reg("i04FixOandMCost"),   # US$2015/kW/yr
      VmProdElec        = reg("VmProdElec"),        # TWh
      i04VarCost        = glo("i04VarCost")         # US$2015/MWh (no region dimension)
    ),
    heat = list(
      VmCapSte              = reg("VmCapSte"),              # Mtoe/yr steam capacity (effective)
      VmProdSte             = reg("VmProdSte"),             # Mtoe steam
      i09ProdLftSte         = raw("i09ProdLftSte"),         # yr (TSTEAM only)
      i09CostInvCostSteProd = glo("i09CostInvCostSteProd"), # US$2015/kW (kW_th for DH, kW_e for CHP)
      i09CostFixOMSteProd   = glo("i09CostFixOMSteProd"),   # US$2015/kW/yr (0 for CHP)
      i09CostVOMSteProd     = glo("i09CostVOMSteProd"),     # US$2015/toe steam
      i09AvailRateSteProd   = glo("i09AvailRateSteProd"),   # availability, share
      i09PowToHeatRatio     = glo("i09PowToHeatRatio"),     # power-to-heat ratio (0 for DH)
      TDHP                  = raw("TDHP", field = NULL)     # set: district-heating plants
    ),
    hydrogen = list(
      VmCapH2          = reg("VmCapH2"),          # GW H2 output, nameplate
      VmProdH2         = reg("VmProdH2"),         # Mtoe H2
      i05CostCapH2Prod = reg("i05CostCapH2Prod"), # US$2015/kW
      i05CostFOMH2Prod = reg("i05CostFOMH2Prod"), # US$2015/kW/yr
      i05CostVOMH2Prod = reg("i05CostVOMH2Prod"), # US$2015/toe H2
      i05ProdLftH2     = glo("i05ProdLftH2")      # yr (no region dimension)
    ),
    demand = list(
      V02CapCostTech        = reg("V02CapCostTech"),        # kUS$2015 per toe useful/yr = (CRF x capex + FOM) / efficiency
      imUsfEneConvSubTech   = reg("imUsfEneConvSubTech"),   # useful / final energy
      V02EquipCapTechSubsec = reg("V02EquipCapTechSubsec"), # Mtoe/yr final-energy capacity
      i02util               = reg("i02util"),               # utilisation (1)
      imFixOMCostTech       = reg("imFixOMCostTech"),       # INDDOM/NENSE: US$2015/toe; PC: kUS$2015/veh/yr
      imVarCostTech         = reg("imVarCostTech"),         # INDDOM/NENSE: US$2015/toe
      V01StockPcYearlyTech  = reg("V01StockPcYearlyTech"),  # million cars, total fleet
      V01CapCostAnnualized  = reg("V01CapCostAnnualized"),  # kUS$2015/veh/yr (CRF x (capex - subsidy))
      VmSubsiDemTech        = reg("VmSubsiDemTech")         # check only: must be 0 for PC
    ),
    co2 = list(
      V06CO2CaptureCCS            = reg("V06CO2CaptureCCS"),           # MtCO2 captured (PG, H2P, industry)
      VmCstCO2SeqCsts             = reg("VmCstCO2SeqCsts"),            # US$2015/tCO2, incl. storage tax
      i06CCSAvailabilityCostAdder = glo("i06CCSAvailabilityCostAdder") # US$2015/tCO2 storage tax
    ),
    cdr = list(
      V06CapCDR      = reg("V06CapCDR"),      # tCO2/yr, removals = capacity
      V06GrossCapCDR = glo("V06GrossCapCDR"), # US$2015/tCO2 (global)
      V06FixOandMCDR = glo("V06FixOandMCDR"), # US$2015/tCO2 (global)
      V06VarCostCDR  = glo("V06VarCostCDR")   # US$2015/tCO2 incl. DAC storage (global)
    ),
    fuel = list(
      VmFinalEnergy           = reg("VmFinalEnergy"),           # Mtoe; priced with getFuelPriceExTax()
      # Model prices, used only for their growth rate to project the ex-tax prices:
      VmPriceFuelSubsecCarVal = reg("VmPriceFuelSubsecCarVal"), # kUS$2015/toe, incl. carbon (and biomass tax)
      VmPriceCarbon           = reg("VmPriceCarbon"),           # kUS$2015/toe carbon cost, removed from the growth
      VmPriceFinal            = reg("VmPriceFinal"),            # kUS$2015/toe excl. carbon; growth for transport/bunkers
      i08BmswasPriceAdder     = glo("i08BmswasPriceAdder"),     # kUS$2015/toe biomass tax, removed from the growth
      lastDataYear            = { # last history year (2023); ex-tax prices are projected from here
        dy <- raw("datay", field = NULL)
        if (is.null(dy)) NULL else paste0("y", max(as.integer(as.character(dy))))
      }
    )
  )
}

# Reads one GDX symbol and subsets it to regions and/or years. NULL + warning on failure.
readCostSymbol <- function(path, name, regions = NULL, years = NULL, field = "l") {
  tryCatch(
    {
      x <- if (is.null(field)) readGDX(path, name) else readGDX(path, name, field = field)
      if (!is.null(regions)) x <- x[regions, , ]
      if (!is.null(years)) x <- x[, years, ]
      x
    },
    error = function(e) {
      warning("Energy system cost: could not read '", name, "': ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

# Ex-tax end-use fuel prices from mrprom --------------------------------------------------------
# Regional ex-tax end-use prices in USD2015/toe (IEA PRICE_X_TAX, else PRICE_TOTAL), as
# region x year x SBS.EF. History only (2010 to the last IEA year, currently 2025).
# Same idea as reportGDP_MER(), with one difference: calcFuelPriceExTax returns weight = NULL,
# so aggregate = TRUE would ADD country prices. Instead, take country values and aggregate
# them with 2020 population weights, as mrprom fullOPEN_PROM() does for IFuelPrice.
# Needs mrprom loaded (library(mrprom)) at a version that has calcFuelPriceExTax.
# NULL + warning if the mrprom call fails.
# TODO (not done here): apply the model's price rules and extend to projection years.
getFuelPriceExTax <- function(regions, years) {
  tryCatch(
    {
      price <- madrat::calcOutput(type = "FuelPriceExTax", aggregate = FALSE)
      pop <- madrat::calcOutput(type = "POP", aggregate = FALSE)[, 2020, ]
      pop <- collapseDim(magclass::setYears(pop, NULL))
      map <- toolGetMapping("regionmappingOPDEV5.csv", type = "regional", where = "mrprom")
      price <- toolAggregate(price,
        rel = map, weight = pop[getItems(price, 1), , ],
        from = "ISO3.Code", to = "Region.Code"
      )
      price <- magclass::collapseNames(price) # drop single-item dimensions such as the unit
      price[regions, intersect(years, getYears(price)), ]
    },
    error = function(e) {
      warning("Energy system cost: could not get ex-tax fuel prices from mrprom: ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
}