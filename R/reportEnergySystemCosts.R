#' Report PRISMA-style energy system costs
#'
#' This function reports a first electricity-focused set of energy system cost
#' variables from OPEN-PROM output. Investment is calculated from gross new
#' nominal electricity capacity additions and overnight power generation capital
#' cost. Capital cost reuses the model's annualized CAPEX and fixed-cost variable.
#' Policy cost reports active economy-module subsidy spending.
#'
#' It also reports the total energy system cost (rows "Energy System Cost|..."):
#' capital, fixed O&M, variable O&M, fuel and CO2 storage per sector, the sector
#' totals and the overall total, in million US$2015/yr (see `escCompute()`).
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

  # Total energy system cost
  costInputs <- readEnergySystemCostInputs(path, regions, years)
  costInputs$fuel$priceExTax <- getFuelPriceExTax(regions, years)
  energySystemCost <- escCompute(costInputs, path, regions, years)

  return(mbind(investment, capitalCost, policyCost, energySystemCost))
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
      pop <- madrat::calcOutput(type = "POP", aggregate = FALSE)[, 2023, ]
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

# =================================================================================================
# Total energy system cost (million US$2015/yr)
# Each cost = stock (or output) in year t x unit cost in year t. Rows per sector, plus the total.
# Method: ENERGY-SYSTEM-COST-FRAMEWORK.md.
# All arithmetic is done on plain arrays [region, year, item] matched by item NAME, so GAMS symbols
# with different set names (e.g. DSBS.ITECH and SBS.TECH) can be combined safely.
# =================================================================================================

# --- helpers --------------------------------------------------------------------------------------

# x (magpie object, number or named vector) as a plain array [regs, yrs, items].
# A missing region, year or item dimension in x is repeated.
# strict = TRUE: an item that x does not have stops with an error. strict = FALSE: it stays NA.
escA <- function(x, regs, yrs, items = NULL, strict = TRUE) {
  ni <- if (is.null(items)) 1 else length(items)
  out <- array(NA_real_, c(length(regs), length(yrs), ni), dimnames = list(regs, yrs, items))
  if (is.null(x)) return(out)
  if (!is.magpie(x)) {
    v <- if (is.null(names(x))) rep(as.numeric(x)[1], ni) else as.numeric(x[items])
    out[] <- rep(v, each = length(regs) * length(yrs))
    return(out)
  }
  a <- as.array(x)
  r <- if (dim(a)[1] == 1) rep(1, length(regs)) else regs
  y <- if (is.null(dimnames(a)[[2]])) rep(1, length(yrs)) else yrs
  if (is.null(items) || is.null(dimnames(a)[[3]])) {
    out[] <- a[r, y, rep(1, ni), drop = FALSE]
  } else {
    have <- items[items %in% dimnames(a)[[3]]]
    if (strict && length(have) < length(items)) {
      stop("Energy system cost: items missing: ", paste(setdiff(items, have), collapse = ", "))
    }
    out[, , have] <- a[r, y, have, drop = FALSE]
  }
  out
}

# x as a matrix [regs, yrs] (one item, or an object without items).
escM <- function(x, regs, yrs, item = NULL) {
  matrix(escA(x, regs, yrs, item), length(regs), length(yrs), dimnames = list(regs, yrs))
}

# Capital recovery factor, continuous form used in OPEN-PROM: r*e^(rL) / (e^(rL) - 1).
escCRF <- function(r, L) {
  r * exp(r * L) / (exp(r * L) - 1)
}

# Sum an array over its items -> matrix [region, year]. NA and Inf count as 0.
escSum <- function(a) {
  a[!is.finite(a)] <- 0
  if (length(dim(a)) == 3) apply(a, c(1, 2), sum) else a
}

# DSBS code -> report sector name. Industry subsector names come from the GDX set text.
escSectorMap <- function(path) {
  txt <- rgdx.set(path, "DSBS", te = TRUE)                  # column 1: code, column .te: text
  indse <- readGDX(path, "INDSE")
  map <- data.frame(DSBS = txt[[1]], sector = NA_character_, stringsAsFactors = FALSE)
  isInd <- map$DSBS %in% indse
  label <- gsub("\\.", "", txt$.te[isInd])                  # no dots: they would split the name
  label[is.na(label) | label == ""] <- map$DSBS[isInd][is.na(label) | label == ""]
  map$sector[isInd] <- paste0("Industry|", label)
  map$sector[map$DSBS %in% c("SE", "ICT")] <- "Commercial"
  map$sector[map$DSBS == "HOU"] <- "Residential"
  map$sector[map$DSBS %in% c("PCH", "NEN")] <- "Non-Energy Use"
  map$sector[map$DSBS == "PC"] <- "Transportation|Cars"
  map$sector[map$DSBS %in% c("PT", "PA", "PB", "PN", "GU", "GT", "GN")] <- "Transportation|Other Modes"
  map$sector[map$DSBS %in% c("BAV", "BMAR")] <- "Bunkers"
  map$sector[map$DSBS == "AG"] <- "Agriculture"
  map$sector[map$DSBS == "DAC"] <- "Carbon Management|DAC"
  map$sector[map$DSBS == "EW"] <- "Carbon Management|EW"
  map[!is.na(map$sector), ]
}

# Array [region, year, "DSBS.xxx" or "DSBS"] -> named list of matrices, one per report sector:
# "Energy System Cost|<sector>|<type>". DSBS codes of the same sector are added up.
escBySector <- function(a, map, type) {
  code <- sub("\\..*$", "", dimnames(a)[[3]])
  sector <- map$sector[match(code, map$DSBS)]
  keep <- which(!is.na(sector))
  if (length(keep) == 0) return(list())
  groups <- split(keep, sector[keep])
  rows <- lapply(groups, function(k) escSum(a[, , k, drop = FALSE]))
  names(rows) <- paste0("Energy System Cost|", names(groups), "|", type)
  rows
}

# Named list of matrices [region, year] -> magpie object with one item per list element.
escRows <- function(rows) {
  first <- rows[[1]]
  arr <- array(unlist(rows), c(nrow(first), ncol(first), length(rows)),
    dimnames = list(region = rownames(first), year = colnames(first), variable = names(rows))
  )
  as.magpie(arr, spatial = 1, temporal = 2)
}

# RUN THIS IN THE CONSOLE:
# devtools::load_all("/Users/kubachri/Desktop/postprom")
# escCRF(0.10, 25)
# map <- escSectorMap(path)
# map

# --- supply sectors: capex + opex only (their fuel is paid through end-use prices) ---------------

escPower <- function(inp, regs, yrs) {
  p <- inp$power
  te <- getItems(p$V04CapElecNominal, 3)                                       # PGALL
  A <- function(x) escA(x, regs, yrs, te)
  capFom <- A(p$V04CapexRESRate) * A(p$V04CapexFixCostPG) * A(p$V04CapElecNominal)  # US$/kW/yr x GW = MUS$
  fom    <- A(p$V04CapexRESRate) * A(p$i04FixOandMCost) * A(p$V04CapElecNominal)    # MUS$
  vom    <- A(p$i04VarCost) * A(p$VmProdElec)                                         # US$/MWh x TWh = MUS$
  list(
    "Energy System Cost|Power|Capital"      = escSum(capFom) - escSum(fom),
    "Energy System Cost|Power|Fixed O&M"    = escSum(fom),
    "Energy System Cost|Power|Variable O&M" = escSum(vom)
  )
}

escHeat <- function(inp, regs, yrs) {
  h <- inp$heat
  s <- inp$shared
  te <- getItems(h$VmCapSte, 3)                                                # TSTEAM
  A <- function(x) escA(x, regs, yrs, te)
  r   <- as.vector(escM(s$imDisc, regs, yrs, "STEAMP"))                        # discount rate
  cgi <- if (is.null(s$imCGI)) 1 else as.vector(escM(s$imCGI, regs, yrs))      # capital goods index
  gw  <- as.vector(escM(s$smGwToTwhPerYear, regs, yrs))                        # TWh per GW-year
  mt  <- as.numeric(s$smTWhToMtoe)[1]                                          # Mtoe per TWh
  tdhp <- A(NULL)                                                              # 1 for district heating
  tdhp[] <- rep(as.numeric(te %in% h$TDHP), each = length(regs) * length(yrs))
  D <- (A(h$i09PowToHeatRatio) + tdhp) * A(h$i09AvailRateSteProd) * gw * mt   # toe steam per kW per yr
  cap <- escCRF(r, A(h$i09ProdLftSte)) * A(h$i09CostInvCostSteProd) * cgi / D * A(h$VmCapSte)  # MUS$
  fom <- A(h$i09CostFixOMSteProd) / D * A(h$VmCapSte)                          # MUS$ (no CRF on FOM)
  vom <- A(h$i09CostVOMSteProd) * A(h$VmProdSte)                               # US$/toe x Mtoe = MUS$
  list(
    "Energy System Cost|Heat|Capital"      = escSum(cap),
    "Energy System Cost|Heat|Fixed O&M"    = escSum(fom),
    "Energy System Cost|Heat|Variable O&M" = escSum(vom)
  )
}

escHydrogen <- function(inp, regs, yrs) {
  hy <- inp$hydrogen
  p <- inp$power
  s <- inp$shared
  te <- getItems(hy$VmCapH2, 3)                                                # H2TECH
  A <- function(x) escA(x, regs, yrs, te)
  M <- function(x, item) escM(x, regs, yrs, item)
  r  <- as.vector(escM(s$imDisc, regs, yrs, "H2P"))
  mt <- as.numeric(s$smTWhToMtoe)[1]
  cap <- escCRF(r, A(hy$i05ProdLftH2)) * A(hy$i05CostCapH2Prod) * A(hy$VmCapH2)  # US$/kW x GW = MUS$
  fom <- A(hy$i05CostFOMH2Prod) * A(hy$VmCapH2)                                  # MUS$
  vom <- A(hy$i05CostVOMH2Prod) * A(hy$VmProdH2)                                 # US$/toe x Mtoe = MUS$
  # dedicated solar PV plants (wes) and offshore wind plants (wew)
  capRes <- (M(p$V04CapexFixCostPG, "PGSOL") - M(p$i04FixOandMCost, "PGSOL")) * M(hy$VmCapH2, "wes") +
    (M(p$V04CapexFixCostPG, "PGAWNO") - M(p$i04FixOandMCost, "PGAWNO")) * M(hy$VmCapH2, "wew")
  fomRes <- M(p$i04FixOandMCost, "PGSOL") * M(hy$VmCapH2, "wes") +
    M(p$i04FixOandMCost, "PGAWNO") * M(hy$VmCapH2, "wew")
  vomRes <- M(p$i04VarCost, "PGSOL") / mt * M(hy$VmProdH2, "wes") +
    M(p$i04VarCost, "PGAWNO") / mt * M(hy$VmProdH2, "wew")
  list(
    "Energy System Cost|Hydrogen|Capital"      = escSum(cap) + escSum(capRes),
    "Energy System Cost|Hydrogen|Fixed O&M"    = escSum(fom) + escSum(fomRes),
    "Energy System Cost|Hydrogen|Variable O&M" = escSum(vom) + escSum(vomRes)
  )
}

# --- demand sectors -------------------------------------------------------------------------------

escDemand <- function(inp, regs, yrs, map, indse) {
  d <- inp$demand
  all <- getItems(d$V02EquipCapTechSubsec, 3)                                  # "DSBS.ITECH"
  te <- all[sub("\\..*$", "", all) %in% c(indse, "SE", "HOU", "PCH", "NEN")]
  A <- function(x) escA(x, regs, yrs, te)
  eq <- A(d$V02EquipCapTechSubsec)                                             # Mtoe/yr capacity
  capFom <- A(d$V02CapCostTech) * A(d$imUsfEneConvSubTech) * eq * 1000         # kUS$/toe x Mtoe x 1000 = MUS$
  fom    <- A(d$imFixOMCostTech) * eq                                          # US$/toe x Mtoe = MUS$
  vom    <- A(d$imVarCostTech) * eq * A(d$i02util)                             # MUS$
  c(
    escBySector(capFom - fom, map, "Capital"),
    escBySector(fom, map, "Fixed O&M"),
    escBySector(vom, map, "Variable O&M")
  )
}

escCars <- function(inp, regs, yrs) {
  d <- inp$demand
  tt <- getItems(d$V01StockPcYearlyTech, 3)                                    # TTECH
  stock  <- escA(d$V01StockPcYearlyTech, regs, yrs, tt)                        # million vehicles
  capCar <- escA(d$V01CapCostAnnualized, regs, yrs, paste0("PC.", tt))         # kUS$/vehicle/yr
  fomCar <- escA(d$imFixOMCostTech, regs, yrs, paste0("PC.", tt))              # kUS$/vehicle/yr
  list(
    "Energy System Cost|Transportation|Cars|Capital"   = escSum(capCar * stock * 1000),  # = MUS$
    "Energy System Cost|Transportation|Cars|Fixed O&M" = escSum(fomCar * stock * 1000)
  )
}

# --- CO2 transport & storage (power, hydrogen, industry CCS) --------------------------------------

escStorage <- function(inp, regs, yrs, map) {
  co2 <- inp$co2
  it <- getItems(co2$V06CO2CaptureCCS, 3)                                      # "SBS.EF"
  cost <- as.vector(escM(co2$VmCstCO2SeqCsts, regs, yrs))                      # US$/tCO2 incl. storage tax
  if (!is.null(co2$i06CCSAvailabilityCostAdder)) {
    cost <- cost - as.vector(escM(co2$i06CCSAvailabilityCostAdder, regs, yrs)) # remove the storage tax
  }
  stor <- escA(co2$V06CO2CaptureCCS, regs, yrs, it) * cost                     # MtCO2 x US$/t = MUS$
  sbs <- sub("\\..*$", "", it)
  industry <- map$DSBS[grepl("^Industry\\|", map$sector)]
  c(
    list(
      "Energy System Cost|Power|CO2 Storage"    = escSum(stor[, , sbs == "PG", drop = FALSE]),
      "Energy System Cost|Hydrogen|CO2 Storage" = escSum(stor[, , sbs == "H2P", drop = FALSE])
    ),
    escBySector(stor[, , sbs %in% industry, drop = FALSE], map, "CO2 Storage")
  )
}

# --- CDR (direct air capture, enhanced weathering) ------------------------------------------------

escCDR <- function(inp, regs, yrs) {
  cd <- inp$cdr
  te <- getItems(cd$V06CapCDR, 3)                                              # CDRTECH
  A <- function(x) escA(x, regs, yrs, te)
  capa <- A(cd$V06CapCDR)                                                      # tCO2/yr
  cap <- A(cd$V06GrossCapCDR) * capa * 1e-6                                    # US$/t x t / 1e6 = MUS$
  fom <- A(cd$V06FixOandMCDR) * capa * 1e-6
  vom <- A(cd$V06VarCostCDR) * capa * 1e-6                                     # incl. DAC storage
  dac <- te %in% c("HTDAC", "H2DAC", "LTDAC")
  ew  <- te == "TEW"
  list(
    "Energy System Cost|Carbon Management|DAC|Capital"      = escSum(cap[, , dac, drop = FALSE]),
    "Energy System Cost|Carbon Management|DAC|Fixed O&M"    = escSum(fom[, , dac, drop = FALSE]),
    "Energy System Cost|Carbon Management|DAC|Variable O&M" = escSum(vom[, , dac, drop = FALSE]),
    "Energy System Cost|Carbon Management|EW|Capital"       = escSum(cap[, , ew, drop = FALSE]),
    "Energy System Cost|Carbon Management|EW|Fixed O&M"     = escSum(fom[, , ew, drop = FALSE]),
    "Energy System Cost|Carbon Management|EW|Variable O&M"  = escSum(vom[, , ew, drop = FALSE])
  )
}

# --- fuel at end use ------------------------------------------------------------------------------

# OPEN-PROM's own price rules (core/input.gms 163-180, core/preloop.gms 66, agriculture/input.gms 40),
# applied to the ex-tax prices. a: array [region, year, "SBS.EF"] in US$/toe.
# needed: every "DSBS.EFS" item of VmFinalEnergy. Returns an array with all items of a and needed.
escPriceRules <- function(a, needed) {
  have <- dimnames(a)[[3]]
  items <- union(have, needed)
  b <- array(NA_real_, c(dim(a)[1], dim(a)[2], length(items)),
    dimnames = list(dimnames(a)[[1]], dimnames(a)[[2]], items)
  )
  b[, , have] <- a
  copy <- function(target, source, factor = 1) {
    if (source %in% items) b[, , target] <<- factor * b[, , source]
  }
  transe <- c("PC", "PT", "PA", "PB", "PN", "GU", "GT", "GN", "BAV", "BMAR")
  for (t in needed) {
    s <- sub("\\..*$", "", t)
    e <- sub("^[^.]*\\.", "", t)
    if (s %in% c("DAC", "EW")) copy(t, paste0("OI.", e))                                            # 163
    if (s %in% c("SE", "HOU") && e %in% c("GSL", "BGSL", "LGN", "OLQ", "RFO")) copy(t, paste0("OI.", e))  # 164-168
    if (s == "SE" && e %in% c("GDO", "BGDO")) copy(t, paste0("OI.", e))                              # 169-170
    if (s == "AG" && e == "BGAS") copy(t, "AG.NGS")                                                  # 171
    if (s == "AG" && e == "BGDO") copy(t, "AG.GDO")                                                  # 172
    if (s %in% transe && e == "RFO") copy(t, "BMAR.RFO")                                             # 174
    if (s %in% transe && e == "OGS") copy(t, paste0(s, ".NGS"))                                      # 175
    if (s %in% transe && e == "OLQ") copy(t, paste0(s, ".GDO"))                                      # 176
    if (e == "H2F") copy(t, "OI.ELC", factor = 1.5)                                                  # 180
    if (e == "STE") copy(t, "OI.ELC")                                                                # preloop 66
    if (t == "AG.STE") copy(t, "AG.GDO")                                                             # agriculture 40
  }
  b
}

# Ex-tax prices for all years. Up to the last history year: ex-tax data. After it:
#   p(t) = p(t-1) x I(t), with the model's own price growth without carbon:
#   I(t) = (P(t) - dVmPriceCarbon(t)) / P(t-1), P = VmPriceFuelSubsecCarVal   (non-transport)
#   I(t) = VmPriceFinal(t) / VmPriceFinal(t-1)                                (transport, bunkers)
# Prices the model sets itself replace the ex-tax price in all years (x1000: kUS$/toe -> US$/toe):
#   BMSWAS = model price - biomass tax; CRO, MET, ETH = model price; SOL, GEO = 0.
# ICT electricity = services electricity. No model price growth: held flat.
escProjectPrices <- function(priceHist, fuel, regs, yrs) {
  items <- dimnames(priceHist)[[3]]
  sec <- sub("\\..*$", "", items)
  ef <- sub("^[^.]*\\.", "", items)
  isTr <- sec %in% c("PC", "PT", "PA", "PB", "PN", "GU", "GT", "GN", "BAV", "BMAR")
  y0 <- as.integer(sub("y", "", fuel$lastDataYear))
  yNum <- as.integer(sub("y", "", yrs))
  hist <- yrs[yNum <= y0]
  proj <- yrs[yNum > y0]
  pick <- function(x, t) escA(x, regs, t, items, strict = FALSE)
  out <- array(NA_real_, c(length(regs), length(yrs), length(items)), dimnames = list(regs, yrs, items))
  out[, hist, ] <- priceHist[regs, hist, , drop = FALSE]
  # 1. grow with the model's price growth, carbon excluded
  for (t in proj) {
    tm1 <- paste0("y", as.integer(sub("y", "", t)) - 1)
    Pt <- pick(fuel$VmPriceFuelSubsecCarVal, t)
    Pm <- pick(fuel$VmPriceFuelSubsecCarVal, tm1)
    dPC <- pick(fuel$VmPriceCarbon, t) - pick(fuel$VmPriceCarbon, tm1)
    dPC[is.na(dPC)] <- 0
    idx <- (Pt - dPC) / Pm
    if (any(isTr)) {
      Ft <- pick(fuel$VmPriceFinal, t)
      Fm <- pick(fuel$VmPriceFinal, tm1)
      idx[, , isTr] <- Ft[, , isTr] / Fm[, , isTr]
    }
    idx[!is.finite(idx)] <- 1                                                  # no model price: flat
    out[, t, ] <- out[, tm1, ] * idx[, 1, ]
  }
  # 2. prices the model sets itself, all years
  bms <- ef == "BMSWAS"
  fixed <- ef %in% c("CRO", "MET", "ETH")
  for (t in yrs) {
    Pt <- pick(fuel$VmPriceFuelSubsecCarVal, t)
    adder <- if (is.null(fuel$i08BmswasPriceAdder)) 0 else as.vector(escM(fuel$i08BmswasPriceAdder, regs, t))
    val <- out[, t, , drop = FALSE]
    val[, , bms] <- ifelse(is.finite(Pt[, , bms]), (Pt[, , bms] - adder) * 1000, val[, , bms])
    val[, , fixed] <- ifelse(is.finite(Pt[, , fixed]), Pt[, , fixed] * 1000, val[, , fixed])
    out[, t, ] <- val[, 1, ]
  }
  out[, , ef %in% c("SOL", "GEO")] <- 0                                        # core/preloop.gms 59-60
  # 3. ICT: services electricity price
  if (all(c("ICT.ELC", "SE.ELC") %in% items)) out[, , "ICT.ELC"] <- out[, , "SE.ELC"]
  out
}

escFuel <- function(inp, regs, yrs, map) {
  f <- inp$fuel
  items <- getItems(f$VmFinalEnergy, 3)                                        # "DSBS.EFS"
  fe <- escA(f$VmFinalEnergy, regs, yrs, items)                               # Mtoe
  hist <- as.array(f$priceExTax)[regs, , , drop = FALSE]                       # US$/toe, 2010-2025
  price <- escProjectPrices(escPriceRules(hist, items), f, regs, yrs)          # US$/toe, all years
  escBySector(fe * price[, , items, drop = FALSE], map, "Fuel")                # Mtoe x US$/toe = MUS$
}

# --- put it together ------------------------------------------------------------------------------

# Returns a magpie object: every energy system cost row, the sector totals and the overall total
# "Energy System Cost", in million US$2015/yr.
escCompute <- function(inp, path, regions, years) {
  map <- escSectorMap(path)
  indse <- readGDX(path, "INDSE")
  rows <- c(
    escPower(inp, regions, years),
    escHeat(inp, regions, years),
    escHydrogen(inp, regions, years),
    escDemand(inp, regions, years, map, indse),
    escCars(inp, regions, years),
    escStorage(inp, regions, years, map),
    escCDR(inp, regions, years),
    escFuel(inp, regions, years, map)
  )
  dup <- names(rows)[duplicated(names(rows))]
  if (length(dup) > 0) stop("Energy system cost: duplicate row names: ", paste(dup, collapse = ", "))
  total <- helperAggregateLevel(escRows(rows), level = 1, recursive = TRUE)
  add_dimension(total, dim = 3.2, add = "unit", nm = "million US$2015/yr")
}