#' Soft-link OPEN-PROM with MAgPIE via coupling-channel (mif)
#'
#' Produces a REMIND-style `.mif` that MAgPIE can consume through its coupling
#' interface (`c56_pollutant_prices = "coupling"`, `c60_2ndgen_biodem = "coupling"`),
#' and — in the reverse direction — reads MAgPIE's `report.mif` to produce
#' two files for OPEN-PROM's round-2 run:
#'   * `iPrices_magpie.csv`    : BMSWAS prices, GAMS-consumed by 08_Prices
#'                               (`.FX`-es BMSWAS prices in round-2 preloop).
#'   * `iEmissions_magpie.mif` : AFOLU emissions (200 vars), IAMC mif format.
#'                               Reporting / future-extension only — current
#'                               GAMS code does not `$include` this file.
#'
#' Two top-level functions:
#'   * `couplePromToMagpie()`  : forward  (OPEN-PROM gdx  -> mif)
#'   * `coupleMagpieToProm()`  : backward (MAgPIE mif     -> csv + mif)
#'
#' Scope:
#'   * Forward bioenergy demand: weighted blend of three OPEN-PROM 2nd-gen
#'     bioenergy carriers from `V03ProdPrimary` (Mtoe/yr):
#'         0.4 × BMSWAS   ("Biomass and Waste"  — raw 2G feedstock)
#'       + 0.6 × BGSL     ("Biogasoline"        — processed 2G liquid biofuel)
#'       + 0.6 × BKRS     ("Biokerosene"        — processed 2G liquid biofuel)
#'     The weights approximate each carrier's effective 2G-biomass content
#'     entering the MAgPIE coupling channel (raw feedstock contributes a
#'     smaller share since not all of it routes through 2G; processed
#'     biofuels contribute more because their feedstock is essentially all
#'     2G biomass). The blend is summed 39 resCy -> 12 h12, unit-converted
#'     Mtoe/yr -> EJ/yr (× 0.041868), written as the IAMC variable
#'     `Primary Energy Production|Biomass|Energy Crops (EJ/yr)`.
#'   * Forward CO2 price: `VmCarVal[,"TRADE",]` (US$2015/t CO2),
#'     bioenergy-weighted-averaged 39 -> 12 h12, deflated US$2015->US$2017.
#'     Non-CO2 prices (N2O, CH4) are derived from CO2 via GWP100 (AR6).
#'   * Backward prices: read MAgPIE `Prices|Bioenergy (US$2017/GJ)`,
#'     unit-convert to k$2015/toe, broadcast 12 h12 -> 39 resCy (intensive
#'     variable, broadcast within EUR is correct: a regional commodity price
#'     applies uniformly to all 28 EU members), then broadcast to 34 SBS
#'     subsectors. Output: `iPrices_magpie.csv`.
#'   * Backward emissions: read 200 AFOLU emission variables from MAgPIE
#'     `report.mif` (curated list + per-leaf weight assignments in
#'     `inst/extdata/magpie-afolu-emission-variables.csv`), disaggregate
#'     12 h12 -> 39 resCy as an extensive variable:
#'       - 11 non-EU regions: 1:1 (each is its own h12, no split needed).
#'       - 28 EU members under EUR: two-pass split over the IAMC `|+|`
#'         tree (parsed from variable names). Leaves get weighted splits;
#'         parents are derived as Σ direct children, so country-level
#'         sum-to-parent identities hold automatically.
#'     Two leaf-weight paradigms:
#'       (a) MAgPIE cell-level land area (135/136 leaves) — aggregate
#'           `cell.{land,land_split,peatland}_0.5.mz` to country via the
#'           run's clustermap, use as proportional weight. Self-contained
#'           (no external data), time-varying, sign-safe.
#'       (b) ClimateWatch year-2010 LULUCF CO2 signed weights (1 leaf:
#'           `Land|+|Indirect`) — needed because per-Mha forest sink
#'           density varies ~8x across EU climate zones, making uniform-
#'           area weighting biased for forest carbon flux.
#'     Output: `iEmissions_magpie.mif` (IAMC standard, `|+|` markers
#'     preserved for IAMC-tooling compatibility, `|++|` variants excluded
#'     to avoid double-count).
#'
#' @importFrom gdx readGDX
#' @importFrom magclass as.magpie read.report write.report getRegions getYears
#'   getNames dimSums collapseDim setNames mbind new.magpie add_dimension
#' @importFrom madrat toolGetMapping
NULL

# ------------------------------------------------------------------- constants

# AR6 GWP100 (used when deriving Price|N2O and Price|CH4 from Price|Carbon).
.GWP_N2O_AR6 <- 273
.GWP_CH4_AR6 <- 27

# Default biomass lower heating value (GJ/tDM). Not used here (since backward
# variable is already per GJ) but kept for reference.
.BIO_LHV_GJ_PER_TDM <- 18

# Unit-conversion shortcut: toe <-> GJ.
.GJ_PER_TOE <- 41.868        # 1 toe = 41.868 GJ
.MTOE_PER_EJ <- 1 / 0.041868  # (for reference) 1 Mtoe = 0.041868 EJ

# EU-28 ISO3 codes that OPEN-PROM represents individually but which all
# aggregate to MAgPIE's EUR region. Kept hard-coded (rather than queried from
# h12.csv) because the set is stable and this keeps couplePromWithMagpie.R
# self-contained for testing.
.EU28 <- c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN",
           "FRA","GBR","GRC","HRV","HUN","IRL","ITA","LTU","LUX","LVA",
           "MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")

# All 12 h12 regions, in stable order.
.H12 <- c("CAZ","CHA","EUR","IND","JPN","LAM","MEA","NEU","OAS","REF","SSA","USA")


# ========================================================== forward: prom -> magpie

#' @param gdxPath       Absolute path to OPEN-PROM `blabla.gdx`.
#' @param outMifPath    Absolute path to write the REMIND-style mif.
#' @param scenario      Scenario label to embed in the mif (must match MAgPIE side).
#' @param nonCo2Mode    "gwp" (default) or "zero": how to derive N2O/CH4 prices.
#' @param deflator15to17 US$2015 -> US$2017 deflator. Default 1.04.
#' @return  `outMifPath` (invisibly).
#' @export
couplePromToMagpie <- function(gdxPath,
                               outMifPath,
                               scenario       = "SSP2-PkBudg650",
                               nonCo2Mode     = c("gwp", "zero"),
                               deflator15to17 = 1.04) {

  nonCo2Mode <- match.arg(nonCo2Mode)

  # ---- 1. read gdx
  co2   <- gdx::readGDX(gdxPath, "VmCarVal",        field = "l", restore_zeros = FALSE)
  bio   <- gdx::readGDX(gdxPath, "V03ProdPrimary", field = "l", restore_zeros = FALSE)

  # CO2 price: read TRADE-context exogenous CO2 value (US$2015/t CO2).
  co2 <- co2[, , "TRADE"]                    # 39 x 91 x 1
  # Bioenergy demand: weighted blend of 3 V03ProdPrimary fuel slices in Mtoe/yr.
  # See header docstring (Scope > Forward bioenergy demand) for the rationale
  # of the 0.4 / 0.6 / 0.6 weights and what BMSWAS / BGSL / BKRS represent.
  bio <- bio[, , "BMSWAS"] * 0.4 +
         bio[, , "BGSL"]   * 0.6 +
         bio[, , "BKRS"]   * 0.6              # 39 x 91 x 1

  # Collapse the 3rd dim naming.
  co2 <- magclass::collapseDim(co2, dim = 3)
  bio <- magclass::collapseDim(bio, dim = 3)

  # ---- 2. aggregate prom countries -> h12
  co2H12 <- .aggregateToH12Price(co2, bio)   # weight by bioenergy
  bioH12 <- .aggregateToH12Sum(bio)

  # ---- 3. unit conversion
  # CO2 price: US$2015/t CO2 -> US$2017/t CO2
  co2H12 <- co2H12 * deflator15to17
  # Bioenergy: Mtoe/yr -> EJ/yr (1 Mtoe = 0.041868 EJ)
  bioH12 <- bioH12 * 0.041868

  # ---- 4. derive non-CO2 prices
  if (nonCo2Mode == "gwp") {
    n2oH12 <- co2H12 * .GWP_N2O_AR6
    ch4H12 <- co2H12 * .GWP_CH4_AR6
  } else {                                          # "zero"
    n2oH12 <- co2H12 * 0
    ch4H12 <- co2H12 * 0
  }

  # ---- 5. stamp variable labels (REMIND-style "Variable (Unit)")
  co2H12 <- magclass::setNames(co2H12, "Price|Carbon (US$2017/t CO2)")
  n2oH12 <- magclass::setNames(n2oH12, "Price|N2O (US$2017/t N2O)")
  ch4H12 <- magclass::setNames(ch4H12, "Price|CH4 (US$2017/t CH4)")
  bioH12 <- magclass::setNames(
    bioH12,
    "Primary Energy Production|Biomass|Energy Crops (EJ/yr)"
  )

  # All four variables share same region/time structure -> mbind on dim 3.
  out <- magclass::mbind(co2H12, n2oH12, ch4H12, bioH12)

  # ---- 6. add Model + Scenario dims (required for write.report)
  out <- magclass::add_dimension(out, dim = 3.1, add = "model",    nm = "OPENPROM")
  out <- magclass::add_dimension(out, dim = 3.1, add = "scenario", nm = scenario)

  # ---- 7. write mif. magpie's getReportData calls time_interpolate to
  # y1990..y2150 internally, so we simply write whatever years we have.
  dir.create(dirname(outMifPath), showWarnings = FALSE, recursive = TRUE)
  magclass::write.report(out, file = outMifPath, ndigit = 4)

  message(sprintf("[couplePromToMagpie] wrote mif: %s", outMifPath))
  invisible(outMifPath)
}

# ---------------- helpers: aggregation from OPEN-PROM (39 regions) to h12 ----

# Sum bioenergy feedstock across 28 EU members -> EUR; other 11 regions 1:1.
.aggregateToH12Sum <- function(m) {
  regs <- magclass::getRegions(m)
  eu   <- intersect(.EU28, regs)
  nonEuH12 <- setdiff(regs, eu)

  # EU-28 sum -> EUR
  eur <- m[eu, , ]
  eur <- magclass::dimSums(eur, dim = 1)
  magclass::getItems(eur, dim = 1) <- "EUR"

  # The remaining regions must coincide with h12 codes (minus EUR).
  stopifnot(all(nonEuH12 %in% .H12))
  out <- magclass::mbind(m[nonEuH12, , ], eur)

  # Re-order canonically.
  out[.H12, , ]
}

# EUR price = bioenergy-weighted average over EU-28; others 1:1.
# If total bio across EU-28 in a given year is zero, fall back to simple mean.
.aggregateToH12Price <- function(price, bio) {
  regs <- magclass::getRegions(price)
  eu   <- intersect(.EU28, regs)
  nonEuH12 <- setdiff(regs, eu)

  # Weighted average by bioenergy across EU-28.
  num <- magclass::dimSums(price[eu, , ] * bio[eu, , ], dim = 1)
  den <- magclass::dimSums(bio[eu, , ],                 dim = 1)

  # Fallback simple average where denominator is zero.
  eur <- num
  meanPrice <- magclass::dimSums(price[eu, , ], dim = 1) / length(eu)
  eur[den == 0] <- meanPrice[den == 0]
  eur[den != 0] <- num[den != 0] / den[den != 0]
  magclass::getItems(eur, dim = 1) <- "EUR"

  stopifnot(all(nonEuH12 %in% .H12))
  out <- magclass::mbind(price[nonEuH12, , ], eur)
  out[.H12, , ]
}


# ========================================================== backward: magpie -> prom

#' @param reportMifPath Absolute path to MAgPIE output `report.mif`.
#' @param outCsvPath    Absolute path to write `iPrices_magpie.csv` (GAMS-table
#'                      CSV format; consumed by OPEN-PROM round-2 GAMS).
#' @param outEmissionsMifPath Absolute path to write `iEmissions_magpie.mif`
#'                      (IAMC standard mif format with `|+|` markers preserved).
#'                      OPEN-PROM's current GAMS code does not `$include` this
#'                      file; it is produced for downstream reporting and
#'                      future use.
#' @param gdxPath       Absolute path to an OPEN-PROM `blabla.gdx`, used only
#'                      to read the runtime `SBS` set (so we can broadcast the
#'                      price across all subsectors).
#' @param scenario      Scenario label embedded in the output mif (Scenario
#'                      column). Defaults to "OPEN-PROM-MAgPIE".
#' @param biomassVariable MAgPIE variable name to extract. Defaults to the
#'                      one present in current report.mif.
#' @param deflator17to15 US$2017 -> US$2015 deflator. Default 0.96.
#' @return A named list of the two written paths (invisibly).
#' @export
coupleMagpieToProm <- function(reportMifPath,
                               outCsvPath,
                               outEmissionsMifPath,
                               gdxPath,
                               scenario        = "OPEN-PROM-MAgPIE",
                               biomassVariable = "Prices|Bioenergy (US$2017/GJ)",
                               deflator17to15  = 0.96) {

  # ---- 1. read mif and slice
  rep <- magclass::read.report(reportMifPath, as.list = FALSE)
  # Strip model/scenario to leave just region x year x variable.
  rep <- magclass::collapseNames(rep)

  # Variable names keep their native IAMC form, including `|+|` and `|++|`
  # markers. emiVars below is curated to the same form so intersect() matches
  # exactly. Preserving + makes the output mif IAMC-standard compliant.

  if (!(biomassVariable %in% magclass::getNames(rep))) {
    stop(sprintf(
      "Variable %s not found in %s. Available Price* variables:\n  %s",
      biomassVariable, reportMifPath,
      paste(grep("[Pp]rice", magclass::getNames(rep), value = TRUE), collapse = "\n  ")
    ))
  }
  price <- rep[, , biomassVariable]             # h12 + World, years x 1

  # ============================================================================
  # AFOLU emission variables: extraction & cleanup
  # ============================================================================
  #
  # MAgPIE report.mif uses IAMC naming: `|`-separated paths with two markers:
  #   `|+|`   sum-to-parent partition (parent = Σ + siblings, mutually exclusive)
  #   `|++|`  orthogonal alternate slicing (parallel to + along the same parent)
  # 11 gases × 4 parallel layer-3 subtrees per gas:
  #   |<gas>|Land|*               endogenous (MAgPIE 76_emissions module)
  #   |<gas>|AFOLU|Land|Fires|*   exogenous GFED wildfire data
  #   |<gas>|AFOLU|Agriculture    exogenous GAINS/CEDS air-pollutant inventory
  #   |CO2|Land Carbon Sink|*     post-process Grassi/LPJmL alias
  #
  # ----------------------------------------------------------------------------
  # Modifications applied to the native data
  # ----------------------------------------------------------------------------
  # (1) Drop `|GWP100AR6|*` and `|*|Cumulative|*` — second-order derived series
  #     OPEN-PROM can compute itself.
  # (2) Drop the 8 `|++|` variants — ++ is an alternate slicing (carbon-pool
  #     location) parallel to + (by use). Keeping both is redundant; we keep +.
  # (3) Drop 1 cross-tree alias `Emissions|CO2|Land Carbon Sink|Grassi|Managed
  #     Land|Managed Forest` — identical series-by-series to `|Land|+|Indirect`.
  # (4) Preserve `|+|` markers (IAMC standard form) — downstream tooling
  #     (piamInterfaces / mip / IIASA Scenario Explorer) relies on them.
  #     emiVars in the curated csv uses the same `|+|`-marked form.
  #
  # The 10 structural single-child parents `|Land|+|Biomass Burning` and
  # `|Land|+|Peatland` for CH4/N2O/NH3/NO2/NO3- (each has exactly one curated
  # |+| child) are kept on purpose so that `|<gas>|Land`'s |+| sum-to-parent
  # identity holds at country level after disaggregation. Without them the
  # disaggregator's "parent = Σ direct children" pass would underestimate
  # Land by ~0.1-0.4 Mt for these gases.
  #
  # ----------------------------------------------------------------------------
  # Extraction result: 200 variables
  # ----------------------------------------------------------------------------
  #   BC  9    CH4 16    CO  9    CO2 58    N2O 22    NH3 22
  #   NO2 22   NO3- 15   OC  9    SO2 9     VOC 9            total 200
  #
  # Ordering: DFS (parent followed by its children). Names IAMC-standard with
  # `|+|` preserved. Units `Mt <pollutant>/yr` kept as-is. NOx reported as NO2.
  # `|<gas>|AFOLU|Agriculture` is all-zero in this scenario for 6 gases (BC/CO/
  # CO2/OC/SO2/VOC); kept for structural completeness.
  #
  # ----------------------------------------------------------------------------
  # Tree diagrams (per gas)
  # ----------------------------------------------------------------------------
  #
  # === BC, CO, OC, SO2, VOC (9 vars each) ===
  # Emissions|<gas>
  # ├── AFOLU|Agriculture                                           (zero in this scenario)
  # └── AFOLU|Land|Fires
  #     ├── Forest Burning
  #     │   ├── Boreal Forest
  #     │   ├── Temperate Forest
  #     │   └── Tropical Forest
  #     ├── Grassland Burning
  #     └── Peat Burning
  # Emissions|<gas>|Land|Biomass Burning|Burning of Crop Residues   (orphan leaf;
  #   |Land| and |Land|Biomass Burning| not present in mif for these 5 gases)
  #
  # === CH4 (16 vars) ===
  # Emissions|CH4
  # ├── AFOLU|Land|Fires
  # │   ├── Forest Burning
  # │   │   ├── Boreal Forest
  # │   │   ├── Temperate Forest
  # │   │   └── Tropical Forest
  # │   ├── Grassland Burning
  # │   └── Peat Burning
  # └── Land
  #     ├── Agriculture
  #     │   ├── Animal waste management
  #     │   ├── Enteric fermentation
  #     │   └── Rice
  #     ├── Biomass Burning
  #     │   └── Burning of Crop Residues
  #     └── Peatland
  #         └── Managed
  #
  # === N2O, NH3, NO2 (22 vars each) ===
  # Emissions|<gas>
  # ├── AFOLU|Land|Fires
  # │   ├── Forest Burning
  # │   │   ├── Boreal Forest
  # │   │   ├── Temperate Forest
  # │   │   └── Tropical Forest
  # │   ├── Grassland Burning
  # │   └── Peat Burning
  # └── Land
  #     ├── Agriculture
  #     │   ├── Agricultural Soils
  #     │   │   ├── Decay of Crop Residues
  #     │   │   ├── Inorganic Fertilizers
  #     │   │   │   ├── Cropland
  #     │   │   │   └── Pasture
  #     │   │   ├── Manure applied to Croplands
  #     │   │   ├── Pasture
  #     │   │   └── Soil Organic Matter Loss
  #     │   └── Animal Waste Management
  #     ├── Biomass Burning
  #     │   └── Burning of Crop Residues
  #     └── Peatland
  #         └── Managed
  #
  # === NO3- (15 vars, no |AFOLU| subtree) ===
  # Emissions|NO3-|Land
  # ├── Agriculture
  # │   ├── Agricultural Soils
  # │   │   ├── Decay of Crop Residues
  # │   │   ├── Inorganic Fertilizers
  # │   │   │   ├── Cropland
  # │   │   │   └── Pasture
  # │   │   ├── Manure applied to Croplands
  # │   │   ├── Pasture
  # │   │   └── Soil Organic Matter Loss
  # │   └── Animal Waste Management
  # ├── Biomass Burning
  # │   └── Burning of Crop Residues
  # └── Peatland
  #     └── Managed
  #
  # === CO2 (58 vars) ===
  # Emissions|CO2
  # ├── AFOLU|Agriculture                                           (zero in this scenario)
  # ├── AFOLU|Land|Fires
  # │   ├── Forest Burning
  # │   │   ├── Boreal Forest
  # │   │   ├── Temperate Forest
  # │   │   └── Tropical Forest
  # │   ├── Grassland Burning
  # │   └── Peat Burning
  # └── Land
  #     ├── Biomass Burning|Burning of Crop Residues   (orphan leaf, no |Biomass Burning| parent)
  #     ├── Indirect                                    (Grassi NGHGI managed-forest sink;
  #     │                                                negative over the historical calibration
  #     │                                                period, all-zero from 2030 onwards)
  #     └── Land-use Change
  #         ├── Deforestation
  #         │   ├── Cropland Tree Cover
  #         │   ├── Forestry plantations
  #         │   ├── Primary forests
  #         │   └── Secondary forests
  #         ├── Forest degradation
  #         │   ├── Primary forests
  #         │   └── Secondary forests
  #         ├── Other land conversion
  #         ├── Peatland
  #         │   ├── Negative
  #         │   └── Positive
  #         ├── Regrowth
  #         │   ├── CO2-price AR
  #         │   │   ├── Natural Forest
  #         │   │   └── Plantation
  #         │   ├── Cropland Tree Cover
  #         │   ├── NPI_NDC AR
  #         │   ├── Other Land
  #         │   ├── Secondary Forest
  #         │   └── Timber Plantations
  #         ├── Residual
  #         │   ├── Negative
  #         │   └── Positive
  #         ├── Soil
  #         │   ├── Cropland management
  #         │   │   ├── Emissions
  #         │   │   └── Withdrawals
  #         │   ├── Land Conversion
  #         │   │   ├── Emissions
  #         │   │   └── Withdrawals
  #         │   └── Soil Carbon Management
  #         │       ├── Emissions
  #         │       └── Withdrawals
  #         ├── Timber
  #         │   ├── Release from HWP
  #         │   │   ├── Buildings
  #         │   │   └── Industrial Roundwood
  #         │   └── Storage in HWP
  #         │       ├── Buildings
  #         │       └── Industrial Roundwood
  #         └── Wood Harvest
  #             ├── Other Land
  #             ├── Primary Forest
  #             ├── Secondary Forest
  #             └── Timber Plantations
  # ============================================================================
  # Single source of truth: variable list, weight_source per leaf, and
  # direct_children list per parent, all in one csv (manually maintained).
  # The hierarchy used at runtime is parsed from IAMC names — see
  # .parseIamcChildren() — not from the direct_children column (which is
  # documentation only).
  emiCsv <- read.csv(
    system.file("extdata", "magpie-afolu-emission-variables.csv",
                package = "postprom"),
    stringsAsFactors = FALSE
  )
  emiVars <- emiCsv$variable
  missingEmi <- setdiff(emiVars, magclass::getNames(rep))
  if (length(missingEmi) > 0) {
    warning(sprintf(
      "Missing %d AFOLU emission variables in mif, skipped: %s",
      length(missingEmi), paste(missingEmi, collapse = "; ")
    ))
    emiVars <- intersect(emiVars, magclass::getNames(rep))
    emiCsv <- emiCsv[emiCsv$variable %in% emiVars, ]
  }
  emi <- rep[, , emiVars]

  # Drop World row if present; keep only h12.
  h12Present <- intersect(.H12, magclass::getRegions(price))
  price <- price[h12Present, , ]
  emi   <- emi[h12Present, , ]

  # ---- 2. unit conversion: US$2017/GJ -> k$2015/toe (price only)
  price <- price * (deflator17to15 * .GJ_PER_TOE / 1000)

  # ---- 2b. interpolate to annual years (OPEN-PROM YTIME is annual 2010..2100;
  # MAgPIE report.mif has sparse years like 1995,2000,...,2050,2055,2060,2070,..)
  price <- magclass::time_interpolate(
    price,
    interpolated_year  = seq(2010, 2100, by = 1),
    extrapolation_type = "constant"
  )
  emi <- magclass::time_interpolate(
    emi,
    interpolated_year  = seq(2010, 2100, by = 1),
    extrapolation_type = "constant"
  )

  # ---- 3. broadcast h12 -> OPEN-PROM resCy (39 regions) via region mapping
  mapping <- madrat::toolGetMapping(
    name  = "regionmappingOPDEV5.csv",
    type  = "regional",
    where = "mrprom"
  )
  # `regionmappingOPDEV5.csv` columns: Full.Country.Name, ISO3.Code, Region.Code
  # For each resCy region we find its h12 membership; EU-28 members map to EUR,
  # the 11 non-EU resCy regions are themselves h12 codes.
  resCy   <- unique(mapping[["Region.Code"]])
  h12For <- ifelse(resCy %in% .EU28, "EUR", resCy)
  ok      <- h12For %in% h12Present
  resCy   <- resCy[ok]
  h12For <- h12For[ok]

  # Prices (intensive, $/toe): broadcast EUR -> 28 EU members.
  # A regional commodity price applies uniformly within a region; broadcasting
  # is conceptually correct for intensive variables.
  priceResCy <- .broadcastToResCy(price, resCy, h12For)

  # Emissions (extensive, Mt/yr): split EUR into 28 EU country values.
  # The disaggregator uses the IAMC `|+|` hierarchy parsed from variable
  # names: leaves are split with their assigned weight; parents take the
  # per-country sum of their direct children, so country-level `|+|` sum-
  # to-parent identities hold automatically.
  #
  # Two weight paradigms (leaves only):
  #   * `magpie_*` — non-negative cell-level land area (135/136 leaves).
  #     Aggregated from `cell.{land,land_split,peatland}_0.5.mz` per
  #     country; used as proportional weight. Self-consistent with model
  #     physics, time-varying, sign-safe by construction.
  #   * `cw_lulucf_co2` — ClimateWatch year-2010 LULUCF CO2 signed weights
  #     (1 leaf: `Land|+|Indirect`). Used because per-Mha forest sink
  #     density varies ~8x across EU climate zones, making uniform-area
  #     weighting biased for forest carbon flux. Branch uses Method G
  #     (history baseline + delta).
  outputdir <- dirname(reportMifPath)
  cellWeights <- .buildCellLevelWeights(outputdir, .EU28)
  cwWeights   <- .loadCwWeights()

  emiResCy <- .disaggregateToResCy(emi, resCy, h12For, emiCsv,
                                   cellWeights, cwWeights)

  # ---- 4. read SBS for broadcasting prices across subsectors
  SBS <- gdx::readGDX(gdxPath, "SBS")
  if (is.list(SBS))       SBS <- SBS[[1]]
  if (is.data.frame(SBS)) SBS <- SBS[[1]]
  SBS <- as.character(SBS)

  # Common header: "dummy,dummy,2010,2011,...,2100"
  # Year labels MUST be bare integers (no "y" prefix) to match OPEN-PROM's
  # YTIME set (core/sets.gms:115 defines `ytime /%fStartHorizon%*%fEndHorizon%/`
  # which expands to 2010..2100 as plain numeric labels). A "y" prefix causes
  # GAMS Error 170 (domain violation) when the CSV is $included.
  yearCols <- as.character(magclass::getYears(priceResCy, as.integer = TRUE))
  header    <- paste(c("dummy", "dummy", yearCols), collapse = ",")

  # ---- 5a. write iPrices_magpie.csv   (region, SBS, years...)
  priceArr <- as.array(priceResCy)[, , 1, drop = FALSE]   # resCy x year x 1
  priceMat <- priceArr[, , 1]                             # resCy x year
  rowsP <- vector("list", length = nrow(priceMat) * length(SBS))
  k <- 1
  for (r in rownames(priceMat)) {
    vals <- formatC(priceMat[r, ], digits = 6, format = "g")
    for (s in SBS) {
      rowsP[[k]] <- paste(c(r, s, vals), collapse = ",")
      k <- k + 1
    }
  }
  dir.create(dirname(outCsvPath), showWarnings = FALSE, recursive = TRUE)
  con <- file(outCsvPath, open = "w")
  writeLines(header, con)
  writeLines(unlist(rowsP), con)
  close(con)
  message(sprintf(
    "[coupleMagpieToProm] wrote iPrices_magpie.csv: %s  (%d regions x %d SBS x %d years)",
    outCsvPath, nrow(priceMat), length(SBS), length(yearCols)
  ))

  # ---- 5b. write iEmissions_magpie.mif via magclass::write.report
  # Output is standard IAMC mif: ;-delimited Model;Scenario;Region;Variable;Unit;<years>
  # Variable names retain `|+|` markers (matching MAgPIE source convention).
  # write.report auto-extracts the "(Mt X/yr)" suffix in variable names into
  # the dedicated Unit column.
  emiOut <- magclass::add_dimension(emiResCy, dim = 3.1, add = "model",
                                    nm = "OPENPROM")
  emiOut <- magclass::add_dimension(emiOut,   dim = 3.2, add = "scenario",
                                    nm = scenario)
  dir.create(dirname(outEmissionsMifPath), showWarnings = FALSE, recursive = TRUE)
  magclass::write.report(emiOut, file = outEmissionsMifPath, ndigit = 4)
  message(sprintf(
    "[coupleMagpieToProm] wrote iEmissions_magpie.mif: %s  (%d regions x %d vars x %d years)",
    outEmissionsMifPath,
    magclass::nregions(emiResCy),
    length(magclass::getNames(emiResCy)),
    length(yearCols)
  ))

  invisible(list(prices = outCsvPath, emissions = outEmissionsMifPath))
}

# ------- helper: broadcast h12-resolution magpie to 39 OPEN-PROM resCy regions
#
# `m`        : magpie at h12 scale (regions x year x [variable])
# `resCy`    : character vector of resCy region codes
# `h12For`  : same length as resCy; the h12 region each resCy pulls from
#
# Returns a magpie at resCy x year x [variable] with values copied from parent
# h12. Preserves variable names on dim 3. Resolves the getItems<- deprecation
# via direct dimnames assignment on the resulting array.
.broadcastToResCy <- function(m, resCy, h12For) {
  years <- magclass::getYears(m)
  vars  <- magclass::getNames(m)
  if (is.null(vars)) vars <- "value"

  arr <- array(
    NA_real_,
    dim      = c(length(resCy), length(years), length(vars)),
    dimnames = list(resCy, years, vars)
  )
  mArr <- as.array(m)
  if (length(dim(mArr)) == 2) dim(mArr) <- c(dim(mArr), 1)
  dimnames(mArr)[[3]] <- vars

  for (i in seq_along(resCy)) {
    arr[i, , ] <- mArr[h12For[i], , ]
  }
  magclass::as.magpie(arr, spatial = 1, temporal = 2)
}

# ============================================================================
# helper: build cell-level country weights for all 11 drivers
# ============================================================================
#
# Reads MAgPIE's 0.5° gridded land-use outputs from `outputdir`, aggregates
# each cell to its ISO3 country (via the run's clustermap), and returns one
# magpie object per driver — country × year area weights.
#
# Why cell-level: MAgPIE solves at cluster scale (200 clusters globally;
# each EU cluster covers up to 23 countries), so cluster-level outputs
# cannot resolve country differences. The 0.5° cell-level outputs (produced
# by MAgPIE's standard `extra/disaggregation.R` postprocessor) DO resolve to
# country, since each cell carries an unambiguous ISO3 tag.
#
# Source files (all required; produced by every standard MAgPIE run):
#   * cell.land_0.5.mz         7 land categories: crop, past, forestry,
#                              primforest, secdforest, urban, other
#   * cell.land_split_0.5.mz   further split: PlantedForest_{Afforestation,
#                              NPiNDC,Timber}, crop_{treecover,fallow},
#                              crop_{kfo,kbe}_{rf,ir}
#   * cell.peatland_0.5.mz     7 peatland states: intact, crop, past,
#                              forestry, peatExtract, rewetted, unused
#   * clustermap_*.rds         cell -> (cluster, region, country) mapping
#
# Per-driver definitions (file + categories summed):
#   magpie_peatland_drained        peatland: crop+past+forestry+peatExtract
#   magpie_forest_managed          land:     secdforest + forestry
#   magpie_secondary_forest        land:     secdforest
#   magpie_primary_forest          land:     primforest
#   magpie_other_land              land:     other
#   magpie_cropland                land:     crop
#   magpie_pasture                 land:     past
#   magpie_plantation              land_split: PlantedForest_Timber
#   magpie_afforestation_npindc    land_split: PlantedForest_NPiNDC
#   magpie_afforestation_co2price  land_split: PlantedForest_Afforestation
#   magpie_cropland_tree           land_split: crop_treecover
#
# Args:
#   outputdir : MAgPIE run directory (contains cell.*.mz + clustermap_*.rds)
#   EU28      : ISO3 country codes to keep
#
# Returns: named list (length 11). Each entry is a magpie object with:
#          dim 1 = 28 EU ISO3 codes (padded with 0 if missing)
#          dim 2 = years from the source cell file (5-yearly typically)
#          dim 3 = singleton (one driver per object)
#
.buildCellLevelWeights <- function(outputdir, EU28) {
  cmFiles <- list.files(outputdir, pattern = "^clustermap_.*\\.rds$",
                        full.names = TRUE)
  if (length(cmFiles) == 0) {
    stop("[buildCellLevelWeights] no clustermap_*.rds in ", outputdir)
  }
  if (length(cmFiles) > 1) {
    warning("multiple clustermap files; using ", basename(cmFiles[1]))
  }
  clustermap <- readRDS(cmFiles[1])
  cellMap    <- clustermap[, c("cell", "country")]

  cellFiles <- list(
    land       = file.path(outputdir, "cell.land_0.5.mz"),
    land_split = file.path(outputdir, "cell.land_split_0.5.mz"),
    peatland   = file.path(outputdir, "cell.peatland_0.5.mz")
  )
  for (f in cellFiles) {
    if (!file.exists(f)) {
      stop("[buildCellLevelWeights] missing cell-level file: ", f, "\n",
           "  Did MAgPIE's extra/disaggregation.R postprocessor run after ",
           "the main run?")
    }
  }
  cells <- list(
    land       = magclass::read.magpie(cellFiles$land),
    land_split = magclass::read.magpie(cellFiles$land_split),
    peatland   = magclass::read.magpie(cellFiles$peatland)
  )

  drivers <- list(
    magpie_peatland_drained       = list(file = "peatland",
                                         cats = c("crop", "past",
                                                  "forestry", "peatExtract")),
    magpie_forest_managed         = list(file = "land",
                                         cats = c("secdforest", "forestry")),
    magpie_secondary_forest       = list(file = "land", cats = "secdforest"),
    magpie_primary_forest         = list(file = "land", cats = "primforest"),
    magpie_other_land             = list(file = "land", cats = "other"),
    magpie_cropland               = list(file = "land", cats = "crop"),
    magpie_pasture                = list(file = "land", cats = "past"),
    magpie_plantation             = list(file = "land_split",
                                         cats = "PlantedForest_Timber"),
    magpie_afforestation_npindc   = list(file = "land_split",
                                         cats = "PlantedForest_NPiNDC"),
    magpie_afforestation_co2price = list(file = "land_split",
                                         cats = "PlantedForest_Afforestation"),
    magpie_cropland_tree          = list(file = "land_split",
                                         cats = "crop_treecover")
  )

  lapply(drivers, function(d) {
    .aggCellsToCountry(cells[[d$file]], d$cats, cellMap, EU28)
  })
}


# Aggregate a subset of cell-level land categories to EU28 country totals.
# Categories are matched against magclass `getNames()` either as exact-name
# (e.g. "secdforest") or as prefix-on-`.` (e.g. "PlantedForest_Timber" matches
# "PlantedForest_Timber.forestry" since cell.land_split uses compound names).
.aggCellsToCountry <- function(cellMagpie, catPatterns, cellMap, EU28) {
  allCats <- magclass::getNames(cellMagpie)
  matched <- unique(unlist(lapply(catPatterns, function(p) {
    allCats[allCats == p | startsWith(allCats, paste0(p, "."))]
  })))

  if (length(matched) == 0) {
    # Driver categories absent (e.g. some scenarios may not produce
    # PlantedForest_Afforestation if endogenous AR isn't active).
    # Return a zero-filled magpie — disaggregator falls back to equal split.
    yrs <- magclass::getYears(cellMagpie)
    return(magclass::new.magpie(EU28, yrs, fill = 0))
  }

  if (length(matched) == 1) {
    summed <- cellMagpie[, , matched]
  } else {
    summed <- magclass::dimSums(cellMagpie[, , matched], dim = 3)
  }
  summed <- magclass::collapseNames(summed)

  byCountry <- madrat::toolAggregate(summed, rel = cellMap,
                                     from = "cell", to = "country")

  available <- intersect(EU28, magclass::getRegions(byCountry))
  yrs       <- magclass::getYears(byCountry)
  if (length(available) == length(EU28)) {
    return(byCountry[EU28, , ])
  }
  # Pad missing EU members with 0
  result_avail <- byCountry[available, , ]
  missing_iso  <- setdiff(EU28, available)
  pad <- magclass::new.magpie(missing_iso, yrs, fill = 0)
  result <- magclass::mbind(result_avail, pad)
  result[EU28, , ]  # canonical EU28 order
}


# ============================================================================
# helper: load static ClimateWatch country weights
# ============================================================================
#
# Loads the narrow CW weights csv shipped with postprom. Currently used by
# exactly one variable (Indirect, weight_source = "cw_lulucf_co2"); see
# build_climatewatch_eu28_weights.R header for why this single variable
# needs an external NGHGI-aligned weight pattern instead of MAgPIE's own
# uniform-area cell-level weight.
#
# Returns a named list keyed by weight_source; each entry is a named numeric
# vector (ISO -> signed weight value).
#
.loadCwWeights <- function() {
  cwPath <- system.file("extdata",
                        "climatewatch-eu28-lulucf-co2-weights.csv",
                        package = "postprom")
  if (!nzchar(cwPath)) {
    stop("[loadCwWeights] Missing climatewatch-eu28-lulucf-co2-weights.csv ",
         "in postprom/inst/extdata/. Run build_climatewatch_eu28_weights.R ",
         "and reinstall postprom.")
  }
  df <- read.csv(cwPath, stringsAsFactors = FALSE)
  split_keys <- split(df, df$weight_source)
  lapply(split_keys, function(d) setNames(d$value_mtco2_yr, d$iso))
}


# ============================================================================
# helper: parse IAMC parent-child hierarchy from variable names
# ============================================================================
#
# IAMC `|+|` convention encodes the partition relation INSIDE the variable
# name itself: in `Emissions|CO2|Land|Land-use Change|+|Regrowth`, the |+|
# appears at the position separating the immediate parent ("Land-use Change")
# from this variable's own token ("Regrowth"). The chain of ancestors above
# the immediate parent uses plain `|` (no |+|).
#
# Parent lookup:
#   stripped name      = "Emissions|CO2|Land|Land-use Change|+|Regrowth"
#   parent inner id    = substring before |+|
#                      = "Emissions|CO2|Land|Land-use Change"
#   parent's full name = the curated variable whose `|+|`-stripped form
#                        equals the parent inner id (here: the variable
#                        `Emissions|CO2|Land|+|Land-use Change`).
#
# Returns a named list keyed by variable: each entry is a character vector
# of that variable's direct |+| children (empty for leaves and orphans).
#
.parseIamcChildren <- function(varsWithUnits) {
  stripUnit <- function(v) sub("\\s*\\([^)]*\\)\\s*$", "", v)
  toInner   <- function(v) gsub("\\|\\+\\|", "|", stripUnit(v))

  innerToVar <- setNames(varsWithUnits, vapply(varsWithUnits, toInner, character(1)))
  children   <- setNames(vector("list", length(varsWithUnits)), varsWithUnits)

  for (w in varsWithUnits) {
    s   <- stripUnit(w)
    pos <- regexpr("\\|\\+\\|", s)
    if (pos < 0) next                       # no |+| -> top of curated tree
    parentInner <- substring(s, 1, pos - 1)
    if (parentInner %in% names(innerToVar)) {
      p <- innerToVar[[parentInner]]
      children[[p]] <- c(children[[p]], w)
    }
    # parent not in curated set -> orphan; treated as leaf for disaggregation
  }
  children
}


# ============================================================================
# helper: extensive-variable disaggregation h12 -> 39 OPEN-PROM resCy
# ============================================================================
#
# Used for emissions (Mt/yr, extensive). Companion to .broadcastToResCy()
# which handles prices (intensive, $/toe).
#
# Strategy:
#   * Non-EU resCy (11 regions, 1:1 with h12) : copy h12 value, all variables.
#   * EU28 resCy (28 members, all in EUR)     : two-pass over the IAMC tree.
#         Pass 1 — leaves: weight-based split (one of two paradigms below).
#         Pass 2 — parents: country values = Σ direct children's country
#                  values, processed in topological order (deepest first).
#                  This makes country-level `|+|` sum-to-parent identities
#                  hold automatically (matching the h12-level identities).
#
# Leaf paradigm 1 — `magpie_*` (cell-level non-negative areas, 135/136 leaves).
# Proportional split:
#     country[c,t] = MAgPIE_EUR(t) × w[c,t] / Σ_c w[c,t]
# Sum-conserving exactly. Sign-safe by construction (areas ≥ 0, so country
# inherits EUR sign — physically correct for leaf emissions). w[c,t] is
# time-varying. Falls back to equal split if Σ_c w[c,t] = 0.
#
# Leaf paradigm 2 — `cw_*` (ClimateWatch signed historical, 1 leaf:
# `Land|+|Indirect`). Method G (history-baseline + delta):
#     delta(t)            = MAgPIE_EUR(t) − Σ_c w[c]
#     country[c,t]        = w[c]  +  delta(t) × |w[c]| / Σ_c |w[c]|
# Sum-conserving exactly. Country signs preserved when |delta| < Σ|w|; for
# Indirect under realistic SSPs |delta|/Σ|w| ≈ 0.08 (well within safe band).
#
# Args:
#   m           : magpie at h12 scale (region x year x variable)
#   resCy       : 39 resCy region codes
#   h12For      : same length as resCy; the h12 region each resCy maps to
#   emiCsv      : data.frame with columns variable, weight_source, ...
#                 (one row per curated variable; weight_source filled for
#                 leaves, blank for parents)
#   cellWeights : named list from .buildCellLevelWeights()
#   cwWeights   : named list from .loadCwWeights()
#
# Returns: magpie at resCy x year x variable.
#
.disaggregateToResCy <- function(m, resCy, h12For, emiCsv,
                                 cellWeights, cwWeights) {
  years <- magclass::getYears(m)
  vars  <- magclass::getNames(m)
  if (is.null(vars)) vars <- "value"

  names(h12For) <- resCy
  euCy  <- resCy[h12For == "EUR"]
  nonEu <- resCy[h12For != "EUR"]
  nYears <- length(years)
  nEu    <- length(euCy)

  arr <- array(
    NA_real_,
    dim      = c(length(resCy), length(years), length(vars)),
    dimnames = list(resCy, years, vars)
  )
  mArr <- as.array(m)
  if (length(dim(mArr)) == 2) dim(mArr) <- c(dim(mArr), 1)
  dimnames(mArr)[[3]] <- vars

  # ---- 1. non-EU resCy: pure broadcast for all variables (h12 already
  #         satisfies sum-to-parent identities)
  for (c in nonEu) {
    arr[c, , ] <- mArr[h12For[c], , ]
  }

  if (length(euCy) == 0) {
    return(magclass::as.magpie(arr, spatial = 1, temporal = 2))
  }

  # ---- 2. EU28 resCy: parse hierarchy + two-pass processing
  childrenMap <- .parseIamcChildren(vars)
  isLeaf <- vapply(vars, function(v) length(childrenMap[[v]]) == 0, logical(1))
  names(isLeaf) <- vars

  # Pre-interpolate cell-level weight magpies to emi's annual year grid.
  # cell.*.mz years are 5-yearly; constant extrapolation handles ranges
  # outside source years (e.g. cell.peatland starts at 2020 — earlier years
  # filled with 2020 value).
  targetYears <- magclass::getYears(m, as.integer = TRUE)
  weightsInterp <- lapply(cellWeights, function(w) {
    magclass::time_interpolate(w,
                               interpolated_year  = targetYears,
                               extrapolation_type = "constant")
  })

  # weight_source lookup keyed by variable name
  wsLookup <- setNames(emiCsv$weight_source, emiCsv$variable)

  # ---- Pass 1: leaves — weight-based split ---------------------------------
  for (v in vars[isLeaf]) {
    ws <- wsLookup[[v]]
    if (is.na(ws) || ws == "") {
      stop(sprintf(
        "[disaggregateToResCy] leaf variable '%s' has empty weight_source. Edit inst/extdata/magpie-afolu-emission-variables.csv.",
        v
      ))
    }

    h12Eur <- as.numeric(mArr["EUR", , v])               # year-vector

    if (startsWith(ws, "cw_")) {
      # CW signed weights — Method G
      if (!ws %in% names(cwWeights)) {
        stop(sprintf("[disaggregateToResCy] unknown CW weight_source '%s' for '%s'",
                     ws, v))
      }
      cwVec <- cwWeights[[ws]]
      w <- setNames(numeric(nEu), euCy)
      common <- intersect(euCy, names(cwVec))
      w[common] <- cwVec[common]

      Wabs    <- sum(abs(w), na.rm = TRUE)
      Wsigned <- sum(w,      na.rm = TRUE)

      if (!is.finite(Wabs) || Wabs < 1e-9) {
        for (t in seq_len(nYears)) {
          arr[euCy, t, v] <- h12Eur[t] / nEu
        }
      } else {
        shareAbs <- abs(w) / Wabs
        for (t in seq_len(nYears)) {
          arr[euCy, t, v] <- w + (h12Eur[t] - Wsigned) * shareAbs
        }
      }
      next
    }

    # MAgPIE cell-level non-negative areas — proportional split
    if (!ws %in% names(weightsInterp)) {
      stop(sprintf("[disaggregateToResCy] unknown weight_source '%s' for '%s'. Valid: %s",
                   ws, v, paste(c(names(weightsInterp), names(cwWeights)),
                                collapse = ", ")))
    }
    wArr <- as.array(weightsInterp[[ws]])
    if (length(dim(wArr)) == 3) wArr <- wArr[, , 1, drop = TRUE]
    if (is.null(dim(wArr))) {
      wArr <- matrix(wArr, ncol = 1, dimnames = list(names(wArr), years[1]))
    }
    wEu <- wArr[euCy, , drop = FALSE]

    for (t in seq_len(nYears)) {
      wYr  <- wEu[, t]
      Wtot <- sum(wYr, na.rm = TRUE)
      if (!is.finite(Wtot) || Wtot < 1e-9) {
        shareYr <- rep(1 / nEu, nEu)                     # zero-driver fallback
      } else {
        shareYr <- wYr / Wtot
      }
      arr[euCy, t, v] <- shareYr * h12Eur[t]
    }
  }

  # ---- Pass 2: parents — sum direct children, deepest first ----------------
  # Topological loop: a parent is "ready" when all its direct children are
  # already filled in arr. Repeat until all parents are done.
  done      <- isLeaf
  remaining <- vars[!isLeaf]
  while (length(remaining) > 0) {
    ready <- remaining[vapply(remaining, function(p) {
      all(done[childrenMap[[p]]])
    }, logical(1))]
    if (length(ready) == 0) {
      stop("[disaggregateToResCy] hierarchy contains a cycle or unresolved ",
           "dependency among: ", paste(remaining, collapse = ", "))
    }
    for (p in ready) {
      ch <- childrenMap[[p]]
      acc <- arr[euCy, , ch[1], drop = FALSE]
      if (length(ch) > 1) {
        for (cv in ch[-1]) {
          acc <- acc + arr[euCy, , cv, drop = FALSE]
        }
      }
      arr[euCy, , p] <- acc
      done[p] <- TRUE
    }
    remaining <- setdiff(remaining, ready)
  }

  magclass::as.magpie(arr, spatial = 1, temporal = 2)
}
