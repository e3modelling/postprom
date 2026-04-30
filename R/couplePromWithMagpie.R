#' Soft-link OPEN-PROM with MAgPIE via coupling-channel (mif)
#'
#' Produces a REMIND-style `.mif` that MAgPIE can consume through its coupling
#' interface (`c56_pollutant_prices = "coupling"`, `c60_2ndgen_biodem = "coupling"`),
#' and — in the reverse direction — reads MAgPIE's `report.mif` to produce
#' `iPrices_magpie.csv` for OPEN-PROM's round-2 run.
#'
#' Two top-level functions:
#'   * `couplePromToMagpie()`  : forward  (OPEN-PROM gdx  -> mif)
#'   * `coupleMagpieToProm()`  : backward (MAgPIE mif     -> iPrices_magpie.csv)
#'
#' Scope:
#'   * Forward exports only the narrow 2nd-gen bioenergy channel
#'     (`V03InpTotTransf[,"LQD","BMSWAS",]`: biomass feedstock entering the
#'     liquid-biofuel refinery step). Broader BMSWAS flows (PG/SLD/CHP) are
#'     not carried through the coupling channel.
#'   * Forward CO2 price is read from `VmCarVal[,"TRADE",]` (US$2015/t CO2).
#'     Non-CO2 prices (N2O, CH4) are derived from CO2 via GWP100 (AR6).
#'   * Backward reads `Prices|Bioenergy (US$2017/GJ)` and broadcasts to
#'     OPEN-PROM's 39 resCy regions.
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

  # Slice: CO2 price under "TRADE", bioenergy under LQD.BMSWAS.
  co2 <- co2[, , "TRADE"]                    # 39 x 91 x 1
  bio <- bio[, , "BMSWAS"] * 0.4 + bio[, , "BGSL"] * 0.6  + bio[, , "BKRS"] * 0.6              # 39 x 91 x 1

  # Collapse the 3rd dim naming.
  co2 <- magclass::collapseDim(co2, dim = 3)
  bio <- magclass::collapseDim(bio, dim = 3)

  # ---- 2. aggregate prom countries -> h12
  co2_h12 <- .aggregateToH12_price(co2, bio)   # weight by bioenergy
  bio_h12 <- .aggregateToH12_sum(bio)

  # ---- 3. unit conversion
  # CO2 price: US$2015/t CO2 -> US$2017/t CO2
  co2_h12 <- co2_h12 * deflator15to17
  # Bioenergy: Mtoe/yr -> EJ/yr (1 Mtoe = 0.041868 EJ)
  bio_h12 <- bio_h12 * 0.041868

  # ---- 4. derive non-CO2 prices
  if (nonCo2Mode == "gwp") {
    n2o_h12 <- co2_h12 * .GWP_N2O_AR6
    ch4_h12 <- co2_h12 * .GWP_CH4_AR6
  } else {                                          # "zero"
    n2o_h12 <- co2_h12 * 0
    ch4_h12 <- co2_h12 * 0
  }

  # ---- 5. stamp variable labels (REMIND-style "Variable (Unit)")
  co2_h12 <- magclass::setNames(co2_h12, "Price|Carbon (US$2017/t CO2)")
  n2o_h12 <- magclass::setNames(n2o_h12, "Price|N2O (US$2017/t N2O)")
  ch4_h12 <- magclass::setNames(ch4_h12, "Price|CH4 (US$2017/t CH4)")
  bio_h12 <- magclass::setNames(
    bio_h12,
    "Primary Energy Production|Biomass|Energy Crops (EJ/yr)"
  )

  # All four variables share same region/time structure -> mbind on dim 3.
  out <- magclass::mbind(co2_h12, n2o_h12, ch4_h12, bio_h12)

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
.aggregateToH12_sum <- function(m) {
  regs <- magclass::getRegions(m)
  eu   <- intersect(.EU28, regs)
  non_eu_h12 <- setdiff(regs, eu)

  # EU-28 sum -> EUR
  eur <- m[eu, , ]
  eur <- magclass::dimSums(eur, dim = 1)
  magclass::getItems(eur, dim = 1) <- "EUR"

  # The remaining regions must coincide with h12 codes (minus EUR).
  stopifnot(all(non_eu_h12 %in% .H12))
  out <- magclass::mbind(m[non_eu_h12, , ], eur)

  # Re-order canonically.
  out[.H12, , ]
}

# EUR price = bioenergy-weighted average over EU-28; others 1:1.
# If total bio across EU-28 in a given year is zero, fall back to simple mean.
.aggregateToH12_price <- function(price, bio) {
  regs <- magclass::getRegions(price)
  eu   <- intersect(.EU28, regs)
  non_eu_h12 <- setdiff(regs, eu)

  # Weighted average by bioenergy across EU-28.
  num <- magclass::dimSums(price[eu, , ] * bio[eu, , ], dim = 1)
  den <- magclass::dimSums(bio[eu, , ],                 dim = 1)

  # Fallback simple average where denominator is zero.
  eur <- num
  mean_price <- magclass::dimSums(price[eu, , ], dim = 1) / length(eu)
  eur[den == 0] <- mean_price[den == 0]
  eur[den != 0] <- num[den != 0] / den[den != 0]
  magclass::getItems(eur, dim = 1) <- "EUR"

  stopifnot(all(non_eu_h12 %in% .H12))
  out <- magclass::mbind(price[non_eu_h12, , ], eur)
  out[.H12, , ]
}


# ========================================================== backward: magpie -> prom

#' @param reportMifPath Absolute path to MAgPIE output `report.mif`.
#' @param outCsvPath    Absolute path to write `iPrices_magpie.csv`.
#' @param outEmissionsCsvPath Absolute path to write `iEmissions_magpie.csv`.
#'                      Even though OPEN-PROM's current GAMS code does not
#'                      `$include` this file, it is produced for consistency
#'                      with the existing workflow and for downstream reporting.
#' @param gdxPath       Absolute path to an OPEN-PROM `blabla.gdx`, used only
#'                      to read the runtime `SBS` set (so we can broadcast the
#'                      price across all subsectors).
#' @param biomassVariable MAgPIE variable name to extract. Defaults to the
#'                      one present in current report.mif.
#' @param deflator17to15 US$2017 -> US$2015 deflator. Default 0.96.
#' @return A named list of the two written paths (invisibly).
#' @export
coupleMagpieToProm <- function(reportMifPath,
                               outCsvPath,
                               outEmissionsCsvPath,
                               gdxPath,
                               biomassVariable = "Prices|Bioenergy (US$2017/GJ)",
                               deflator17to15  = 0.96) {

  # ---- 1. read mif and slice
  rep <- magclass::read.report(reportMifPath, as.list = FALSE)
  # Strip model/scenario to leave just region x year x variable.
  rep <- magclass::collapseNames(rep)

  # Strip single `+` markers globally so that emi_vars below can be specified
  # in their flattened IAMC form (parent-child expressed via `|` only).
  # `++` markers are NOT stripped: ++ variables are not requested by emi_vars,
  # and stripping them too would risk name collisions with `+` siblings.
  magclass::getNames(rep) <- gsub("\\|\\+\\|", "|", magclass::getNames(rep))

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
  # The native MAgPIE report.mif uses IAMC naming conventions:
  #
  # * Names are `|`-separated hierarchical paths, e.g.
  #   `Emissions|CO2|Land|+|Land-use Change|+|Deforestation`
  # * Two markers encode aggregation relationships:
  #     `|+|`   sum-to-parent partition: parent = sum of all +-marked siblings
  #             (mutually-exclusive slices along one dimension)
  #     `|++|`  orthogonal alternate decomposition: a parallel slicing of the same
  #             node from a different perspective (e.g. + slices by "use", ++
  #             slices by "carbon-pool location"). Sum of + siblings = sum of ++
  #             siblings = parent.
  # * 11 gases/pollutants: BC, CH4, CO, CO2, N2O, NH3, NO2, NO3-, OC, SO2, VOC.
  # * Three parallel layer-3 subtrees per gas, with distinct provenance — they
  #   are NOT in a parent-child relationship:
  #     |<gas>|Land|*               endogenous output of MAgPIE's 76_emissions
  #                                 module (land-system equilibrium)
  #     |<gas>|AFOLU|Land|Fires|*   exogenous GFED fire data, split by climate zone
  #     |<gas>|AFOLU|Agriculture    exogenous GAINS/CEDS agricultural activity
  #                                 (non-zero only for BC/CO/OC/SO2/VOC)
  #     |CO2|Land Carbon Sink|*     post-processing mapping (Grassi NGHGI alias +
  #                                 full LPJmL carbon flux)
  # * Two derived families we do NOT extract: `|GWP100AR6|...` (× CO2eq weighting)
  #   and `|...|Cumulative|...` (cumulative integrals). OPEN-PROM does its own
  #   weighting and accumulation; keeping these would just duplicate storage.
  # * Overall scale: ~700+ emission variables in the raw mif.
  #
  # ----------------------------------------------------------------------------
  # Modifications applied to the native data
  # ----------------------------------------------------------------------------
  #
  # (1) Drop GWP100AR6 weighted columns and Cumulative integrals
  #     Native: `Emissions|GWP100AR6|Land`, `Emissions|N2O_GWP100AR6|Land*`,
  #             `Emissions|<gas>|...|Cumulative|...`
  #     Why:    These are second-order derived quantities MAgPIE provides for
  #             IAMC reporting convenience; OPEN-PROM can compute them itself,
  #             so keeping them only duplicates storage.
  #     Action: emi_vars excludes both families.
  #
  # (2) Drop the 8 |++| variants
  #     Native: `Emissions|CO2|Land|++|Above Ground Carbon`,
  #             `...|Land|++|Below Ground Carbon`,
  #             `...|Indirect|++|Above/Below`,
  #             `...|Land-use Change|++|Above/Below`,
  #             `...|Soil|++|Emissions/Withdrawals`
  #     Why:    ++ provides an alternate slicing parallel to +. Under the same
  #             parent, + siblings and ++ siblings each sum back to the parent,
  #             so keeping both is redundant — and mixing them is error-prone
  #             (++ slices by carbon-pool location, + slices by use).
  #     Action: keep the + decomposition (mainline, by use), drop all ++.
  #
  # (3) Drop 1 cross-tree alias
  #     Native: `Emissions|CO2|Land Carbon Sink|Grassi|Managed Land|Managed Forest`
  #     Why:    For every region and year this series is **identical** to
  #             `Emissions|CO2|Land|+|Indirect`. MAgPIE reports both to satisfy
  #             two naming conventions (IAMC mif `Land|+|Indirect` vs. IPCC
  #             NGHGI `Land Carbon Sink|Grassi|...`). OPEN-PROM does no NGHGI
  #             reporting, so the alias only adds double-count risk.
  #     Action: keep only the `|Land|+|Indirect` form.
  #
  # (4) Drop 10 structural single-child parents (5 gases × 2 places)
  #     Native: `Emissions|<gas>|Land|+|Biomass Burning` and
  #             `Emissions|<gas>|Land|+|Peatland`,
  #             where <gas> ∈ {CH4, N2O, NH3, NO2, NO3-}
  #     Why:    In the mif each of these parents has structurally exactly ONE
  #             child (`|+|Burning of Crop Residues` and `|+|Managed`), so the
  #             parent value always equals the child value, and the child name
  #             carries strictly more information.
  #     Action: keep the more specific children (e.g.
  #             `|Land|Biomass Burning|Burning of Crop Residues`), drop the
  #             redundant parents.
  #
  # (5) Strip `|+|` markers globally
  #     Native: variable names look like
  #             `Emissions|CO2|Land|+|Land-use Change|+|Deforestation`
  #     Why:    The + marker is a hint for IAMC reporting tooling (piamInterfaces
  #             / mip / IIASA Scenario Explorer) to identify which siblings sum
  #             to a given parent row. Downstream OPEN-PROM consumes this csv by
  #             selecting on names, so the marker is unnecessary; worse, level-
  #             splitting helpers like `helperAggregateLevel` would otherwise
  #             treat `+` as a real path component.
  #     Action: gsub `\|\+\|` → `\|` immediately after collapseNames (line 204);
  #             emi_vars uses the flattened names directly so they match the
  #             stripped rep names exactly. `|++|` is left untouched by the gsub
  #             (to avoid name collisions with the stripped `+` siblings), but
  #             since (2) already drops all ++ variables this is a no-op.
  #
  # ----------------------------------------------------------------------------
  # Extraction result: 190 variables
  # ----------------------------------------------------------------------------
  #
  #   BC  9    CH4  14    CO  9    CO2 58    N2O 20    NH3 20
  #   NO2 20   NO3- 13    OC  9    SO2 9     VOC 9              total 190
  #
  # Ordering: DFS (depth-first); each parent is followed immediately by its
  #           children.
  # Name format: flattened IAMC, no `+` / `++` markers.
  # Units: native MAgPIE "Mt <pollutant>/yr", kept as-is.
  # NOx is reported under MAgPIE's `NO2` label.
  # `|<gas>|AFOLU|Agriculture` is all-zero in the current scenario for 6 gases
  #   (BC/CO/CO2/OC/SO2/VOC); kept for structural completeness (it can be non-
  #   zero under other GAINS/CEDS scenarios).
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
  # === CH4 (14 vars) ===
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
  #     ├── Biomass Burning|Burning of Crop Residues  (single-child parent |Biomass Burning| stripped)
  #     └── Peatland|Managed                          (single-child parent |Peatland| stripped)
  #
  # === N2O, NH3, NO2 (20 vars each) ===
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
  #     ├── Biomass Burning|Burning of Crop Residues  (single-child parent stripped)
  #     └── Peatland|Managed                          (single-child parent stripped)
  #
  # === NO3- (13 vars, no |AFOLU| subtree) ===
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
  # ├── Biomass Burning|Burning of Crop Residues  (single-child parent stripped)
  # └── Peatland|Managed                          (single-child parent stripped)
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
  emi_vars <- c(
    # ----- BC (9 vars) -----
    "Emissions|BC|AFOLU|Agriculture (Mt BC/yr)",
    "Emissions|BC|AFOLU|Land|Fires (Mt BC/yr)",
    "Emissions|BC|AFOLU|Land|Fires|Forest Burning (Mt BC/yr)",
    "Emissions|BC|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt BC/yr)",
    "Emissions|BC|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt BC/yr)",
    "Emissions|BC|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt BC/yr)",
    "Emissions|BC|AFOLU|Land|Fires|Grassland Burning (Mt BC/yr)",
    "Emissions|BC|AFOLU|Land|Fires|Peat Burning (Mt BC/yr)",
    "Emissions|BC|Land|Biomass Burning|Burning of Crop Residues (Mt BC/yr)",
    # ----- CH4 (14 vars) -----
    "Emissions|CH4|AFOLU|Land|Fires (Mt CH4/yr)",
    "Emissions|CH4|AFOLU|Land|Fires|Forest Burning (Mt CH4/yr)",
    "Emissions|CH4|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt CH4/yr)",
    "Emissions|CH4|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt CH4/yr)",
    "Emissions|CH4|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt CH4/yr)",
    "Emissions|CH4|AFOLU|Land|Fires|Grassland Burning (Mt CH4/yr)",
    "Emissions|CH4|AFOLU|Land|Fires|Peat Burning (Mt CH4/yr)",
    "Emissions|CH4|Land (Mt CH4/yr)",
    "Emissions|CH4|Land|Agriculture (Mt CH4/yr)",
    "Emissions|CH4|Land|Agriculture|Animal waste management (Mt CH4/yr)",
    "Emissions|CH4|Land|Agriculture|Enteric fermentation (Mt CH4/yr)",
    "Emissions|CH4|Land|Agriculture|Rice (Mt CH4/yr)",
    "Emissions|CH4|Land|Biomass Burning|Burning of Crop Residues (Mt CH4/yr)",
    "Emissions|CH4|Land|Peatland|Managed (Mt CH4/yr)",
    # ----- CO (9 vars) -----
    "Emissions|CO|AFOLU|Agriculture (Mt CO/yr)",
    "Emissions|CO|AFOLU|Land|Fires (Mt CO/yr)",
    "Emissions|CO|AFOLU|Land|Fires|Forest Burning (Mt CO/yr)",
    "Emissions|CO|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt CO/yr)",
    "Emissions|CO|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt CO/yr)",
    "Emissions|CO|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt CO/yr)",
    "Emissions|CO|AFOLU|Land|Fires|Grassland Burning (Mt CO/yr)",
    "Emissions|CO|AFOLU|Land|Fires|Peat Burning (Mt CO/yr)",
    "Emissions|CO|Land|Biomass Burning|Burning of Crop Residues (Mt CO/yr)",
    # ----- CO2 (58 vars) -----
    "Emissions|CO2|AFOLU|Agriculture (Mt CO2/yr)",
    "Emissions|CO2|AFOLU|Land|Fires (Mt CO2/yr)",
    "Emissions|CO2|AFOLU|Land|Fires|Forest Burning (Mt CO2/yr)",
    "Emissions|CO2|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt CO2/yr)",
    "Emissions|CO2|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt CO2/yr)",
    "Emissions|CO2|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt CO2/yr)",
    "Emissions|CO2|AFOLU|Land|Fires|Grassland Burning (Mt CO2/yr)",
    "Emissions|CO2|AFOLU|Land|Fires|Peat Burning (Mt CO2/yr)",
    "Emissions|CO2|Land (Mt CO2/yr)",
    "Emissions|CO2|Land|Biomass Burning|Burning of Crop Residues (Mt CO2/yr)",
    "Emissions|CO2|Land|Indirect (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Deforestation (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Deforestation|Cropland Tree Cover (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Deforestation|Forestry plantations (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Deforestation|Primary forests (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Deforestation|Secondary forests (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Forest degradation (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Forest degradation|Primary forests (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Forest degradation|Secondary forests (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Other land conversion (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Peatland (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Peatland|Negative (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Peatland|Positive (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Regrowth (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Regrowth|CO2-price AR (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Regrowth|CO2-price AR|Natural Forest (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Regrowth|CO2-price AR|Plantation (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Regrowth|Cropland Tree Cover (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Regrowth|NPI_NDC AR (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Regrowth|Other Land (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Regrowth|Secondary Forest (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Regrowth|Timber Plantations (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Residual (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Residual|Negative (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Residual|Positive (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil|Cropland management (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil|Cropland management|Emissions (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil|Cropland management|Withdrawals (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil|Land Conversion (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil|Land Conversion|Emissions (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil|Land Conversion|Withdrawals (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil|Soil Carbon Management (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil|Soil Carbon Management|Emissions (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Soil|Soil Carbon Management|Withdrawals (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Timber (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Timber|Release from HWP (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Timber|Release from HWP|Buildings (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Timber|Release from HWP|Industrial Roundwood (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Timber|Storage in HWP (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Timber|Storage in HWP|Buildings (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Timber|Storage in HWP|Industrial Roundwood (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Wood Harvest (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Wood Harvest|Other Land (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Wood Harvest|Primary Forest (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Wood Harvest|Secondary Forest (Mt CO2/yr)",
    "Emissions|CO2|Land|Land-use Change|Wood Harvest|Timber Plantations (Mt CO2/yr)",
    # ----- N2O (20 vars) -----
    "Emissions|N2O|AFOLU|Land|Fires (Mt N2O/yr)",
    "Emissions|N2O|AFOLU|Land|Fires|Forest Burning (Mt N2O/yr)",
    "Emissions|N2O|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt N2O/yr)",
    "Emissions|N2O|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt N2O/yr)",
    "Emissions|N2O|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt N2O/yr)",
    "Emissions|N2O|AFOLU|Land|Fires|Grassland Burning (Mt N2O/yr)",
    "Emissions|N2O|AFOLU|Land|Fires|Peat Burning (Mt N2O/yr)",
    "Emissions|N2O|Land (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture|Agricultural Soils (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture|Agricultural Soils|Decay of Crop Residues (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers|Cropland (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers|Pasture (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture|Agricultural Soils|Manure applied to Croplands (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture|Agricultural Soils|Pasture (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture|Agricultural Soils|Soil Organic Matter Loss (Mt N2O/yr)",
    "Emissions|N2O|Land|Agriculture|Animal Waste Management (Mt N2O/yr)",
    "Emissions|N2O|Land|Biomass Burning|Burning of Crop Residues (Mt N2O/yr)",
    "Emissions|N2O|Land|Peatland|Managed (Mt N2O/yr)",
    # ----- NH3 (20 vars) -----
    "Emissions|NH3|AFOLU|Land|Fires (Mt NH3/yr)",
    "Emissions|NH3|AFOLU|Land|Fires|Forest Burning (Mt NH3/yr)",
    "Emissions|NH3|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt NH3/yr)",
    "Emissions|NH3|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt NH3/yr)",
    "Emissions|NH3|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt NH3/yr)",
    "Emissions|NH3|AFOLU|Land|Fires|Grassland Burning (Mt NH3/yr)",
    "Emissions|NH3|AFOLU|Land|Fires|Peat Burning (Mt NH3/yr)",
    "Emissions|NH3|Land (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture|Agricultural Soils (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture|Agricultural Soils|Decay of Crop Residues (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers|Cropland (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers|Pasture (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture|Agricultural Soils|Manure applied to Croplands (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture|Agricultural Soils|Pasture (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture|Agricultural Soils|Soil Organic Matter Loss (Mt NH3/yr)",
    "Emissions|NH3|Land|Agriculture|Animal Waste Management (Mt NH3/yr)",
    "Emissions|NH3|Land|Biomass Burning|Burning of Crop Residues (Mt NH3/yr)",
    "Emissions|NH3|Land|Peatland|Managed (Mt NH3/yr)",
    # ----- NO2 (20 vars) -----
    "Emissions|NO2|AFOLU|Land|Fires (Mt NO2/yr)",
    "Emissions|NO2|AFOLU|Land|Fires|Forest Burning (Mt NO2/yr)",
    "Emissions|NO2|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt NO2/yr)",
    "Emissions|NO2|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt NO2/yr)",
    "Emissions|NO2|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt NO2/yr)",
    "Emissions|NO2|AFOLU|Land|Fires|Grassland Burning (Mt NO2/yr)",
    "Emissions|NO2|AFOLU|Land|Fires|Peat Burning (Mt NO2/yr)",
    "Emissions|NO2|Land (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture|Agricultural Soils (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture|Agricultural Soils|Decay of Crop Residues (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers|Cropland (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers|Pasture (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture|Agricultural Soils|Manure applied to Croplands (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture|Agricultural Soils|Pasture (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture|Agricultural Soils|Soil Organic Matter Loss (Mt NO2/yr)",
    "Emissions|NO2|Land|Agriculture|Animal Waste Management (Mt NO2/yr)",
    "Emissions|NO2|Land|Biomass Burning|Burning of Crop Residues (Mt NO2/yr)",
    "Emissions|NO2|Land|Peatland|Managed (Mt NO2/yr)",
    # ----- NO3- (13 vars) -----
    "Emissions|NO3-|Land (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture|Agricultural Soils (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture|Agricultural Soils|Decay of Crop Residues (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers|Cropland (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture|Agricultural Soils|Inorganic Fertilizers|Pasture (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture|Agricultural Soils|Manure applied to Croplands (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture|Agricultural Soils|Pasture (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture|Agricultural Soils|Soil Organic Matter Loss (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Agriculture|Animal Waste Management (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Biomass Burning|Burning of Crop Residues (Mt NO3-/yr)",
    "Emissions|NO3-|Land|Peatland|Managed (Mt NO3-/yr)",
    # ----- OC (9 vars) -----
    "Emissions|OC|AFOLU|Agriculture (Mt OC/yr)",
    "Emissions|OC|AFOLU|Land|Fires (Mt OC/yr)",
    "Emissions|OC|AFOLU|Land|Fires|Forest Burning (Mt OC/yr)",
    "Emissions|OC|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt OC/yr)",
    "Emissions|OC|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt OC/yr)",
    "Emissions|OC|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt OC/yr)",
    "Emissions|OC|AFOLU|Land|Fires|Grassland Burning (Mt OC/yr)",
    "Emissions|OC|AFOLU|Land|Fires|Peat Burning (Mt OC/yr)",
    "Emissions|OC|Land|Biomass Burning|Burning of Crop Residues (Mt OC/yr)",
    # ----- SO2 (9 vars) -----
    "Emissions|SO2|AFOLU|Agriculture (Mt SO2/yr)",
    "Emissions|SO2|AFOLU|Land|Fires (Mt SO2/yr)",
    "Emissions|SO2|AFOLU|Land|Fires|Forest Burning (Mt SO2/yr)",
    "Emissions|SO2|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt SO2/yr)",
    "Emissions|SO2|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt SO2/yr)",
    "Emissions|SO2|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt SO2/yr)",
    "Emissions|SO2|AFOLU|Land|Fires|Grassland Burning (Mt SO2/yr)",
    "Emissions|SO2|AFOLU|Land|Fires|Peat Burning (Mt SO2/yr)",
    "Emissions|SO2|Land|Biomass Burning|Burning of Crop Residues (Mt SO2/yr)",
    # ----- VOC (9 vars) -----
    "Emissions|VOC|AFOLU|Agriculture (Mt VOC/yr)",
    "Emissions|VOC|AFOLU|Land|Fires (Mt VOC/yr)",
    "Emissions|VOC|AFOLU|Land|Fires|Forest Burning (Mt VOC/yr)",
    "Emissions|VOC|AFOLU|Land|Fires|Forest Burning|Boreal Forest (Mt VOC/yr)",
    "Emissions|VOC|AFOLU|Land|Fires|Forest Burning|Temperate Forest (Mt VOC/yr)",
    "Emissions|VOC|AFOLU|Land|Fires|Forest Burning|Tropical Forest (Mt VOC/yr)",
    "Emissions|VOC|AFOLU|Land|Fires|Grassland Burning (Mt VOC/yr)",
    "Emissions|VOC|AFOLU|Land|Fires|Peat Burning (Mt VOC/yr)",
    "Emissions|VOC|Land|Biomass Burning|Burning of Crop Residues (Mt VOC/yr)"
  )
  missing_emi <- setdiff(emi_vars, magclass::getNames(rep))
  if (length(missing_emi) > 0) {
    warning(sprintf(
      "Missing %d AFOLU emission variables in mif, skipped: %s",
      length(missing_emi), paste(missing_emi, collapse = "; ")
    ))
    emi_vars <- intersect(emi_vars, magclass::getNames(rep))
  }
  emi <- rep[, , emi_vars]

  # Drop World row if present; keep only h12.
  h12_present <- intersect(.H12, magclass::getRegions(price))
  price <- price[h12_present, , ]
  emi   <- emi[h12_present, , ]

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
  h12_for <- ifelse(resCy %in% .EU28, "EUR", resCy)
  ok      <- h12_for %in% h12_present
  resCy   <- resCy[ok]
  h12_for <- h12_for[ok]

  price_resCy <- .broadcastToResCy(price, resCy, h12_for)
  emi_resCy   <- .broadcastToResCy(emi,   resCy, h12_for)

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
  year_cols <- as.character(magclass::getYears(price_resCy, as.integer = TRUE))
  header    <- paste(c("dummy", "dummy", year_cols), collapse = ",")

  # ---- 5a. write iPrices_magpie.csv   (region, SBS, years...)
  price_arr <- as.array(price_resCy)[, , 1, drop = FALSE]   # resCy x year x 1
  price_mat <- price_arr[, , 1]                             # resCy x year
  rows_p <- vector("list", length = nrow(price_mat) * length(SBS))
  k <- 1
  for (r in rownames(price_mat)) {
    vals <- formatC(price_mat[r, ], digits = 6, format = "g")
    for (s in SBS) {
      rows_p[[k]] <- paste(c(r, s, vals), collapse = ",")
      k <- k + 1
    }
  }
  dir.create(dirname(outCsvPath), showWarnings = FALSE, recursive = TRUE)
  con <- file(outCsvPath, open = "w")
  writeLines(header, con)
  writeLines(unlist(rows_p), con)
  close(con)
  message(sprintf(
    "[coupleMagpieToProm] wrote iPrices_magpie.csv: %s  (%d regions x %d SBS x %d years)",
    outCsvPath, nrow(price_mat), length(SBS), length(year_cols)
  ))

  # ---- 5b. write iEmissions_magpie.csv (region, variable, years...)
  # Strip "(Mt X/yr)" suffix from variable names so the CSV stores the variable
  # stem only — matches the convention in the previous linkPromToMagpie.R output.
  emi_names_raw  <- magclass::getNames(emi_resCy)
  emi_names_stem <- trimws(sub("\\s*\\([^)]*\\)\\s*$", "", emi_names_raw))

  emi_arr <- as.array(emi_resCy)                            # resCy x year x variable
  rows_e  <- vector("list", length = dim(emi_arr)[1] * dim(emi_arr)[3])
  k <- 1
  for (r in dimnames(emi_arr)[[1]]) {
    for (i in seq_along(emi_names_raw)) {
      vals <- formatC(emi_arr[r, , i], digits = 6, format = "g")
      rows_e[[k]] <- paste(c(r, emi_names_stem[i], vals), collapse = ",")
      k <- k + 1
    }
  }
  dir.create(dirname(outEmissionsCsvPath), showWarnings = FALSE, recursive = TRUE)
  con <- file(outEmissionsCsvPath, open = "w")
  writeLines(header, con)
  writeLines(unlist(rows_e), con)
  close(con)
  message(sprintf(
    "[coupleMagpieToProm] wrote iEmissions_magpie.csv: %s  (%d regions x %d vars x %d years)",
    outEmissionsCsvPath, dim(emi_arr)[1], dim(emi_arr)[3], length(year_cols)
  ))

  invisible(list(prices = outCsvPath, emissions = outEmissionsCsvPath))
}

# ------- helper: broadcast h12-resolution magpie to 39 OPEN-PROM resCy regions
#
# `m`        : magpie at h12 scale (regions x year x [variable])
# `resCy`    : character vector of resCy region codes
# `h12_for`  : same length as resCy; the h12 region each resCy pulls from
#
# Returns a magpie at resCy x year x [variable] with values copied from parent
# h12. Preserves variable names on dim 3. Resolves the getItems<- deprecation
# via direct dimnames assignment on the resulting array.
.broadcastToResCy <- function(m, resCy, h12_for) {
  years <- magclass::getYears(m)
  vars  <- magclass::getNames(m)
  if (is.null(vars)) vars <- "value"

  arr <- array(
    NA_real_,
    dim      = c(length(resCy), length(years), length(vars)),
    dimnames = list(resCy, years, vars)
  )
  m_arr <- as.array(m)
  if (length(dim(m_arr)) == 2) dim(m_arr) <- c(dim(m_arr), 1)
  dimnames(m_arr)[[3]] <- vars

  for (i in seq_along(resCy)) {
    arr[i, , ] <- m_arr[h12_for[i], , ]
  }
  magclass::as.magpie(arr, spatial = 1, temporal = 2)
}
