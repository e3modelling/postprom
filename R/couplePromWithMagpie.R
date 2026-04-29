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

  if (!(biomassVariable %in% magclass::getNames(rep))) {
    stop(sprintf(
      "Variable %s not found in %s. Available Price* variables:\n  %s",
      biomassVariable, reportMifPath,
      paste(grep("[Pp]rice", magclass::getNames(rep), value = TRUE), collapse = "\n  ")
    ))
  }
  price <- rep[, , biomassVariable]             # h12 + World, years x 1

  # Land-use / AFOLU emission variables. No unit conversion: the csv preserves
  # MAgPIE's native "Mt <pollutant>/yr" units so downstream consumers can
  # decide what to do. The file is not currently $include'd by OPEN-PROM GAMS
  # code (see notes/TASK7-implementation-plan.md §2.5).
  emi_vars <- c(
    "Emissions|CO2|Land (Mt CO2/yr)",
    "Emissions|BC|AFOLU|Land|Fires (Mt BC/yr)",
    "Emissions|CH4|Land (Mt CH4/yr)",
    "Emissions|CO|AFOLU|Land|Fires (Mt CO/yr)",
    "Emissions|N2O|Land (Mt N2O/yr)",
    "Emissions|NH3|Land (Mt NH3/yr)",
    "Emissions|NO2|Land (Mt NO2/yr)",
    "Emissions|NO3-|Land (Mt NO3-/yr)",
    "Emissions|OC|AFOLU|Land|Fires (Mt OC/yr)",
    "Emissions|SO2|AFOLU|Land|Fires (Mt SO2/yr)",
    "Emissions|VOC|AFOLU|Land|Fires (Mt VOC/yr)"
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
