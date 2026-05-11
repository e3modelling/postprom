# =============================================================================
# build_climatewatch_eu28_weights.R
# -----------------------------------------------------------------------------
# Maintainer tool for the OPEN-PROM <-> MAgPIE coupling backward bridge.
#
# At runtime, the 28 EU28 country weights live inlined in
#   postprom/R/couplePromWithMagpie.R    (function .loadCwWeights)
# This script is NOT loaded by the package at runtime. It is run manually,
# only when the ClimateWatch upstream source data is updated, by maintainers
# who have access to the OneDrive CW source csv.
#
# USAGE (internal, one-time refresh)
# ----------------------------------
#   source("postprom/inst/scripts/build_climatewatch_eu28_weights.R")
#   refreshFromUpstream()
# then copy the printed code block and use it to replace the body of
# `cwLulucfCo2` in .loadCwWeights() in postprom/R/couplePromWithMagpie.R;
# commit the change.
#
# Weight scope
# ------------
# Produces signed country weights for exactly ONE emission variable in the
# AFOLU disaggregation pipeline:
#
#     Emissions|CO2|Land|+|Indirect (Mt CO2/yr)
#
# All other emission variables use MAgPIE cell-level land-use weights
# (see .buildCellLevelWeights() in R/couplePromWithMagpie.R; per-leaf
# weight assignments in inst/extdata/magpie-afolu-emission-variables.csv).
#
# WHY INDIRECT NEEDS A SPECIAL WEIGHT
# -----------------------------------
# `Indirect` represents the Grassi NGHGI-aligned managed-forest net carbon
# sink. The other 189 MAgPIE emission variables use MAgPIE's own cell-level
# land-area distributions as country weights, which works well when the
# emission factor per Mha is roughly uniform across EU climate zones
# (peatland drainage, agricultural soil N flux, etc.).
#
# For Indirect specifically, that assumption breaks: per-Mha forest carbon
# sink density varies ~8x across EU climate zones, since older boreal forest
# (SWE, FIN) is at near-stem-volume saturation while temperate forest (FRA,
# DEU) is still actively accumulating biomass. A uniform-area weight gives:
#
#   country / EU28 (forest area share, MAgPIE)   vs   actual NGHGI flux share
#   ───────────────────────────────────────────────────────────────────────
#   SWE     ~17%                                       ~6%   (over-weighted)
#   FIN     ~16%                                       ~3%   (over-weighted)
#   FRA      ~9%                                      ~25%   (under-weighted)
#   POL      ~5%                                      ~20%   (under-weighted)
#
# Using ClimateWatch LULUCF CO2 historical country values directly as the
# weight (signed values) preserves the actual NGHGI per-country flux
# pattern, since CW LULUCF and MAgPIE Indirect measure conceptually the
# same quantity (managed-forest net flux ≈ -260 Mt EU long-term).
#
# WHY YEAR 2010 (and not a multi-year average)
# --------------------------------------------
# ClimateWatch's "annual" data is segment-filled (not raw NGHGI): each
# country has only ~6-28 distinct values across the 1990-2023 window, with
# ~5-25 year plateaus and abrupt jumps between segments (NGHGI submission
# batches that backfill). The EU28 LULUCF CO2 sum has a step structure:
#   1990-2015 plateau : Σw ≈ -262 Mt/yr (matches NGHGI long-term sink)
#   2016-2020 segment : Σw ≈  -70 Mt/yr (regime shift, partial)
#   2021-2023 segment : Σw ≈ +130 Mt/yr (incomplete recent data)
# Multi-year averages (e.g. 2015-2023 mean) straddle the regime change and
# under-represent the true long-term EU sink. Year 2010 sits in the stable
# plateau (Σw ≈ -262, matches NGHGI), aligns with MAgPIE's baseline year,
# and avoids artifacts from averaging across step-function segments.
#
# DOWNSTREAM CONTRACT
# -------------------
# Consumed by `coupleMagpieToProm()`. For each variable mapped to
# `cw_lulucf_co2`, the runtime applies Method G (history-baseline + delta):
#     delta(t)            = MAgPIE_EUR(t) − Σ w[c]
#     country_value[c,t]  = w[c]  +  delta(t) × |w[c]| / Σ|w[c]|
# Sum-conserving exactly. Sign-preserving when |delta| < Σ|w|; for Indirect
# 2010 (delta=27, Σ|w|=332), |delta|/Σ|w|=0.08 — well within the safe band.
# =============================================================================

# EU27 + UK ISO3 codes — must match .EU28 constant in couplePromWithMagpie.R
.EU28 <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN",
           "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA",
           "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")


#' Read ClimateWatch upstream data, filter EU28 LULUCF CO2 year 2010,
#' and print the result as a paste-ready R code block.
#'
#' Internal maintenance use only. Requires access to the OneDrive CW source.
#' Output is intended to be copy-pasted into .loadCwWeights() in
#' postprom/R/couplePromWithMagpie.R.
#'
#' @return invisibly: a data.frame with columns iso, value_mtco2_yr.
refreshFromUpstream <- function() {
  cw_path <- file.path(
    "...",
    "Global Integrated Assessment Models - Documents",
    "Work/PROMETHEUS Model/madratverse/sources",
    "ClimateWatch/historical_emissions_ClimateWatch.csv"
  )

  if (!file.exists(cw_path)) {
    stop("[refreshFromUpstream] CW source csv not found at:\n  ", cw_path,
         "\nThis script is internal maintainer tooling; it requires the ",
         "OneDrive-hosted PROMETHEUS sources to be mounted.")
  }

  cat("[refreshFromUpstream]\n")
  cat("  source:", cw_path, "\n\n")

  cw <- read.csv(cw_path, stringsAsFactors = FALSE, check.names = TRUE)

  # Filter to EU28 + LULUCF + CO2
  cw_filt <- cw[cw$ISO    %in% .EU28                              &
                cw$Sector == "Land Use, Land-Use Change and Forestry" &
                cw$Gas    == "CO2", ]

  cat("Filtered:", nrow(cw_filt), "rows (expect 28)\n")

  missing_iso <- setdiff(.EU28, unique(cw_filt$ISO))
  if (length(missing_iso) > 0) {
    warning("EU28 countries missing: ", paste(missing_iso, collapse = ", "))
  }

  # Climate Watch year columns are X<year>; missing values stored as "N/A".
  year_col <- "X2010"
  if (!year_col %in% names(cw_filt)) {
    stop("Climate Watch csv missing expected year column: ", year_col)
  }

  cw_filt$value_mtco2_yr <- suppressWarnings(as.numeric(cw_filt[[year_col]]))

  out <- data.frame(
    iso              = cw_filt$ISO,
    value_mtco2_yr   = round(cw_filt$value_mtco2_yr, 4),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$iso), ]

  # ---- print as a paste-ready R code block (4 per line for readability) ----
  cat("\n# === paste below into .loadCwWeights() in ",
      "postprom/R/couplePromWithMagpie.R ===\n", sep = "")
  cat("cwLulucfCo2 <- c(\n")
  formatted <- sprintf("%s = %6.2f", out$iso, out$value_mtco2_yr)
  # Wrap 4 entries per line, comma-separated, with 2-space indent.
  per_line <- 4
  for (i in seq(1, nrow(out), by = per_line)) {
    chunk <- formatted[i:min(i + per_line - 1, nrow(out))]
    sep_comma <- if (i + per_line - 1 >= nrow(out)) "" else ","
    cat("  ", paste(chunk, collapse = ", "), sep_comma, "\n", sep = "")
  }
  cat(")\n")
  cat("# === end paste block ===\n\n")

  # ---- sanity print ----
  cat("Σw (signed)  =", round(sum(out$value_mtco2_yr, na.rm = TRUE), 2),
      "Mt CO2/yr  (NGHGI long-term EU LULUCF ≈ -260)\n")
  cat("Σ|w|         =", round(sum(abs(out$value_mtco2_yr), na.rm = TRUE), 2),
      "Mt CO2/yr\n\n")

  cat("Top 10 sinks (most negative):\n")
  print(head(out[order(out$value_mtco2_yr), c("iso", "value_mtco2_yr")], 10),
        row.names = FALSE)
  cat("\nDone. Copy the code block above into .loadCwWeights() and commit.\n")

  invisible(out)
}
