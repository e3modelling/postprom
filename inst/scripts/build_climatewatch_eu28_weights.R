# =============================================================================
# build_climatewatch_eu28_weights.R
# -----------------------------------------------------------------------------
# Build-time tooling for the OPEN-PROM <-> MAgPIE coupling backward bridge.
# Generates a narrow EU28 country-weight csv used by exactly ONE emission
# variable in the disaggregation pipeline:
#
#     Emissions|CO2|Land|+|Indirect (Mt CO2/yr)
#
# All other emission variables use MAgPIE cell-level land-use weights
# (see .buildCellLevelWeights() in R/couplePromWithMagpie.R; per-leaf
# weight assignments in inst/extdata/magpie-afolu-emission-variables.csv).
#
# Run:  Rscript postprom/inst/scripts/build_climatewatch_eu28_weights.R
#
# OUTPUT
# ------
#   postprom/inst/extdata/climatewatch-eu28-lulucf-co2-weights.csv
#   28 rows (one per EU27+UK country). Columns:
#     iso              : ISO3 (EU27 + UK)
#     weight_source    : "cw_lulucf_co2"  (matches mapping csv key)
#     value_mtco2_yr   : 2010 NGHGI net LULUCF CO2 flux, signed (-=sink)
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

# Climate Watch source path (madrat sources, local OneDrive mount)
cw_path <- file.path(
  "...",
  "Global Integrated Assessment Models - Documents",
  "Work/PROMETHEUS Model/madratverse/sources",
  "ClimateWatch/historical_emissions_ClimateWatch.csv"
)

out_path <- "postprom/inst/extdata/climatewatch-eu28-lulucf-co2-weights.csv"

# EU27 + UK ISO3 codes — must match .EU28 constant in couplePromWithMagpie.R
EU28 <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN",
          "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA",
          "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")

stopifnot(file.exists(cw_path))

cat("[build_climatewatch_eu28_weights]\n")
cat("  source:", cw_path, "\n")
cat("  output:", out_path, "\n\n")

cw <- read.csv(cw_path, stringsAsFactors = FALSE, check.names = TRUE)

# Filter to EU28 + LULUCF + CO2 + year 2010
cw_filt <- cw[cw$ISO    %in% EU28                                &
              cw$Sector == "Land Use, Land-Use Change and Forestry" &
              cw$Gas    == "CO2", ]

cat("Filtered:", nrow(cw_filt), "rows (expect 28)\n")

missing_iso <- setdiff(EU28, unique(cw_filt$ISO))
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
  iso             = cw_filt$ISO,
  weight_source   = "cw_lulucf_co2",
  value_mtco2_yr  = round(cw_filt$value_mtco2_yr, 4),
  stringsAsFactors = FALSE
)
out <- out[order(out$iso), ]

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write.csv(out, out_path, row.names = FALSE, quote = TRUE)

cat("Wrote:\n  ", out_path, "(", nrow(out), "rows)\n\n")

# Sanity print
cat("Σw (signed)  =", round(sum(out$value_mtco2_yr, na.rm = TRUE), 2),
    "Mt CO2/yr  (NGHGI long-term EU LULUCF ≈ -260)\n")
cat("Σ|w|         =", round(sum(abs(out$value_mtco2_yr), na.rm = TRUE), 2),
    "Mt CO2/yr\n\n")

cat("Top 10 sinks (most negative):\n")
print(head(out[order(out$value_mtco2_yr), c("iso", "value_mtco2_yr")], 10),
      row.names = FALSE)

cat("\nDone.\n")
