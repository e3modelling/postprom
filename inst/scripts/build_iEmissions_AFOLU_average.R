# =============================================================================
# build_iEmissions_AFOLU_average.R
# -----------------------------------------------------------------------------
# Maintainer tool for the AFOLU regional override in R/reportEmissions.R.
#
# reportEmissions() (legacy / "exo" branch) optionally reads a temporary file
#   <gdx dir>/iEmissions_AFOLU.mif
# and, for the regions LAM and SSA, overrides the slices
#   Emissions|CO2|AFOLU      -> AFOLUCO2
#   Carbon Removal|Land Use  -> CDRCO2
#
# This script BUILDS that override file by taking the per-(region, year)
# AVERAGE of an NPI run and a 2C run. It is NOT loaded by the package at
# runtime; run it manually whenever the NPI / 2C source mifs change.
#
# USAGE
# -----
#   source("postprom/inst/scripts/build_iEmissions_AFOLU_average.R")
#   buildAfoluAverage(
#     dir     = "path/to/folder/with/the/two/mifs",
#     outFile = "path/to/iEmissions_AFOLU.mif"   # e.g. next to the gdx
#   )
#
# The two source mifs are located inside `dir` by filename pattern: one file
# matching "NPI" and one matching "2C" (case-insensitive). Override the
# patterns via the `npiPattern` / `twoCPattern` arguments if your naming
# differs.
# =============================================================================

suppressPackageStartupMessages({
  library(magclass)   # read.report, write.report, getItems, mbind
})

# Only these variables are written to the override file (what reportEmissions
# actually consumes). Names are unit-free; the "(unit)" suffix in the source
# mifs is stripped before matching.
.AFOLU_OVERRIDE_VARS <- c(
  "Emissions|CO2|AFOLU",
  "Carbon Removal|Land Use"
)

# Only these regions are overridden in reportEmissions, so only these are
# written out.
.AFOLU_OVERRIDE_REGIONS <- c("LAM", "SSA")

# ---- helpers ----------------------------------------------------------------

# Locate a single file in `dir` whose name matches `pattern` (case-insensitive).
# Errors if zero or more than one match, so the caller never silently averages
# the wrong file.
.findOne <- function(dir, pattern, label) {
  hits <- list.files(dir, pattern = pattern, ignore.case = TRUE, full.names = TRUE)
  hits <- hits[grepl("\\.mif$", hits, ignore.case = TRUE)]
  if (length(hits) == 0) {
    stop(sprintf("No %s mif (pattern '%s') found in %s", label, pattern, dir))
  }
  if (length(hits) > 1) {
    stop(sprintf(
      "Ambiguous %s mif (pattern '%s') in %s:\n  %s",
      label, pattern, dir, paste(basename(hits), collapse = "\n  ")
    ))
  }
  hits
}

# Read a mif -> magpie object, drop the model/scenario list wrapper, strip the
# "(unit)" suffix from variable names, and subset to the override regions/vars
# that are present. Returns NULL-free, dimension-clean magpie.
.readAfolu <- function(file) {
  x <- read.report(file)[[1]][[1]]
  getItems(x, 3.1) <- trimws(gsub("\\s*\\(.*\\)$", "", getItems(x, dim = 3)))

  regs <- intersect(.AFOLU_OVERRIDE_REGIONS, getItems(x, 1))
  vars <- intersect(.AFOLU_OVERRIDE_VARS, getItems(x, 3.1))
  if (length(regs) == 0) {
    stop("None of the override regions (", paste(.AFOLU_OVERRIDE_REGIONS, collapse = ", "),
         ") found in ", basename(file))
  }
  if (length(vars) == 0) {
    stop("None of the override variables found in ", basename(file))
  }
  x[regs, , vars]
}

# ---- main -------------------------------------------------------------------

#' Build the AFOLU override mif as the average of an NPI and a 2C run.
#'
#' @param dir          Folder containing the two source mifs.
#' @param outFile      Output path for the averaged override mif.
#' @param npiPattern   Filename regex identifying the NPI run (default "NPI").
#' @param twoCPattern  Filename regex identifying the 2C run (default "2C").
#' @param scenario     Scenario label written to the output mif.
#' @param model        Model label written to the output mif.
#' @return (invisibly) the averaged magpie object.
buildAfoluAverage <- function(dir,
                              outFile,
                              npiPattern  = "NPI",
                              twoCPattern = "2C",
                              scenario    = "NPI_2C_avg",
                              model       = "OPENPROM") {
  stopifnot(dir.exists(dir))

  npiFile  <- .findOne(dir, npiPattern,  "NPI")
  twoCFile <- .findOne(dir, twoCPattern, "2C")
  message("NPI source: ", basename(npiFile))
  message("2C  source: ", basename(twoCFile))

  npi  <- .readAfolu(npiFile)
  twoC <- .readAfolu(twoCFile)

  # Average only over the regions/years/vars common to BOTH files, so a gap in
  # one run can't pull the mean toward the other run's lone value.
  regs  <- intersect(getItems(npi, 1),   getItems(twoC, 1))
  years <- intersect(getItems(npi, 2),   getItems(twoC, 2))
  vars  <- intersect(getItems(npi, 3.1), getItems(twoC, 3.1))
  if (length(years) == 0) stop("NPI and 2C mifs share no common years")

  npi  <- npi[regs, years, vars]
  twoC <- twoC[regs, years, vars]

  avg <- (npi + twoC) / 2

  write.report(avg, file = outFile, model = model, scenario = scenario)
  message("Wrote averaged AFOLU override -> ", outFile,
          "  (regions: ", paste(regs, collapse = ", "), ")")

  invisible(avg)
}
