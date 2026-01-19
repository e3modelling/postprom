# Helpers functions -------------------------------------------------

#' @export
helperRenameItems <- function(magpie, mapping, prefix, dim = 3.1) {
  # Rename magpie from OPEN-PROM Efs to labels (e.g. Gas -> Secondary Energy|Electricity|)
  # and aggregate in case of duplicates (Coal from lignite and Hard coal to single Coal)
  magpie <- toolAggregate(
    magpie,
    dim = 3, rel = mapping, from = "Tech", to = "EF"
  )

  name <- getItems(magpie, dim)
  title <- if (!is.null(prefix)) paste0(prefix, name) else name
  getItems(magpie, 3) <- title
  return(magpie)
}

#' Aggregate magpie dimension by hierarchical name levels
#'
#' Aggregates a magpie object along a given dimension by truncating
#' pipe-separated (\code{"|"}) item names to a specified hierarchical level.
#'
#' For example, variables such as
#' \code{"Secondary Energy|Electricity|Gas|w/o CCS"} and
#' \code{"Secondary Energy|Electricity|Gas|w/ CCS"} can be aggregated to
#' \code{"Secondary Energy|Electricity|Gas"} by choosing \code{level = 3}.
#'
#' The hierarchy is inferred from the number of \code{"|"} separators in the
#' item names. Aggregation is performed using \code{\link{toolAggregate}}.
#'
#' @param magpie_object A \code{magpie} object to be aggregated.
#' @param level Integer specifying the hierarchical depth to retain. All
#'   name components below this level are aggregated. Must be smaller than the
#'   maximum depth present in the data.
#' @param dim Numeric dimension identifier of the magpie object to aggregate
#'   (e.g. \code{3.1} for variables). Defaults to \code{3.1}.
#' @param recursive Logical. If \code{FALSE} (default), aggregation is performed
#'   directly to the specified level. If \code{TRUE}, aggregation maps for all
#'   intermediate levels between the maximum depth and \code{level} are combined
#'   before aggregation.
#'
#' @return A \code{magpie} object aggregated to the specified hierarchical level.
#'   If \code{level} is greater than or equal to the maximum depth of the item
#'   names, the original object is returned unchanged.
#'
#' @details
#' Item names are assumed to follow a hierarchical structure separated by
#' \code{"|"} characters. The function truncates names to the requested level
#' and aggregates all matching entries accordingly.
#'
#' Recursive aggregation is mathematically equivalent to direct aggregation and
#' is mainly intended for compatibility with workflows that explicitly require
#' multi-level mapping.
#'
#' @examples
#' \dontrun{
#' helperAggregateLevel(x, level = 3)
#' helperAggregateLevel(x, level = 2, recursive = TRUE)
#' }
#'
#' @importFrom magclass is.magpie getItems
#' @importFrom madrat toolAggregate
#' @importFrom dplyr bind_rows filter
#' @importFrom stringr str_extract
#' @export
helperAggregateLevel <- function(magpie_object, level, dim = 3.1, recursive = FALSE) {
  if (!is.magpie(magpie_object)) stop("Input must be a magpie object")
  if (!is.numeric(level) || level < 1) stop("Level must be a positive integer")

  full_names <- getItems(magpie_object, dim = dim)
  max_depth <- max(sapply(strsplit(full_names, "\\|"), length))

  if (level >= max_depth) {
    return(magpie_object)
  }

  if (recursive == TRUE) {
    map <- NULL
    for (lvl in seq(max_depth, level, -1)) {
      pattern <- paste0("^([^|]+\\|){", lvl - 1, "}[^|]+")
      new_names <- str_extract(full_names, pattern)
      temp_map <- data.frame(names = full_names, parent = new_names)
      map <- bind_rows(map, temp_map)
    }
  } else {
    # keep name up to the level of depth
    pattern <- paste0("^([^|]+\\|){", level - 1, "}[^|]+")
    new_names <- str_extract(full_names, pattern)

    # Aggregate based on new level structure
    map <- data.frame(names = full_names, parent = new_names)
  }

  map <- map %>% filter(!is.na(parent))
  x <- toolAggregate(
    magpie_object,
    dim = dim, rel = map, from = "names", to = "parent"
  )
  return(x)
}

#' Convert units in a magpie object to expected units
#'
#' Converts variables in a magpie object (dimension 3 items) from the units found
#' in the object to the units expected by a provided unit table. Conversions are
#' applied variable-by-variable using audited, hard-coded conversion factors for
#' a limited set of supported unit pairs (energy quantities, selected prices,
#' population scaling, and macro aggregates).
#'
#' The function updates both the numeric values (by multiplying with a conversion
#' factor) and the unit labels embedded in the third-dimension item names
#' (e.g. `"GDP (billion USD_2010/yr)"`). Unsupported conversions either stop with an 
#' error (default) or are skipped with a warning if `allowUnrecognized = TRUE`.
#'
#' @param magpieObj A magpie object whose third-dimension item names follow the
#'   pattern `"variable (unit)"`. Values will be scaled in-place for variables
#'   requiring conversion.
#' @param unitTable A data.frame with (at least) the columns:
#'   `variable`, `magpieUnit`, `expectedUnit`, and `unitMatches`.
#'   Rows with `unitMatches` not `TRUE` are treated as mismatches to convert.
#'   `variable` should match the variable name *without* the trailing `" (unit)"`
#'   part in the magpie object.
#' @param usd2015to2010 Numeric scalar deflator to convert values from USD_2015 to
#'   USD_2010 (multiplicative factor). Used for currency and price conversions.
#' @param allowUnrecognized Logical. If `FALSE` (default), unsupported unit pairs
#'   cause an error and the function stops. If `TRUE`, unsupported conversions are
#'   skipped (with a warning) and the variable name is recorded in `skipped`.
#' @param quiet Logical. If `FALSE` (default), prints a short summary of how many
#'   variables were converted and shows the first rows of the conversion log.
#'
#' @return A list with:
#' \describe{
#'   \item{object}{The converted magpie object (same class as input).}
#'   \item{log}{A data.frame recording each successful conversion with columns
#'     `variable`, `fromUnit`, `toUnit`, `factorUsed`.}
#'   \item{skipped}{Character vector of variables skipped due to unsupported unit
#'     pairs (only non-empty when `allowUnrecognized = TRUE`).}
#' }
#' @examples
#' \dontrun{
#' # unitTable is expected to have: variable, magpieUnit, expectedUnit, unitMatches
#' res <- convertUnitsToExpected(
#'   magpieObj        = x,
#'   unitTable        = unitTable,
#'   usd2015to2010    = 0.93,
#'   allowUnrecognized = TRUE
#' )}
#' @importFrom magclass getItems
#' @export
convertUnitsToExpected <- function(magpieObj, unitTable, usd2015to2010, 
                          allowUnrecognized = FALSE, quiet = FALSE) {

  trim <- function(x) trimws(as.character(x))
  cleanVar <- function(x) sub("\\s*\\(.*\\)$", "", x) # drop trailing " (unit)"
  safeEq <- function(a, b) trim(a) == trim(b)

  # normalize unit strings (case-insensitive)
  normUnit <- function(u) {
    u <- trim(u)
    u <- gsub("KWh", "kWh", u, fixed = TRUE) # harmonize capitalization
    u <- gsub("US\\$","USD_", u)             # normalize currency prefix
    u <- gsub("USD__", "USD_", u)            # double underscore guard
    u <- gsub("tn CO2", "t CO2", u, fixed = TRUE) # treat tn and t as same
    u
  }

  # exact constants (audited)
  TOE_PER_GJ   <- 1/41.868             # toe per GJ (for per-energy prices)
  GJ_PER_TOE   <- 41.868               # GJ per toe
  EJ_PER_MTOE  <- 0.041868             # EJ per Mtoe
  EJ_PER_TWH   <- 0.0036               # EJ per TWh
  GJ_PER_KWH   <- 0.0036               # GJ per kWh
  KWH_PER_GJ   <- 1 / GJ_PER_KWH       # 277.7777777778
  BILLION_TO_MILLION <- 1000

  # compute factor for one pair of units
  computeFactor <- function(fromU, toU) {
    fromU0 <- normUnit(fromU)
    toU0   <- normUnit(toU)

    # identical after normalization -> factor 1
    if (fromU0 == toU0) return(1)

    # ---- energy quantity flows ----
    # Mtoe -> EJ(/yr)
    if (fromU0 == "Mtoe" && grepl("^EJ", toU0)) return(EJ_PER_MTOE)

    # TWh -> EJ(/yr)
    if (fromU0 == "TWh" && grepl("^EJ", toU0)) return(EJ_PER_TWH)

    # ---- population ----
    if (fromU0 == "billion" && toU0 == "million") return(BILLION_TO_MILLION)

    # ---- prices ----
    # KUSD_2015/toe -> USD_2010/GJ
    if (fromU0 == "KUSD_2015/toe" && toU0 == "USD_2010/GJ")
      return(1000 * usd2015to2010 * (1 / GJ_PER_TOE))  # ×1000 (KUSD->USD) × deflator × 1/41.868

    # USD_2015/kWh -> USD_2010/GJ
    if (fromU0 == "USD_2015/kWh" && toU0 == "USD_2010/GJ")
      return(usd2015to2010 * KWH_PER_GJ)               # × deflator × 277.777...

    # US$2015/KWh cases may have been normalized above
    if (fromU0 == "USD_2015/kWh" && toU0 == "USD_2010/GJ")
      return(usd2015to2010 * KWH_PER_GJ)

    # Carbon price: USD_2015/t CO2 -> USD_2010/t CO2
    if (fromU0 == "USD_2015/t CO2" && toU0 == "USD_2010/t CO2")
      return(usd2015to2010)

    # Some sources write USD_2015/tn CO2; normalized to t CO2 above
    if (fromU0 == "USD_2015/t CO2" && toU0 == "USD_2010/t CO2")
      return(usd2015to2010)

    # ---- macro aggregates ----
    # GDP: billion USD_2015/yr -> billion USD_2010/yr
    if (fromU0 == "billion USD_2015/yr" && toU0 == "billion USD_2010/yr")
      return(usd2015to2010)

    # ---- unsupported pair ----
    return(NA_real_)
  }

  # current 3rd-dimension item names and their clean labels
  curNames <- magclass::getItems(magpieObj, dim = 3)
  curClean <- cleanVar(curNames)

  # keep only rows that actually differ and exist in the object
  unitTable$variable    <- trim(unitTable$variable)
  unitTable$magpieUnit  <- trim(unitTable$magpieUnit)
  unitTable$expectedUnit<- trim(unitTable$expectedUnit)

  toFix <- subset(unitTable, !isTRUE(unitMatches))
  toFix <- toFix[toFix$variable %in% curClean, , drop = FALSE]

  if (nrow(toFix) == 0L) {
    if (!quiet) message("No unit mismatches found to convert.")
    return(list(object = magpieObj, log = data.frame(), skipped = character()))
  }

  # containers for audit
  audit <- list()
  skipped <- character()

  # perform conversions variable by variable
  for (i in seq_len(nrow(toFix))) {
    v   <- toFix$variable[i]
    uIn <- toFix$magpieUnit[i]
    uEx <- toFix$expectedUnit[i]

    idx <- which(curClean == v)
    if (!length(idx)) next

    factor <- computeFactor(uIn, uEx)

    if (is.na(factor)) {
      msg <- paste0("Unsupported conversion: '", uIn, "' -> '", uEx, "' for variable '", v, "'.")
      if (allowUnrecognized) {
        warning(msg)
        skipped <- c(skipped, v)
        next
      } else {
        stop(msg, call. = FALSE)
      }
    }

    # scale values
    magpieObj[,, idx] <- magpieObj[,, idx] * factor

    # update unit label in the 3rd-dimension name(s)
    curNames[idx] <- paste0(v, " (", uEx, ")")

    # add to audit
    audit[[length(audit) + 1]] <- data.frame(
      variable    = v,
      fromUnit    = uIn,
      toUnit      = uEx,
      factorUsed  = factor,
      stringsAsFactors = FALSE
    )
  }
  getItems(magpieObj, dim = 3) <- curNames

  auditDf <- if (length(audit)) do.call(rbind, audit) else data.frame()

  if (!quiet && nrow(auditDf)) {
    message("Converted ", nrow(auditDf), " variables. Example:\n",
            utils::capture.output(print(utils::head(auditDf), row.names = FALSE)) |> paste(collapse = "\n"))
    if (length(skipped)) message("Skipped (unsupported): ", paste(unique(skipped), collapse = "; "))
  }

  return(list(object = magpieObj, log = auditDf, skipped = unique(skipped)))
}