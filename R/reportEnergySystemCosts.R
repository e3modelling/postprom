#' Report energy system costs and demand-side fuel prices
#'
#' Reports demand-side fuel costs from an OPEN-PROM GDX in two alternative
#' variants (do not add them together):
#' \itemize{
#'   \item including carbon price: VmFinalEnergy * VmPriceFuelSubsecCarVal
#'   \item excluding carbon price: VmFinalEnergy * (VmPriceFuelSubsecCarVal - VmPriceCarbon)
#' }
#' Mtoe * k$2015/toe = billion US$2015/yr. Each variant is reported as a total,
#' per sector (e.g. Industry), and per subsector (e.g. Industry|Chemicals).
#' Fuel prices per demand subsector and fuel are also reported in k$2015/toe:
#' VmPriceFuelSubsecCarVal (including carbon tax), VmPriceCarbon (fuel carbon
#' tax), and their difference (excluding carbon tax).
#'
#' Variable names must not contain "." (magclass sub-dimension separator) and must
#' not end in " (...)" (write.report reads that as "Name (unit)" and breaks the
#' Unit column of the whole MIF); hence "w/" and "w/o" instead of "(incl. ...)".
#'
#' @param path Character string specifying the file path to the GDX data file.
#' @param regions Character vector of region names used to subset the data.
#' @param years Character vector of years to include in the report.
#'
#' @return A magpie object with energy-system-cost totals, sector and subsector
#'   breakdowns, and fuel-price variables.
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
#' @importFrom magclass getItems add_dimension mbind as.magpie
#' @importFrom gdxrrw rgdx.set
#' @export
reportEnergySystemCosts <- function(path, regions, years) {
  escFuelCostsAndPrices(path, regions, years)
}

# Names --------------------------------------------------------------------------------------------
# Change the reported names here. Rules (checked in escToMagpie):
# - no "." (magclass sub-dimension separator);
# - no name may end in " (...)": write.report treats that as "Name (unit)" and
#   then writes Unit = N/A for every variable in the MIF.
# Price names must start with "Price|" so that aggregateMIF averages them (GDP-weighted)
# instead of summing them across regions.
escNames <- list(
  costIncluding  = "Energy System Cost w/ Carbon Price",
  costExcluding  = "Energy System Cost w/o Carbon Price",
  priceIncluding = "Price|Final Fuel w/ Carbon Tax",
  priceCarbon    = "Price|Fuel Carbon Tax",
  priceExcluding = "Price|Final Fuel w/o Carbon Tax"
)

# Helpers ------------------------------------------------------------------------------------------
escFuelCostsAndPrices <- function(path, regions, years) {
  finalEnergy <- readGDX(path, "VmFinalEnergy", field = "l")[regions, years, ]           # Mtoe
  fuelPrice <- readGDX(path, "VmPriceFuelSubsecCarVal", field = "l")[regions, years, ]   # k$2015/toe
  carbonPrice <- readGDX(path, "VmPriceCarbon", field = "l")[regions, years, ]           # k$2015/toe

  feArray <- as.array(finalEnergy)
  if (any(!is.finite(feArray))) {
    stop("Energy system cost: VmFinalEnergy contains missing or non-finite values.")
  }
  dn <- dimnames(feArray)[1:2]
  tol <- 1e-6 # Mtoe

  # Demand subsector x fuel pairs that the model allows (SECtoEF). readGDX restores
  # zeros over the full DSBS x EFS grid, so without this filter every combination
  # would be reported. VmFinalEnergy is fixed to 0 outside SECtoEF.
  dsbs <- rgdx.set(path, "DSBS", te = TRUE)
  secToEF <- rgdx.set(path, "SECtoEF", te = FALSE)
  secToEF <- secToEF[as.character(secToEF[[1]]) %in% as.character(dsbs[[1]]), ]
  items <- intersect(getItems(finalEnergy, 3), paste(secToEF[[1]], secToEF[[2]], sep = "."))
  outside <- setdiff(getItems(finalEnergy, 3), items)
  if (length(outside) > 0 && any(abs(feArray[, , outside, drop = FALSE]) > tol)) {
    warning("Energy system cost: VmFinalEnergy is nonzero outside SECtoEF; not costed.", call. = FALSE)
  }

  quantity <- escAlign(finalEnergy, items, dn)
  price <- escAlign(fuelPrice, items, dn)
  carbon <- escAlign(carbonPrice, items, dn)
  priceExCarbon <- price - carbon

  # Checks: readGDX returns missing prices as 0, so look for zeros, not NAs.
  # SOL and GEO are priced at 0 by the model on purpose (core/preloop.gms).
  subsec <- sub("\\..*$", "", items)
  fuelCode <- sub("^[^.]*\\.", "", items)
  used <- quantity > tol
  pricedFuel <- rep(!fuelCode %in% c("SOL", "GEO"), each = prod(dim(quantity)[1:2]))
  escWarnItems(used & price <= 0 & pricedFuel, items,
    "zero or missing VmPriceFuelSubsecCarVal for nonzero final energy (costed at 0)")
  escWarnItems(used & priceExCarbon < 0, items,
    "VmPriceFuelSubsecCarVal - VmPriceCarbon is negative")

  costIncluding <- quantity * price
  costExcluding <- quantity * priceExCarbon

  # Names
  sectorMap <- escSectorMap(path, dsbs)
  idx <- match(subsec, sectorMap$DSBS)
  if (anyNA(idx)) {
    stop("Energy system cost: unmapped demand subsectors: ", paste(unique(subsec[is.na(idx)]), collapse = ", "))
  }
  level1 <- sectorMap$level1[idx]
  level2 <- sectorMap$level2[idx]
  sectorPath <- ifelse(is.na(level2), level1, paste(level1, level2, sep = "|"))

  efs <- rgdx.set(path, "EFS", te = TRUE)
  fuelLabels <- stats::setNames(escClean(efs$.te), as.character(efs[[1]]))
  fuel <- unname(fuelLabels[fuelCode])
  fuel[is.na(fuel) | fuel == ""] <- fuelCode[is.na(fuel) | fuel == ""]

  # Energy system cost: total, sector, subsector (no fuel level)
  costRows <- function(root) {
    n <- length(items)
    rel <- data.frame(
      item = rep(seq_len(n), 3),
      row = c(rep(root, n),
              paste(root, level1, sep = "|"),
              ifelse(is.na(level2), NA_character_, paste(root, level1, level2, sep = "|"))),
      stringsAsFactors = FALSE
    )
    rel[!is.na(rel$row), ]
  }
  cost <- mbind(
    escToMagpie(escSumRows(costIncluding, costRows(escNames$costIncluding)), "billion US$2015/yr"),
    escToMagpie(escSumRows(costExcluding, costRows(escNames$costExcluding)), "billion US$2015/yr")
  )

  # Prices: one row per demand subsector and fuel
  priceRowNames <- paste(sectorPath, fuel, sep = "|")
  dupNames <- unique(priceRowNames[duplicated(priceRowNames)])
  if (length(dupNames) > 0) {
    stop("Energy system cost: duplicate price names: ", paste(dupNames, collapse = ", "))
  }
  priceRows <- function(values, root) {
    dimnames(values)[[3]] <- paste(root, priceRowNames, sep = "|")
    values
  }
  prices <- escToMagpie(
    escBind(
      priceRows(price, escNames$priceIncluding),
      priceRows(carbon, escNames$priceCarbon),
      priceRows(priceExCarbon, escNames$priceExcluding)
    ),
    "k$2015/toe"
  )

  mbind(cost, prices)
}

# Demand subsector -> level 1 (sector) and level 2 (subsector), following the
# Final Energy naming (e.g. Industry|Chemicals, Transportation|Passenger Transport - Cars).
# Subsectors outside the groups below (Residential; Agriculture, Fishing, Forestry)
# are a sector of their own with no level 2.
escSectorMap <- function(path, dsbs) {
  code <- as.character(dsbs[[1]])
  label <- escClean(dsbs$.te)
  label[is.na(label) | label == ""] <- code[is.na(label) | label == ""]

  level1 <- rep(NA_character_, length(code))
  level1[code %in% as.character(readGDX(path, "INDSE"))] <- "Industry"
  level1[code %in% as.character(readGDX(path, "TRANSE"))] <- "Transportation"
  level1[code %in% c("BAV", "BMAR")] <- "Bunkers"
  level1[code %in% c("SE", "ICT")] <- "Commercial"
  level1[code %in% c("PCH", "NEN")] <- "Non-Energy Use"
  level1[code %in% as.character(readGDX(path, "CDR"))] <- "Carbon Management"

  ownSector <- is.na(level1)
  level2 <- ifelse(ownSector, NA_character_, label)
  level1[ownSector] <- label[ownSector]
  data.frame(DSBS = code, level1 = level1, level2 = level2, stringsAsFactors = FALSE)
}

# Remove "(...)" and "." from set texts ("." would break magclass names).
escClean <- function(x) {
  x <- gsub("\\s*\\([^)]*\\)", "", as.character(x))
  trimws(gsub("\\.", "", x))
}

# magpie -> region x year x items array, with 0 for missing items or values.
escAlign <- function(x, items, dn) {
  arr <- as.array(x)
  out <- array(0, dim = c(length(dn[[1]]), length(dn[[2]]), length(items)),
    dimnames = list(dn[[1]], dn[[2]], items))
  idx <- match(items, dimnames(arr)[[3]])
  ok <- !is.na(idx)
  if (any(ok)) {
    out[, , ok] <- arr[dn[[1]], dn[[2]], idx[ok], drop = FALSE]
  }
  out[!is.finite(out)] <- 0
  out
}

# Sum items into rows; rel has one line per (item index, row name).
escSumRows <- function(values, rel) {
  rows <- unique(rel$row)
  weights <- matrix(0, nrow = dim(values)[3], ncol = length(rows))
  weights[cbind(rel$item, match(rel$row, rows))] <- 1
  flat <- matrix(values, ncol = dim(values)[3]) # (region x year) x item
  array(flat %*% weights, dim = c(dim(values)[1:2], length(rows)),
    dimnames = c(dimnames(values)[1:2], list(rows)))
}

# Bind region x year x item arrays along the item dimension.
escBind <- function(...) {
  parts <- list(...)
  array(unlist(parts), dim = c(dim(parts[[1]])[1:2], sum(vapply(parts, function(p) dim(p)[3], 1))),
    dimnames = c(dimnames(parts[[1]])[1:2], list(unlist(lapply(parts, function(p) dimnames(p)[[3]])))))
}

escToMagpie <- function(values, unit) {
  bad <- grepl(".", dimnames(values)[[3]], fixed = TRUE) | grepl(" \\(.*\\)$", dimnames(values)[[3]])
  if (any(bad)) {
    stop("Energy system cost: names must not contain '.' or end in ' (...)': ",
      paste(dimnames(values)[[3]][bad], collapse = ", "))
  }
  dimnames(values) <- list(region = dimnames(values)[[1]], year = dimnames(values)[[2]],
    variable = dimnames(values)[[3]])
  add_dimension(as.magpie(values, spatial = 1, temporal = 2), dim = 3.2, add = "unit", nm = unit)
}

escWarnItems <- function(mask, items, message) {
  if (any(mask)) {
    hit <- items[apply(mask, 3, any)]
    warning("Energy system cost: ", message, ": ", paste(hit, collapse = ", "), call. = FALSE)
  }
}