# Helpers functions -------------------------------------------------

#' @export
helperRenameItems <- function(magpie, mapping, prefix, dim = 3.1) {
  # Rename magpie from OPEN-PROM Efs to labels (e.g. Gas -> Secondary Energy|Electricity|)
  # and aggregate in case of duplicates (Coal from lignite and Hard coal to single Coal)
  magpie <- toolAggregate(
    magpie, dim = 3, rel = mapping, from = "Tech", to = "EF"
  )

  name <- getItems(magpie, dim)
  title <- if (!is.null(prefix)) paste0(prefix, name) else name
  getItems(magpie, 3) <- title
  return(magpie)
}

#' @export
helperAggregateLevel <- function(magpie_object, level) {
  # Aggregate a dimension based on names' levels:
  # Secondary Energy|Electricity|Gas|w/o CCS, Secondary Energy|Electricity|Gas|w/ CCS
  # to Secondary Energy|Electricity|Gas
  if (!is.magpie(magpie_object)) stop("Input must be a magpie object")
  if (!is.numeric(level) || level < 1) stop("Level must be a positive integer")

  full_names <- getItems(magpie_object, dim = 3)
  split_names <- strsplit(full_names, "\\|")
  keep_idx <- sapply(split_names, length) == level

  if (!any(keep_idx)) {
    warning(paste("No entries found with exactly", level, "levels."))
    return(NULL)
  }
  x <- magpie_object[,,keep_idx]

  parent_names <- sapply(split_names[keep_idx], function(x) paste(x[1:level-1], collapse = "|"))

  map <- data.frame(names=full_names[keep_idx], parent=parent_names)

  # Aggregate across identical parent names
  x <- toolAggregate(
    x, dim = 3, rel = map, from = "names", to = "parent"
  )
  return(x)
}
