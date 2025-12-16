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

#' @export
helperAggregateLevel <- function(magpie_object, level, dim = 3.1, recursive = FALSE) {
  # Aggregate a dimension based on names' levels:
  # Secondary Energy|Electricity|Gas|w/o CCS, Secondary Energy|Electricity|Gas|w/ CCS
  # to Secondary Energy|Electricity|Gas
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
