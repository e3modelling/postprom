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
