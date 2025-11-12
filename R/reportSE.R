#' Process and Aggregate Secondary Energy Data
#'
#' This function processes and aggregates secondary energy data.
#' It maps fuel categories, aggregates data by reporting categories, and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated secondary energy data.
#'
#' @examples
#' \dontrun{
#' result <- reportSE(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind
#' @importFrom quitte as.quitte
#' @importFrom dplyr mutate %>%
#' @importFrom madrat toolAggregate
#' @export
reportSE <- function(path, regions, years) {
  rename_EF <- c(
    "LGN" = "Coal",
    "HCL" = "Coal",
    "GDO" = "Oil",
    "NGS" = "Gas",
    "BMSWAS" = "Biomass",
    "NUC" = "Nuclear",
    "HYD" = "Hydro",
    "WND" = "Wind",
    "SOL" = "Solar",
    "GEO" = "Geothermal",
    "H2F" = "Hydrogen",
    "STE" = "Steam"
  )
  mapCCS <- readGDX(path, "CCS_NOCCS") %>% as.data.frame() %>%
    rename(CCS = PGALL, NOCCS = PGALL1)

  TSTEAMtoEF <- data.frame(Tech = "TSTE", EF = "STE")
  PGALLtoEF <- readGDX(path, "PGALLtoEF") %>%
    rename(Tech = PGALL, EF = EFS) %>%
    bind_rows(TSTEAMtoEF)
  CCStoEF <- PGALLtoEF %>%
    filter(Tech %in% mapCCS$CCS)

  PGALLtoEF$EF <- str_replace_all(PGALLtoEF$EF, rename_EF)
  CCStoEF$EF <- str_replace_all(CCStoEF$EF, rename_EF)
  TSTEAMtoEF$EF <- str_replace_all(TSTEAMtoEF$EF, rename_EF)

  CCS <- PGALLtoEF %>%
    filter(Tech %in% CCStoEF$Tech) %>%
    select(Tech) %>%
    unlist(use.names = FALSE)

  prodElecCHP <- readGDX(path, c("V04ProdElecEstCHP", "V04ProdElecCHP"), field = "l", format = "first_found")[regions, years, ]
  prodElecCHP <- add_dimension(prodElecCHP, nm="TSTE", add = "PGALL", dim = 3.1)
  prodElec <- readGDX(path, "VmProdElec", field = "l")[regions, years, ]
  prodElec <- mbind(prodElec, prodElecCHP)

  # Create a mapping for naming (e.g., ATHLGN->Lignite|w/o CCS, etc.)
  mapping <- PGALLtoEF %>%
    mutate(
      EF = ifelse(EF %in% c("Hard coal", "Lignite"), "Coal", EF),
      EF = ifelse(Tech %in% CCS, paste0(EF, "|w/ CCS"), EF),
      EF = ifelse(EF %in% CCStoEF$EF & !(Tech %in% CCS), paste0(EF, "|w/o CCS"), EF)
    )

  prefix <- "Secondary Energy|Electricity|"
  prodElec <- helperPrepareProd(prodElec, prefix = prefix, mapping = mapping)
  prodAll <- mbind(prodElec, helperAggregateLevel(prodElec, level = 4))
  prodAll <- mbind(prodAll, helperAggregateLevel(prodElec, level = 3))
  prodAll <- add_dimension(prodAll, dim = 3.2, add = "unit",nm = "TWh")
  return(prodAll)
}

# Helpers -----------------------------------------------------------------------
helperPrepareProd <- function(magpie, mapping, prefix, name = NULL) {
  # Rename magpie from OPEN-PROM Efs to labels (e.g. Gas -> Secondary Energy|Electricity|)
  # and aggregate in case of duplicates (Coal from lignite and Hard coal to single Coal)
  magpie <- toolAggregate(
    magpie, dim = 3, rel = mapping, from = "Tech", to = "EF"
  )

  title <- paste0(prefix, getItems(magpie, 3))
  title <- if (!is.null(name)) paste(title, name, sep = "|") else title
  getItems(magpie, 3) <- title
  return(magpie)
}

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
