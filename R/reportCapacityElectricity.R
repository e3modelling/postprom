#' Process and Aggregate Electricity Capacity Data
#'
#' This function processes and aggregates electricity capacity data from a GDX file.
#' It maps fuel categories, aggregates data by reporting categories,
#' and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed electricity capacity data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportCapacityElectricity(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind dimSums
#' @importFrom madrat toolAggregate
#' @importFrom dplyr %>% rename
#' @export
reportCapacityElectricity <- function(path, regions, years) {
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

  PGALLtoEF <- readGDX(path, "PGALLtoEF") %>%
    rename(Tech = PGALL, EF = EFS)
  CCStoEF <- PGALLtoEF %>%
    filter(Tech %in% mapCCS$CCS)

  PGALLtoEF$EF <- str_replace_all(PGALLtoEF$EF, rename_EF)
  CCStoEF$EF <- str_replace_all(CCStoEF$EF, rename_EF)

  CCS <- PGALLtoEF %>%
    filter(Tech %in% CCStoEF$Tech) %>%
    select(Tech) %>%
    unlist(use.names = FALSE)

  capacity <- readGDX(path, "V04CapElecNominal",
                      field = "l")[regions, years, ]

  mapping <- PGALLtoEF %>%
    mutate(
      EF = ifelse(EF %in% c("Hard coal", "Lignite"), "Coal", EF),
      EF = ifelse(Tech %in% CCS, paste0(EF, "|w/ CCS"), EF),
      EF = ifelse(EF %in% CCStoEF$EF & !(Tech %in% CCS), paste0(EF, "|w/o CCS"), EF)
    )

  prefix <- "Capacity|Electricity|"
  capacity <- helperRenameItems(capacity, mapping = mapping, prefix = prefix)
  capAll <- mbind(capacity, helperAggregateLevel(capacity, level = 4))
  capAll <- mbind(capAll, helperAggregateLevel(capacity, level = 3))
  capAll <- add_dimension(capAll, dim = 3.2, add = "unit",nm = "GW")
  return(capAll)
}
