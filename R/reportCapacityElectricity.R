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
    "LGN" = "Lignite",
    "HCL" = "Hard coal",
    "GDO" = "Oil",
    "NGS" = "Gas",
    "BMSWAS" = "Biomass",
    "NUC" = "Nuclear",
    "HYD" = "Hydro",
    "WND" = "Wind",
    "SOL" = "Solar",
    "GEO" = "Geothermal",
    "H2F" = "Hydrogen"
  )
  CapacityNonCHP <- readGDX(path, "V04CapElecNominal",
                       field = "l")[regions, years, ]
  CapacityCHP <- readGDX(path, "V04CapElecCHP",
                       field = "l")[regions, years, ]
  Capacity <- mbind(CapacityNonCHP, CapacityCHP)

  PGALLtoEF <- readGDX(path, "PGALLtoEF") %>%
    rename(Tech = PGALL, EF = PGEF)
  CHPtoEF <- readGDX(path, "CHPtoEF") %>%
    rename(Tech = EF, EF = EF1) %>%
    filter(EF %in% PGALLtoEF$EF)
  PGALLtoEF$EF <- str_replace_all(PGALLtoEF$EF, rename_EF)
  CHPtoEF$EF <- str_replace_all(CHPtoEF$EF, rename_EF)
  mapping <- bind_rows(PGALLtoEF, CHPtoEF)
  CHPs <- filter(PGALLtoEF,!EF %in% setdiff(PGALLtoEF$EF, CHPtoEF$EF))

  CapacityNonCHP <- helperPrepare(CapacityNonCHP[,,CHPs$Tech], CHPs, "Non-CHP")
  CapacityCHP <- helperPrepare(CapacityCHP, CHPtoEF, "CHP")
  Capacity <- helperPrepare(Capacity, mapping)

  Total <- dimSums(Capacity, 3.1, na.rm = TRUE)
  getItems(Total, 3.1) <- "Capacity|Electricity"

  TotalCHP <- dimSums(CapacityCHP, 3.1, na.rm = TRUE)
  getItems(TotalCHP, 3.1) <- "Capacity|Electricity|CHP"

  TotalNonCHP <- Total - TotalCHP
  TotalNonCHP <- collapseDim(TotalNonCHP, 3.2)
  getItems(TotalNonCHP, 3.1) <- "Capacity|Electricity|Non-CHP"

  CapacityNonCHP <- mbind(CapacityNonCHP, helperAggregateCoal(CapacityNonCHP, name = "Non-CHP"))
  CapacityCHP <- mbind(CapacityCHP, helperAggregateCoal(CapacityCHP, name = "CHP"))
  Capacity <- mbind(Capacity, helperAggregateCoal(Capacity))

  magpie_object <- mbind(Capacity, CapacityNonCHP, CapacityCHP,
                         Total, TotalCHP, TotalNonCHP)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit",nm = "GW")
  return(magpie_object)
}

# Helper -----------------------------------------------------------------------
helperPrepare <- function(magpie, mapping, name = NULL) {
  magpie <- toolAggregate(
    magpie, dim = 3, rel = mapping, from = "Tech", to = "EF"
  )

  title <- paste0("Capacity|Electricity|", getItems(magpie, 3))
  title <- if (!is.null(name)) paste(title, name, sep = "|") else title
  getItems(magpie, 3) <- title
  return(magpie)
}

helperAggregateCoal <- function(magpie_object, name = NULL) {
  temp <- magpie_object[,,c("Lignite", "Hard coal"), pmatch=TRUE]
  temp <- dimSums(temp, 3.1, na.rm = TRUE)
  title <- "Capacity|Electricity|Coal"
  title <- if (!is.null(name)) paste(title, name, sep = "|") else title
  getItems(temp, 3) <- title
  return(temp)
}
