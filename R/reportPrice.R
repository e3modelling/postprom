#' Process and Aggregate Final Energy Prices
#'
#' This function processes and aggregates electricity and fuel price data from a GDX file.
#' It maps energy forms to reporting categories, calculates averages, and formats the data into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed energy price data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportPrice(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind as.magpie
#' @importFrom madrat toolAggregate
#' @importFrom quitte as.quitte
#' @importFrom dplyr select filter mutate left_join distinct %>%
#' @importFrom tidyr drop_na
#' @export
reportPrice <- function(path, regions, years) {
  DSBS <- rgdx.set(path, "DSBS", te = FALSE)
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE)
  EFTable <- rgdx.set(path, "EF", te = TRUE)
  
  #---------- Create a DSBS TO SBS mapping (e.g., Iron & Steel -> Industry)
  DSBS_Industry <- readGDX(path, "INDSE") %>%
    as.data.frame() %>%
    mutate(SBS = "Industry")
  DSBS_Transport <- readGDX(path, "TRANSE") %>%
    as.data.frame() %>%
    mutate(SBS = "Transportation")
  DSBS_NonEnergy <- readGDX(path, "NENSE") %>%
    as.data.frame() %>%
    filter(. != "BU") %>%
    mutate(SBS = "Non-Energy Use")
  DSBS_CDR <- readGDX(path, "CDR") %>%
    as.data.frame() %>%
    mutate(SBS = "Carbon Management")
  DSBS_COMM <- data.frame(
    "." = c("SE", "ICT"),
    "SBS" = "Commercial"
  )
  DSBS_SBS <- bind_rows(
    DSBS_Industry, DSBS_Transport,
    DSBS_NonEnergy, DSBS_CDR, DSBS_COMM
  ) %>%
    rename(DSBS = 1) %>%
    left_join(DSBSTable, by = c("DSBS" = "SBS")) %>%
    select(-DSBS) %>%
    rename(DSBS = .te)
  lookup <- setNames(DSBS_SBS$SBS, DSBS_SBS$DSBS)
  # -------------------------- Prepare data --------------------------------------
  prices <- readGDX(path, "VmPriceFuelSubsecCarVal", field = "l")[regions, years, DSBS]
  years <- getYears(prices)
  units <- sub(".*\\((.*)\\).*", "\\1", prices@description)
  # -------------------------- Renamings ------------------------------
  name <- DSBSTable$.te[match(getItems(prices, 3.1), DSBSTable$SBS)]
  getItems(prices, 3.1) <- name
  # VmPriceFuelSubsecCarVal spans the full EF set (incl. aggregates LQD/SLD/GAS/REN,
  # NFF, NEF, HEATPUMP), which is wider than EFS - use EF so every code gets a name
  # (matching against EFS leaves the 7 extras as NA, collapsing into duplicate names).
  fuelNames <- EFTable$.te[match(getItems(prices, 3.2), EFTable[[1]])]
  # Strip any trailing "(...)" - write.report treats a parenthetical in a variable
  # name as the unit (EF's GEO = "...renewable sources (Tidal, etc)"), which would
  # otherwise blank out the Unit column for the entire reporting.mif.
  getItems(prices, 3.2) <- trimws(sub("\\s*\\(.*?\\)", "", fuelNames))

  # Replace sep in dimensions and prepend the sector
  name <- gsub("\\.", "|", getItems(prices, dim = 3)) # e.g., IS.HCL --> IS|HCL
  key <- str_extract(name, "^[^|]+")
  mapped <- lookup[key]

  name <- if_else(
    !is.na(mapped),
    str_replace(name, "^[^|]+", paste0(mapped, "|\\0")),
    name
  ) # prepend SBS (e.g., IS|HCL -> Industry|IS|HCL)

  getItems(prices, 3) <- paste0("Price|Final Energy|", name)
  
  prices <- add_dimension(prices, dim = 3.2, add = "unit", nm = "k$2015/toe")

  # Long-term average power generation cost -> Price|Secondary Energy|Electricity
  costPowGen <- readGDX(path, "VmCostPowGenAvgLng", field = "l")[regions, years, ]
  getItems(costPowGen, 3) <- "Price|Secondary Energy|Electricity"
  costPowGen <- add_dimension(
    costPowGen,
    dim = 3.2, add = "unit", nm = "US$2015/kWh"
  )

  prices <- mbind(prices, costPowGen)

  return(prices)
}
