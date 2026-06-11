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
  EFSTable <- rgdx.set(path, "EFS", te = TRUE)
  
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
  getItems(prices, 3.2) <- EFSTable$.te[match(getItems(prices, 3.2), EFSTable$EF)]

  # Replace sep in dimensions and prepend the sector
  name <- gsub("\\.", "|", getItems(prices, dim = 3)) # e.g., IS.HCL --> IS|HCL
  key <- str_extract(name, "^[^|]+")
  mapped <- lookup[key]

  name <- if_else(
    !is.na(mapped),
    str_replace(name, "^[^|]+", paste0(mapped, "|\\0")),
    name
  ) # prepend SBS (e.g., IS|HCL -> Industry|IS|HCL)

  getItems(prices, 3) <- paste0("Prices|Final Energy|", name)

  return(prices)
}
