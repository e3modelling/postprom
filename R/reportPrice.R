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
#' @importFrom magclass getItems getNames add_dimension mbind as.magpie getYears collapseDim
#' @importFrom madrat toolAggregate
#' @importFrom quitte as.quitte
#' @importFrom dplyr select filter mutate left_join full_join bind_rows rename if_else %>%
#' @importFrom tidyr drop_na crossing
#' @importFrom stringr str_extract str_replace str_count fixed
#' @export
reportPrice <- function(path, regions, years, weightsForreportPrice) {
  DSBS <- rgdx.set(path, "DSBS", te = FALSE)
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE)
  EFTable <- rgdx.set(path, "EF", te = TRUE)
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
  pricesNoAgr <- prices
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
  
  prices <- add_dimension(prices, dim = 3.2, add = "unit", nm = units)
  
  # Aggregate prices from SBS codes to readable .te sector names
  pricesNoAgr <- toolAggregate(pricesNoAgr, dim = 3.1, rel = DSBSTable, from = "SBS", to = ".te")
  
  # Rename fuel/energy carrier codes to readable EFS names
  getItems(pricesNoAgr, 3.2) <- EFSTable$.te[match(getItems(pricesNoAgr, 3.2), EFSTable$EF)]
  
  # Build complete DSBS-to-SBS mapping table
  DSBS_SBS_full <- bind_rows(
    DSBS_Industry, DSBS_Transport, DSBS_NonEnergy, DSBS_CDR, DSBS_COMM
  ) %>%
    rename(DSBS = 1) %>%
    full_join(DSBSTable, by = c("DSBS" = "SBS")) %>%
    filter(!is.na(SBS))
  
  # Collapse unit subdimension in the weights
  weightsForreportPrice <- collapseDim(weightsForreportPrice, 3.2)
  
  # Clean weight item names: remove leading hierarchy before the second "|"
  items <- getItems(weightsForreportPrice, 3)
  items <- sub("^[^|]+\\|[^|]+\\|", "", items)
  
  # Keep only items that still have at least one hierarchy separator
  items <- ifelse(
    stringr::str_count(items, fixed("|")) >= 1,
    items,
    NA_character_
  )
  
  # Apply cleaned names and remove invalid NA items
  getItems(weightsForreportPrice, 3) <- items
  weightsForreportPrice <- weightsForreportPrice[, , !is.na(getItems(weightsForreportPrice, 3))]
  
  # Select only price items needed for aggregation
  foraggr <- pricesNoAgr[, , DSBS_SBS_full$.te]
  
  # Replace "." separators with "|" to match weight naming convention
  name <- gsub("\\.", "|", getItems(foraggr, dim = 3))
  getItems(foraggr, 3) <- name
  
  # Identify matching and non-matching items between prices and weights
  weitemsdiff <- setdiff(getItems(foraggr, 3), getItems(weightsForreportPrice, 3))
  
  # Fix Data Centers naming mismatch in price items
  l <- getNames(foraggr) == weitemsdiff
  getNames(foraggr)[l] <- paste0(sub(
    "^Data centers and Networks\\|",
    "Data centers and Networks|Data Centers|",
    weitemsdiff
  ))
  
  weitems <- intersect(getItems(foraggr, 3), getItems(weightsForreportPrice, 3))
  
  # Build sector-energy-carrier mapping for weighted aggregation
  sectors <- DSBS_SBS_full %>%
    select(DSBS, SBS, .te)
  
  efs <- data.frame(EFS = EFSTable[[".te"]])
  
  weightMap <- crossing(sectors, efs) %>%
    mutate(.te = paste(.te, EFS, sep = "|")) %>%
    mutate(SBS = paste(SBS, EFS, sep = "|"))
  
  # Apply same Data Centers naming fix in the mapping table
  weightMap$.te <- sub(
    "^Data centers and Networks\\|",
    "Data centers and Networks|Data Centers|",
    weightMap$.te
  )
  
  # Keep only weights that match available price items
  weightsForPrice <- weightsForreportPrice[, , weitems]
  
  # Aggregate prices to SBS level using matching weights
  pricesAg <- toolAggregate(
    foraggr,
    weight = weightsForPrice,
    dim = 3,
    rel = weightMap,
    from = ".te",
    to = "SBS",
    zeroWeight = "allow"
  )
  
  # Add full reporting name prefix
  getItems(pricesAg, 3.1) <- paste0("Price|Final Energy|", getItems(pricesAg, 3.1))
  
  # Add unit dimension and combine aggregated prices with detailed prices
  pricesAg <- add_dimension(pricesAg, dim = 3.2, add = "unit", nm = units)
  prices <- mbind(prices, pricesAg)
  return(prices)
}
