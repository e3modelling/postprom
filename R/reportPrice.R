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
  pricesNoAgr <- prices
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

  getItems(prices, 3) <- paste0("Price|Final Energy|", name)
  
  prices <- add_dimension(prices, dim = 3.2, add = "unit", nm = units)
  
  BALEF2EFS <- readGDX(path, "BALEF2EFS")
  names(BALEF2EFS) <- c("BAL", "EF")
  
  BALEF2EFS[["BAL"]] <- gsub("Gas fuels", "Gases", BALEF2EFS[["BAL"]])
  BALEF2EFS[["BAL"]] <- gsub("Steam", "Heat", BALEF2EFS[["BAL"]])
  
  pricesNoAgr <- toolAggregate(pricesNoAgr, dim = 3.1, rel = DSBSTable, from = "SBS", to = ".te")
  
  DSBS_SBS_full <- bind_rows(
    DSBS_Industry, DSBS_Transport,
    DSBS_NonEnergy, DSBS_CDR, DSBS_COMM
  ) %>%
    rename(DSBS = 1) %>%
    full_join(DSBSTable, by = c("DSBS" = "SBS")) %>%
    filter(!is.na(SBS))
  
  pricesNoAgr <- toolAggregate(pricesNoAgr[,,DSBS_SBS_full$.te], dim = 3.1, rel = DSBS_SBS_full, from = ".te", to = "SBS")
  
  # aggregate from fuels to reporting fuel categories
  sum_open_prom <- pricesNoAgr %>%
    as.quitte() %>%
    left_join(BALEF2EFS, by = "EF") %>% ## add mapping
    mutate(value = mean(value, na.rm = TRUE), .by = c("model", "scenario", "region",
                                                      "unit","period","BAL", "SBS")) %>%
    distinct() %>%
    select(c("model","scenario","region","unit", "period","value", "SBS", "BAL")) %>%
    distinct() %>%
    drop_na() %>%
    as.quitte() %>%
    as.magpie()
  
  # complete names
  getItems(sum_open_prom, 3.1) <- paste0("Price|Final Energy|", getItems(sum_open_prom, 3.1))
  
  # Replace sep in dimensions and prepend the sector
  name <- gsub("\\.", "|", getItems(sum_open_prom, dim = 3)) # e.g., IS.HCL --> IS|HCL
  key <- str_extract(name, "^[^|]+")
  mapped <- lookup[key]
  
  name <- if_else(
    !is.na(mapped),
    str_replace(name, "^[^|]+", paste0(mapped, "|\\0")),
    name
  ) # prepend SBS (e.g., IS|HCL -> Industry|IS|HCL)
  
  getItems(sum_open_prom, 3) <- name
  
  sum_open_prom <- add_dimension(sum_open_prom, dim = 3.2, add = "unit", nm = units)
  prices <- mbind(prices, sum_open_prom)

  return(prices)
}
