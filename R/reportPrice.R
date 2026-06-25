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
#' @importFrom magclass getItems add_dimension mbind as.magpie getYears collapseDim
#' @importFrom madrat toolAggregate
#' @importFrom quitte as.quitte
#' @importFrom dplyr select filter mutate left_join full_join bind_rows rename if_else rowwise ungroup %>%
#' @importFrom tidyr drop_na
#' @importFrom stringr str_extract str_replace str_count str_starts fixed
#' @export
reportPrice <- function(path, regions, years, weightsForreportPrice) {
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
  
  pricesNoAgr <- toolAggregate(pricesNoAgr, dim = 3.1, rel = DSBSTable, from = "SBS", to = ".te")
  
  DSBS_SBS_full <- bind_rows(
    DSBS_Industry, DSBS_Transport,
    DSBS_NonEnergy, DSBS_CDR, DSBS_COMM
  ) %>%
    rename(DSBS = 1) %>%
    full_join(DSBSTable, by = c("DSBS" = "SBS")) %>%
    filter(!is.na(SBS))
  
  weightsForreportPrice <- collapseDim(weightsForreportPrice, 3.2)
  
  df <- as.data.frame(getItems(weightsForreportPrice,3)) %>%
    rename("variable" = "getItems(weightsForreportPrice, 3)")
  
  vars_vec <- df$variable
  
  # Filter the weighting structure to retain only top-level sectors that:
  # (1) appear at hierarchy level 3 (exactly two "|" separators),
  # (2) have at least four child categories,
  # and extract their sector names (e.g. "Final Energy|Industry|Iron and Steel"
  # -> "Iron and Steel").
  #
  # Then clean the weight-item dimension by:
  # (1) setting all entries outside hierarchy level 3 to NA,
  # (2) keeping only sectors identified above,
  # (3) replacing full hierarchy strings with the extracted sector names.
  #
  # The resulting item dimension contains only valid aggregation sectors used
  # as weights in the subsequent SBS-level price aggregation.
  
  df_filtered <- df %>%
    filter(str_count(variable, fixed("|")) == 2) %>%
    rowwise() %>%
    mutate(
      n_children = sum(str_starts(vars_vec, fixed(paste0(variable, "|"))))
    ) %>%
    ungroup() %>%
    filter(n_children >= 4)
  
  df_filtered <- df_filtered %>%
    mutate(variable = sub("^[^|]+\\|[^|]+\\|", "", variable))
  
  items <- getItems(weightsForreportPrice, 3)
  
  items_new <- ifelse(
    str_count(items, fixed("|")) == 2,
    items,
    NA_character_
  )
  
  keep_names <- df_filtered$variable
  
  items_new <- sapply(items_new, function(x) {
    if (is.na(x)) return(NA_character_)
    
    name <- sub("^[^|]+\\|[^|]+\\|", "", x)
    
    if (name %in% keep_names) name else NA_character_
  })
  
  names(items_new) <- NULL
  
  getItems(weightsForreportPrice, 3) <- items_new
  
  weightsForreportPrice <- weightsForreportPrice[,,!is.na(getItems(weightsForreportPrice, 3))]
  
  weightsForreportPrice <- weightsForreportPrice[,,df_filtered[["variable"]]]
  
  pricesAg <- toolAggregate(pricesNoAgr[,,DSBS_SBS_full$.te], weight = weightsForreportPrice,
                     dim = 3.1, rel = DSBS_SBS_full, from = ".te", to = "SBS",
                     zeroWeight = "allow")
  
  # complete names
  getItems(pricesAg, 3.1) <- paste0("Price|Final Energy|", getItems(pricesAg, 3.1))
  getItems(pricesAg, 3.2) <- EFSTable$.te[match(getItems(pricesAg, 3.2), EFSTable$EF)]
  
  # Replace sep in dimensions and prepend the sector
  name <- gsub("\\.", "|", getItems(pricesAg, dim = 3)) # e.g., IS.HCL --> IS|HCL
  getItems(pricesAg, 3) <- name
  
  pricesAg <- add_dimension(pricesAg, dim = 3.2, add = "unit", nm = units)
  prices <- mbind(prices, pricesAg)

  return(prices)
}
