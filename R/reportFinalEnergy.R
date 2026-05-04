#' Process and Aggregate Final Energy Data
#'
#' A function to process and aggregate final energy consumption data from a GDX file.
#' This function reads and maps data from model subsectors and fuels to reporting categories,
#' aggregates it by subsectors and energy forms, and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated final energy data.
#'
#' @examples
#' \dontrun{
#' result <- reportFinalEnergy(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#' @importFrom gdx readGDX
#' @importFrom madrat toolAggregate
#' @importFrom magclass getItems add_dimension mbind dimSums
#' @importFrom dplyr select filter arrange bind_rows %>%
#' @importFrom stringr str_replace
#' @importFrom gdxrrw rgdx.set
#' @export
reportFinalEnergy <- function(path, regions, years) {
  EFSTable <- rgdx.set(path, "EFS", te = TRUE)
  EFSTable$.te <- gsub("\\s*\\([^)]*\\)", "", EFSTable$.te)
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE)

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
  DSBS_SBS <- bind_rows(DSBS_Industry, DSBS_Transport, DSBS_NonEnergy, DSBS_CDR) %>%
    rename(DSBS = 1) %>%
    left_join(DSBSTable, by = c("DSBS" = "SBS")) %>%
    select(-DSBS) %>%
    rename(DSBS = .te)
  lookup <- setNames(DSBS_SBS$SBS, DSBS_SBS$DSBS)
  # -------------------------- Prepare data --------------------------------------
  fuel <- readGDX(path, "VmFinalEnergy", field = "l")[regions, years, ]
  units <- sub(".*\\((.*)\\).*", "\\1", fuel@description)
  # fuel <- fuel[, , EFSTable$EF]
  # -------------------------- Fuel Aggregations ------------------------------
  BALEFtoEF <- read.csv(
    system.file("mappings", "BALEFtoEF.csv", package = "postprom")
  ) %>%
    separate_rows(EF) %>%
    filter(BALEF %in% c(
      "Solids", "Fossil Liquids", "Gas", "Heat", "Renewables",
      "Electricity", "Hydrogen", "Other fuels", "Biofuels", "Nuclear"
    ))

  finalPerFuel <- dimSums(fuel, dim = 3.1)
  finalPerFuelAggregated <- toolAggregate(finalPerFuel,
    dim = 3, rel = BALEFtoEF,
    from = "EF", to = "BALEF", partrel = TRUE
  )
  keep <- setdiff(unique(BALEFtoEF$BALEF), EFSTable$.te[match(getItems(finalPerFuel, 3.1), EFSTable$EF)])
  finalPerFuelAggregated <- finalPerFuelAggregated[, , keep]
  fuelWOBunkers <- dimSums(fuel[, , "BU", invert = TRUE], dim = 3)

  # -------------------------- Rename Variables -------------------------------
  getItems(fuel, 3.1) <- DSBSTable$.te[match(getItems(fuel, 3.1), DSBSTable$SBS)]
  getItems(fuel, 3.2) <- EFSTable$.te[match(getItems(fuel, 3.2), EFSTable$EF)]
  getItems(finalPerFuel, 3.1) <- paste0("Final Energy|", EFSTable$.te[match(getItems(finalPerFuel, 3.1), EFSTable$EF)])
  getItems(fuelWOBunkers, 3.1) <- paste0("Final Energy w/o bunkers", getItems(fuelWOBunkers, 3.1))
  getItems(finalPerFuelAggregated, 3.1) <- paste0("Final Energy|", getItems(finalPerFuelAggregated, 3.1))
  # ---------------------------------------------------------------------------
  # Replace sep in dimensions and prepend the sector
  name <- gsub("\\.", "|", getItems(fuel, dim = 3)) # e.g., IS.HCL --> IS|HCL
  key <- str_extract(name, "^[^|]+")
  mapped <- lookup[key]

  name <- if_else(
    !is.na(mapped),
    str_replace(name, "^[^|]+", paste0(mapped, "|\\0")),
    name
  ) # prepend SBS (e.g., IS|HCL -> Industry|IS|HCL)

  getItems(fuel, 3) <- paste0("Final Energy|", name)

  fuel <- helperAggregateLevel(fuel, level = 1, recursive = TRUE)

  # =========================== Auxiliary variables ======================
  # --------------------------- Residential & Comercial ------------------
  resCom <- fuel[, , c("Final Energy|Residential", "Final Energy|Commercial", "Final Energy|Agriculture, Fishing, Forestry")]
  resCom <- dimSums(resCom, 3)
  getItems(resCom, 3.1) <- "Final Energy|Residential and Commercial"

  # ============================ Add units ================================
  magpie_object <- mbind(fuel, finalPerFuel, fuelWOBunkers, resCom, finalPerFuelAggregated)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  return(magpie_object)
}
