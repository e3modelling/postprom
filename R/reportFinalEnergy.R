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
#' @importFrom stringr str_replace_all
#' @export
reportFinalEnergy <- function(path, regions, years) {
  EFSTable <- rgdx.set(path, "EFS", te = TRUE)
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE)
  # DSBSTable[12,".te"] <- "Agriculture, Fishing, Forestry, etc"

  #---------- Create a DSBS TO SBS mapping (e.g., Iron & Steel -> Industry) -----
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
  DSBS_Rest <- data.frame(
    DSBS = c("HOU", "AG", "SE", "BU", "DAC")
  ) %>%
    mutate(SBS = "Temp")
  DSBS_SBS <- bind_rows(DSBS_Industry, DSBS_Transport, DSBS_NonEnergy) %>%
    rename(DSBS = 1)
  DSBStoSBS <- bind_rows(DSBS_SBS, DSBS_Rest) %>%
    left_join(DSBSTable, by = c("DSBS" = "SBS")) %>%
    select(-DSBS) %>%
    rename(DSBS = .te)
  lookup <- setNames(DSBStoSBS$SBS, DSBStoSBS$DSBS)

  # -------------------------- Prepare data --------------------------------------
  fuel <- readGDX(path, "VmConsFuel", field = "l")[regions, years, ]
  VFuelTransport <- readGDX(path, "VmDemFinEneTranspPerFuel", field = "l")[regions, years, ]
  fuel[, , getItems(VFuelTransport, 3)] <- VFuelTransport[, , getItems(VFuelTransport, 3)]
  VFuelDAC <-readGDX(path,"VmConsFuelDACProd", field = "l")[regions, years, ]
  dimnames(VFuelDAC)[[3]] <- paste0("DAC.", getItems(VFuelDAC, 3))
  fuel[, , getItems(VFuelDAC, 3)] <- VFuelDAC[, , getItems(VFuelDAC, 3)]
  fuel <- fuel[, , EFSTable$EF]

  # Rename Sectors
  getItems(fuel, 3.1) <- DSBSTable$.te[match(getItems(fuel, 3.1), DSBSTable$SBS)]
  # Rename Fuels
  getItems(fuel, 3.2) <- EFSTable$.te[match(getItems(fuel, 3.2), EFSTable$EF)]

  # Replace sep in dimensions and prepend the sector
  name <- gsub("\\.", "|", getItems(fuel, dim = 3)) # IS.HCL --> IS|HCL
  name <- str_replace(
    name,
    "^[^|]+",
    paste0(lookup[str_extract(name, "^[^|]+")], "|\\0")
  ) # prepend SBS (e.g., IS|HCL -> Industry|IS|HCL)
  name <- str_replace(name, "Temp\\|", "")
  prefix <- "Final Energy|"
  title <- if (!is.null(prefix)) paste0(prefix, name) else name
  getItems(fuel, 3) <- title

  magpie_object <- helperAggregateLevel(fuel, level = 1, recursive = TRUE)
  return(magpie_object)
}
