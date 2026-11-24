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
  PRIM_PRICES <- readGDX(path, "PRIM_PRICES", field = "l")
  pricesPrimary <- readGDX(path, "V08PricePrimary", field = "l")[regions, years, PRIM_PRICES]
  # --------------------------------------------------------------
  DSBS <- rgdx.set(path, "DSBS", te = FALSE) %>%
    # filter(!(SBS %in% c("BAV", "BMAR"))) %>%
    rbind(data.frame(SBS = "BU"))
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE) %>%
    # filter(!(SBS %in% c("BAV", "BMAR"))) %>%
    rbind(data.frame(SBS = "BU", .te = "Bunkers"))
  EFSTable <- rgdx.set(path, "EFS", te = TRUE)
  #---------- Create a DSBS TO SBS mapping (e.g., Iron & Steel -> Industry)
  DSBS_Industry <- readGDX(path, "INDSE") %>%
    as.data.frame() %>%
    mutate(SBS = "Industry")
  DSBS_Transport <- readGDX(path, "TRANSE") %>%
    as.data.frame() %>%
    filter(!. %in% c("BAV", "BMAR")) %>%
    mutate(SBS = "Transportation")
  Bunkers <- data.frame(
    . = c("BAV", "BMAR"),
    SBS = c("Bunkers", "Bunkers")
  )
  DSBS_Transport <- rbind(DSBS_Transport, Bunkers)
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

  SECtoEF <- rgdx.set(path, "SECtoEF", te = FALSE) %>%
    filter(SBS %in% rgdx.set(path, "DSBS", te = FALSE)$SBS)
  # -------------------------- Prepare data --------------------------------------
  pricesFinal <- readGDX(path, "VmPriceFinal", field = "l")[regions, years, c(paste(SECtoEF$SBS, SECtoEF$EF, sep = "."))]
  units <- sub(".*\\((.*)\\).*", "\\1", pricesFinal@description)
  # -------------------------- Rename Variables -------------------------------
  getItems(pricesFinal, 3.1) <- DSBSTable$.te[match(getItems(pricesFinal, 3.1), DSBSTable$SBS)]
  getItems(pricesFinal, 3.2) <- EFSTable$.te[match(getItems(pricesFinal, 3.2), EFSTable$EF)]
  getItems(pricesPrimary, 3) <- EFSTable$.te[match(getItems(pricesPrimary, 3), EFSTable$EF)]
  # ---------------------------------------------------------------------------
  # Replace sep in dimensions and prepend the sector
  name <- gsub("\\.", "|", getItems(pricesFinal, dim = 3)) # e.g., IS.HCL --> IS|HCL
  key <- str_extract(name, "^[^|]+")
  mapped <- lookup[key]
  name <- if_else(
    !is.na(mapped),
    str_replace(name, "^[^|]+", paste0(mapped, "|\\0")),
    name
  ) # prepend SBS (e.g., IS|HCL -> Industry|IS|HCL)

  getItems(pricesFinal, 3) <- paste0("Price|Final Energy|", name)
  getItems(pricesPrimary, 3) <- paste0("Price|Primary Energy|", getItems(pricesPrimary, 3))
  # --------------------------------------------------------------------
  magpie_object <- mbind(pricesFinal, pricesPrimary)
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  return(magpie_object)
}
