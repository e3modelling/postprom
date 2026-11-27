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
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE) %>%
    rbind(data.frame(SBS = "BU", .te = "Bunkers"))

  #---------- Create a DSBS TO SBS mapping (e.g., Iron & Steel -> Industry)
  DSBS_Industry <- readGDX(path, "INDSE") %>%
    as.data.frame() %>%
    mutate(SBS = "Industry")
  DSBS_Transport <- readGDX(path, "TRANSE") %>%
    as.data.frame() %>%
    mutate(SBS = "Transportation")
  DSBS_NonEnergy <- readGDX(path, "NENSE") %>%
    as.data.frame() %>%
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
  fuel <- readGDX(path, "VmFinalEnergy", field = "l")[regions, years, ]
  bunkerCodes <- intersect(c("BAV", "BMAR"), getItems(fuel, 3.1))
  BAV_BMAR <- NULL
  if (length(bunkerCodes)) {
    BAV_BMAR <- fuel[, , bunkerCodes]
  }
  units <- sub(".*\\((.*)\\).*", "\\1", fuel@description)
  tableBU <- data.frame(
    GRAN = getItems(fuel, dim = 3.1),
    AGGR = getItems(fuel, dim = 3.1),
    stringsAsFactors = FALSE
  ) %>%
    mutate(AGGR = ifelse(AGGR %in% c("BAV", "BMAR"), "BU", AGGR))
  fuel <- toolAggregate(fuel, dim = 3.1, rel = tableBU, from = "GRAN", to = "AGGR", partrel = TRUE)
  years <- getYears(fuel)
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
  fuelWOBunkers <- dimSums(fuel[, , c("BU"), invert = TRUE], dim = 3)

  # -------------------------- Sector by carrier ------------------------------
  sectorMap <- bind_rows(
    DSBS_Industry, DSBS_Transport,
    data.frame("." = c("HOU", "SE", "ICT"), "SBS" = "Residential and Commercial")
  ) %>%
    rename(DSBS = 1)
  carrierMap <- data.frame(
    EF = c(
      "ELC", "NGS", "OGS", "BGAS", "STE", "H2F",
      "LPG", "GSL", "KRS", "GDO", "RFO", "OLQ", "CRO", "MET", "ETH", "BGDO", "BGSL", "BKRS",
      "HCL", "LGN", "BMSWAS", "BMSWAS", "HCL", "LGN", "SOL", "GEO"
    ),
    Carrier = c(
      "Electricity", rep("Gases", 3), "Heat", "Hydrogen", rep("Liquids", 12),
      rep("Solids", 3), "Solids|Biomass", rep("Solids|Coal", 2), rep("Other", 2)
    )
  )
  sectorCarrier <- toolAggregate(fuel, dim = 3.1, rel = sectorMap, from = "DSBS", to = "SBS", partrel = TRUE)
  sectorCarrier <- toolAggregate(sectorCarrier, dim = 3.2, rel = carrierMap, from = "EF", to = "Carrier", partrel = TRUE)
  getItems(sectorCarrier, 3) <- paste0("Final Energy|", gsub("\\.", "|", getItems(sectorCarrier, 3)))

  # domesticAviation <- dimSums(fuel[, , "PA"], dim = 3)
  # getItems(domesticAviation, 3) <- "Final Energy|Transportation|Domestic Aviation"

  # # -------------------------- Useful Energy ----------------------------------
  # useful <- readGDX(path, "V02DemSubUsefulSubsec", field = "l")[regions, years, ] +
  #   readGDX(path, "V02UsefulElecNonSubIndTert", field = "l")[regions, years, ]
  # useful <- toolAggregate(useful,
  #   dim = 3, rel = sectorMap[sectorMap$SBS != "Transportation", ],
  #   from = "DSBS", to = "SBS", partrel = TRUE
  # )
  # getItems(useful, 3) <- paste0("Useful Energy|", getItems(useful, 3))
  
  # Report final bioenergy by solid, liquid, and gaseous carrier.
  Solids_Biomass <- finalPerFuel[,,"BMSWAS"]
  getItems(Solids_Biomass, 3) <- "Final Energy|Solids|Biomass"
  Liquids_Biomass <- finalPerFuel[,,c("BGDO","BGSL","BKRS")]
  Liquids_Biomass <- dimSums(Liquids_Biomass, 3)
  getItems(Liquids_Biomass, 3) <- "Final Energy|Liquids|Biomass"
  Gas_Biomass <- finalPerFuel[,,"BGAS"]
  getItems(Gas_Biomass, 3) <- "Final Energy|Gas|Biomass"
  Biofuels <- mbind(Solids_Biomass, Liquids_Biomass, Gas_Biomass)
  # -------------------------- Rename Variables -------------------------------
  getItems(fuel, 3.1) <- DSBSTable$.te[match(getItems(fuel, 3.1), DSBSTable$SBS)]
  getItems(fuel, 3.2) <- EFSTable$.te[match(getItems(fuel, 3.2), EFSTable$EF)]
  getItems(finalPerFuel, 3.1) <- paste0("Final Energy|", EFSTable$.te[match(getItems(finalPerFuel, 3.1), EFSTable$EF)])
  getItems(fuelWOBunkers, 3.1) <- paste0("Final Energy w/o bunkers", getItems(fuelWOBunkers, 3.1))
  getItems(finalPerFuelAggregated, 3.1) <- paste0("Final Energy|", getItems(finalPerFuelAggregated, 3.1))

  if (!is.null(BAV_BMAR)) {
    getItems(BAV_BMAR, 3.1) <- DSBSTable$.te[match(getItems(BAV_BMAR, 3.1), DSBSTable$SBS)]
    getItems(BAV_BMAR, 3.2) <- EFSTable$.te[match(getItems(BAV_BMAR, 3.2), EFSTable$EF)]
    getItems(BAV_BMAR, 3) <- paste0("Final Energy|Bunkers|", gsub("\\.", "|", getItems(BAV_BMAR, 3)))
    BAV_BMAR <- helperAggregateLevel(BAV_BMAR, level = 3, recursive = TRUE)
  }
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
  # -------------------------- Data centers, Infrastructure --------------
  scenarioICT <- readGDX(path, "ICT")[[1]]
  getItems(fuel, 3) <- gsub(
    "^Final Energy\\|Commercial\\|Data centers and Networks\\|",
    "Final Energy|Commercial|Data centers and Networks|Data Centers|",
    getItems(fuel, 3)
  )

  dataCenters <- fuel[, , "Final Energy|Commercial|Data centers and Networks|Data Centers", pmatch = TRUE]
  getItems(dataCenters, 3) <- gsub("\\|Data Centers\\|", "|Infrastructure|", getItems(dataCenters, 3))
  fuel <- mbind(fuel, dataCenters)
  fuel[, years[years > "y2023"], "Final Energy|Commercial|Data centers and Networks|Data Centers", pmatch = TRUE] <- fuel[, years[years > "y2023"], "Final Energy|Commercial|Data centers and Networks|Data Centers", pmatch = TRUE] * 1 / (1 + ifelse(scenarioICT == "Upper", 0.91, 0.78))
  fuel[, years[years <= "y2023"], "Final Energy|Commercial|Data centers and Networks|Data Centers", pmatch = TRUE] <- fuel[, years[years <= "y2023"], "Final Energy|Commercial|Data centers and Networks|Data Centers", pmatch = TRUE] * 1 / (1 + 0.91)
  fuel[, years[years > "y2023"], "Final Energy|Commercial|Data centers and Networks|Infrastructure", pmatch = TRUE] <- fuel[, years[years > "y2023"], "Final Energy|Commercial|Data centers and Networks|Infrastructure", pmatch = TRUE] * (1 - 1 / (1 + ifelse(scenarioICT == "Upper", 0.91, 0.78)))
  fuel[, years[years <= "y2023"], "Final Energy|Commercial|Data centers and Networks|Infrastructure", pmatch = TRUE] <- fuel[, years[years <= "y2023"], "Final Energy|Commercial|Data centers and Networks|Infrastructure", pmatch = TRUE] * (1 - 1 / (1 + 0.91))
  # ----------------------------------------------------------------------------
  fuel <- helperAggregateLevel(fuel, level = 1, recursive = TRUE)
  # =========================== Auxiliary variables ======================
  # --------------------------- Residential & Comercial ------------------
  resCom <- fuel[, , c("Final Energy|Residential", "Final Energy|Commercial")]
  resCom <- dimSums(resCom, 3)
  getItems(resCom, 3.1) <- "Final Energy|Residential and Commercial"
  # --------------------------- Agriculture --------------------------------
  agriculture <- readGDX(path, "V12ConsFuel", field = "l")[regions, years, ]

  AGRI_MODEStoEF <- rgdx.set(path, "AGRI_MODEStoEF", te = FALSE)
  AGRI_MODES <- rgdx.set(path, "AGRI_MODES", te = TRUE)

  agriculture <- agriculture[, , c(paste(AGRI_MODEStoEF$AGRI_MODES, AGRI_MODEStoEF$EF, sep = "."))]
  getItems(agriculture, 3.1) <- AGRI_MODES$.te[match(getItems(agriculture, 3.1), AGRI_MODES$i)]
  getItems(agriculture, 3.2) <- EFSTable$.te[match(getItems(agriculture, 3.2), EFSTable$EF)]

  name <- gsub("\\.", "|", getItems(agriculture, dim = 3)) # e.g., IS.HCL --> IS|HCL
  key <- str_extract(name, "^[^|]+")
  mapped <- lookup[key]

  name <- if_else(
    !is.na(mapped),
    str_replace(name, "^[^|]+", paste0(mapped, "|\\0")),
    name
  ) # prepend SBS (e.g., IS|HCL -> Industry|IS|HCL)

  getItems(agriculture, 3) <- paste0("Final Energy|", DSBSTable[DSBSTable$SBS == "AG", ".te"], "|", name)
  # ============================ Add units ================================
  magpie_object <- mbind(
    fuel, finalPerFuel, fuelWOBunkers,
    resCom, finalPerFuelAggregated, agriculture, Biofuels, BAV_BMAR, sectorCarrier,
    reportHeatingFuel(path, regions, years, DSBSTable, sectorMap),
    reportDACEnergy(path, regions, years)
  )
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = units)
  return(magpie_object)
}

# Electricity used for heat in industry and buildings.
# OPEN-PROM splits sectoral electricity into a substitutable part, served by the
# TELC / THEATPUMP equipment stock and competing with fuels for (mostly thermal)
# useful demand, and non-substitutable electricity (V02FinalElecNonSubIndTert:
# appliances, lighting, motors). Only the substitutable part is reported here.
# Heat pumps exist only in buildings (HOU, SE); industry has direct electric
# heating only. Other carriers need no split: all of their use is substitutable,
# so Final Energy|<sector>|<fuel> already equals their heating use.
reportHeatingFuel <- function(path, regions, years, subsectorLabels, sectorMap) {
  equipment <- readGDX(path, "V02EquipCapTechSubsec", field = "l", react = "silent")
  utilisation <- readGDX(path, "i02util", react = "silent")
  fuelShares <- readGDX(path, "i02ShareBlend", react = "silent")
  if (is.null(equipment) || is.null(utilisation) || is.null(fuelShares)) return(NULL)

  industry <- sectorMap$DSBS[sectorMap$SBS == "Industry"]
  subsectors <- c(industry, "HOU", "SE")
  sectorNames <- c(rep("Industry", length(industry)), "Residential", "Commercial")
  labels <- subsectorLabels$.te[match(subsectors, subsectorLabels$SBS)]
  paths <- ifelse(subsectors == "HOU", sectorNames, paste(sectorNames, labels, sep = "|"))
  specs <- data.frame(
    tech = c("THEATPUMP", "TELC"),
    label = c("Heat Pump|Electricity", "Electric Heating|Electricity"),
    buildingsOnly = c(TRUE, FALSE)
  )

  details <- NULL
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]
    # OPEN-PROM has no industrial heat pumps, only zero-valued placeholders
    inScope <- !spec$buildingsOnly | subsectors %in% c("HOU", "SE")
    items <- paste(subsectors, spec$tech, sep = ".")
    shareItems <- paste(items, "ELC", sep = ".")
    available <- inScope & items %in% getItems(equipment, 3) &
      items %in% getItems(utilisation, 3) & shareItems %in% getItems(fuelShares, 3)
    if (!any(available)) next

    consumption <- equipment[regions, years, items[available]]
    shares <- dimSums(fuelShares[regions, years, shareItems[available]], dim = 3.3)
    consumption <- consumption * utilisation[regions, years, items[available]] * shares
    getItems(consumption, 3) <- paste("Final Energy", paths[available], spec$label, sep = "|")
    details <- mbind(details, consumption)
  }
  details
}

reportDACEnergy <- function(path, regions, years) {
  demand <- readGDX(path, "VmConsFuelTechCDRProd", field = "l", react = "silent")
  if (is.null(demand)) return(NULL)

  fuels <- c(ELC = "Electricity", NGS = "Natural Gas", H2F = "Hydrogen")
  technology <- c(names(dacTechnologyLabels), "HTDAC", "H2DAC")
  fuel <- c(rep("ELC", length(dacTechnologyLabels)), "NGS", "H2F")
  items <- paste(technology, fuel, sep = ".")
  available <- items %in% getItems(demand, 3)
  if (!any(available)) return(NULL)

  demand <- demand[regions, years, items[available]]
  getItems(demand, 3) <- paste(
    "Final Energy|Carbon Management|Direct Air Capture",
    dacTechnologyLabels[technology[available]], fuels[fuel[available]], sep = "|"
  )
  demand
}
