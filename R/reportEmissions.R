#' Process and Aggregate CO2 Emissions Data
#'
#' This function processes and aggregates CO2 emissions data from a GDX file.
#' It combines multiple components, including final consumption, energy sector inputs,
#' transportation, and carbon capture, to provide a comprehensive emissions report.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated CO2 emissions data.
#'
#' @examples
#' \dontrun{
#' result <- reportEmissions(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind collapseDim
#' @importFrom stringr str_replace
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter left_join mutate select group_by %>%
#' @importFrom gdxrrw rgdx.set
#' @export
reportEmissions <- function(path, regions, years) {
  # ====================== Energy ====================================
  # ------------------------- CO2 ------------------------------------
  variables <- readGDX(
    path,
    c("V07GrossEmissCO2Demand", "V06CapCO2ElecHydr", "V07EmissCO2Supply"),
    field = "l"
  )
  grossCO2Demand <- variables$V07GrossEmissCO2Demand[regions, years, ]
  names(dimnames(grossCO2Demand))[3] <- "SBS"
  grossCO2Supply <- variables$V07EmissCO2Supply[regions, years, ]
  names(dimnames(grossCO2Supply))[3] <- "SBS"
  captured <- variables$V06CapCO2ElecHydr[regions, years, ]
  netCO2Demand <- grossCO2Demand - captured[, , getItems(grossCO2Demand, 3.1)]
  netCO2Supply <- grossCO2Supply - captured[, , getItems(grossCO2Supply, 3.1)]
  # ------------------------ Renamings --------------------------------
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE)
  SSBSTable <- rgdx.set(path, "SSBS", te = TRUE)
  TotalTable <- bind_rows(DSBSTable, SSBSTable)

  # -------- Create a DSBS TO SBS mapping (e.g., Iron & Steel -> Industry) ---
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
  DSBS_SBS <- bind_rows(DSBS_Industry, DSBS_Transport, DSBS_NonEnergy) %>%
    rename(DSBS = 1) %>%
    left_join(DSBSTable, by = c("DSBS" = "SBS")) %>%
    select(-DSBS) %>%
    rename(DSBS = .te)
  lookup <- setNames(DSBS_SBS$SBS, DSBS_SBS$DSBS)
  # ------------- Demand -------------------------
  name <- DSBSTable$.te[match(getItems(grossCO2Demand, 3.1), DSBSTable$SBS)]
  key <- str_extract(name, "^[^|]+")
  mapped <- lookup[key]
  name <- if_else(
    !is.na(mapped),
    str_replace(name, "^[^|]+", paste0(mapped, "|\\0")),
    name
  )
  getItems(grossCO2Demand, 3.1) <- paste0("Gross Emissions|CO2|Energy|Demand|", name)
  getItems(netCO2Demand, 3.1) <- paste0("Emissions|CO2|Energy|Demand|", name)
  # ------------- --------------Supply ---------------------------------
  name <- SSBSTable$.te[match(getItems(grossCO2Supply, 3.1), SSBSTable$SBS)]
  getItems(grossCO2Supply, 3.1) <- paste0("Gross Emissions|CO2|Energy|Supply|", name)
  getItems(netCO2Supply, 3.1) <- paste0("Emissions|CO2|Energy|Supply|", name)
  # -------------------------- Carbon Capture -----------------------------
  side <- ifelse(getItems(captured, 3.1) %in% SSBSTable$SBS, "Supply", "Demand")
  name <- TotalTable$.te[match(getItems(captured, 3.1), TotalTable$SBS)]
  getItems(captured, 3.1) <- paste0("Carbon Capture|Energy|", side, "|", name)
  # -----------------------------------------------------------------------
  EmissionsCo2 <- mbind(grossCO2Demand, netCO2Demand, grossCO2Supply, netCO2Supply, captured)
  EmissionsCo2 <- helperAggregateLevel(EmissionsCo2, level = 2, recursive = TRUE)

  # =============================== Non-CO2===================================
  emissionsNonCO2 <- readGDX(path, "V07EmiActBySrcRegTim", field = "l")[regions, years, ]
  if (is.null(emissionsNonCO2)) {
    message("V07EmiActBySrcRegTim not found – creating empty emissionsNonCO2 placeholder")
    # Zero placeholder for nonCO2 emissions with a single variable
    emissionsNonCO2 <- new.magpie(regions, years, "CH4_manure", fill = 0)
  }
  nonCO2mapping <- toolGetMapping("open-prom-emissions-mapping.csv",
    type = "sectoral",
    where = "postprom"
  )

  emissionsNonCO2 <- toolAggregate(
    emissionsNonCO2,
    dim = 3,
    rel = nonCO2mapping,
    partrel = TRUE
  )
  # Convert CH4 and N20 from Mt Ceq to "Mt CH4/yr" or "kt N2O/yr"
  allVariables <- getNames(emissionsNonCO2)
  ch4Vars <- grep("CH4", allVariables, value = TRUE)
  n2oVars <- grep("N2O", allVariables, value = TRUE)

  # Factor = (C -> CO2) / GWP * (Mt -> kt) for N2O
  # Constants: C_TO_CO2 = 44/12, GWP_CH4 = 25, GWP_N2O = 298
  conversionFactorCh4 <- (44 / 12) / 25
  conversionFactorN2o <- (44 / 12) / 298 * 1e3

  emissionsNonCO2[, , ch4Vars] <- emissionsNonCO2[, , ch4Vars] * conversionFactorCh4
  emissionsNonCO2[, , n2oVars] <- emissionsNonCO2[, , n2oVars] * conversionFactorN2o
  names(dimnames(emissionsNonCO2))[3] <- "SBS"
  # -------------------------- Kyoto Gases ------------------------------------
  emissionsCO2eq <- calculateGhg(emissionsNonCO2)
  kyotoGases <- dimSums(emissionsCO2eq, dim = 3)[, , ] + EmissionsCo2[, , "Emissions|CO2"]
  getItems(kyotoGases, 3.1) <- "Emissions|Kyoto Gases"
  names(dimnames(kyotoGases))[3] <- "SBS"
  # ===============================Add Dimensions =======================================
  emissionsNonCO2 <- add_dimension(
    emissionsNonCO2,
    dim = 3.2,
    add = "unit",
    nm = unname(sapply(getNames(emissionsNonCO2), getUnit)) %>% as.vector()
  )
  EmissionsCo2 <- add_dimension(EmissionsCo2, dim = 3.2, add = "unit", nm = "Mt CO2/yr")
  kyotoGases <- add_dimension(kyotoGases, dim = 3.2, add = "unit", nm = "Mt CO2-equiv/yr")
  magpie_object <- mbind(emissionsNonCO2, EmissionsCo2, kyotoGases)
  return(magpie_object)
}

# Helpers -------------------------------------------------------------
# Define a function to generate the unit for a single variable name - nonCO2
getUnit <- function(varName) {
  if (grepl("CH4", varName)) {
    # CH4 is always Mt
    return("Mt CH4/yr")
  } else if (grepl("N2O", varName)) {
    # N2O is kt
    return("kt N2O/yr")
  } else {
    # Extract the "leaf" (everything after the last '|')
    # Example: "Emissions|HFC|HFC152a" -> "HFC152a"
    gasName <- sub(".*\\|", "", varName)
    # Return the kt unit
    return(paste0("kt ", gasName, "/yr"))
  }
}
# Sum CO2 and Non-CO2 for a specific category - to be used in the future
calcKyoto <- function(cat) {
  # A. Find CO2 Aggregate
  #    Regex: Matches "Emissions|CO2|<cat>" exactly (or with .unit suffix)
  safeCat <- gsub("|", "\\|", cat, fixed = TRUE)
  patCo2 <- paste0("^Emissions\\|CO2\\|", safeCat, "($|\\.)")
  varsCo2 <- grep(patCo2, getItems(magpie_object, 3), value = TRUE)
  valCo2 <- if (length(varsCo2) > 0) dimSums(magpie_object[, , varsCo2], dim = 3) else 0

  # B. Find Non-CO2 Components (CH4 or N2O only)
  #    Regex: Matches "Emissions|CH4|<cat>..." OR "Emissions|N2O|<cat>..."
  patNonCo2 <- paste0("^Emissions\\|(CH4|N2O)\\|", safeCat, "($|\\.|\\|)")
  varsNonCo2 <- grep(patNonCo2, getItems(emissionsCO2eq, 3), value = TRUE)

  if (length(varsNonCo2) > 0) {
    valNonCo2 <- dimSums(emissionsCO2eq[, , varsNonCo2], dim = 3)
  } else {
    valNonCo2 <- 0
  }

  # C. Sum and Rename
  result <- valCo2 + valNonCo2
  result <- setNames(result, paste0("Emissions|Kyoto Gases|", cat))
  return(result)
}
# Get CO2 Equivalent Factor
getCo2EqFactor <- function(varName) {
  # Define GWP Factors (AR4 100-year values standard for reporting)
  gwpMap <- c(
    "CH4" = 25,
    "N2O" = 298,
    "SF6" = 22800,
    "HFC125" = 3500,
    "HFC134a" = 1430,
    "HFC143a" = 4470,
    "HFC152a" = 124,
    "HFC227ea" = 3220,
    "HFC23" = 14800,
    "HFC32" = 675,
    "HFC43-10" = 1640,
    "HFC245fa" = 693,
    "HFC236fa" = 9810,
    "PFC" = 7390,
    "CF4" = 7390,
    "C2F6" = 12200,
    "C6F14" = 9300
  )

  cleanName <- sub("(\\s*\\(.*\\)|\\.[^.]*)$", "", varName)

  if (grepl("CH4", cleanName)) {
    return(gwpMap[["CH4"]])
  } else if (grepl("N2O", cleanName)) {
    return(gwpMap[["N2O"]] / 1000)
  } else {
    gasLeaf <- sub(".*\\|", "", cleanName)
    if (gasLeaf %in% names(gwpMap)) {
      return(gwpMap[[gasLeaf]] / 1000)
    } else {
      warning(paste("GWP not found for:", varName))
      return(0)
    }
  }
}
# Calculate GHG Emissions (Mt CO2eq) from a magpie object
calculateGhg <- function(dataMagpie) {
  # --- Apply Conversion ---
  allVars <- getNames(dataMagpie)
  targetVars <- grep("Emissions\\|", allVars, value = TRUE)

  totalCo2Eq <- NULL

  for (var in targetVars) {
    factor <- getCo2EqFactor(var)

    if (factor != 0) {
      currentGas <- dataMagpie[, , var] * factor
      totalCo2Eq <- mbind(totalCo2Eq, currentGas)
    }
  }
  return(totalCo2Eq)
}
extractAggregatedData <- function(scenario, x, years, ...) {
  map <- toolGetMapping(
    name = "NavigateEmissions.csv",
    type = "sectoral",
    where = "mrprom"
  )

  new_row <- data.frame(
    Emissions = "Carbon Removal|Land Use",
    stringsAsFactors = FALSE
  )

  # Combine with the existing map
  map <- rbind(map, new_row)

  # Get the aggregated data for World, LAM etc
  if (scenario %in% c(0, 1)) {
    xa <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)
  } else if (scenario %in% c(2, 5, 6)) {
    xa <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = FALSE)
  } else if (scenario == 3) {
    xa <- readSource("Navigate", subtype = "SUP_2C_Default", convert = FALSE)
  }

  xa <- xa[, , map[, "Emissions"]]
  xa <- xa[, years, "REMIND-MAgPIE 3_2-4_6"]
  xa <- as.quitte(xa)
  xa <- interpolate_missing_periods(xa, 2010:2100, expand.values = TRUE)
  xa <- as.magpie(xa)
  xa <- xa[, years, "REMIND-MAgPIE 3_2-4_6"]

  desiredRegions <- c(
    "REMIND 3_2|Canada, Australia, New Zealand",
    "REMIND 3_2|Latin America and the Caribbean",
    "REMIND 3_2|Middle East and North Africa",
    "REMIND 3_2|Non-EU28 Europe",
    "REMIND 3_2|Other Asia",
    "REMIND 3_2|Russia and Reforming Economies",
    "REMIND 3_2|Sub-Saharan Africa",
    "World"
  )

  # If only RWO is needed (e.g., DEV mode), then substract the regions (e.g., USA) from the world.

  xa <- xa[desiredRegions, , ]
  # Mapping
  RegionMap <- c(
    "REMIND 3_2|Canada, Australia, New Zealand" = "CAZ",
    "REMIND 3_2|EU 28" = "ELL",
    "REMIND 3_2|Latin America and the Caribbean" = "LAM",
    "REMIND 3_2|Middle East and North Africa" = "MEA",
    "REMIND 3_2|Non-EU28 Europe" = "NEU",
    "REMIND 3_2|Other Asia" = "OAS",
    "REMIND 3_2|Russia and Reforming Economies" = "REF",
    "REMIND 3_2|Sub-Saharan Africa" = "SSA"
  )

  commonRegions <- intersect(RegionMap[getRegions(xa)], getRegions(x))
  xaRegions <- names(RegionMap)[RegionMap %in% commonRegions]
  matchRegions <- RegionMap[xaRegions]

  for (i in seq_along(matchRegions)) {
    x[matchRegions[i], , ] <- xa[xaRegions[i], , ]
  }

  # Check if 'RWO' exists as a region and then compute RWO as World - sum of countries
  if ("RWO" %in% getItems(x, 1)) {
    extracted <- xa["World", , ]
    withoutRWO <- x[!getItems(x, 1) %in% "RWO", , ]
    mainCountriesSum <- dimSums(withoutRWO, dim = 1)
    newRWO <- extracted - mainCountriesSum
    getItems(newRWO, 1) <- "RWO"

    x <- mbind(withoutRWO, newRWO)
  }


  # set NA to 0
  x[is.na(x)] <- 10^-6

  return(x)
}
