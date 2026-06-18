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
#' @importFrom magclass getItems dimSums add_dimension mbind add_columns
#' @importFrom stringr str_replace
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter left_join mutate select group_by %>%
#' @importFrom gdxrrw rgdx.set
#' @export
reportEmissions <- function(path, regions, years) {
  # ====================== Energy ====================================
  # ------------------------- CO2 ------------------------------------
  BALEF2EFS <- rgdx.set(path, "BALEF2EFS") %>%
    filter(BALEF %in% c("Fossil", "Biofuels"))

  variables <- readGDX(
    path,
    c(
      "V07GrossEmissCO2Demand", "V06CO2CaptureCCS",
      "V07GrossEmissCO2Supply", "V06CapCDR"
    ),
    field = "l"
  )

  grossCO2Demand <- variables$V07GrossEmissCO2Demand[regions, years]
  grossCO2Demand <- grossCO2Demand[c("NEN", "PCH"), invert = TRUE]

  grossCO2Supply <- variables$V07GrossEmissCO2Supply[regions, years, ]

  CCS <- variables$V06CO2CaptureCCS[regions, years]
  CCS <- CCS[, , BALEF2EFS$EF]
  CCS <- toolAggregate(CCS, dim = 3.2, rel = BALEF2EFS, from = "EFS", to = "BALEF")

  CDR <- new.magpie(
    getRegions(CCS),
    years = getYears(CCS),
    names = getItems(CCS, 3.1),
    fill = 0
  )
  CDR[, , "DAC"] <- dimSums(
    variables$V06CapCDR[regions, years][, , "TEW", invert = TRUE], 3.1
  ) * 1e-6
  CDR[, , "EW"] <- dimSums(variables$V06CapCDR[regions, years][, , "TEW"], 3.1) * 1e-6

  temp <- dimSums(CCS, 3.2) + CDR
  netCO2Demand <- grossCO2Demand - temp[, , getItems(grossCO2Demand, 3.1)]
  netCO2Supply <- grossCO2Supply - temp[, , getItems(grossCO2Supply, 3.1)]
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
  DSBS_COMM <- data.frame(
    "." = c("SE", "ICT"),
    "SBS" = "Commercial"
  )
  DSBS_SBS <- bind_rows(DSBS_Industry, DSBS_Transport, DSBS_COMM) %>%
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
  # ========================= AFOLU & Land Use ===============================
  # Single source of truth: the GAMS-derived tag sLandEmiMode (written to the gdx)
  # decides the AFOLU source — no more file.exists guessing (which mis-read a
  # stale mif as a soft-link run and double-counted). Three modes:
  #   curve   = internal land-use-emulator curves computed in postsolve (read gdx)
  #   softmif = MAgPIE soft-link emissions (read iEmissions_magpie.mif; error if absent)
  #   exo     = external default sources (legacy compatibility)
  iEmissions_magpie <- file.path(dirname(path), "iEmissions_magpie.mif")
  landEmiMode <- readGDX(path, "sLandEmiMode", react = "silent")
  landEmiMode <- if (!is.null(landEmiMode) && length(landEmiMode))
    as.character(landEmiMode)[1]
  else if (file.exists(iEmissions_magpie)) "softmif" else "exo"  # fallback for pre-tag gdx

  AFOLUCh4N2o <- NULL; extraAFOLU <- NULL; kyotoAfolu <- NULL
  
  # Use the old version of AFOLU emissions
  landEmiMode <- "legacy"
  
  if (landEmiMode == "softmif") {
    if (!file.exists(iEmissions_magpie))
      stop("sLandEmiMode=softmif but iEmissions_magpie.mif is missing at ", iEmissions_magpie)
    magpieAfolu <- prepareMagpieAfolu(iEmissions_magpie)
    AFOLU_CDR <- magpieAfolu$afolu
    AFOLUCO2 <- magpieAfolu$co2
    CDRCO2 <- magpieAfolu$cdr
    AFOLUCh4N2o <- magpieAfolu$ch4N2o
    extraAFOLU <- magpieAfolu$extra
    kyotoAfolu <- magpieAfolu$kyoto
  } else if (landEmiMode == "curve") {
    internalAfolu <- getInternalAfolu(path, regions, years)
    AFOLU_CDR <- internalAfolu$afolu
    AFOLUCO2 <- internalAfolu$co2
    CDRCO2 <- internalAfolu$cdr
    AFOLUCh4N2o <- internalAfolu$ch4N2o
    extraAFOLU <- internalAfolu$extra
  } else {
    # exo: external default sources (legacy)
    AFOLU_CDR <- mbind(
      getGLOBIOMEU(path, grossCO2Demand)[, years, ],
      getREMIND_MAgPIE_SoCDR(path, grossCO2Demand)[, years, ]
    )[regions, , ]
    AFOLUCO2 <- AFOLU_CDR[, , "Emissions|CO2|AFOLU"]
    CDRCO2 <- AFOLU_CDR[, , "Carbon Removal|Land Use"]
  }
  useMagpieAfolu <- landEmiMode %in% c("softmif", "curve")   # both provide AFOLU CH4/N2O
  # ========================= Industrial Processes ===========================
  IndustrialProcesses <- getIndustrialProcesses(
    path, grossCO2Demand
  )[regions, years, ]
  # -----------------------------------------------------------------------
  EmissionsCo2 <- mbind(
    grossCO2Demand, netCO2Demand, grossCO2Supply,
    netCO2Supply, AFOLUCO2, IndustrialProcesses
  )
  EmissionsCo2 <- helperAggregateLevel(EmissionsCo2, level = 2, recursive = TRUE)
  # ------------------------ Carbon Capture & Removal --------------------------
  CCS <- CCS[, , c("DAC", "EW"), invert = TRUE]
  side <- ifelse(getItems(CCS, 3.1) %in% SSBSTable$SBS, "Supply", "Demand")
  name <- TotalTable$.te[match(getItems(CCS, 3.1), TotalTable$SBS)]

  getItems(CCS, 3.1) <- paste0("Carbon Capture|Energy|", side, "|", name)
  getItems(CCS, 3) <- gsub("\\.", "|", getItems(CCS, dim = 3))

  CCS <- add_columns(CCS, addnm = "Carbon Capture|Direct Air Capture", dim = 3, fill = 0)
  CCS <- add_columns(CCS, addnm = "Carbon Capture|Utilization", dim = 3, fill = 0)
  CCS[, , "Carbon Capture|Direct Air Capture"] <- CDR[, , "DAC"]

  CDR <- CDR[, , c("DAC", "EW")]

  midfix <- case_when(
    getItems(CDR, 3.1) == "DAC" ~ "Geological Storage|Direct Air Capture",
    getItems(CDR, 3.1) == "EW" ~ "Enhanced Weathering",
  )
  getItems(CDR, 3.1) <- paste0("Carbon Removal|", midfix)
  CDR <- add_columns(
    CDR,
    addnm = paste0("Carbon Removal|Geological Storage|", c("Biomass", "Other Sources")),
    dim = 3,
    fill = 0
  )
  
  CDR[, , "Carbon Removal|Geological Storage|Biomass"] <- dimSums(CCS[, , getItems(CCS, 3)[grepl("\\|Biofuels$", getItems(CCS, 3))]], 3.1)
  
  captured <- mbind(CDR, CCS, CDRCO2)
  captured <- helperAggregateLevel(captured, level = 1, recursive = TRUE)
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

  # Replace emissionsNonCO2 with AFOLU_CDR CH4 and N2O variables if a coupled run is made
  if (useMagpieAfolu) {
    emissionsNonCO2NonAFOLU <- getNames(emissionsNonCO2)[!grepl("AFOLU", getNames(emissionsNonCO2))]
    emissionsNonCO2 <- emissionsNonCO2[, , emissionsNonCO2NonAFOLU]
    emissionsNonCO2 <- mbind(emissionsNonCO2, AFOLUCh4N2o)
  }
  emissionsCO2eq <- calculateGhg(emissionsNonCO2)
  names(dimnames(emissionsNonCO2))[3] <- "SBS"
  emissionsNonCO2 <- helperAggregateLevel(emissionsNonCO2, level = 2, recursive = TRUE)
  emissionsNonCO2 <- emissionsNonCO2[, , c("Emissions|HFC", "Emissions|PFC"), invert = TRUE]
  # -------------------------- Kyoto Gases ------------------------------------
  kyotoGases <- dimSums(emissionsCO2eq, dim = 3)[, , ] + EmissionsCo2[, , "Emissions|CO2"]
  getItems(kyotoGases, 3.1) <- "Emissions|Kyoto Gases"
  names(dimnames(kyotoGases))[3] <- "SBS"
  # =========================== Cumulated CO2 ==================================
  Cumulated <- as.quitte(EmissionsCo2[, , "Emissions|CO2"]) %>%
    group_by(region) %>%
    mutate(
      value = cumsum(value),
      value = value / 1000
    ) %>%
    as.data.frame() %>%
    as.quitte() %>%
    as.magpie()
  getItems(Cumulated, 3) <- "Emissions|CO2|Cumulated"
  names(dimnames(Cumulated))[3] <- "SBS"
  # =============================== Auxiliary =================================
  #  ---------------- Emissions|CO2|Energy and Industrial Processes -----------
  sumIPEnergy <- EmissionsCo2[, , c("Emissions|CO2|Energy", "Emissions|CO2|Industrial Processes")]
  sumIPEnergy <- dimSums(sumIPEnergy, dim = 3, na.rm = TRUE)
  getItems(sumIPEnergy, 3) <- "Emissions|CO2|Energy and Industrial Processes"
  # ------------ Emissions|CO2|Energy|Demand|Residential and Commercial -------
  resCom <- EmissionsCo2[, , c("Emissions|CO2|Energy|Demand|Residential", "Emissions|CO2|Energy|Demand|Commercial", "Emissions|CO2|Energy|Demand|Agriculture, Fishing, Forestry")]
  resCom <- dimSums(resCom, 3)
  getItems(resCom, 3.1) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
  # ------------ Carbon Capture|Geological Storage|Direct Air Capture -------
  captureGeoStorage <- captured[, , grepl("Geological Storage", getItems(captured, dim = 3))]
  getItems(captureGeoStorage, dim = 3) <- gsub("Carbon Removal", "Carbon Capture", getItems(captureGeoStorage, dim = 3))
  # -------------------------- Transport Passenger -------
  TRANP <- EmissionsCo2[, , c("Emissions|CO2|Energy|Demand|Transportation|Passenger Transport - Cars",
                              "Emissions|CO2|Energy|Demand|Transportation|Passenger Transport - Busses",
                              "Emissions|CO2|Energy|Demand|Transportation|Passenger Transport - Rail",
                              "Emissions|CO2|Energy|Demand|Transportation|Passenger Transport - Inland Navigation",
                              "Emissions|CO2|Energy|Demand|Transportation|Passenger Transport - Aviation")]
  TRANP <- dimSums(TRANP, 3)
  getItems(TRANP, 3.1) <- "Emissions|CO2|Energy|Demand|Transportation|Passenger"
  # -------------------------- Transport Freight -------
  TRANG <- EmissionsCo2[, , c("Emissions|CO2|Energy|Demand|Transportation|Goods Transport - Trucks",
                              "Emissions|CO2|Energy|Demand|Transportation|Goods Transport - Rail",
                              "Emissions|CO2|Energy|Demand|Transportation|Goods Transport - Inland Navigation")]
  TRANG <- dimSums(TRANG, 3)
  getItems(TRANG, 3.1) <- "Emissions|CO2|Energy|Demand|Transportation|Freight"
  # -------------------------- Other Fuel Transformation (Liquids, Solids, Gases) -------
  OtherFuelTransformation <- EmissionsCo2[, , c("Emissions|CO2|Energy|Supply|Liquids",
                                       "Emissions|CO2|Energy|Supply|Solids",
                                       "Emissions|CO2|Energy|Supply|Gases")]
  OtherFuelTransformation <- dimSums(OtherFuelTransformation, dim = 3, na.rm = TRUE)
  getItems(OtherFuelTransformation, 3) <- "Emissions|CO2|Energy|Supply|Other Fuel Transformation"
  # ------------ Emissions|HFC (all HFCs aggregated in HFC134a-equiv with GWP (1430/1000 = 1.43)) -------
  allHfcVars <- grep("Emissions\\|HFC", getNames(emissionsCO2eq), value = TRUE)
  HFCAgg <- emissionsCO2eq[, , allHfcVars] / 1.43
  HFCAgg <- dimSums(HFCAgg, 3)
  getItems(HFCAgg, 3.1) <- "Emissions|HFC"
  # ------------ Emissions|PFC (all PFCs aggregated in CF4-equiv with GWP (7390/1000 = 7.39)) -------
  allPfcVars <- grep("Emissions\\|PFC", getNames(emissionsCO2eq), value = TRUE)
  PFCAgg <- emissionsCO2eq[, , allPfcVars] / 7.39
  PFCAgg <- dimSums(PFCAgg, 3)
  getItems(PFCAgg, 3.1) <- "Emissions|PFC"
  # ------------ Emissions|F-gases (HFCs, PFCs, SF6 in CO2-equiv) -------
  allFgasVars <- grep("Emissions\\|(HFC|PFC|SF6)", getNames(emissionsCO2eq), value = TRUE)
  FgasAgg <- emissionsCO2eq[, , allFgasVars]
  FgasAgg <- dimSums(FgasAgg, 3)
  getItems(FgasAgg, 3.1) <- "Emissions|F-gases"
  # -------------------------- Emissions|CO2 (w/o bunkers), Emissions|Kyoto Gases (w/o bunkers) -------
  emissionsCO2woBunkers <- EmissionsCo2[, , "Emissions|CO2"] - EmissionsCo2[, , "Emissions|CO2|Energy|Demand|Bunkers"]
  getItems(emissionsCO2woBunkers, dim = 3) <- "Emissions|CO2-(w/o bunkers)"
  getSets(emissionsCO2woBunkers)[3] <- "DSBS"
  emissionsKyotowoBunkers <- kyotoGases[, , "Emissions|Kyoto Gases"] - EmissionsCo2[, , "Emissions|CO2|Energy|Demand|Bunkers"]
  getItems(emissionsKyotowoBunkers, dim = 3) <- "Emissions|Kyoto Gases-(w/o bunkers)"
  getSets(emissionsKyotowoBunkers)[3] <- "DSBS"
  # =============================== Add Dimensions ============================
  emissionsNonCO2 <- add_dimension(
    emissionsNonCO2,
    dim = 3.2,
    add = "unit",
    nm = unname(sapply(getNames(emissionsNonCO2), getUnit)),
    expand = FALSE
  )
  unitsCO2 <- sub(".*\\((.*)\\).*", "\\1", grossCO2Supply@description)
  captured <- add_dimension(captured, dim = 3.2, add = "unit", nm = unitsCO2)
  EmissionsCo2 <- add_dimension(EmissionsCo2, dim = 3.2, add = "unit", nm = unitsCO2)
  kyotoGases <- add_dimension(kyotoGases, dim = 3.2, add = "unit", nm = "Mt CO2-equiv/yr")
  Cumulated <- add_dimension(Cumulated, dim = 3.2, add = "unit", nm = "Gt CO2")
  sumIPEnergy <- add_dimension(sumIPEnergy, dim = 3.2, add = "unit", nm = unitsCO2)
  resCom <- add_dimension(resCom, dim = 3.2, add = "unit", nm = unitsCO2)
  captureGeoStorage <- add_dimension(captureGeoStorage, dim = 3.2, add = "unit", nm = unitsCO2)  
  TRANP <- add_dimension(TRANP, dim = 3.2, add = "unit", nm = unitsCO2)
  TRANG <- add_dimension(TRANG, dim = 3.2, add = "unit", nm = unitsCO2)
  OtherFuelTransformation <- add_dimension(OtherFuelTransformation, dim = 3.2, add = "unit", nm = unitsCO2)
  HFCAgg <- add_dimension(HFCAgg, dim = 3.2, add = "unit", nm = "kt HFC134a-equiv/yr")
  PFCAgg <- add_dimension(PFCAgg, dim = 3.2, add = "unit", nm = "kt CF4-equiv/yr")
  FgasAgg <- add_dimension(FgasAgg, dim = 3.2, add = "unit", nm = "Mt CO2-equiv/yr")
  emissionsCO2woBunkers <- add_dimension(emissionsCO2woBunkers, dim = 3.2, add = "unit", nm = unitsCO2)
  emissionsKyotowoBunkers <- add_dimension(emissionsKyotowoBunkers, dim = 3.2, add = "unit", nm =  "Mt CO2-equiv/yr")

  magpie_object <- mbind(
    emissionsNonCO2, EmissionsCo2, kyotoGases,
    Cumulated, sumIPEnergy, resCom, captured, captureGeoStorage,
    TRANP, TRANG, OtherFuelTransformation, HFCAgg, PFCAgg, FgasAgg,
    emissionsCO2woBunkers, emissionsKyotowoBunkers
  )
  # Add other emissions from magpie run if they are available
  if (!is.null(extraAFOLU)) {
    extraAFOLU <- add_dimension(
      extraAFOLU,
      dim = 3.2,
      add = "unit",
      nm = unname(sapply(getNames(extraAFOLU), getUnit)),
      expand = FALSE
    )
    magpie_object <- mbind(magpie_object, extraAFOLU)
  }
  # AFOLU-sector Kyoto Gases (Mt CO2-equiv/yr), synthesized from the gas trees.
  if (!is.null(kyotoAfolu)) {
    kyotoAfolu <- add_dimension(kyotoAfolu, dim = 3.2, add = "unit",
                                nm = "Mt CO2-equiv/yr")
    magpie_object <- mbind(magpie_object, kyotoAfolu)
  }

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
  } else if (grepl("HFC|PFC|CF4|C2F6|C6F14|SF6", varName)) {
    # HFC, PFC, CF4, C2F6, C6F14 are kt
    # Example: "Emissions|HFC|HFC152a" -> "HFC152a"
    gasNameParts <- strsplit(varName, "\\|")[[1]]
    if (length(gasNameParts) > 1) {
      gasName <- tail(gasNameParts, 1)
    } else {
      gasName <- sub(".*\\|", "", varName)
    }
    return(paste0("kt ", gasName, "/yr"))
  } else {
    # For any other gases, default to Mt and extract the gas name (second element)
    # Example: "Emissions|VOC|AFOLU|Land|..." -> "VOC"
    gasNameParts <- strsplit(varName, "\\|")[[1]]
    if (length(gasNameParts) > 1) {
      gasName <- gasNameParts[2]
    } else {
      gasName <- sub(".*\\|", "", varName)
    }
    # IAMC reports Sulfur in SO2-equivalent mass and NOx in NO2-equivalent mass.
    gasName <- switch(gasName, "Sulfur" = "SO2", "NOx" = "NO2", gasName)
    return(paste0("Mt ", gasName, "/yr"))
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
  # Define GWP Factors (GWP-100). CH4/N2O updated to AR6 (27 / 273); F-gas values
  # remain AR4 — AFOLU carries no F-gas, so update these too only if full-AR6
  # F-gas accounting is required elsewhere.
  gwpMap <- c(
    "CH4" = 27,
    "N2O" = 273,
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

# Synthesize Emissions|Kyoto Gases|AFOLU|* (Mt CO2-equiv/yr) from the IAMC-aligned
# CO2/CH4/N2O AFOLU tree: Kyoto(node) = CO2(node) + CH4(node)*GWP + N2O(node)*GWP.
# DRY: the Kyoto node set is derived from the gas trees (gas -> "Kyoto Gases"), so
# it tracks exactly the AFOLU nodes the alignment produced — no hard-coded list and
# no separate csv column. GWP from getCo2EqFactor (AR6: CH4 27, N2O 273/1000 since
# N2O is in kt); CO2 factor = 1. F-gases are absent in AFOLU. Nodes MAgPIE does not
# produce (e.g. Land|Other) never appear, so they are silently skipped.
buildKyotoAfolu <- function(afolu) {
  v <- getItems(afolu, dim = 3.1)
  ghg <- grep("^Emissions\\|(CO2|CH4|N2O)\\|AFOLU", v, value = TRUE)
  if (length(ghg) == 0) return(NULL)
  kyotoNames <- unique(sub("^Emissions\\|(CO2|CH4|N2O)\\|",
                           "Emissions|Kyoto Gases|", ghg))
  out <- NULL
  for (kv in kyotoNames) {
    acc <- NULL
    for (g in c("CO2", "CH4", "N2O")) {
      gv <- sub("^Emissions\\|Kyoto Gases\\|", paste0("Emissions|", g, "|"), kv)
      if (gv %in% v) {
        f <- if (g == "CO2") 1 else getCo2EqFactor(gv)
        contrib <- afolu[, , gv] * f
        acc <- if (is.null(acc)) contrib else acc + contrib
      }
    }
    out <- mbind(out, setNames(acc, kv))
  }
  out
}

prepareMagpieAfolu <- function(iEmissions_magpie) {
  dataMagpie <- read.report(iEmissions_magpie)
  afolu <- dataMagpie[[1]][[1]]

  # Strip the "(unit)" suffix so names match the csv's `variable` column and
  # the IAMC `iamc_variable` targets (both unit-free).
  getItems(afolu, 3.1) <- trimws(gsub("\\s*\\(.*\\)$", "", getItems(afolu, dim = 3)))

  # ---- (1) Carbon Removal|Land Use ----------------------------------------
  # Must run BEFORE the IAMC remap: the CDR mapping keys on the fine MAgPIE LUC
  # leaves (Regrowth / Indirect / Soil Withdrawals ...) that the remap dissolves
  # into "Land Use and Land-Use Change". The CDR mapping keys on the AFOLU-
  # prefixed names, so add the prefix on a throwaway copy first.
  afoluPrefixed <- afolu
  pn <- getItems(afoluPrefixed, 3.1)
  pn <- ifelse(
    grepl("\\|AFOLU\\|Land", pn),
    pn,
    str_replace(pn, "^(Emissions\\|[^|]+)\\|Land(.*)$", "\\1|AFOLU|Land\\2")
  )
  getItems(afoluPrefixed, 3.1) <- pn
  magpieCDRmapping <- toolGetMapping("open-prom-magpie-CDR-mapping.csv",
    type = "sectoral",
    where = "postprom"
  )
  cdr <- suppressMessages(toolAggregate(
    afoluPrefixed,
    dim = 3,
    rel = magpieCDRmapping,
    partrel = TRUE
  ))
  # MAgPIE reports land-based CO2 sinks as negative emissions; relabel as
  # `Carbon Removal|*` (IAMC convention: positive gross flux). Same abs() as
  # getGLOBIOMEU() / getREMIND_MAgPIE_SoCDR().
  cdr <- abs(cdr)

  # ---- (2) Align variable names to IAMC common-definitions ----------------
  # Merge MAgPIE leaves -> IAMC names (pure '|', no '+') via the `iamc_variable`
  # column of magpie-afolu-emission-variables.csv. Rows with a blank target
  # (NO3-, CO2 agricultural-waste-burning, and every parent row) are dropped;
  # many-to-one rows are summed (over-fine MAgPIE leaves roll up to the coarser
  # IAMC node — e.g. all LUC leaves + Indirect -> Land Use and Land-Use Change).
  emiCsv <- read.csv(
    system.file("extdata", "magpie-afolu-emission-variables.csv", package = "postprom"),
    stringsAsFactors = FALSE
  )
  emiCsv$variable <- trimws(gsub("\\s*\\(.*\\)$", "", emiCsv$variable))
  rel <- emiCsv[nzchar(emiCsv$iamc_variable), c("variable", "iamc_variable")]
  rel <- rel[rel$variable %in% getItems(afolu, 3.1), ]
  leaves <- suppressMessages(toolAggregate(
    afolu,
    dim = 3.1, rel = rel, from = "variable", to = "iamc_variable", partrel = TRUE
  ))

  # ---- (3) Rebuild IAMC parent nodes from the leaves ----------------------
  # IAMC names are pure '|', so helperAggregateLevel (truncates on '|')
  # reconstructs every parent down to level 3 = Emissions|<gas>|AFOLU. Level-2
  # totals (Emissions|<gas>) are left to the caller's own aggregation.
  afolu <- helperAggregateLevel(leaves, level = 3, recursive = TRUE)

  # N2O is already kt here: coupleMagpieToProm now converts MAgPIE-native Mt -> kt
  # at the boundary when it writes iEmissions_magpie.mif, so no conversion is done
  # here (all OPEN-PROM N2O is kt: GLOBIOM lookup, EMTYPE, GAMS, the Kyoto factor
  # below). CH4/CO2 stay in Mt.

  # ---- (4) Split for the caller -------------------------------------------
  # IMPORTANT: co2 / ch4N2o hand the caller ONLY the gas-AFOLU TOP nodes. The
  # caller runs helperAggregateLevel(level = 2) on them to build the gas totals
  # (Emissions|CO2, Emissions|CH4, ...); passing the whole subtree there would
  # double-count parents + children. All finer nodes ride in `extra`, which is
  # mbind-ed verbatim (no further aggregation), so the IAMC sub-trees are kept.
  nm <- getItems(afolu, 3.1)
  # CO2 tops fed into the CO2 sum (Fires now lives under AFOLU|Land):
  co2Tops    <- intersect(c("Emissions|CO2|AFOLU|Land",
                            "Emissions|CO2|AFOLU|Agriculture"), nm)
  # CH4/N2O AFOLU level-3 tops; replace the nonCO2 AFOLU block in the caller:
  ch4n2oTops <- intersect(c("Emissions|CH4|AFOLU", "Emissions|N2O|AFOLU"), nm)
  # Everything else: CO2 AFOLU|Land sub-tree + CH4/N2O AFOLU sub-trees (below
  # their tops) + all air-pollutant AFOLU trees. Drop the tops handed above and
  # the redundant Emissions|CO2|AFOLU level-3 total (the CO2 aggregation rebuilds
  # it; the CH4/N2O level-3 tops are kept here only via ch4N2o, not extra).
  extraSel   <- setdiff(nm, c(co2Tops, "Emissions|CO2|AFOLU", ch4n2oTops))

  return(list(
    afolu  = afolu,
    co2    = afolu[, , co2Tops],
    cdr    = cdr,
    ch4N2o = afolu[, , ch4n2oTops],
    extra  = afolu[, , extraSel],
    kyoto  = buildKyotoAfolu(afolu)
  ))
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

# getMAGPIE function to generate AFOLU and Land_CDR
getMAGPIE <- function(path, magpie_object) {
  fscenario <- readGDX(path, "fscenario")

  # Add MAGPIE run
  MAGPIE_runs <- readSource("MAGPIE_runs")

  # Filter MAGPIE by scenario
  if (fscenario %in% c(0, 1)) {
    MAGPIE_runs <- MAGPIE_runs[, , "MAGPIE_NPI"]
  } else if (fscenario %in% c(2, 5, 6)) {
    MAGPIE_runs <- MAGPIE_runs[, , "MAGPIE_1P5C"]
  } else if (fscenario == 3) {
    MAGPIE_runs <- MAGPIE_runs[, , "MAGPIE_2C"]
  }

  # Filter with variables
  MAGPIE_runs <- MAGPIE_runs[, , c(
    "Emissions|CO2|AFOLU|Agriculture", "Emissions|CO2|Land",
    "Emissions|CO2|Land|Land-use Change|+|Deforestation",
    "Emissions|CO2|Land Carbon Sink|Grassi|Managed Land|Managed Forest",
    "Emissions|CO2|Land|Land-use Change|Soil|+|Land Conversion"
  )]

  # Check if 'RWO' exists as a region and then compute RWO as World - sum of countries
  if ("RWO" %in% getItems(magpie_object, 1)) {
    World_MAGPIE <- dimSums(MAGPIE_runs, 1)
    withoutRWO_MAGPIE <- MAGPIE_runs[!getItems(magpie_object, 1) %in% "RWO", , ]
    mainCountriesSum_MAGPIE <- dimSums(withoutRWO_MAGPIE, dim = 1)
    newRWO_MAGPIE <- World_MAGPIE - mainCountriesSum_MAGPIE
    getItems(newRWO_MAGPIE, 1) <- "RWO"

    MAGPIE_runs <- mbind(withoutRWO_MAGPIE, newRWO_MAGPIE)
  }
  # interpolate years
  MAGPIE_runs <- as.quitte(MAGPIE_runs) %>%
    select(-scenario) %>%
    interpolate_missing_periods(period = getYears(magpie_object, as.integer = T), expand.values = TRUE) %>%
    as.quitte() %>%
    as.magpie()

  # calculate AFOLU emissions of Magpie
  AFOLU <- MAGPIE_runs[, , c("Emissions|CO2|AFOLU|Agriculture", "Emissions|CO2|Land")]
  AFOLU <- dimSums(AFOLU, 3)

  # calculate Land_CDR emissions of Magpie
  Land_CDR <- MAGPIE_runs[, , c(
    "Emissions|CO2|Land|Land-use Change|+|Deforestation",
    "Emissions|CO2|Land Carbon Sink|Grassi|Managed Land|Managed Forest",
    "Emissions|CO2|Land|Land-use Change|Soil|+|Land Conversion"
  )]
  Land_CDR <- dimSums(Land_CDR, 3)

  # Take AFOLU emissions from Magpie
  getItems(AFOLU, 3) <- "Emissions|CO2|AFOLU"
  AFOLU_unit <- add_dimension(AFOLU, dim = 3.2, add = "unit", nm = "Mt CO2/yr")

  # Take Land_CDR emissions from Magpie
  getItems(Land_CDR, 3) <- "Carbon Removal|Land Use"
  Land_CDR_unit <- add_dimension(Land_CDR, dim = 3.2, add = "unit", nm = "Mt CO2/yr")

  MAGPIE <- mbind(AFOLU_unit, Land_CDR_unit)

  return(MAGPIE)
}

# getGLOBIOMEU function to generate AFOLU and Land_CDR from GLOBIOMEU
# getInternalAfolu: read the land-use-emulator emissions computed in GAMS postsolve
# (imAfoluLandEmis = land CO2, imAfoluAgriEmis = agriculture CH4/N2O) and assemble the
# full IAMC AFOLU sub-tree the caller writes: both AFOLU|Land and AFOLU|Agriculture
# nodes, each with all three gases, plus the AFOLU gas tops (= Land + Agriculture).
# In the emulator's split CO2 lives entirely under Land and CH4/N2O entirely under
# Agriculture, so the cross cells (CO2|Agriculture, CH4/N2O|Land) are 0 by design,
# which makes that modelling choice explicit in the report.
# Units chain-consistent: CO2/CH4 Mt/yr, N2O kt/yr. Carbon Removal = 0 (the emulator
# CO2 is a net flux; sinks are in the regression intercept). Backend-agnostic
# (globiom/magpie write the same gdx variables).
getInternalAfolu <- function(path, regions, years) {
  land <- readGDX(path, "imAfoluLandEmis")[regions, years, ]   # CO2 populated, CH4/N2O = 0
  agri <- readGDX(path, "imAfoluAgriEmis")[regions, years, ]   # CH4/N2O populated, CO2 = 0
  emt <- c(CO2 = "CO2LandUse", CH4 = "CH4LandUse", N2O = "N2OLandUse")
  named <- function(x, em, name) { y <- collapseNames(x[, , em]); getItems(y, 3) <- name; y }

  parts <- list()
  for (g in names(emt)) {
    parts[[paste0("Emissions|", g, "|AFOLU|Land")]] <-
      named(land, emt[[g]], paste0("Emissions|", g, "|AFOLU|Land"))
    parts[[paste0("Emissions|", g, "|AFOLU|Agriculture")]] <-
      named(agri, emt[[g]], paste0("Emissions|", g, "|AFOLU|Agriculture"))
    top <- parts[[paste0("Emissions|", g, "|AFOLU|Land")]] +
           parts[[paste0("Emissions|", g, "|AFOLU|Agriculture")]]
    getItems(top, 3) <- paste0("Emissions|", g, "|AFOLU")
    parts[[paste0("Emissions|", g, "|AFOLU")]] <- top
  }
  cdr <- parts[[1]]; cdr[, , ] <- 0; getItems(cdr, 3) <- "Carbon Removal|Land Use"
  afolu <- mbind(c(unname(parts), list(cdr)))

  list(
    afolu  = afolu,
    # CO2 sub-nodes feed the CO2 aggregation (helperAggregateLevel rebuilds CO2|AFOLU):
    co2    = afolu[, , c("Emissions|CO2|AFOLU|Land", "Emissions|CO2|AFOLU|Agriculture")],
    cdr    = afolu[, , "Carbon Removal|Land Use"],
    # CH4/N2O AFOLU tops replace the non-CO2 AFOLU block in the caller:
    ch4N2o = afolu[, , c("Emissions|CH4|AFOLU", "Emissions|N2O|AFOLU")],
    # CH4/N2O Land/Agriculture sub-nodes written directly via extraAFOLU:
    extra  = afolu[, , c("Emissions|CH4|AFOLU|Land", "Emissions|CH4|AFOLU|Agriculture",
                         "Emissions|N2O|AFOLU|Land", "Emissions|N2O|AFOLU|Agriculture")]
  )
}

getGLOBIOMEU <- function(path, magpie_object) {
  # Add Globiom
  GLOBIOMEU <- readSource("GLOBIOMEU", convert = FALSE)
  GLOBIOMEU_LAND <- readSource("GLOBIOMEU_LAND")
  GLOBIOMEU_LAND <- GLOBIOMEU_LAND[, , "Total Forest Land"]
  getItems(GLOBIOMEU_LAND, 3.1) <- "Carbon Removal|Land Use"

  Globiom <- mbind(GLOBIOMEU, GLOBIOMEU_LAND)

  # interpolate years
  Globiom <- as.quitte(Globiom) %>%
    select(-c(scenario, unit)) %>%
    interpolate_missing_periods(period = getYears(magpie_object, as.integer = T), expand.values = TRUE) %>%
    as.quitte() %>%
    as.magpie()

  # take absolute value
  Globiom[, , "Carbon Removal|Land Use"] <-
    abs(Globiom[, , "Carbon Removal|Land Use"])

  return(Globiom)
}

# getREMIND_MAgPIE_SoCDR function to generate AFOLU and Land_CDR from REMIND_MAgPIE_SoCDR
getREMIND_MAgPIE_SoCDR <- function(path, magpie_object) {
  fscenario <- readGDX(path, "fscenario")

  # Add REMIND_MAgPIE_SoCDR run
  REMIND_MAgPIE_SoCDR <- readSource("REMIND_MAgPIE_SoCDR")

  # Filter REMIND_MAgPIE_SoCDR by scenario
  if (fscenario %in% c(0, 1)) {
    REMIND_MAgPIE_SoCDR <- REMIND_MAgPIE_SoCDR[, , "SoCDR_Ed3_CurrentTargets"]
  } else if (fscenario %in% c(2, 5, 6)) {
    REMIND_MAgPIE_SoCDR <- REMIND_MAgPIE_SoCDR[, , "SoCDR_Ed3_HighestAmbition"]
  } else if (fscenario == 3) {
    REMIND_MAgPIE_SoCDR <- REMIND_MAgPIE_SoCDR[, , "SoCDR_Ed3_DelayedAction"]
  }

  # Check if 'RWO' exists as a region and then compute RWO as World - sum of countries
  if ("RWO" %in% getItems(magpie_object, 1)) {
    World_REMIND_MAgPIE_SoCDR <- REMIND_MAgPIE_SoCDR["GLO", , ]
    withoutRWO_REMIND_MAgPIE_SoCDR <- REMIND_MAgPIE_SoCDR[!getItems(REMIND_MAgPIE_SoCDR, 1) %in% "GLO", , ]
    mainCountriesSum_REMIND_MAgPIE_SoCDR <- dimSums(withoutRWO_REMIND_MAgPIE_SoCDR, dim = 1)
    newRWO_REMIND_MAgPIE_SoCDR <- World_REMIND_MAgPIE_SoCDR - mainCountriesSum_REMIND_MAgPIE_SoCDR
    getItems(newRWO_REMIND_MAgPIE_SoCDR, 1) <- "RWO"

    REMIND_MAgPIE_SoCDR <- mbind(withoutRWO_REMIND_MAgPIE_SoCDR, newRWO_REMIND_MAgPIE_SoCDR)
  }

  # interpolate years
  REMIND_MAgPIE_SoCDR <- as.quitte(REMIND_MAgPIE_SoCDR) %>%
    select(-c(scenario, model, unit)) %>%
    interpolate_missing_periods(period = getYears(magpie_object, as.integer = T), expand.values = TRUE) %>%
    as.quitte() %>%
    as.magpie()

  weight_EMI_CO2 <- readSource("PIK", convert = TRUE)
  weight_EMI_CO2 <- weight_EMI_CO2[, 2020, "Energy.MtCO2.CO2"]
  weight_EMI_CO2[is.na(weight_EMI_CO2)] <- 0

  EU28 <- toolGetMapping(
    name = "EU28.csv",
    type = "regional",
    where = "mrprom"
  )

  EU28[["ISO3.Code"]] <- "EU28"

  GBR <- toolAggregate(REMIND_MAgPIE_SoCDR["EU28", , ], weight = weight_EMI_CO2[EU28[["Region.Code"]], , ], rel = EU28, from = "ISO3.Code", to = "Region.Code")
  GBR <- GBR["GBR", , ]

  AFOLU_CDR <- mbind(REMIND_MAgPIE_SoCDR, GBR)

  # take absolute value
  AFOLU_CDR[, , "Carbon Removal|Land Use"] <-
    abs(AFOLU_CDR[, , "Carbon Removal|Land Use"])

  return(AFOLU_CDR)
}

# getIndustrialProcesses function from IEAPrimes
getIndustrialProcesses <- function(path, magpie_object) {
  # Add IndustrialProcesses
  IndustrialProcesses <- readSource("IndustrialProcessesEmissionsIEAPrimes")

  fscenario <- readGDX(path, "fscenario")

  # Filter REMIND_MAgPIE_SoCDR by scenario
  if (fscenario %in% c(0)) {
    IndustrialProcesses <- IndustrialProcesses[, , "CurrentPolicies"]
  } else if (fscenario %in% c(1)) {
    IndustrialProcesses <- IndustrialProcesses[, , "STEPS-LTT"]
  } else if (fscenario %in% c(2, 5, 6)) {
    IndustrialProcesses <- IndustrialProcesses[, , "NT-Zero2050"]
  } else if (fscenario == 3) {
    IndustrialProcesses <- IndustrialProcesses[, , "STEPS-LTT"]
  }

  # Check if 'RWO' exists as a region and then compute RWO as World - sum of countries
  if ("RWO" %in% getItems(magpie_object, 1)) {
    World_IndustrialProcesses <- IndustrialProcesses["World", , ]
    withoutRWO_IndustrialProcesses <- IndustrialProcesses[!getItems(IndustrialProcesses, 1) %in% "World", , ]
    mainCountriesSum_IndustrialProcesses <- dimSums(withoutRWO_IndustrialProcesses, dim = 1)
    newRWO_IndustrialProcesses <- World_IndustrialProcesses - mainCountriesSum_IndustrialProcesses
    getItems(newRWO_IndustrialProcesses, 1) <- "RWO"

    IndustrialProcesses <- mbind(withoutRWO_IndustrialProcesses, newRWO_IndustrialProcesses)
  }

  # interpolate years
  IndustrialProcesses <- as.quitte(IndustrialProcesses) %>%
    select(-c(scenario, model, unit)) %>%
    interpolate_missing_periods(period = getYears(magpie_object, as.integer = T), expand.values = TRUE) %>%
    as.quitte() %>%
    as.magpie()

  return(IndustrialProcesses)
}
