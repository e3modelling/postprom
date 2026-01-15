#' Create a project-ready MIF report from OPEN-PROM results
#'
#' Reads an OPEN-PROM MIF output file, renames variables to match a project
#' template using a mapping table, checks units against the project template,
#' converts mismatching units (where supported), and writes a standardized MIF
#' report (`outputForProject.mif`) for downstream use.
#' 
#' @param openPromFile A mif file that contains OPEN-PROM results.
#' @return This function creates a report file specific to each project needs
#'
#' @examples
#' \dontrun{
#' projectReport(.path = "path/to/run", "path/to/mif")
#' }
#' 
#' @importFrom magclass mbind dimSums getItems getRegions write.report read.report
#' @export
projectReport <- function(.path, openPromFile) {

  if (!file.exists(openPromFile)) {
    stop("Error: The data file '", openPromFile, "' does not exist.\n",
         "Current working directory: ", getwd())
  }
  # --- Read data ---
  dataMagpie <- read.report(openPromFile)
  scenarioName <- names(dataMagpie)[1]
  modelName <- names(dataMagpie[[1]])[1]
  dataMagpie <- dataMagpie[[1]][[1]]

  templatePath <- file.path(.path, "..", "project-template.csv")
 
  # Safety check before reading
  if (!file.exists(templatePath)) {
    stop("Error: Could not find project-template.csv at: ", normalizePath(templatePath, mustWork = FALSE))
  }

  project <- read.csv(templatePath)
  #project <- read.csv("..\\project-template.csv")

  # --- Mapping ---
  map <- toolGetMapping(name = "prom-project-template.csv", where = "postprom")

  # --- Extract variable names and units ---
  varNames <- getItems(dataMagpie, dim = 3)
  varsClean <- trimws(gsub("\\s*\\(.*\\)$", "", varNames))
  units <- gsub(".*\\((.*)\\)$", "\\1", varNames)
  hasUnits <- grepl("\\(.*\\)$", varNames)

  # --- Rename setup ---
  map$OPEN_PROM <- trimws(map$OPEN_PROM)
  rename_lookup <- setNames(map$project_template, map$OPEN_PROM)

  # --- Rename variables ---
  renamedVars <- ifelse(varsClean %in% names(rename_lookup),
                        rename_lookup[varsClean],
                        varsClean)

  # --- Preserve units ---
  renamedVarsFinal <- ifelse(hasUnits, paste0(renamedVars, " (", units, ")"), renamedVars)

  # --- Apply names to magpie object ---
  getItems(dataMagpie, dim = 3) <- renamedVarsFinal

  # --- Clean renamed variable list ---
  renamedClean <- gsub("\\s*\\(.*\\)$", "", renamedVarsFinal)

  # --- Intersection with project variables ---
  commonVars <- intersect(renamedClean, project$variable)

  # --- Find indices in magpie object ---
  keepIndices <- match(commonVars, renamedClean)

  # --- Extract current units (after rename) ---
  currentVarNames <- getItems(dataMagpie, dim = 3)
  currentUnits <- gsub(".*\\((.*)\\)$", "\\1", currentVarNames)
  currentUnits[!grepl("\\(.*\\)$", currentVarNames)] <- NA

  # --- Get expected units from project ---
  expectedUnits <- project$unit[match(commonVars, project$variable)]
  commonUnits <- currentUnits[keepIndices]
  unitMatch <- commonUnits == expectedUnits

  # --- Build units table for checking---
  checkUnits <- data.frame(
    variable = commonVars,
    magpieUnit = commonUnits,
    expectedUnit = expectedUnits,
    unitMatches = unitMatch,
    stringsAsFactors = FALSE
  )
  filteredMagpie <- dataMagpie[, , keepIndices]

  # Run the conversion of units
  usd2015to2010 <- 0.9253 # https://www.in2013dollars.com/us/inflation/2015?amount=15&endYear=2010

  finalResults <- convertUnitsToExpected(
    magpieObj  = filteredMagpie,
    unitTable  = checkUnits,
    usd2015to2010  = usd2015to2010,
    allowUnrecognized = FALSE,
    quiet  = TRUE
    )
  #print(finalResults$log)

  # Add variables from the project needs as 0 values. 
  # Select multiple tiers and get variable and units as vectors
  selectedVariables <- project %>%
  filter(tier %in% c(1, 2)) %>%
  pull(variable)
  selectedUnits <- project %>%
  filter(tier %in% c(1, 2)) %>%
  pull(unit)

  combinedVector <- paste(selectedVariables, selectedUnits, sep = " (")  # needed formatting for units
  combinedVector <- paste0(combinedVector, ")") 

  allRegions <- getRegions(finalResults$object)
  allYears <- getYears(finalResults$object)

  tierMagpie <-new.magpie(cells = allRegions, years = allYears, names  = combinedVector, fill = 0)
  names(dimnames(tierMagpie))[3] <- "variable"

  #finalDataMagpie <- mbind(finalResults$object,tierMagpie)
  finalDataMagpie <- mbind(finalResults$object)
  write.report(finalDataMagpie,file.path(.path,"outputForProject.mif"), model = "OPEN-PROM 2.0", scenario = scenarioName)
  message(paste0("Saving project mif file in ", .path))
}