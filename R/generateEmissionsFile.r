#' Generate Emissions CSV file for using it with climate-assessment tool of IIASA
#'
#' This function processes the final output of the mif file and filters the required
#' variables for the emissions csv file. The emissions csv file is then used by the 
#' climate-assessment tool of IIASA. Only OPEN-PROM output data are used. 
#' It reads values from magpie object and avoid reading exported file.
#' 
#' @param openPromVariables A magpie object that contains OPEN-PROM results.
#' @param years Optional; a character vector specifying the years. Defaults to the basename of `.path`.
#' @param scenario_name Optional; a character vector specifying the scenario names. Defaults to the basename of `.path`.
#' @return This function does not return a value but writes a CSV file to the specified path.
#'
#' @examples
#' \dontrun{
#' generateEmissionsFile(.path = "path/to/gdx2", reports,
#'                 years = c("y2015", "y2025"),
#' s               scenario_name = "NPI")
#' }
#' 
#' @importFrom magclass getItems dimSums mbind
#' @importFrom dplyr intersect
#' @export 
generateEmissionsFile <- function(.path, openPromVariables,
                                  years = NULL,
                                  scenario_name = NULL) {

  emissionVariableUnit <- read.csv(system.file(package = "postprom", file.path("extdata", "climate-assessment.csv")))

  # Add Emissions|CO2|Energy + Emissions|CO2|Industrial processes to create Emissions|CO2|Industrial Processes
  sum <- openPromVariables[, ,c("Emissions|CO2|Energy.Mt CO2/yr","Emissions|CO2|Industrial Processes.Mt CO2/yr")]
  sum <- dimSums(sum, dim = 3, na.rm = TRUE)
  getItems(sum,3) <- "Emissions|CO2|Energy and Industrial Processes.Mt CO2/yr"
  # Fix separator "p" to "."
  dimnames(sum)$d3 <- gsub("pMt", ".Mt", dimnames(sum)$d3)
  
  openPromVariables <- mbind(openPromVariables, sum)

  # Select columns that are numeric years and >= 2015 
  allYears <- getItems(openPromVariables, dim = 2)
  allYears <- allYears[as.numeric(sub("^y", "", allYears)) >= 2015]

  # Filter only the required emission variables
  emissionUnits <- paste0(emissionVariableUnit$Variable,".",emissionVariableUnit$Unit)
  commonVariables <- intersect(getItems(openPromVariables, dim = 3),emissionUnits)
  filteredMagpie <- openPromVariables["World", allYears, commonVariables]

  # Write mif file as a temporary file
  write.report(
        filteredMagpie,
        file = "emissions.mif",
        model = "OPEN-PROM",
        scenario = scenario_name
      )

  # A fix is made here in the export file by reading again the mif 
  # and changing the separator to "," instead of ";" and removing a trailing column in the mif file
  temporarydf <- read.csv("emissions.mif",sep = ";")
  # Remove the 'X' from the beginning of year column names
  years <- grep("^X\\d{4}$", names(temporarydf), value = TRUE)
  names(temporarydf)[names(temporarydf) %in% years] <- sub("^X", "", years)
  temporarydf <- temporarydf[, names(temporarydf) != "X"]

  tryCatch({
    write.csv(temporarydf, file = file.path(.path, 'emissions.csv'),row.names = FALSE,na = "")
  }, error = function(e) {
    stop("Error writing the CSV file. Error: ", e$message)
  })
  # Remove temporary temp MIF file
  file.remove("emissions.mif")

}