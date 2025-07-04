#' Generate Emissions CSV file for using it with climate-assessment tool of IIASA
#'
#' This function processes the final output of the mif file and filters the required
#' variables for the emissions csv file. The emissions csv file is then used by the 
#' climate-assessment tool of IIASA. Only OPEN-PROM output data are used. 
#' It reads values from magpie object and avoid reading exported file.
#' 
#' @param regions Optional; a character vector specifying the regions to include. Defaults to `NULL`, in which case regions are read from the GDX file.
#' @param scenario_name Optional; a character vector specifying the scenario names. Defaults to the basename of `.path`.
#'
#' @return This function does not return a value but writes a CSV file to the specified path.
#' @export
#'
#' @examples

#' 
generateEmissionsFile <- function(report,
                                  regions = NULL, years = NULL
                                  ) {

  #report <- read.report("C:/Users/at39/2-Models/test.mif")
  # Select only OPEN-PROM output
  openPromVariables <- report[[2]][[1]]
  # emissionVariableUnit <- read.csv(system.file(package = "postprom", file.path("extdata", "climate-assessment.csv")))
  emissionVariableUnit <- read.csv("inst/extdata/climate-assessment.csv")

  # Add Emissions|CO2|Energy + Emissions|CO2|Industrial processes to create Emissions|CO2|Industrial Processes
  sum <- openPromVariables[, ,c("Emissions|CO2|Energy (Mt CO2/yr)","Emissions|CO2|Industrial Processes (Mt CO2/yr)")]
  sum <- dimSums(sum, dim = 3, na.rm = TRUE)
  getItems(sum,3) <- "Emissions|CO2|Energy and Industrial Processes (Mt CO2/yr)"
  openPromVariables <- mbind(openPromVariables, sum)

  # Select columns that are numeric years and >= 2015
  allYears <- getItems(openPromVariables, dim = 2)
  allYears <- allYears[as.numeric(sub("^y", "", allYears)) >= 2015]

  # Filter only the required emission variables
  emissionUnits <- paste0(emissionVariableUnit$Variable," (",emissionVariableUnit$Unit,")")
  commonVariables <- intersect(getItems(openPromVariables, dim = 3),emissionUnits)
  filteredMagpie <- openPromVariables["GLO", allYears, commonVariables]

  write.report(
        filteredMagpie,
        file = "open_prom_test.mif",
        model = "OPEN-PROM",
        scenario = 'NPI'
      )

  # A fix is made here in the export file by reading again the mif 
  # and changing the separator to "," instead of ";" and removing a trailing column in the mif file
  temporarydf <- read.csv("open_prom_test.mif",sep = ";")
  # Remove the 'X' from the beginning of year column names
  years <- grep("^X\\d{4}$", names(temporarydf), value = TRUE)
  names(temporarydf)[names(temporarydf) %in% years] <- sub("^X", "", years)
  temporarydf <- temporarydf[, names(temporarydf) != "X"]

  tryCatch({
    write.csv(temporarydf, file = 'open_prom_test.csv',row.names = FALSE,na = "")
  }, error = function(e) {
    stop("Error writing the CSV file. Error: ", e$message)
  })
}