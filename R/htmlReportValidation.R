#' Creates a validation report file inn html with piamValidation
#' 
#' @param openPromFile A mif file that contains OPEN-PROM results.
#' @return This function does not return a value but it creates a validation html file with piamValidation.
#'
#' @examples
#' \dontrun{
#' htmlReportValidation(.path = "path/to/run", "path/to/mif")
#' }
#' 
#' @importFrom piamValidation validationReport
#' @importFrom pandoc pandoc_activate
#' @import rmarkdown
#' @export 
htmlReportValidation <- function(.path, openPromFile) {

  configFile <- system.file(package = "postprom", file.path("extdata", "validationConfig_OPEN-PROM.csv"))

  # Activate pandoc latest version for creating html report
  pandoc_activate()

  validationFile <- file.path(dirname(dirname(.path[1])), "fullValidation2.mif")

  if (file.exists(validationFile)) {

    # Uncomment for taking results in dataframe 
    #df<-validateScenarios(dataPath = c(reportingFile,validationFile), config = configFile, outputFile = "validateScenarios.csv")
    
    message("Creating HTML report.")

    # Save old wd and temporarily switch to the base dir
    oldwd <- getwd()
    on.exit(setwd(oldwd))

    # Change wd to prevent validationReport() from duplicating path
    # Pass only the *last folder name* as outputDir
    setwd(dirname(.path))

    validationReport(
      dataPath = c(openPromFile, validationFile),
      config = configFile,
      outputDir = basename(.path),
      extraColors = FALSE
    )

  } else {
      message(sprintf("Skipping HTML report: 'fullValidation2.mif' not found at %s", validationFile))
  }

}