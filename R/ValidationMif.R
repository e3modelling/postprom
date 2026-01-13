#' Convert mif file to magpie object.
#'
#' This function processes validation mif file and converts to magpie object.
#'
#' @param .path A character vector specifying the path(s) to the GDX files.
#' @param mif_name A string specifying the name of the MIF file to create.
#' @param Validation_data_for_plots Optional; a logical value indicating whether to add to plots validation data. Defaults to `TRUE`.
#'
#' @return A magpie object provided by validation mif.
#'
#' @examples
#' \dontrun{
#' ValidationMif(.path = c("path/to/gdx1"),
#' mif_name = "output.mif",Validation_data_for_plots = TRUE)
#' }
#' @importFrom magclass mbind dimSums getItems getRegions write.report read.report
#' @importFrom gdx readGDX
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% mutate select rename collapse
#' @export
ValidationMif <- function(.path, mif_name = "fullValidation2.mif", Validation_data_for_plots = TRUE) {
  
  if (Validation_data_for_plots == TRUE) {
    val_data <- ValidationMif2(.path[1], mif_name)[[1]]
    # rename GLO to World
    reports_val <- lapply(val_data, function(x) {
    regions <- getRegions(x)
    if ("GLO" %in% regions) {
      getRegions(x)[regions == "GLO"] <- "World"
    }
    x
  })
    region_sig <- function(x) paste(sort(getRegions(x)), collapse = "|")
    sigs <- vapply(reports_val, region_sig, FUN.VALUE = character(1))
    
    # Find the most common region set
    most_common_sig <- names(sort(table(sigs), decreasing = TRUE))[1]
    
    # Keep only magpie objects with that region set
    same_region_list <- reports_val[sigs == most_common_sig]
    
    # Combine them
    if (length(same_region_list) > 1) {
      combined_all <- do.call(mbind, same_region_list)
    } else if (length(same_region_list) == 1) {
      combined_all <- same_region_list[[1]]
    } else {
      combined_all <- NULL
      warning("No magpie objects with identical regions found.")
    }
    reports_val <- combined_all
    reports_val <- as.quitte(reports_val) %>% as.magpie()
    getItems(reports_val, 3.1) <- paste0(getItems(reports_val, 3.1), "|VAL")
  } else {reports_val = NULL}
  
  ###### projections
  Validation <- ValidationMif2(.path[1], mif_name)[[2]]
  
  proj_IEA <- mbind(Validation[[3]], Validation[[4]])

  proj_IEA <- as.quitte(proj_IEA) %>% as.magpie()
  getItems(proj_IEA, 3.1) <- paste0(getItems(proj_IEA, 3.1), "|Validation")
  proj_IEA <- add_columns(proj_IEA, addnm = setdiff(getRegions(reports_val),getRegions(proj_IEA)), dim = 1, fill = NA)
  proj_IEA <- proj_IEA[getRegions(reports_val),, ]
  
  reports_val <- as.quitte(reports_val) %>% as.magpie()
  
  reports_val <- mbind(reports_val, proj_IEA)
  
  return(reports_val)
}

ValidationMif2 <- function(runpath, mif_name) {
  base_dir <- dirname(dirname(runpath[1]))
  validation_path <- file.path(base_dir, mif_name)
  
  if (file.exists(validation_path)) {
    fullVALIDATION2 <- read.report(validation_path)
  } else {
    message(paste0(
      "Skipping validation data for plots: 'fullValidation2.mif' not found in ",
      base_dir
    ))
  }
  historical <- fullVALIDATION2[["historical"]]
  Validation <- fullVALIDATION2[["Validation"]]
  return(list(historical, Validation))
}