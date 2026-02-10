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
ValidationMif <- function(.path, mif_name = "fullValidation2.mif", Validation_data_for_plots = Validation_data_for_plots, 
                          reportOPEN_PROM = reports, metadata_run = metadata_run, Validation2050 = Validation2050) {
  
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
  
    ###### projections
    Validation <- ValidationMif2(.path[1], mif_name)[[2]]
    
    proj_IEA <- mbind(Validation[[3]], Validation[[4]])
  
    proj_IEA <- as.quitte(proj_IEA) %>% as.magpie()
    getItems(proj_IEA, 3.1) <- paste0(getItems(proj_IEA, 3.1), "|Validation")
    proj_IEA_World <- dimSums(proj_IEA, 1)
    getItems(proj_IEA_World, 1) <- "World"
    proj_IEA <- mbind(proj_IEA, proj_IEA_World)
    proj_IEA <- add_columns(proj_IEA, addnm = setdiff(getRegions(reports_val),getRegions(proj_IEA)), dim = 1, fill = NA)
    proj_IEA <- proj_IEA[getRegions(reports_val),, ]
    
    reports_val <- as.quitte(reports_val) %>% as.magpie()
    
    Val_Mif_fullval2 <- mbind(reports_val, proj_IEA)
    
    val_years <- getYears(reportOPEN_PROM)
    Val_Mif <- add_columns(Val_Mif_fullval2, addnm = setdiff(getYears(reportOPEN_PROM),getYears(Val_Mif_fullval2)), dim = 2, fill = NA)
    Val_Mif <- Val_Mif[,getYears(reportOPEN_PROM),]
    Val_Mif <- add_columns(Val_Mif, addnm = setdiff(getRegions(reportOPEN_PROM),getRegions(Val_Mif)), dim = 1, fill = NA)
    Val_Mif <- Val_Mif[getRegions(reportOPEN_PROM),,]
    
    Val_Mif[,val_years,"Final Energy|Industry|VAL"] <- Val_Mif[,val_years,"Final Energy|Industry|VAL"] +
      reportOPEN_PROM[,val_years,"Final Energy|Residential and Commercial"] + reportOPEN_PROM[,val_years,"Final Energy|Transportation"] +
      reportOPEN_PROM[,val_years,"Final Energy|Non Energy"] + reportOPEN_PROM[,val_years,"Final Energy|Bunkers"]
    Val_Mif[,val_years,"Final Energy|Transportation|VAL"] <- Val_Mif[,val_years,"Final Energy|Transportation|VAL"] +
      reportOPEN_PROM[,val_years,"Final Energy|Residential and Commercial"] + reportOPEN_PROM[,val_years,"Final Energy|Non Energy"] +
      reportOPEN_PROM[,val_years,"Final Energy|Bunkers"]
    
    if (metadata_run == 1 & Validation2050 == TRUE) {
      Val_Mif[,val_years,"Final Energy|Industry|Validation"] <- Val_Mif[,val_years,"Final Energy|Industry|Validation"] +
        reportOPEN_PROM[,val_years,"Final Energy|Residential and Commercial"] + reportOPEN_PROM[,val_years,"Final Energy|Transportation"] +
        reportOPEN_PROM[,val_years,"Final Energy|Non Energy"] + reportOPEN_PROM[,val_years,"Final Energy|Bunkers"]
      Val_Mif[,val_years,"Final Energy|Transportation|Validation"] <- Val_Mif[,val_years,"Final Energy|Transportation|Validation"] +
        reportOPEN_PROM[,val_years,"Final Energy|Residential and Commercial"] + reportOPEN_PROM[,val_years,"Final Energy|Non Energy"] +
        reportOPEN_PROM[,val_years,"Final Energy|Bunkers"]
    }  else {
      Val_Mif[,val_years,c("Final Energy|Validation","Final Energy|Industry|Validation",
                           "Final Energy|Transportation|Validation",
                           "Secondary Energy|Electricity|Validation","Emissions|CO2|Validation")] <-  NA
    }
    
    reports_with_val <- mbind(reportOPEN_PROM, Val_Mif)
    output <- reports_with_val
    output[output==0]=NA
    
    } else {dummy <- new.magpie(getRegions(reportOPEN_PROM), getYears(reportOPEN_PROM), c("Emissions|CO2|VAL","Emissions|CO2|Validation",
                                                                                    "Final Energy|Transportation|VAL","Final Energy|Transportation|Validation",
                                                                                    "Final Energy|Industry|VAL","Final Energy|Industry|Validation",
                                                                                    "Final Energy|VAL","Final Energy|Validation",
                                                                                    "Secondary Energy|Electricity|VAL","Secondary Energy|Electricity|Validation",
                                                                                    "Capacity|Electricity|VAL"), fill = NA)
    dummy <- add_dimension(dummy, dim = 3.2, add = "unit", nm  = c("Mt CO2/yr","Mt CO2/yr","Mtoe","Mtoe","Mtoe","Mtoe","Mtoe","Mtoe","GW","GW","GW"))
    output <- mbind(reportOPEN_PROM, dummy)
    }
    
  return(output)
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
  historical <- fullVALIDATION2[["historical"]][c(1,4,5,6,7)]
  Validation <- fullVALIDATION2[["Validation"]]
  return(list(historical, Validation))
}