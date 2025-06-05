#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind
#' @importFrom madrat toolAggregate
#' @importFrom stringr str_replace_all
#' @export
reportCapacityAdditions <- function(path, regions, years) {
  VNewCapElec <- readGDX(path, "V04NewCapElec", field = "l")[regions, years, ]
  VNetNewCapElec <- readGDX(path, "V04NetNewCapElec", field = "l")[regions, years, ]
  PGALLtoEF <- readGDX(path, "PGALLtoEF")
  names(PGALLtoEF) <- c("PGALL", "EF")

  magpie_object <- helper(VNewCapElec, PGALLtoEF, "Capacity Additions")
  magpie_object <- mbind(magpie_object, helper(VNetNewCapElec, PGALLtoEF, "Net Capacity Additions"))
  return(magpie_object)
}

# Helper ----------------------------------------------------------------------------------------
helper <- function(variable, PGALLtoEF, title) {
  title <- paste0(title, "|Electricity|")
  add_LGN <- as.data.frame(PGALLtoEF[which(PGALLtoEF[, 2] == "LGN"), 1])
  add_LGN["EF"] <- "Lignite"
  names(add_LGN) <- c("PGALL", "EF")

  rename_EF <- c(
    "LGN" = "Coal",
    "HCL" = "Coal",
    "RFO" = "Residual Fuel Oil",
    "GDO" = "Oil",
    "NGS" = "Gas",
    "BMSWAS" = "Biomass",
    "NUC" = "Nuclear",
    "HYD" = "Hydro",
    "WND" = "Wind",
    "SOL" = "Solar",
    "GEO" = "Geothermal"
  )
  PGALLtoEF$EF <- str_replace_all(PGALLtoEF$EF, rename_EF)

  var_without_aggr <- variable
  getItems(var_without_aggr, 3) <- paste0(title, getItems(var_without_aggr, 3))

  magpie_object <- NULL
  var_without_aggr <- add_dimension(var_without_aggr, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, var_without_aggr)


  # aggregate to reporting fuel categories
  var_LGN <- toolAggregate(variable[, , add_LGN[["PGALL"]]], dim = 3, rel = add_LGN, from = "PGALL", to = "EF")
  variable <- toolAggregate(variable[, , PGALLtoEF[["PGALL"]]], dim = 3, rel = PGALLtoEF, from = "PGALL", to = "EF")

  getItems(var_LGN, 3) <- paste0(title, getItems(var_LGN, 3))

  var_LGN <- add_dimension(var_LGN, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, var_LGN)

  getItems(variable, 3) <- paste0(title, getItems(variable, 3))

  variable <- add_dimension(variable, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, variable)

  # electricity production
  var_total <- dimSums(variable, dim = 3, na.rm = TRUE)
  getItems(var_total, 3) <- title

  var_total <- add_dimension(var_total, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, var_total)
  return(magpie_object)
}
