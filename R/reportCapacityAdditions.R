#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind
#' @importFrom madrat toolAggregate
#' @importFrom stringr str_replace_all
#' @export
reportCapacityAdditions <- function(path, regions, years) {
  VNewCapElec <- readGDX(path, "VNewCapElec", field = "l")[regions, years, ]

  PGALLtoEF <- readGDX(path, "PGALLtoEF")
  names(PGALLtoEF) <- c("PGALL", "EF")

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

  VNewCapElec_without_aggr <- VNewCapElec
  getItems(VNewCapElec_without_aggr, 3) <- paste0("Capacity Additions|Electricity|", getItems(VNewCapElec_without_aggr, 3))

  magpie_object <- NULL
  VNewCapElec_without_aggr <- add_dimension(VNewCapElec_without_aggr, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, VNewCapElec_without_aggr)


  # aggregate to reporting fuel categories
  VNewCapElec_LGN <- toolAggregate(VNewCapElec[, , add_LGN[["PGALL"]]], dim = 3, rel = add_LGN, from = "PGALL", to = "EF")
  VNewCapElec <- toolAggregate(VNewCapElec[, , PGALLtoEF[["PGALL"]]], dim = 3, rel = PGALLtoEF, from = "PGALL", to = "EF")

  getItems(VNewCapElec_LGN, 3) <- paste0("Capacity Additions|Electricity|", getItems(VNewCapElec_LGN, 3))

  VNewCapElec_LGN <- add_dimension(VNewCapElec_LGN, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, VNewCapElec_LGN)

  getItems(VNewCapElec, 3) <- paste0("Capacity Additions|Electricity|", getItems(VNewCapElec, 3))

  VNewCapElec <- add_dimension(VNewCapElec, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, VNewCapElec)

  # electricity production
  VNewCapElec_total <- dimSums(VNewCapElec, dim = 3, na.rm = TRUE)
  getItems(VNewCapElec_total, 3) <- "Capacity Additions|Electricity"

  VNewCapElec_total <- add_dimension(VNewCapElec_total, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, VNewCapElec_total)
  return(magpie_object)
}
