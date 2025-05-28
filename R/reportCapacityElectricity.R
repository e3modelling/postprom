#' Process and Aggregate Electricity Capacity Data
#'
#' This function processes and aggregates electricity capacity data from a GDX file.
#' It maps fuel categories, aggregates data by reporting categories,
#' and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed electricity capacity data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportCapacityElectricity(system.file("extdata", "blabla.gdx", package = "openprom"), c("MEA"))
#' }
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind dimSums
#' @importFrom madrat toolAggregate
#' @importFrom dplyr %>%
#' @export
reportCapacityElectricity <- function(path, regions, years) {
  # add model OPEN-PROM data electricity capacity
  VCapElec2 <- readGDX(path, "VcapElecNominal", field = "l")[regions, years, ]

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

  VCapElec2_without_aggr <- VCapElec2
  getItems(VCapElec2_without_aggr, 3) <- paste0("Capacity|Electricity|", getItems(VCapElec2_without_aggr, 3))

  VCapElec2_without_aggr <- add_dimension(
    VCapElec2_without_aggr,
    dim = 3.2, add = "unit", nm = "GW"
  )
  magpie_object <- mbind(NULL, VCapElec2_without_aggr)

  VCapElec2_LGN <- VCapElec2
  # aggregate to reporting fuel categories
  VCapElec2 <- toolAggregate(
    VCapElec2[, , PGALLtoEF[["PGALL"]]],
    dim = 3, rel = PGALLtoEF, from = "PGALL", to = "EF"
  )
  VCapElec2_LGN <- toolAggregate(
    VCapElec2_LGN[, , add_LGN[["PGALL"]]],
    dim = 3, rel = add_LGN, from = "PGALL", to = "EF"
  )

  getItems(VCapElec2, 3) <- paste0("Capacity|Electricity|", getItems(VCapElec2, 3))

  getItems(VCapElec2_LGN, 3) <- paste0("Capacity|Electricity|", getItems(VCapElec2_LGN, 3))

  VCapElec2_LGN <- add_dimension(
    VCapElec2_LGN,
    dim = 3.2, add = "unit", nm = "GW"
  )
  magpie_object <- mbind(magpie_object, VCapElec2_LGN)

  VCapElec2 <- add_dimension(
    VCapElec2,
    dim = 3.2, add = "unit", nm = "GW"
  )
  magpie_object <- mbind(magpie_object, VCapElec2)

  # electricity production
  VCapElec2_total <- dimSums(VCapElec2, dim = 3, na.rm = TRUE)

  getItems(VCapElec2_total, 3) <- "Capacity|Electricity"

  VCapElec2_total <- add_dimension(
    VCapElec2_total,
    dim = 3.2, add = "unit", nm = "GW"
  )
  magpie_object <- mbind(magpie_object, VCapElec2_total)

  VcapElecChp <- readGDX(path, "VcapElecChp", field = "l")[regions, years, ]
  VcapElecChp_total <- dimSums(VcapElecChp, dim = 3, na.rm = TRUE)
  getItems(VcapElecChp_total, 3) <- "Capacity|Electricity|CHP"
  VcapElecChp_total <- add_dimension(VcapElecChp_total, dim = 3.2, add = "unit", nm = "GW")

  magpie_object <- mbind(magpie_object, VcapElecChp_total)
  return(magpie_object)
}
