#' Process and Aggregate Secondary Energy Data
#'
#' This function processes and aggregates secondary energy data.
#' It maps fuel categories, aggregates data by reporting categories, and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated secondary energy data.
#'
#' @examples
#' \dontrun{
#' result <- reportSE(system.file("extdata", "blabla.gdx", package = "openprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind
#' @importFrom quitte as.quitte
#' @importFrom dplyr mutate %>%
#' @importFrom madrat toolAggregate
#' @export
reportSE <- function(path, regions, years) {
  # add model OPEN-PROM data electricity production
  VProdElec <- readGDX(path, "VProdElec", field = "l")[regions, years, ]
  VProdElec <- as.quitte(VProdElec) %>% as.magpie()

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

  VProdElec_without_aggr <- VProdElec
  getItems(VProdElec_without_aggr, 3) <- paste0("Secondary Energy|Electricity|", getItems(VProdElec_without_aggr, 3))

  magpie_object <- NULL
  VProdElec_without_aggr <- add_dimension(VProdElec_without_aggr, dim = 3.2, add = "unit", nm = "TWh")
  magpie_object <- mbind(magpie_object, VProdElec_without_aggr)

  VProdElec_LGN <- VProdElec
  # aggregate to reporting fuel categories
  VProdElec <- toolAggregate(VProdElec[, , PGALLtoEF[["PGALL"]]], dim = 3, rel = PGALLtoEF, from = "PGALL", to = "EF")
  VProdElec_LGN <- toolAggregate(VProdElec_LGN[, , add_LGN[["PGALL"]]], dim = 3, rel = add_LGN, from = "PGALL", to = "EF")

  getItems(VProdElec_LGN, 3) <- paste0("Secondary Energy|Electricity|", getItems(VProdElec_LGN, 3))

  VProdElec_LGN <- add_dimension(VProdElec_LGN, dim = 3.2, add = "unit", nm = "TWh")
  magpie_object <- mbind(magpie_object, VProdElec_LGN)

  getItems(VProdElec, 3) <- paste0("Secondary Energy|Electricity|", getItems(VProdElec, 3))

  VProdElec <- add_dimension(VProdElec, dim = 3.2, add = "unit", nm = "TWh")
  magpie_object <- mbind(magpie_object, VProdElec)

  # electricity production
  VProdElec_total <- dimSums(VProdElec, dim = 3, na.rm = TRUE)

  getItems(VProdElec_total, 3) <- "Secondary Energy|Electricity"

  VProdElec_total <- add_dimension(VProdElec_total, dim = 3.2, add = "unit", nm = "TWh")
  magpie_object <- mbind(magpie_object, VProdElec_total)

  VprodElecChp <- readGDX(path, "VprodElecChp", field = "l")[regions, years, ]
  VProdElecChp_total <- dimSums(VprodElecChp, dim = 3, na.rm = TRUE)
  getItems(VProdElecChp_total, 3) <- "Secondary Energy|Electricity|CHP"
  VProdElecChp_total <- add_dimension(VProdElecChp_total, dim = 3.2, add = "unit", nm = "TWh")

  magpie_object <- mbind(magpie_object, VProdElecChp_total)
  return(magpie_object)
}
