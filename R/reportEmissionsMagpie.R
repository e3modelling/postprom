#' Process and Aggregate CO2 Emissions Data
#'
#' This function processes and aggregates CO2 emissions data from a mif file of
#' Magpie.
#'
#' @param path The file path to the mif data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated CO2 emissions data.
#'
#' @examples
#' \dontrun{
#' result <- reportEmissions(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind collapseDim
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter left_join mutate select group_by %>%

#' @export
reportEmissionsMagpie <- function(path, regions, years) {
  
  magpie <- read.report(paste0(dirname(path), "/report.mif"))[[1]][[1]]
  
  z <- as.data.frame(getItems(magpie,3))
  get_items <- z[grep("^Emissions", getItems(magpie,3)),1]
  
  magpie <- magpie[,,get_items]
  
  magpie <- as.quitte(magpie) %>% as.magpie()
  
  Globiom <- readSource("GLOBIOMEU", convert = FALSE)
  
  years_GBR <- getYears(Globiom, as.integer = TRUE)
  
  regionOP <- toolGetMapping(name = "regionmappingOPDEV5.csv",
                             type = "regional",
                             where = "mrprom")
  
  regionMag <- toolGetMapping(name = "h12.csv",
                              type = "regional",
                              where = "mrprom")
  
  EU27 <- setdiff(getRegions(Globiom), "EU27")
  inc <- intersect(regionOP[,3],regionMag[,3])
  
  Globiom <- Globiom[EU27,,]
  Globiom <- as.quitte(Globiom) %>% mutate(value = mean(value, na.rm = TRUE), .by = c("region"))
  Globiom <- distinct(Globiom)
  Globiom <- as.quitte(Globiom)
  Globiom <- as.magpie(Globiom)
  Globiom <- Globiom[,2020,]
  Globiom <- dimSums(Globiom,2)
  Globiom_OP <- Globiom[intersect(getRegions(Globiom),regionOP[,3]),,]
  eur_map_op_prom <- Globiom_OP
  
  Emi_GBR <- readSource("UN_GBR_LULUCF")
  years_GBR2 <- getYears(Emi_GBR, as.integer = TRUE)
  years_GBR2 <- years_GBR2[years_GBR2 %in% years_GBR]
  Emi_GBR <- Emi_GBR[,years_GBR2,]
  Emi_GBR <- mean(Emi_GBR)
  eur_map_op_prom <- add_columns(eur_map_op_prom, addnm = c("GBR"), dim = 1, fill = Emi_GBR)
  
  rmap <- data.frame(EUR_24 = rep("EUR", 28),
                     EUR_24_OP = getRegions(eur_map_op_prom))
  
  OPEN_PROM_1 <- toolAggregate(magpie["EUR",,], rel = rmap, weight = eur_map_op_prom)
  OPEN_PROM_2 <- magpie[inc,,]
  
  OPEN_PROM <- mbind(OPEN_PROM_1,OPEN_PROM_2)
  
  OPEN_PROM <- as.quitte(OPEN_PROM) %>%
    interpolate_missing_periods(period = min(getYears(OPEN_PROM,as.integer = TRUE)) : max(getYears(OPEN_PROM,as.integer = TRUE)), expand.values = TRUE)
  OPEN_PROM <- as.magpie(OPEN_PROM)
  OPEN_PROM <- OPEN_PROM[,2010 : 2100,]
  
  return(OPEN_PROM)
}