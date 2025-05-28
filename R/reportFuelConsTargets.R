#' Process and Aggregate Fuel Consumption targets
#'
#' This function processes and aggregates Fuel Consumption targets from csv files in folder data.
#' Is including final consumption for TRANSE,NENSE,INDSE,DOMSE sectors.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated Fuel Consumption.
#'
#' @examples
#' \dontrun{
#' result <- reportFuelConsTargets(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind collapseDim
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter left_join mutate select group_by %>%
#' @importFrom tidyr pivot_longer
#' @export
reportFuelConsTargets <- function(path, regions, years) {
  
  DOMSE <- read.csv(file.path(dirname(path), "data", "iFuelConsDOMSE.csv"))
  INDSE <- read.csv(file.path(dirname(path), "data", "iFuelConsINDSE.csv"))
  NENSE <- read.csv(file.path(dirname(path), "data", "iFuelConsNENSE.csv"))
  TRANSE <- read.csv(file.path(dirname(path), "data", "iFuelConsTRANSE.csv"))
  
  FuelCons <- rbind(DOMSE, INDSE, NENSE, TRANSE)
  
  names(FuelCons) <- gsub("X","",names(FuelCons))
  FuelCons <- FuelCons %>% pivot_longer(!c("dummy","dummy.1","dummy.2"), names_to = "period", values_to = "value")
  names(FuelCons)[1] <- "region"
  names(FuelCons)[2] <- "variable"
  names(FuelCons)[3] <- "fuel"
  FuelCons <- as.quitte(FuelCons)
  
  FuelCons <- as.magpie(FuelCons)
  FuelCons <- FuelCons[regions, years, ]
  
  sector <- c("TRANSE", "INDSE", "DOMSE", "NENSE")
  sector_name <- c("Transportation", "Industry", "Residential and Commercial", "Non Energy and Bunkers")
  
  magpie_object <- NULL
  
  for (y in 1:length(sector)) {
    # read GAMS set used for reporting of Targets different for each sector
    sets6 <- readGDX(path, sector[y]) %>% as.data.frame()
    names(sets6) <- sector[y]
    FuelCons_sector <- FuelCons[,,sets6[,1]]
    FuelCons_sector_by_subsector <- dimSums(FuelCons_sector,3.2)
    getItems(FuelCons_sector_by_subsector, 3) <- paste0("Targets|", sector_name[y], "|", getItems(FuelCons_sector_by_subsector, 3))
    FuelCons_sector_by_sector <- dimSums(FuelCons_sector,3)
    getItems(FuelCons_sector_by_sector, 3) <- paste0("Targets|", sector_name[y])
    FuelCons_sector_by_fuel <- dimSums(FuelCons_sector,3.1)
    getItems(FuelCons_sector_by_fuel, 3) <- paste0("Targets|", sector_name[y], "|", getItems(FuelCons_sector_by_fuel, 3))
    FuelCons_by_sector_s <- FuelCons_sector
    getItems(FuelCons_by_sector_s, 3.1)  <- paste0("Targets|", sector_name[y], "|", getItems(FuelCons_by_sector_s, 3.1))
    
    # remove . from magpie object and replace with |
    FuelCons_by_sector_s <- as.quitte(FuelCons_by_sector_s)
    FuelCons_by_sector_s[[names(FuelCons_by_sector_s[, 4])]] <- paste0(FuelCons_by_sector_s[[names(FuelCons_by_sector_s[, 4])]], "|", FuelCons_by_sector_s[["fuel"]])
    FuelCons_by_sector_s <- select(FuelCons_by_sector_s, -c("fuel"))
    FuelCons_by_sector_s <- as.quitte(FuelCons_by_sector_s) %>% as.magpie()
    
    magpie_object <- mbind(magpie_object, FuelCons_by_sector_s, FuelCons_sector_by_fuel, FuelCons_sector_by_sector, FuelCons_sector_by_subsector)
  }
  
  FuelCons <- dimSums(FuelCons,3)
  getItems(FuelCons, 3) <- "Targets"
  magpie_object <- mbind(magpie_object, FuelCons)
  
  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = "Mtoe")
  
  return(magpie_object)
}
