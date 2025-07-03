#' Soft-Link OPEN-PROM Outputs to MAgPIE Inputs
#'
#' This function facilitates the soft-linking between the OPEN-PROM model and the MAgPIE framework.
#' It extracts pollutant price and bioenergy demand data from OPEN-PROM outputs and reformats them
#' into CSV files compatible with MAgPIE’s input structure. Depending on the `forward` flag, the function
#' can perform either forward linking from OPEN-PROM to MAgPIE or backward linking in the other direction.
#'
#' @param path A character string specifying the file path to the GDX output from OPEN-PROM.
#' @param pathPollutantPrices A character string indicating the path to the pollutant prices CSV file used by MAgPIE.
#' @param pathBioenergyDemand A character string pointing to the bioenergy demand CSV file for MAgPIE.
#' @param forward Logical; if `TRUE`, performs data extraction from OPEN-PROM and reformatting for MAgPIE.
#' If `FALSE`, executes the reverse linking using `MAgPIE2OPEN()`. Defaults to `TRUE`.
#'
#' @return No return value. Writes updated pollutant prices and bioenergy demand CSVs to disk.
#'
#' @details
#' - Reads region mappings and variables (e.g., `VmCarVal`, `V03ConsGrssInl`) from the GDX file.
#' - Aggregates and interpolates missing time periods to generate consistent time-series data.
#' - Writes two output files: one for pollutant prices and another for bioenergy demand, both for MAgPIE.
#' - The pollutant prices are modified for the `"co2_c"` pollutant, applying a conversion factor.
#' - Bioenergy demand is converted from Mtoe to PJ and formatted for compatibility with MAgPIE's data structure.
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr %>% select filter rename mutate inner_join
#' @importFrom magclass as.magpie collapseDim add_dimension
#'
#' @examples
#' \dontrun{
#' linkPromToMagpie("output.gdx", "pollutant_prices.csv", "bioenergy_demand.csv")
#' }
#' @export
linkPromToMagpie <- function(path, pathPollutantPrices,
                             pathBioenergyDemand, forward = TRUE) {
  message("[✓] Starting soft-linking routine...")
  if (forward == TRUE) OPEN2MAgPIE(path, pathPollutantPrices, pathBioenergyDemand)
  else if (forward == FALSE) MAgPIE2OPEN()
  else message("Wrong forward input")

}

# Helpers ------------------------------------------------------------------
OPEN2MAgPIE <- function(path, pathPollutantPrices, pathBioenergyDemand) {
  message("[OPEN2MAgPIE] Extracting outputs from OPEN-PROM...")
  regionMapping <- toolGetMapping(name = "regionmappingOPDEV3.csv",
                                  type = "regional",
                                  where = "mrprom")

  regionMapping2 <- toolGetMapping(name = "h12.csv",
                                   type = "regional",
                                   where = "mrprom")
  names(regionMapping2)<-names(regionMapping)
  mapping <- left_join(regionMapping, regionMapping2, by=c("Full.Country.Name", "ISO3.Code"))

  variables <- readGDX(path, c("VmcarVal", "V03ConsGrssInl"), field = "l")

  co2 <- variables$VmCarVal
  co2_magpie <- toolAggregate(co2[unique(mapping[,"Region.Code.x"]),,],dim = 1,rel = mapping, from = "Region.Code.y", to = "Region.Code.x")
  co2_magpie <- as.quitte(co2_magpie) %>%
    interpolate_missing_periods(period = 1995:2150, expand.values = TRUE) %>%
    as.magpie()
  co2_magpie[, 1995:2020,] <- co2_magpie[, 2021,]
  co2_magpie <- co2_magpie[, seq(1995,2150,by=5), "TRADE"]
  co2_magpie <-collapseDim(co2_magpie,3)
  co2_magpie <- add_dimension(co2_magpie, dim = 3.1, add = "pollutant", nm = "co2_c")

  xq <- as.quitte(co2_magpie) %>%
    select(c("period", "region", "pollutant","value")) %>%
    mutate(value = value * 103.42/100.) %>%
    rename(years = period)

  test <- xq
  names(test) <- c("year", "region", "var", "R32M46-SSP2EU-NPi")
  test$year <- paste0("y", test$year)
  final <- read.csv(pathPollutantPrices, skip = 5, check.names = FALSE)
  temp <- names(final)
  names(final)[1:3] <- c("year", "region", "var")

  temp <- final %>%
    inner_join(test, by=c("year", "region", "var")) %>%
    select("R32M46-SSP2EU-NPi.y")
  final["R32M46-SSP2EU-NPi"][final$var == "co2_c", ]<- temp

  write.csv(final, "result_f56_pollutant_prices.cs3", quote = FALSE, row.names = FALSE)


  countries <- unique(mapping[,"Region.Code.x"])
  biomass_demand <- variables$V03ConsGrssInl
  biomass_demand <- toolAggregate(biomass_demand[countries,,],dim = 1,rel = mapping, from = "Region.Code.y", to = "Region.Code.x")
  biomass_demand <- as.quitte(biomass_demand) %>%
    interpolate_missing_periods(period = 1995:2150, expand.values = TRUE) %>%
    as.magpie()
  biomass_demand[, 1995:2020,] <- biomass_demand[, 2021,]
  biomass_demand <- biomass_demand[, seq(1995,2150,by=5), "BMSWAS"]

  MtoeToPJ <- 41.868
  biomass_demand <- biomass_demand * MtoeToPJ
  biomass_demand <-collapseDim(biomass_demand,3)
  biomass_demand <- add_dimension(biomass_demand, dim = 3.2, add = "value", nm = NA)

  xq <- as.quitte(biomass_demand)
  xq <- xq %>%
    select(c("period", "region","value")) %>%
    rename(years = period)

  test <- xq
  names(test) <- c("year", "region", "woodfuel")
  test$year <- paste0("y", test$year)
  test['var'] <- "const2020"
  final <- read.csv(pathBioenergyDemand, skip = 4, check.names = FALSE)
  names(final)[1:3] <- c("year", "region", "var")
  final <- filter(final, year>="y1995" & year<="y2150")
  final[ , !names(final) %in% c("year", "region", "var")] <- 0

  temp <- final %>%
    inner_join(test, by=c("year", "region", "var")) %>%
    select("woodfuel.y")
  final["woodfuel"][final$var == "const2020", ] <- temp

  write.csv(final, "result_f60_1stgen_bioenergy_dem.cs3", quote = FALSE, row.names = FALSE)
  }
