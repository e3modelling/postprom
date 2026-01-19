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
#' @importFrom magclass as.magpie collapseDim add_dimension read.report
#' @importFrom gdx readGDX
#'
#' @examples
#' \dontrun{
#' linkPromToMagpie("output.gdx", "pollutant_prices.csv", "bioenergy_demand.csv")
#' }
#' @export
linkPromToMagpie <- function(path, pathPollutantPrices,
                             pathBioenergyDemand,pathReport=NULL,pathSave=NULL, forward = TRUE, 
                             scenario="R32M46-SSP2EU-NPi") {
  message("[✓] Starting soft-linking routine...")
  if (forward == TRUE) OPEN2MAgPIE(path, pathPollutantPrices, pathBioenergyDemand,scenario)
  else if (forward == FALSE) MAgPIE2OPEN(path, pathReport, pathSave)
  else message("Wrong forward input")

}

# Helpers ------------------------------------------------------------------
OPEN2MAgPIE <- function(path, pathPollutantPrices, pathBioenergyDemand, scenario) {
  message("[OPEN2MAgPIE] Extracting outputs from OPEN-PROM...")
  regionMapping <- toolGetMapping(name = "regionmappingOPDEV5.csv",
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
  names(test) <- c("year", "region", "var", scenario)
  test$year <- paste0("y", test$year)
  final <- read.csv(pathPollutantPrices, skip = 5, check.names = FALSE)
  temp <- names(final)
  names(final)[1:3] <- c("year", "region", "var")

  temp <- final %>%
    inner_join(test, by=c("year", "region", "var")) %>%
    select(paste0(scenario, ".y"))
  final[scenario][final$var == "co2_c", ]<- temp

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


MAgPIE2OPEN <- function(path, pathReport, pathSave) {
  message("[MAgPIE2OPEN] Extracting outputs from MAgPIE report...")
  magpie <- read.report(pathReport)[[1]][[1]]
  vars <- c("Prices|Bioenergy (US$2017/GJ)", "Emissions|CO2|Land (Mt CO2/yr)",
            "Emissions|BC|AFOLU|Land|Fires (Mt BC/yr)","Emissions|CH4|Land (Mt CH4/yr)",
            "Emissions|CO|AFOLU|Land|Fires (Mt CO/yr)","Emissions|N2O|Land (Mt N2O/yr)",
            "Emissions|NH3|Land (Mt NH3/yr)","Emissions|NO2|Land (Mt NO2/yr)",
            "Emissions|NO3-|Land (Mt NO3-/yr)","Emissions|OC|AFOLU|Land|Fires (Mt OC/yr)",
            "Emissions|SO2|AFOLU|Land|Fires (Mt SO2/yr)","Emissions|VOC|AFOLU|Land|Fires (Mt VOC/yr)")
  magpie <- magpie[, , vars]
  MtoeToPJ <- 41.868
  magpie[,, "Prices|Bioenergy (US$2017/GJ)"] <- magpie[,,"Prices|Bioenergy (US$2017/GJ)"]*100/103.42*MtoeToPJ/1000 # to USD2015 & 1/toe
  
  getItems(magpie, 3)[getItems(magpie, 3)=="Prices|Bioenergy (US$2017/GJ)"] <- "Prices|Bioenergy (US$2015/Mtoe)"
  
  Globiom <- readSource("GLOBIOMEU", convert = FALSE)
  
  regionOP <- toolGetMapping(name = "regionmappingOPDEV5.csv",
                             type = "regional",
                             where = "mrprom")
  
  regionMag <- toolGetMapping(name = "h12.csv",
                              type = "regional",
                              where = "mrprom")
  
  EU27 <- setdiff(getRegions(Globiom), "EU27")
  #ELL <- regionOP[which(regionOP[,3]=="ELL"),2]
  dif <- setdiff(regionOP[,3],regionMag[,3])
  inc <- intersect(regionOP[,3],regionMag[,3])
  #in_of_ELL <- intersect(EU27, ELL)
  #rest_of_ELL <- ELL[!(ELL %in% in_of_ELL)]
  
  Globiom <- Globiom[EU27,,]
  Globiom <- as.quitte(Globiom) %>% mutate(value = mean(value, na.rm = TRUE), .by = c("region"))
  #Globiom[["period"]] <- NA
  Globiom <- distinct(Globiom)
  Globiom <- as.quitte(Globiom)
  Globiom <- as.magpie(Globiom)
  Globiom <- Globiom[,2020,]
  Globiom <- dimSums(Globiom,2)
  Globiom_OP <- Globiom[intersect(getRegions(Globiom),regionOP[,3]),,]
  #Globiom_ELL <- mean(Globiom[in_of_ELL,,])
  #eur_map_op_prom <- add_columns(Globiom_OP, addnm = c("ELL"), dim = 1, fill = Globiom_ELL)
  eur_map_op_prom <- Globiom_OP
                                 
  OPEN_PROM_biom1 <- magpie[inc,,setdiff(getItems(magpie,3),"Prices|Bioenergy (US$2015/Mtoe)")]
  Emi_GBR <- readSource("UN_GBR_LULUCF")
  Emi_GBR <- mean(Emi_GBR)
  eur_map_op_prom <- add_columns(eur_map_op_prom, addnm = c("GBR"), dim = 1, fill = Emi_GBR)
  
  rmap <- data.frame(EUR_24 = rep("EUR", 25),
                     EUR_24_OP = getRegions(eur_map_op_prom))
  
  OPEN_PROM_biom2 <- toolAggregate(magpie["EUR",,setdiff(getItems(magpie,3),"Prices|Bioenergy (US$2015/Mtoe)")], rel = rmap, weight = eur_map_op_prom)
  
  OPEN_PROM_biom <- mbind(OPEN_PROM_biom1,OPEN_PROM_biom2)
  
  OPEN_PROM_price1 <- magpie[inc,,"Prices|Bioenergy (US$2015/Mtoe)"]
  
  OPEN_PROM_price3 <-  toolAggregate(magpie["EUR",,"Prices|Bioenergy (US$2015/Mtoe)"], rel = rmap,weight = NULL)
  
  OPEN_PROM_price <- mbind(OPEN_PROM_price1,OPEN_PROM_price3)
  
  OPEN_PROM <- mbind(OPEN_PROM_price,OPEN_PROM_biom)
  
  OPEN_PROM <- as.quitte(OPEN_PROM) %>%
    interpolate_missing_periods(period = min(getYears(OPEN_PROM,as.integer = TRUE)) : max(getYears(OPEN_PROM,as.integer = TRUE)), expand.values = TRUE)
  OPEN_PROM <- as.magpie(OPEN_PROM)
  OPEN_PROM <- OPEN_PROM[,2010 : 2100,]
  
  OPEN_PROM_pri <- OPEN_PROM[,,"Prices|Bioenergy.US$2015/Mtoe"]
  
  SBS <- readGDX(path, c("SBS"), field = "l")
  
  OPEN_PROM_pri <- as.quitte(OPEN_PROM_pri)
  
  df <- expand(OPEN_PROM_pri, nesting(region,variable,unit,period), variable2 = SBS)
  
  df2 <- left_join(df, OPEN_PROM_pri, by = c("region","variable","unit","period"))
  
  df2 <- select(df2,-variable)
  
  names(df2) <- sub("variable2", "variable", names(df2))
  
  df2 <- as.quitte(df2)
  
  xq <- as.quitte(OPEN_PROM[,,setdiff(getItems(OPEN_PROM,3.1),"Prices|Bioenergy")]) %>%
    select(c("region","period", "variable", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iEmissions_magpie.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = file.path("iEmissions_magpie.csv"),
              sep = ",",
              col.names = FALSE,
              append = TRUE)
  
  xq <- as.quitte(df2) %>%
    select(c("region","period" , "variable", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iPrices_magpie.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = file.path("iPrices_magpie.csv"),
              sep = ",",
              col.names = FALSE,
              append = TRUE)
  
}
