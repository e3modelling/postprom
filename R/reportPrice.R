#' Process and Aggregate Final Energy Prices
#'
#' This function processes and aggregates electricity and fuel price data from a GDX file.
#' It maps energy forms to reporting categories, calculates averages, and formats the data into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed energy price data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportPrice(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind as.magpie
#' @importFrom madrat toolAggregate
#' @importFrom quitte as.quitte
#' @importFrom dplyr select filter mutate left_join distinct %>%
#' @importFrom tidyr drop_na
#' @export
reportPrice <- function(path, regions, years) {
  #add model OPEN-PROM data Electricity prices

  set_names <- c(
    "iSet",
    "rSet",
    "SECtoEF", # Link between Model Subsectors and Fuels
    "BALEF2EFS" # GAMS set used for reporting of Final Energy
  )
  VPriceElecIndResConsu <- readGDX(path, "VmPriceElecIndResConsu", field = 'l')[regions, years, ]
  sets <- readGDX(path, set_names)

  sets_i <- data.frame(iSet = sets$iSet)
  sets_r <- data.frame(rSet = sets$rSet)

  names(sets$BALEF2EFS) <- c("BAL", "EF")
  sets$BALEF2EFS[["BAL"]] <- gsub("Gas fuels", "Gases", sets$BALEF2EFS[["BAL"]])
  sets$BALEF2EFS[["BAL"]] <- gsub("Steam", "Heat", sets$BALEF2EFS[["BAL"]])

  elec_prices_Industry <- VPriceElecIndResConsu[,,sets_i[1,1]]
  getNames(elec_prices_Industry) <- "Price|Final Energy|Industry|Electricity"

  elec_prices_Residential <- VPriceElecIndResConsu[,,sets_r[1,1]]
  getNames(elec_prices_Residential) <- "Price|Final Energy|Residential|Electricity"

  elec_prices <- mbind(elec_prices_Industry, elec_prices_Residential)
  elec_prices <- add_dimension(elec_prices, dim = 3.2, add = "unit", nm = "US$2015/KWh")
  magpie_object <- mbind(NULL, elec_prices)


  sector <- c("TRANSE", "INDSE", "DOMSE", "NENSE", "PG")
  sector_name <- c("Transportation", "Industry", "Residential and Commercial", "Non Energy and Bunkers",
                   "Power and Steam Generation")


  z <- NULL
  for (y in seq_along(sector)) {
    # read GAMS set used for reporting of Final Energy different for each sector
    sets6 <- NULL
    # load current postprom set configuration for each sector
    sets6 <- tryCatch({
      readGDX(path, sector[y])
    }, warning = function(w) {
      message("Custom warning: Sector ", sector[y], " not found in GDX. Returning NULL.")
      sector[y]
    })
    sets6 <- as.data.frame(sets6)

    map_subsectors <- sets$SECtoEF %>% filter(SBS %in% as.character(sets6[, 1]))
    map_subsectors$EF = paste(map_subsectors$SBS, map_subsectors$EF, sep=".")

    #add model OPEN-PROM data VPriceFuelSubsecCarVal
    iFuelPrice <- readGDX(path, "VmPriceFuelSubsecCarVal", field = "l")[regions, years, ][,,map_subsectors$EF]
    PRICE_by_sector_and_EF <- iFuelPrice
    # complete names
    getItems(PRICE_by_sector_and_EF, 3.1) <- paste0("Price|Final Energy|", sector_name[y],"|", getItems(PRICE_by_sector_and_EF, 3.1))
    # remove . from magpie object and replace with |
    PRICE_by_sector_and_EF <- as.quitte(PRICE_by_sector_and_EF)
    PRICE_by_sector_and_EF[["SBS"]] <- paste0(PRICE_by_sector_and_EF[["SBS"]], "|", PRICE_by_sector_and_EF[["EF"]])
    PRICE_by_sector_and_EF <- select(PRICE_by_sector_and_EF, -c("variable","EF"))
    PRICE_by_sector_and_EF <- as.quitte(PRICE_by_sector_and_EF) %>% as.magpie()

    PRICE_by_sector_and_EF <- add_dimension(PRICE_by_sector_and_EF, dim = 3.2, add = "unit", nm = "KUS$2015/toe")
    magpie_object <- mbind(magpie_object, PRICE_by_sector_and_EF)

    #aggregation by SECTOR and EF
    iFuelPrice2 <- iFuelPrice %>%
      as.quitte() %>%
      mutate(value = mean(value, na.rm = TRUE), .by = c("model", "scenario","period", "variable","EF")) %>%
      select(c("EF", "model", "scenario", "region", "variable", "unit", "period", "value")) %>%
      distinct()
    PRICE_by_EF_OPEN_PROM <- as.quitte(iFuelPrice2) %>% as.magpie()

    # complete names
    getItems(PRICE_by_EF_OPEN_PROM, 3) <- paste0("Price|Final Energy|", sector_name[y],"|", getItems(PRICE_by_EF_OPEN_PROM, 3))
    PRICE_by_EF_OPEN_PROM <- add_dimension(PRICE_by_EF_OPEN_PROM, dim = 3.2, add = "unit", nm = "KUS$2015/toe")
    magpie_object <- mbind(magpie_object, PRICE_by_EF_OPEN_PROM)

    # Fuel categories
    # Energy Forms Aggregations
    EFtoEFA <- readGDX(path, "EFtoEFA")
    EFtoEFA <- EFtoEFA[grep("^STE", EFtoEFA[,1]),]
    EFtoEFA[,2] <- "Heat"
    names(EFtoEFA) <- sub("EFA", "BAL", names(EFtoEFA))
    sets$BALEF2EFS <- rbind(sets$BALEF2EFS, EFtoEFA)
    # aggregate from fuels to reporting fuel categories
    sum_open_prom <- iFuelPrice %>%
      as.quitte() %>%
      left_join(sets$BALEF2EFS, by = "EF") %>% ## add mapping
      mutate(value = mean(value, na.rm = TRUE), .by = c("model", "scenario", "region",
                                                        "unit","period","BAL" )) %>%
      distinct() %>%
      select(c("model","scenario","region","unit", "period","value","BAL")) %>%
      distinct() %>%
      drop_na() %>%
      as.quitte() %>%
      as.magpie()
    # complete names
    getItems(sum_open_prom, 3) <- paste0("Price|Final Energy|", sector_name[y],"|", getItems(sum_open_prom, 3))

    sum_open_prom <- add_dimension(sum_open_prom, dim = 3.2, add = "unit", nm = "KUS$2015/toe")
    magpie_object <- mbind(magpie_object, sum_open_prom)
  }
  return(magpie_object)
}
