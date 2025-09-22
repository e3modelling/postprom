#' Process and Aggregate Final Energy Data
#'
#' A function to process and aggregate final energy consumption data from a GDX file.
#' This function reads and maps data from model subsectors and fuels to reporting categories,
#' aggregates it by subsectors and energy forms, and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated final energy data.
#'
#' @examples
#' \dontrun{
#' result <- reportFinalEnergy(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#' @importFrom gdx readGDX
#' @importFrom madrat toolAggregate
#' @importFrom magclass getItems add_dimension mbind dimSums
#' @importFrom dplyr select filter arrange bind_rows %>%
#' @importFrom stringr str_replace_all
#' @export
reportFinalEnergy <- function(path, regions, years) {
  # read GAMS set used for reporting of Final Energy

  sets <- readGDX(path, "BALEF2EFS")
  names(sets) <- c("BAL", "EF")
  sets[["BAL"]] <- gsub("Gas fuels", "Gases", sets[["BAL"]])
  sets[["BAL"]] <- gsub("Steam", "Heat", sets[["BAL"]])

  # add model OPEN-PROM data Total final energy consumnption (Mtoe)
  VConsFinEneCountry <- readGDX(path, name = c("VmConsFinEneCountry", "VConsFinEneCountry"),
                                field = "l", format = "first_found")[regions, years, ]

  # aggregate from PROM fuels to reporting fuel categories
  VConsFinEneCountry <- toolAggregate(VConsFinEneCountry[, , unique(sets$EF)], dim = 3, rel = sets, from = "EF", to = "BAL")
  getItems(VConsFinEneCountry, 3) <- paste0("Final Energy|", getItems(VConsFinEneCountry, 3))

  l <- getNames(VConsFinEneCountry) == "Final Energy|Total"
  getNames(VConsFinEneCountry)[l] <- "Final Energy"

  VConsFinEneCountry <- add_dimension(VConsFinEneCountry, dim = 3.2, add = "unit", nm = "Mtoe")

  magpie_object <- mbind(NULL, VConsFinEneCountry)

  # Link between Model Subsectors and Fuels
  sets4 <- readGDX(path, "SECtoEF")

  # OPEN-PROM sectors
  sector <- c("TRANSE", "INDSE", "DOMSE", "NENSE")
  sector_name <- c("Transportation", "Industry", "Residential and Commercial", "Non Energy and Bunkers")

  # variables of OPEN-PROM related to sectors
  blabla_var <- c("VmDemFinEneTranspPerFuel", "VmConsFuel", "VmConsFuel", "VmConsFuel")

  for (y in 1:length(sector)) {
    # read GAMS set used for reporting of Final Energy different for each sector
    sets6 <- readGDX(path, sector[y]) %>% as.data.frame()
    names(sets6) <- sector[y]

    var_gdx <- readGDX(path, blabla_var[y], field = "l")[regions, years, ]
    FCONS_by_sector_and_EF_open <- var_gdx[, , sets6[, 1]]

    map_subsectors <- sets4 %>% filter(SBS %in% as.character(sets6[, 1]))

    map_subsectors$EF <- paste(map_subsectors$SBS, map_subsectors$EF, sep = ".")

    # aggregate from PROM fuels to subsectors
    FCONS_by_sector_open <- toolAggregate(FCONS_by_sector_and_EF_open[, , unique(map_subsectors$EF)], dim = 3, rel = map_subsectors, from = "EF", to = "SBS")
    getItems(FCONS_by_sector_open, 3) <- paste0("Final Energy|", sector_name[y], "|", getItems(FCONS_by_sector_open, 3))

    FCONS_by_sector_open <- add_dimension(FCONS_by_sector_open, dim = 3.2, add = "unit", nm = "Mtoe")

    magpie_object <- mbind(magpie_object, FCONS_by_sector_open)

    # Final Energy by sector
    sector_open <- dimSums(FCONS_by_sector_open, dim = 3, na.rm = TRUE)
    getItems(sector_open, 3) <- paste0("Final Energy|", sector_name[y])

    sector_open <- add_dimension(sector_open, dim = 3.2, add = "unit", nm = "Mtoe")
    magpie_object <- mbind(magpie_object, sector_open)

    # Energy Forms Aggregations
    sets5 <- readGDX(path, "EFtoEFA")

    # Add electricity, Hydrogen, Biomass and Waste
    ELC <- readGDX(path, "ELCEF") %>% as.data.frame()
    names(ELC) <- "ELCEF"

    sets5[nrow(sets5) + 1, ] <- ELC[1, 1]

    rename_EFA <- c(
      "SLD" = "Solids",
      "LQD" = "Liquids",
      "OLQT" = "All liquids but GDO, RFO, GSL",
      "GAS" = "Gases",
      "NFF" = "Non Fossil Fuels",
      "REN" = "Renewables except Hydro",
      "NEF" = "New energy forms",
      "STE" = "Heat",
      "ELC" = "Electricity"
    )
    sets5$EFA <- str_replace_all(sets5$EFA, rename_EFA)

    sets10 <- sets5 %>% filter(EF %in% getItems(var_gdx[, , sets6[, 1]], 3.2))

    # Aggregate model OPEN-PROM by subsector and by energy form
    by_energy_form_and_by_subsector_open <- toolAggregate(FCONS_by_sector_and_EF_open[, , as.character(unique(sets10$EF))], dim = 3.2, rel = sets10, from = "EF", to = "EFA")

    # sector by subsector and by energy form form OPEN-PROM
    open_by_subsector_by_energy_form <- by_energy_form_and_by_subsector_open
    getItems(open_by_subsector_by_energy_form, 3.1) <- paste0("Final Energy|", sector_name[y], "|", getItems(open_by_subsector_by_energy_form, 3.1))

    # remove . from magpie object and replace with |
    open_by_subsector_by_energy_form <- as.quitte(open_by_subsector_by_energy_form)
    open_by_subsector_by_energy_form[[names(open_by_subsector_by_energy_form[, 8])]] <- paste0(open_by_subsector_by_energy_form[[names(open_by_subsector_by_energy_form[, 8])]], "|", open_by_subsector_by_energy_form[["EF"]])
    open_by_subsector_by_energy_form <- select(open_by_subsector_by_energy_form, -c("EF"))
    open_by_subsector_by_energy_form <- as.quitte(open_by_subsector_by_energy_form) %>% as.magpie()

    open_by_subsector_by_energy_form <- add_dimension(open_by_subsector_by_energy_form, dim = 3.2, add = "unit", nm = "Mtoe")
    magpie_object <- mbind(magpie_object, open_by_subsector_by_energy_form)

    # sector_by_energy_form
    by_energy_form_open <- dimSums(by_energy_form_and_by_subsector_open, 3.1, na.rm = TRUE)
    getItems(by_energy_form_open, 3.1) <- paste0("Final Energy|", sector_name[y], "|", getItems(by_energy_form_open, 3.1))

    by_energy_form_open <- add_dimension(by_energy_form_open, dim = 3.2, add = "unit", nm = "Mtoe")
    magpie_object <- mbind(magpie_object, by_energy_form_open)

    # per fuel
    FCONS_per_fuel <- FCONS_by_sector_and_EF_open[, , sets6[, 1]][, , !(getItems(FCONS_by_sector_and_EF_open, 3.2)) %in% (getItems(by_energy_form_and_by_subsector_open, 3.2))]

    # remove . from magpie object and replace with |
    FCONS_per_fuel <- as.quitte(FCONS_per_fuel)
    FCONS_per_fuel[[names(FCONS_per_fuel[, 8])]] <- paste0(FCONS_per_fuel[[names(FCONS_per_fuel[, 8])]], "|", FCONS_per_fuel[["EF"]])
    FCONS_per_fuel <- select(FCONS_per_fuel, -c("EF"))
    FCONS_per_fuel <- as.quitte(FCONS_per_fuel) %>% as.magpie()
    getItems(FCONS_per_fuel, 3) <- paste0("Final Energy|", sector_name[y], "|", getItems(FCONS_per_fuel, 3))

    FCONS_per_fuel <- add_dimension(FCONS_per_fuel, dim = 3.2, add = "unit", nm = "Mtoe")
    magpie_object <- mbind(magpie_object, FCONS_per_fuel)
  }
  
  #########       DAC     #################
  
  VmConsFuelTechDACProd <- readGDX(path, c("VmConsFuelTechDACProd"), field = "l")[regions, years, ]
  
  VmConsFuelTechDACProd_EWDAC <- dimSums(VmConsFuelTechDACProd[,,"EWDAC"][,,"ELC"], dim = 3.1)
  getItems(VmConsFuelTechDACProd_EWDAC, 3) <- paste0("Final Energy|Enhanced Weathering")
  VmConsFuelTechDACProd_EWDAC2 <- VmConsFuelTechDACProd_EWDAC
  getItems(VmConsFuelTechDACProd_EWDAC2, 3) <- paste0("Final Energy|Enhanced Weathering|Electricity")
  
  Direct_Air_Capture <- dimSums(VmConsFuelTechDACProd[,,c("HTDAC","LTDAC")], dim = 3)
  getItems(Direct_Air_Capture, 3) <- paste0("Final Energy|Direct Air Capture")
  
  Direct_Air_Capture_HTDAC <- dimSums(VmConsFuelTechDACProd[,,c("HTDAC")], dim = 3)
  getItems(Direct_Air_Capture_HTDAC, 3) <- paste0("Final Energy|Direct Air Capture|HTDAC")
  
  Direct_Air_Capture_LTDAC <- dimSums(VmConsFuelTechDACProd[,,c("LTDAC")], dim = 3)
  getItems(Direct_Air_Capture_LTDAC, 3) <- paste0("Final Energy|Direct Air Capture|LTDAC")
  
  Direct_Air_Capture_HTDAC_ELC <- dimSums(VmConsFuelTechDACProd[,,c("HTDAC")][,,"ELC"], dim = 3)
  getItems(Direct_Air_Capture_HTDAC_ELC, 3) <- paste0("Final Energy|Direct Air Capture|HTDAC|ELC")
  Direct_Air_Capture_HTDAC_NGS <- dimSums(VmConsFuelTechDACProd[,,c("HTDAC")][,,"NGS"], dim = 3)
  getItems(Direct_Air_Capture_HTDAC_NGS, 3) <- paste0("Final Energy|Direct Air Capture|HTDAC|NGS")
  
  Direct_Air_Capture_LTDAC_ELC <- dimSums(VmConsFuelTechDACProd[,,c("LTDAC")][,,"ELC"], dim = 3)
  getItems(Direct_Air_Capture_LTDAC_ELC, 3) <- paste0("Final Energy|Direct Air Capture|LTDAC|ELC")
  
  DAC <- mbind(VmConsFuelTechDACProd_EWDAC, VmConsFuelTechDACProd_EWDAC2, Direct_Air_Capture,
               Direct_Air_Capture_HTDAC, Direct_Air_Capture_LTDAC, Direct_Air_Capture_HTDAC_ELC,
               Direct_Air_Capture_HTDAC_NGS, Direct_Air_Capture_LTDAC_ELC)
  
  DAC <- add_dimension(DAC, dim = 3.2, add = "unit", nm = "Mtoe")
  magpie_object <- mbind(magpie_object, DAC)
  
  #######################################
  
  return(magpie_object)
}
