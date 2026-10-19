#' Process and Aggregate Primary Energy Data
#'
#' A function to process and aggregate primary energy data from a GDX file.
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated primary energy data.
#'
#' @examples
#' \dontrun{
#' result <- reportPE(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom madrat toolAggregate
#' @importFrom magclass getItems getNames add_dimension mbind collapseDim dimSums
#' @importFrom dplyr filter
#' @importFrom tidyr separate_rows
#' @export
reportPE <- function(path, regions, years) {
  BALEFtoEF <- read.csv(
    system.file("mappings", "BALEFtoEF.csv", package = "postprom")
  ) %>%
    separate_rows(EF) %>%
    filter(BALEF %in% c(
      "Coal", "Gas", "Nuclear", "Biofuels", "Oil", "Electricity", "Heat",
      "Other fuels", "Solar", "Wind", "Geothermal and other renewable sources",
      "Hydrogen", "Hydro"
    )) %>%
    rename(EFS =EF)
  
  #MtoeToEJ <- 0.041868 units to be Mtoe
  V03ConsGrssInl <- readGDX(path, "V03ConsGrssInl", field = "l")[regions, years, ]

  # Primary energy consumption: gross inland consumption net of the domestic
  # transformation output of each fuel (= primary production + net imports)
  OutTotTransf <- dimSums(readGDX(path, "V03OutTotTransf", field = "l")[regions, years, ], dim = 3.1)

  PrimaryEnergy <- V03ConsGrssInl - OutTotTransf
  # Coal and gas codes also hold secondary products (e.g. petroleum coke, biomethane)
  # whose exports can exceed primary supply; floor them at zero
  coalGas <- PrimaryEnergy[, , c("HCL", "LGN", "NGS", "OGS")]
  coalGas[coalGas < 0] <- 0
  PrimaryEnergy[, , c("HCL", "LGN", "NGS", "OGS")] <- coalGas

  PrimaryEnergy <- toolAggregate(PrimaryEnergy,
    weight = NULL, dim = 3,
    rel = BALEFtoEF, from = "BALEF", to = "EFS"
  )

  
  getItems(PrimaryEnergy, 3) <- paste0("Primary Energy|", getItems(PrimaryEnergy, 3))

# -------------------------- Remove non-primary carriers ------------------
# before aggregating, so that the total equals the sum of its children
PrimaryEnergy <- PrimaryEnergy[, ,
  !getItems(PrimaryEnergy, 3) %in% c(
    "Primary Energy|Electricity",
    "Primary Energy|Heat",
    "Primary Energy|Hydrogen"
  )
]

  magpie_object <- helperAggregateLevel(PrimaryEnergy, level = 1, recursive = TRUE)

  # -------------------------- CCS split ------------------------------------
  # Fuel input of CCS power, hydrogen and industry technologies (as in Q06CO2CaptureCCS)
  efCCS <- c("HCL", "LGN", "NGS", "OGS")
  CCS <- readGDX(path, "CCS")
  H2CCS <- readGDX(path, "H2CCS")
  CCSTECH <- readGDX(path, "CCSTECH")
  INDSE1 <- readGDX(path, "INDSE1")
  inputH2 <- readGDX(path, c("i05InputOverOutH2ProdFeed", "i05InputOverOutH2ProdEnergy"))
  fuelCCSElec <- readGDX(path, "VmProdElec", field = "l")[regions, years, CCS] *
    readGDX(path, "smTWhToMtoe") /
    collapseDim(readGDX(path, "imPlantEffByType")[regions, years, paste0(CCS, ".effElc")], dim = 3.2) *
    readGDX(path, "i04ShareFuels")[regions, , CCS]
  fuelCCSH2 <- readGDX(path, "VmProdH2", field = "l")[regions, years, H2CCS] *
    (inputH2[[1]][regions, years, H2CCS] + inputH2[[2]][regions, years, H2CCS])
  fuelCCSInd <- readGDX(path, "V02EquipCapTechSubsec", field = "l")[regions, years, CCSTECH][, , INDSE1] *
    readGDX(path, "i02util")[regions, years, CCSTECH][, , INDSE1] *
    readGDX(path, "i02ShareBlend")[regions, years, CCSTECH][, , INDSE1]

  fuelCCS <- dimSums(fuelCCSElec, dim = 3.1)[, , efCCS] + dimSums(fuelCCSH2, dim = 3.1)[, , efCCS] +
    dimSums(fuelCCSInd, dim = c(3.1, 3.2))[, , efCCS]

  CCStoEF <- BALEFtoEF %>% filter(BALEF %in% c("Coal", "Gas"))
  fuelCCS <- toolAggregate(fuelCCS, dim = 3, rel = CCStoEF, from = "EFS", to = "BALEF")
  getItems(fuelCCS, 3) <- paste0("Primary Energy|", getItems(fuelCCS, 3))
  # CCS fuel cannot exceed primary consumption
  totalCCS <- magpie_object[, , getItems(fuelCCS, 3)]
  fuelCCS[fuelCCS > totalCCS] <- totalCCS[fuelCCS > totalCCS]

  withCCS <- fuelCCS
  getItems(withCCS, 3) <- paste0(getItems(withCCS, 3), "|w/ CCS")
  withoutCCS <- magpie_object[, , getItems(fuelCCS, 3)] - fuelCCS
  getItems(withoutCCS, 3) <- paste0(getItems(withoutCCS, 3), "|w/o CCS")
  oilWithoutCCS <- magpie_object[, , "Primary Energy|Oil"]
  getItems(oilWithoutCCS, 3) <- "Primary Energy|Oil|w/o CCS"

  magpie_object <- mbind(magpie_object, withCCS, withoutCCS, oilWithoutCCS)

  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = "Mtoe")
  return(magpie_object)
}
