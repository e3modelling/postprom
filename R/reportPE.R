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
#' @importFrom magclass getItems getNames add_dimension mbind
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

     
  RatioPrimaryFuels <- readGDX(path, "i03RatioPrimaryFuels")[regions, years, ]
  RatioPrimaryFuels[, paste0("y", seq(2024, 2100)), ] <- RatioPrimaryFuels[, "y2023", ]

  PrimaryEnergy <- V03ConsGrssInl * RatioPrimaryFuels

  PrimaryEnergy <- toolAggregate(PrimaryEnergy,
    weight = NULL, dim = 3,
    rel = BALEFtoEF, from = "BALEF", to = "EFS"
  )

  
  getItems(PrimaryEnergy, 3) <- paste0("Primary Energy|", getItems(PrimaryEnergy, 3))

  magpie_object <- helperAggregateLevel(PrimaryEnergy, level = 1, recursive = TRUE)
  
# -------------------------- Remove non-primary carriers ------------------
magpie_object <- magpie_object[, , 
  !getItems(magpie_object, 3) %in% c(
    "Primary Energy|Electricity",
    "Primary Energy|Heat",
    "Primary Energy|Hydrogen"
  )
]

  magpie_object <- add_dimension(magpie_object, dim = 3.2, add = "unit", nm = "Mtoe")
  return(magpie_object)
}
