#' Equipment Capacity Share.
#'
#' This function computes Equipment Capacity rates.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed Equipment Capacity rates data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportEquipmentCapacityShare(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind collapseDim
#' @importFrom quitte as.quitte
#' @importFrom gdxrrw rgdx.set
#' @importFrom dplyr filter left_join mutate select group_by %>% arrange ungroup rename
#' @export
reportEquipmentCapacityShare <- function(path, regions, years) {
  
  V02EquipCapTechSubsec <- readGDX(path, "V02EquipCapTechSubsec", field = 'l')[regions, years, ]
  
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE)
  
  V02EquipCapTechSubsec <- toolAggregate(V02EquipCapTechSubsec,
                                         dim = 3.1,
                                         rel = DSBSTable, from = "SBS", to = ".te"
  )
  
  V02EquipCapTechSubsec <- as.quitte(V02EquipCapTechSubsec) %>% 
    arrange(DSBS, unit, region, period, ITECH) %>%
    group_by(DSBS, unit, region, ITECH) %>%
    mutate(
      Share = ifelse(lag(value) == 0, NA, 100 * (value - lag(value)) / abs(lag(value))),
      unit = "%",
      DSBS = paste0("Share Equipment Capacity|", DSBS, "|", ITECH)
    ) %>%
    ungroup() %>% 
    select("region", "period", "DSBS", "unit", "Share") %>%
    rename(variable = DSBS) %>% 
    rename(value = Share)  %>%
    as.quitte() %>% as.magpie()
  
  V02EquipCapTechSubsec[is.na(V02EquipCapTechSubsec)] <- 0
  
  return(V02EquipCapTechSubsec)
}
