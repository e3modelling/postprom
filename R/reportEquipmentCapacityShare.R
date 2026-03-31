#' Equipment Capacity Share.
#'
#' This function plots Equipment Capacity shares.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing Equipment Capacity shares data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportEquipmentCapacityShare(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom madrat toolAggregate
#' @importFrom magclass getItems add_dimension mbind dimSums
#' @importFrom dplyr select filter arrange bind_rows %>% mutate group_by ungroup rename if_else
#' @importFrom stringr str_replace
#' @importFrom gdxrrw rgdx.set
#' @export
reportEquipmentCapacityShare <- function(path, regions, years) {
  V02EquipCapTechSubsec <- readGDX(path, "V02EquipCapTechSubsec", field = "l")[regions, years, ]
  
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE)
  
  V02EquipCapTechSubsec <- toolAggregate(V02EquipCapTechSubsec,
                                         dim = 3.1,
                                         rel = DSBSTable, from = "SBS", to = ".te"
  )
  
  # complete names
  getItems(V02EquipCapTechSubsec, 3.1) <- paste0("Share Equipment Capacity|", getItems(V02EquipCapTechSubsec, 3.1))
  
  #####################   Share  ############################
  
  V02EquipCapTechSubsec <- as.quitte(V02EquipCapTechSubsec) %>%
    group_by(region, period, DSBS) %>%
    mutate(share = value / sum(value, na.rm = TRUE)) %>%
    ungroup() %>% select(-value) %>% rename(value = share) %>% as.quitte() %>% as.magpie()
  
  V02EquipCapTechSubsec[is.na(V02EquipCapTechSubsec)] <- 0
  ################################################
  
  # ---------------------------------------------------------------------------
  # Replace sep in dimensions and prepend the sector
  lookup <- setNames(getItems(V02EquipCapTechSubsec, 3.1), getItems(V02EquipCapTechSubsec, 3.2))
  name <- gsub("\\.", "|", getItems(V02EquipCapTechSubsec, dim = 3)) # e.g., IS.HCL --> IS|HCL
  key <- str_extract(name, "^[^|]+")
  mapped <- lookup[key]
  
  name <- if_else(
    !is.na(mapped),
    str_replace(name, "^[^|]+", paste0(mapped, "|\\0")),
    name
  ) # prepend SBS (e.g., IS|HCL -> Industry|IS|HCL)
  
  getItems(V02EquipCapTechSubsec, 3) <- name
  
  ## add Units
  V02EquipCapTechSubsec <- add_dimension(V02EquipCapTechSubsec, dim = 3.2, add = "unit", nm = "%")
  
  return(V02EquipCapTechSubsec)
}
