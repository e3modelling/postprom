#' Growth Rates of Emissions
#'
#' This function computes Growth Rates of Emissions, Gross Emissions,
#' Carbon Capture and Carbon Removal.
#'
#' @param reports Magpie object created from postprom.
#' @return A magpie object containing Growth Rates.
#'
#' @examples
#' \dontrun{
#' result <- reportGrowthRates(reports)
#' }
#'
#' @importFrom magclass getItems dimSums add_dimension mbind collapseDim
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter left_join mutate select group_by %>% arrange ungroup rename
#' @export
reportGrowthRates <- function(reports) {
  # ============ Growth Rates of Emissions, Gross Emissions,====================
  # ============ Carbon Capture and Carbon Removal =============================
  
  items <- getItems(reports,3)
  
  # take from reports the emissions items
  get_items <- items[
    grep("^(Emissions|Gross Emissions|Carbon Capture|Carbon Removal)(?!.*Budget)", items, perl = TRUE)
  ]

  EmissionsGrowthRateMP <- as.quitte(reports[,,get_items]) %>% 
  arrange(DSBSpEF, unit, region, period) %>%
  group_by(DSBSpEF, unit, region) %>%
  mutate(
    GrowthRate = ifelse(lag(value) == 0, NA, 100 * (value - lag(value)) / lag(value)),
    unit = "%",
    DSBSpEF = paste0("Growth Rate|", DSBSpEF)
  ) %>%
  ungroup() %>% 
  select("region", "period", "DSBSpEF", "unit", "GrowthRate") %>%
  rename(variable = DSBSpEF) %>% 
  rename(value = GrowthRate)  %>%
  as.quitte() %>% as.magpie()
  
  reports <- mbind(
    reports, EmissionsGrowthRateMP
  )

  return(reports)
}