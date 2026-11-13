#' Report PRISMA-style energy system costs
#'
#' This function reports a first electricity-focused set of energy system cost
#' variables from OPEN-PROM output. Investment is calculated from gross new
#' nominal electricity capacity additions and overnight power generation capital
#' cost. Capital cost reuses the model's annualized CAPEX and fixed-cost variable.
#' Policy cost reports active economy-module subsidy spending.
#'
#' @param path Character string specifying the file path to the GDX data file.
#' @param regions Character vector of region names used to subset the data.
#' @param years Character vector of years to include in the report.
#'
#' @return A magpie object with IAMC-style investment, capital-cost, and policy-cost variables.
#'
#' @examples
#' \dontrun{
#' result <- reportEnergySystemCosts(
#'   system.file("extdata", "blabla.gdx", package = "postprom"),
#'   regions = c("MEA"),
#'   years = c("y2030", "y2040", "y2050")
#' )
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind collapseDim dimSums
#' @importFrom dplyr %>% rename filter
#' @importFrom gdxrrw rgdx.set
#' @importFrom madrat calcOutput toolAggregate
#' @export
reportEnergySystemCosts <- function(path, regions, years) {
  DSBSTable <- rgdx.set(path, "DSBS", te = TRUE) %>%
    filter(!(SBS %in% c("BAV", "BMAR"))) %>%
    rbind(data.frame(SBS = "BU", .te = "Bunkers"))
  
  finalEnergy <- readGDX(path, "VmFinalEnergy", field = "l")[regions, years, ]           # Mtoe
  fuelPrice <- readGDX(path, "VmPriceFuelSubsecCarVal", field = "l")[regions, years, ]   # k$2015/toe
  carbonPrice <- readGDX(path, "VmPriceCarbon", field = "l")[regions, years, ]           # k$2015/toe
  iGDP <- calcOutput("iGDP_MER", aggregate = TRUE,  regionmapping = "regionmappingOPDEV5.csv")
  iGDP <- collapseDim(iGDP,3)
  iGDP <- iGDP[regions, years,]
  
  fuelPrice <- fuelPrice[,,getItems(finalEnergy, 3.1)]
  carbonPrice <- carbonPrice[,,getItems(finalEnergy, 3.1)]
  
  xa <- finalEnergy * fuelPrice
  xb <- finalEnergy * (fuelPrice - carbonPrice)
  
  xaBU <- dimSums(xa[,,c("BAV", "BMAR")], 3)
  getItems(xaBU, 3) <- "BU"
  xbBU <- dimSums(xb[,,c("BAV", "BMAR")], 3)
  getItems(xbBU, 3) <- "BU"
  
  xa <- dimSums(xa, 3.2)
  xb <- dimSums(xb, 3.2)
  
  xa <- mbind(xa[,,c("BAV", "BMAR"), invert = T], xaBU)
  xb <- mbind(xb[,,c("BAV", "BMAR"), invert = T], xbBU)
  
  xa <- toolAggregate(xa, dim = 3, rel = DSBSTable, from = "SBS", to = ".te")
  xb <- toolAggregate(xb, dim = 3, rel = DSBSTable, from = "SBS", to = ".te")
  
  getItems(xa, 3) <- paste0("Energy System Cost w/o Carbon Price|", getItems(xa, 3))
  getItems(xb, 3) <- paste0("Energy System Cost with Carbon Price|", getItems(xb, 3))
  
  xa <- helperAggregateLevel(xa, level = 1, recursive = TRUE)
  xb <- helperAggregateLevel(xb, level = 1, recursive = TRUE)
  
  xaGDP <- xa[,,"Energy System Cost w/o Carbon Price"] / iGDP * 100
  getItems(xaGDP, 3) <- "Energy System Cost w/o Carbon Price (% GDP|MER)"
  
  xbGDP <- xb[,,"Energy System Cost with Carbon Price"] / iGDP * 100
  getItems(xbGDP, 3) <- "Energy System Cost with Carbon Price (% GDP|MER)"
  
  
  xa <- add_dimension(xa, dim = 3.2, add = "unit", nm = "billion US$2015/yr")
  xb <- add_dimension(xb, dim = 3.2, add = "unit", nm = "billion US$2015/yr")
  xaGDP <- add_dimension(xaGDP, dim = 3.2, add = "unit", nm = "1")
  xbGDP <- add_dimension(xbGDP, dim = 3.2, add = "unit", nm = "1")
  
  final <- mbind(xa, xb, xaGDP, xbGDP)

  return(final)

}

