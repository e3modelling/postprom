#' @importFrom gdx readGDX
#' @importFrom magclass getItems dimSums add_dimension mbind
#' @importFrom madrat toolAggregate
#' @importFrom stringr str_replace_all
#' @export
reportCapacityAdditions <- function(path, regions, years) {
  VNewCapElec <- readGDX(path, "V04NewCapElec", field = "l")[regions, years, ]
  VNetNewCapElec <- readGDX(path, "V04NetNewCapElec", field = "l")[regions, years, ]
  PGALLtoEF <- readGDX(path, "PGALLtoEF")
  names(PGALLtoEF) <- c("PGALL", "EF")

  magpie_object <- helper(VNewCapElec, "Capacity Additions")
  magpie_object <- mbind(magpie_object, helper(VNetNewCapElec, "Net Capacity Additions"))
  return(magpie_object)
}

# Helper ----------------------------------------------------------------------------------------
helper <- function(variable, title) {
  title <- paste0(title, "|Electricity|")

  getItems(variable, 3) <- paste0(title, getItems(variable, 3))

  magpie_object <- NULL
  variable <- add_dimension(variable, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, variable)

  # electricity production
  var_total <- dimSums(variable, dim = 3, na.rm = TRUE)
  getItems(var_total, 3) <- title

  var_total <- add_dimension(var_total, dim = 3.2, add = "unit", nm = "GW/yr")
  magpie_object <- mbind(magpie_object, var_total)
  return(magpie_object)
}
