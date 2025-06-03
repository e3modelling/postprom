#' @export
getMetadata <- function(path) {
  path_gdx <- file.path(path, "blabla.gdx")

  metadata <- sapply(path_gdx, function(path_gdx) {
    readGDX(path_gdx, "fScenario")[[1]]
  })
  return(unname(metadata))
}
