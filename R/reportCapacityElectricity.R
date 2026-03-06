#' Process and Aggregate Electricity Capacity Data
#'
#' This function processes and aggregates electricity capacity data from a GDX file.
#' It maps fuel categories, aggregates data by reporting categories,
#' and formats the results into a magpie object.
#'
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed electricity capacity data with proper units.
#'
#' @examples
#' \dontrun{
#' result <- reportCapacityElectricity(system.file("extdata", "blabla.gdx", package = "postprom"), c("MEA"))
#' }
#' @importFrom gdx readGDX
#' @importFrom magclass getItems add_dimension mbind dimSums
#' @importFrom madrat toolAggregate
#' @importFrom dplyr %>% rename coalesce group_by summarise
#' @export
reportCapacityElectricity <- function(path, regions, years) {
  BALEFtoEF <- read.csv(
    system.file("mappings", "BALEFtoEF.csv", package = "postprom")
  ) %>%
    separate_rows(EF) %>%
    filter(BALEF %in% c(
      "Coal", "Gas", "Nuclear", "Biofuels", "Oil",
      "Solar", "Wind", "Geothermal and other renewable sources", "Hydrogen", "Hydro"
    ))

  PGALLtoEF <- readGDX(path, "PGALLtoEF")

  sets <- full_join(PGALLtoEF %>% rename(EF = EFS), BALEFtoEF, by = "EF")

  capacity <- readGDX(path, "V04CapElecNominal",
    field = "l"
  )[regions, years, ]
  units <- sub(".*\\((.*)\\).*", "\\1", capacity@description)

  i04ShareFuels <- readGDX(path, "i04ShareFuels", field = "l")[regions, , ]
  i04ShareFuels <- i04ShareFuels[, , paste0(PGALLtoEF$PGALL, ".", PGALLtoEF$EFS)]
  PGALLtoEF[, 2] <- paste0(PGALLtoEF$PGALL, ".", PGALLtoEF$EFS)

  capacity <- toolAggregate(capacity,
    dim = 3, weight = i04ShareFuels,
    rel = PGALLtoEF, from = "PGALL", to = "EFS"
  )

  prefix <- sub("\\..*", "", getItems(capacity, 3.1))
  withCCS <- case_when(
    prefix %in% readGDX(path, "CCS") ~ "w/ CCS",
    prefix %in% c(readGDX(path, "NOCCS"), "ATHOIL") ~ "w/o CCS",
    TRUE ~ ""
  )

  HYD <- case_when(
    prefix %in% "PGLHYD" ~ "Large",
    prefix %in% "PGSHYD" ~ "Small",
    TRUE ~ ""
  )

  WND <- case_when(
    prefix %in% "PGAWND" ~ "Onshore",
    prefix %in% "PGAWNO" ~ "Offshore",
    TRUE ~ ""
  )

  SOL <- case_when(
    prefix %in% "PGCSP" ~ "CSP",
    prefix %in% "PGSOL" ~ "PV",
    TRUE ~ ""
  )

  tech <- coalesce(
    na_if(SOL, ""),
    na_if(WND, ""),
    na_if(HYD, ""),
    na_if(withCCS, "")
  )

  sets <- filter(sets, PGALL %in% getItems(capacity, 3.1))

  getItems(capacity, 3.2) <- sets$BALEF[match(getItems(capacity, 3.2), sets$EF)]

  x1 <- as.data.frame(getItems(capacity, 3.1))
  x2 <- as.data.frame(tech)

  tech_join <- cbind(x1, x2)
  names(tech_join) <- c("pg", "tech")

  getItems(capacity, 3.1) <- tech_join$tech[match(getItems(capacity, 3.1), tech_join$pg)]

  capacity <- as.quitte(capacity)

  capacity <- capacity %>%
    group_by(region, PGALL, data, period) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    select(region, period, value, PGALL, data) %>%
    rename(variable = data, tech = PGALL) %>%
    as.quitte() %>%
    as.magpie()


  name <- gsub("\\.", "|", sub("\\.NA$", "", getItems(capacity, dim = 3)))
  getItems(capacity, 3) <- name
  # =============================

  names(dimnames(capacity))[3.1] <- "variable"

  getItems(capacity, 3) <- paste0("Capacity|Electricity|", getItems(capacity, 3))

  capacity <- helperAggregateLevel(capacity, level = 1, recursive = TRUE)
  capacity <- add_dimension(capacity, dim = 3.2, add = "unit", nm = units)
  return(capacity)
}
