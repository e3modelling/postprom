#' Generate and save a batch plot report as a PDF
#'
#' This function reads a predefined CSV mapping of plots, groups them,
#' filters the necessary report data, and generates plots for each unique
#' group. The plots are then saved as a PDF using an R Markdown template.
#'
#' @param report A data object containing report information.
#' @param save_pdf A character string specifying the file path where the
#'   generated PDF report should be saved.
#' @return None. The function saves a PDF report and prints a message.
#' @importFrom dplyr group_by group_keys group_split %>%
#' @importFrom purrr map2
#' @importFrom rmarkdown render find_pandoc
#' @export
batchPlotReport <- function(report, save_pdf) {
  plot_mappings <- read.csv(
    system.file(package = "openprom", file.path("extdata", "plot_mapping.csv"))
  )

  # for each unique plot, use filter the magpie obj and plot its vars
  grouped <- plot_mappings %>% group_by(Name)
  plots_list <- map2(
    group_split(grouped),
    group_keys(grouped)$Name,
    ~ plotGroups(.x$Variables, .y, report)
  ) %>% setNames(group_keys(grouped)$Name)

  message(paste0("Saving pdf in ", save_pdf))

  Sys.setenv(RSTUDIO_PANDOC = rmarkdown::find_pandoc()$dir)

  # Save the plots list to a temporary file
  plot_rds_path <- tempfile(fileext = ".rds")
  saveRDS(plots_list, file = plot_rds_path)

  template_path <- system.file("templates/pdf.Rmd", package = "openprom")
  rmarkdown::render(
    input = template_path,
    output_file = basename(save_pdf),
    output_dir = dirname(save_pdf),
    params = list(plot_file = plot_rds_path),
    quiet = TRUE
  )
}
# Helpers -------------------------------------------------------------
plotGroups <- function(vars, name, report, ...) {
  vars <- paste0(sub("\\|[^|]*$", "|", name), vars) # retrieve variable names
  magpie_obj <- report[, , vars]
  plot <- plotReport(magpie_obj)
  return(plot)
}
