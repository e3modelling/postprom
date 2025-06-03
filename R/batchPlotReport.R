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
#' @importFrom knitr knit2pdf opts_knit
#' @export
batchPlotReport <- function(report, metadata, save_pdf) {
  if (!tinytex::is_tinytex()) {
    message("⚠️ TinyTeX (LaTeX engine) is not installed. Skipping PDF creation.")
    message("To enable PDF output, install TinyTeX with: tinytex::install_tinytex()")
    return(invisible(NULL))
  }

  plot_mappings <- read.csv(
    system.file(package = "postprom", file.path("extdata", "plot_mapping.csv"))
  )
  plot_mappings$Name <- factor(plot_mappings$Name, levels = unique(plot_mappings$Name))

  # for each unique plot, use filter the magpie obj and plot its vars
  grouped <- plot_mappings %>% group_by(Name)
  plots_list <- map2(
    group_split(grouped),
    group_keys(grouped)$Name,
    ~ plotGroups(.x$Variables, .y, report)
  ) %>% setNames(group_keys(grouped)$Name)

  message(paste0("Saving pdf in ", sub("\\.tex$", ".pdf", save_pdf)))

  # Save the plots list to a temporary file
  plot_rds_path <- tempfile(fileext = ".rds")
  saveRDS(plots_list, file = plot_rds_path)
  render_env <- new.env()
  render_env$plot_rds_path <- plot_rds_path
  render_env$pdf_title <- gsub("_", "-", basename(dirname(save_pdf)))
  render_env$fScenario <- metadata

  template_path <- system.file("templates/pdf.Rnw", package = "postprom")
  output_path <- dirname(save_pdf)
  output_path <- file.path(basename(dirname(output_path)), basename(output_path))
  opts_knit$set(base.dir = output_path)

  knit2pdf(
    input = template_path,
    output = file.path(save_pdf),
    envir = render_env,
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
