#' Generate and save a batch plot report as a PDF
#'
#' This function reads a predefined CSV mapping of plots, groups them,
#' filters the necessary report data, and generates plots for each unique
#' group. The plots are then saved as a PDF using an R Markdown template.
#' Optionally includes a traffic light validation section comparing results
#' against policy targets.
#'
#' @param report A data object containing report information.
#' @param metadata Metadata for the report (e.g., scenario name).
#' @param save_pdf A character string specifying the file path where the
#'   generated PDF report should be saved.
#' @param targets_file Optional path to an Excel file containing policy targets.
#'   If provided, a traffic light validation section will be included.
#'   Excel file should have columns: Policy, Region, Year, Target, Variable, Unit
#' @param use_traffic_light_template Logical. If TRUE and targets_file is provided,
#'   uses the enhanced template with traffic light section. Default: TRUE
#'
#' @return None. The function saves a PDF report and prints a message.
#' @importFrom dplyr group_by group_keys group_split %>%
#' @importFrom purrr map2
#' @importFrom knitr knit2pdf opts_knit
#' @export
batchPlotReport <- function(report, metadata, save_pdf, targets_file = NULL,
                           use_traffic_light_template = TRUE) {
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

  # Generate traffic light section if targets file is provided
  traffic_light_section <- ""

  # Convert to absolute path if it's valid
  if (file.exists(targets_file)) {
    message("📊 Generating policy targets section...")
    message(sprintf("   Using targets file: %s", targets_file))
    tryCatch(
      {
        traffic_light_result <- createTrafficLightReport(report, targets_file)
        traffic_light_section <- traffic_light_result$latex_code
        
        # Print summary to console
        message("✓ Policy targets comparison complete:")
        summary_table <- traffic_light_result$summary
        message(sprintf("  - Green (within 5%%): %d", sum(summary_table$Status == "Green", na.rm = TRUE)))
        message(sprintf("  - Yellow (5-15%%): %d", sum(summary_table$Status == "Yellow", na.rm = TRUE)))
        message(sprintf("  - Red (>15%%): %d", sum(summary_table$Status == "Red", na.rm = TRUE)))
      },
      error = function(e) {
        message("⚠️ Warning: Could not generate policy targets section: ", e$message)
        message("   Continuing with regular plot report...")
      }
    )
  }

  render_env$traffic_light_section <- traffic_light_section

  # Choose template based on whether traffic light is included
  if (!is.null(targets_file) && targets_file != "" && use_traffic_light_template) {
    template_path <- system.file("templates/pdf_with_trafficlight.Rnw", package = "postprom")
  } else {
    template_path <- system.file("templates/pdf.Rnw", package = "postprom")
  }
  
  # Set proper output directory
  output_dir <- dirname(save_pdf)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  opts_knit$set(base.dir = output_dir)

  # Prepare output files - knit2pdf wants .tex as output, generates .pdf
  tex_file <- sub("\\.pdf$", ".tex", save_pdf)
  
  knit2pdf(
    input = template_path,
    output = tex_file,
    envir = render_env,
    quiet = TRUE
  )
}
# Helpers -------------------------------------------------------------
#' Plot a group of related variables
#'
#' Internal helper for `batchPlotReport()`. It prefixes each variable
#' with the group name, extracts those variables from the report, and
#' generates a combined plot.
#'
#' @param vars Character vector of variable names to include in the group.
#' @param name Character string representing the group name (e.g., "Emissions|CO2|Total").
#' @param report magpie object containing the data to be plotted.
#' @param ... Additional arguments passed to the main plotting function.
#'
#' @return A ggplot object reporting the grouped variables.
#' @keywords internal
#' @noRd
plotGroups <- function(vars, name, report, ...) {
  # Strip the part after the last "|" and append vars (vector)
  vars <- paste0(sub("\\|[^|]*$", "|", name), vars)
  vars <- sub("\\|$", "", vars) # Remove trailing "|", if present
  magpie_obj <- report[, , unique(vars)]
  plot <- plotReport(magpie_obj)
  return(plot)
}
