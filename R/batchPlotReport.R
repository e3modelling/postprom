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
batchPlotReport <- function(report, metadata, save_pdf, PngFiles) {
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
  
  if (PngFiles == TRUE) reportAreaPNG(report = report,
                                      grouped = grouped,
                                      output_dir = output_path)

  message(paste0("Saving pdf in ", sub("\\.tex$", ".pdf", save_pdf)))
  
  knit2pdf(
    input = template_path,
    output = file.path(save_pdf),
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
#' Create PNG area plots for the report
#'
#' @param report MAgPIE report object.
#' @param grouped Data frame containing variable grouping information.
#' @param output_dir Character string specifying the parent output directory.
#'
#' @return Invisibly returns NULL.
#'
#' @author Fotis Sioutas
#'
#' @importFrom dplyr filter group_split group_keys
#' @importFrom purrr map2 imap
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 labs facet_wrap geom_area theme element_text ggsave
#' @importFrom stats setNames
#' 
reportAreaPNG <- function(report,
                          grouped,
                          output_dir) {
  
  regionsPNG <- c("World", "EU", "IND", "CHA", "USA")
  yearsPNG <- c(2023, seq(2025, max(getYears(report, as.integer = TRUE)), by = 5))
  if (all(regionsPNG %in% getRegions(report))) {
    if (all(yearsPNG %in% as.numeric(sub("^y", "", getYears(report))))) {
      output_dir <- file.path(output_dir, "PNG_area_plots")
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      message("Saving png files in ", output_dir)
      dataPlotPNG <- grouped[
        !grepl(
          "(VAL|Validation|Budget1p5C|Budget2C)$",
          grouped$Variables
        ) &
          !is.na(grouped$Variables) &
          trimws(grouped$Variables) != "",
      ]
      
      dataPlotPNG <- dataPlotPNG %>% filter(!(Variables == "Demand"))
      
      magpiePNG <- report[regionsPNG, yearsPNG, ]
      
      plots_list <- map2(
        group_split(dataPlotPNG),
        group_keys(dataPlotPNG)$Name,
        ~ plotGroups(.x$Variables, .y, magpiePNG)
      ) %>% setNames(group_keys(dataPlotPNG)$Name)
      
      purrr::imap(
        plots_list,
        function(plot_group, list_name) {
          
          # Each element contains one ggplot inside a list
          p <- plot_group[[1]]
          
          # Use the plots_list element name as the title
          p <- p +
            ggplot2::labs(
              title = list_name
            )
          
          # Remove the existing layers
          p$layers <- list()
          
          # Change colour mapping to fill mapping
          p$mapping$fill <- p$mapping$colour
          p$mapping$colour <- NULL
          
          # Keep all region panels in one row with separate y-axes
          p$facet <- ggplot2::facet_wrap(
            ~region,
            nrow = 1,
            scales = "free_y"
          )
          
          # Add stacked area geometry
          p <- p +
            ggplot2::geom_area(
              position = "stack",
              alpha = 0.85,
              colour = "white",
              linewidth = 0.15
            ) +
            ggplot2::labs(
              colour = NULL,
              fill = NULL
            ) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(
                face = "bold",
                size = 18,
                hjust = 0.5
              ),
              legend.position = "bottom"
            )
          
          file_name <- paste0(
            stringr::str_replace_all(
              list_name,
              "[^A-Za-z0-9]+",
              "_"
            ),
            ".png"
          )
          
          ggplot2::ggsave(
            filename = file.path(
              output_dir,
              file_name
            ),
            plot = p,
            width = 24,
            height = 6,
            dpi = 300,
            bg = "white"
          )
          
          invisible(NULL)
        }
      )
    }
  }
}