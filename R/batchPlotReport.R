#' Generate and save a batch plot report as a PDF
#'
#' This function reads a predefined CSV mapping of plots, groups them,
#' filters the necessary report data, and generates plots for each unique
#' group. The plots are then saved as a PDF using an R Markdown template.
#'
#' @param report A data object containing report information.
#' @param metadata Scenario metadata shown in the generated PDF.
#' @param save_pdf A character string specifying the file path where the
#'   generated PDF report should be saved.
#' @param PngFiles Logical; whether to save the area plots as PNG files.
#' @param include_validation Logical; create a separate
#'   \code{Validation.pdf}.
#' @param validation_checks Historical-number checks passed to
#'   \code{\link{validationPdfReport}}.
#' @param policy_checks Country policy checks passed to
#'   \code{\link{validationPdfReport}}.
#' @param indicators_checks Indicator checks passed to
#'   \code{\link{validateResults}}.
#' @param long_term_checks Long-term target checks passed to
#'   \code{\link{validationPdfReport}}.
#' @param validation_output Target path for the separate validation PDF.
#' @return Invisibly returns the result from \code{\link{validationPdfReport}},
#'   or \code{NULL} when validation is disabled. The function also saves the plot
#'   report PDF.
#' @importFrom dplyr group_by group_keys group_split %>%
#' @importFrom purrr map2
#' @importFrom knitr knit2pdf opts_knit
#' @export
batchPlotReport <- function(report, metadata, save_pdf, PngFiles,
                            include_validation = TRUE,
                            validation_checks = defaultValidationChecks(),
                            policy_checks = defaultPolicyValidationChecks(),
                            indicators_checks = defaultIndicatorsChecks(),
                            long_term_checks = defaultLongTermValidationChecks(),
                            validation_output = file.path(
                              dirname(save_pdf), "Validation.pdf"
                            )) {
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
  on.exit(unlink(plot_rds_path), add = TRUE)
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

  validationResult <- if (isTRUE(include_validation)) {
    validationPdfReport(
      report = report,
      metadata = metadata,
      output_file = validation_output,
      scenario = basename(dirname(save_pdf)),
      validation_checks = validation_checks,
      policy_checks = policy_checks,
      indicators_checks = indicators_checks,
      long_term_checks = long_term_checks
    )
  } else {
    NULL
  }

  invisible(validationResult)
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
  
  regionsPNG <- c("World", "DEU", "IND", "CHA", "USA")
  yearsPNG <- c(2023, seq(2025, max(getYears(report, as.integer = TRUE)), by = 5))
  if (all(regionsPNG %in% getRegions(report))) {
    if (all(yearsPNG %in% as.numeric(sub("^y", "", getYears(report))))) {
      
      output_dir <- file.path(output_dir, "PNG_area_plots")
      dir.create(
        output_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      
      message("Saving png files in ", output_dir)
      
      dataPlotPNG <- grouped[
        !grepl(
          "(VAL|Validation|Budget1p5C|Budget2C)$",
          grouped$Variables
        ) &
          !is.na(grouped$Variables) &
          trimws(grouped$Variables) != "",
      ]
      
      dataPlotPNG <- dataPlotPNG %>%
        filter(!(Variables == "Demand"))
      
      magpiePNG <- report[
        regionsPNG,
        yearsPNG,
      ]
      
      plots_list <- map2(
        group_split(dataPlotPNG),
        group_keys(dataPlotPNG)$Name,
        ~ plotGroups(
          .x$Variables,
          .y,
          magpiePNG
        )
      ) %>%
        setNames(
          group_keys(dataPlotPNG)$Name
        )
      
      purrr::imap(
        plots_list,
        function(plot_group, list_name) {
          
          # Get ggplot
          p <- plot_group[[1]]
          
          # Main title
          p <- p +
            ggplot2::labs(
              title = list_name
            )
          
          # Remove existing layers
          p$layers <- list()
          
          # Change colour mapping to fill mapping
          p$mapping$fill <- p$mapping$colour
          p$mapping$colour <- NULL
          
          # --------------------------------------------------
          # AREA PLOT LAYOUT
          #
          # CHA       DEU
          # FRA       IND
          # USA       LEGEND
          # --------------------------------------------------
          
          p$facet <- ggplot2::facet_wrap(
            ~region,
            ncol = 2,
            scales = "free_y"
          )
          
          p <- p +
            
            # Stacked area
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
            
            # ------------------------------------------------
          # LEGEND CONTENT
          # Put legend variables on 6 rows
          # ------------------------------------------------
          ggplot2::guides(
            fill = ggplot2::guide_legend(
              nrow = 6,
              byrow = TRUE
            )
          ) +
            
            ggplot2::theme(
              
              # Main plot title
              plot.title = ggplot2::element_text(
                face = "bold",
                size = 18,
                hjust = 0.5
              ),
              
              # ------------------------------------------------
              # ONLY REGION NAMES ARE MADE BIGGER
              # ------------------------------------------------
              strip.text = ggplot2::element_text(
                face = "bold",
                size = 30
              ),
              
              # Normal axis titles
              axis.title = ggplot2::element_text(
                size = 12
              ),
              
              # Normal axis values
              axis.text = ggplot2::element_text(
                size = 10
              ),
              
              # ------------------------------------------------
              # LEGEND POSITION
              # Empty bottom-right area
              # ------------------------------------------------
              legend.position = "inside",
              
              legend.position.inside = c(
                0.75,
                0.17
              ),
              
              # Legend variable names
              legend.text = ggplot2::element_text(
                size = 25
              ),
              
              legend.title = ggplot2::element_text(
                size = 10
              ),
              
              # Legend keys
              legend.key.size = grid::unit(
                0.6,
                "cm"
              ),
              
              # Horizontal spacing between legend items
              legend.spacing.x = grid::unit(
                0.15,
                "cm"
              ),
              
              # Vertical spacing between legend rows
              legend.spacing.y = grid::unit(
                0.10,
                "cm"
              ),
              
              # Transparent legend background
              legend.background = ggplot2::element_rect(
                fill = "transparent",
                colour = NA
              )
            )
          
          # --------------------------------------------------
          # File name
          # --------------------------------------------------
          
          file_name <- paste0(
            stringr::str_replace_all(
              list_name,
              "[^A-Za-z0-9]+",
              "_"
            ),
            ".png"
          )
          
          # --------------------------------------------------
          # Save PNG
          # --------------------------------------------------
          
          ggplot2::ggsave(
            filename = file.path(
              output_dir,
              file_name
            ),
            plot = p,
            width = 16,
            height = 18,
            dpi = 300,
            bg = "white"
          )
          
          invisible(NULL)
        }
      )
    }
  }
}
