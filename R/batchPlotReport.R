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
  
  regionsPNG <- c("World", "EU", "IND", "CHA", "USA")
  yearsPNG <- c(2023, seq(2025, max(getYears(report, as.integer = TRUE)), by = 5))
  
  plotstyle <- read.csv(
    system.file(package = "postprom", file.path("extdata", "plotstyle.csv"))
  )
  
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
  
  output_PNG <- file.path(output_path, "PNG_area_plots")
  dir.create(output_PNG, recursive = TRUE, showWarnings = FALSE)
  
  reportAreaPNG(
    report = report,
    grouped = grouped,
    plotstyle = plotstyle,
    regionsPNG = regionsPNG,
    yearsPNG = yearsPNG,
    output_dir = output_PNG
  )

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
#' @param plotstyle Data frame containing colors for each variable.
#' @param regionsPNG Character vector of regions to plot.
#' @param yearsPNG Numeric vector of years to plot.
#' @param output_dir Output directory for the PNG files.
#'
#' @return Invisibly returns NULL.
#'
#' @author Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr mutate filter left_join distinct pull rename select recode
#' @importFrom stringr str_remove str_replace_all
#' @importFrom ggplot2 ggplot aes geom_area facet_wrap labs scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme_bw theme
#' @importFrom ggplot2 element_text element_rect element_blank guides guide_legend
#' @importFrom ggplot2 expansion ggsave
#' @importFrom scales pretty_breaks label_number
#' @importFrom grid unit
reportAreaPNG <- function(report,
                          grouped,
                          plotstyle,
                          regionsPNG,
                          yearsPNG,
                          output_dir) {
  
  if (all(regionsPNG %in% getRegions(report))) {
    if (all(yearsPNG %in% as.numeric(sub("^y", "", getYears(report))))) {
      message("Saving png files")
      dataPlotPNG <- grouped[
        !grepl(
          "(VAL|Validation|Budget1p5C|Budget2C)$",
          grouped$Variables
        ) &
          !is.na(grouped$Variables) &
          trimws(grouped$Variables) != "",
      ]
      
      dataPlotPNG <- dataPlotPNG %>%
        mutate(Group = match(Name, unique(Name)))
      
      dataPlotPNG$Group <- cumsum(!duplicated(dataPlotPNG$Name))
      
      dataPlotPNGFilter <- dataPlotPNG %>%
        mutate(
          Name = paste0(
            str_remove(Name, "\\|[^|]+$"),
            "|",
            Variables
          )
        )
      
      dataPlotPNGFilter <- dataPlotPNGFilter %>%
        filter(!(Group %in% c(11, 12))) %>% filter(!(Name == "Secondary Energy|Electricity|Demand"))
      
      plotstyle <- plotstyle %>%
        rename(Name = X) %>%
        select(Name, color)
      
      dataPlotPNGFilter <- dataPlotPNGFilter %>%
        left_join(plotstyle, by = "Name")
      
      magpiePNG <- report[, , unique(dataPlotPNGFilter$Name)]
      magpiePNG <- magpiePNG[regionsPNG, yearsPNG, ]
      
      qmagpiePNG <- as.quitte(magpiePNG)
      
      # Original region codes and their displayed names
      region_order <- c(
        "World",
        "EU",
        "IND",
        "CHA",
        "USA"
      )
      
      region_labels <- c(
        "World" = "World",
        "EU"    = "European Union",
        "IND"   = "India",
        "CHA"   = "China",
        "USA"   = "USA"
      )
      
      display_region_order <- unname(region_labels[region_order])
      
      # Join plotting information, including colors
      plot_data <- qmagpiePNG %>%
        filter(
          DSBSpEFS %in% dataPlotPNGFilter$Name,
          region %in% region_order,
          !is.na(value)
        ) %>%
        left_join(
          dataPlotPNGFilter %>%
            distinct(Name, Variables, Group, color),
          by = c("DSBSpEFS" = "Name")
        ) %>%
        mutate(
          region = recode(
            as.character(region),
            !!!region_labels
          ),
          region = factor(
            region,
            levels = display_region_order
          )
        )
      
      # Titles for the PNGs
      group_titles <- c(
        "1"  = "CO2 Emissions",
        "2"  = "Cumulated CO2 Emissions",
        "3"  = "Final Energy by Sector",
        "4"  = "Final Energy by Fuel",
        "5"  = "Electricity Generation",
        "6"  = "Electricity Generation by CCS",
        "7"  = "Electricity Capacity",
        "8"  = "Carbon Price",
        "9"  = "Gross Inland Consumption",
        "10" = "Gross CO2 Emissions"
      )
      
      # Loop over groups
      for (current_group in sort(unique(plot_data$Group))) {
        
        data_group <- plot_data %>%
          filter(Group == current_group)
        
        if (nrow(data_group) == 0) next
        
        # Preserve legend and stacking order
        variable_order <- dataPlotPNGFilter %>%
          filter(Group == current_group) %>%
          pull(Variables) %>%
          unique()
        
        # Named color vector for the current group
        group_colors <- dataPlotPNGFilter %>%
          filter(Group == current_group) %>%
          distinct(Variables, color) %>%
          filter(!is.na(color), trimws(color) != "") %>%
          {
            setNames(.$color, .$Variables)
          }
        
        data_group <- data_group %>%
          mutate(
            Variables = factor(
              Variables,
              levels = variable_order
            )
          )
        
        unit_label <- data_group %>%
          filter(!is.na(unit)) %>%
          pull(unit) %>%
          unique() %>%
          first()
        
        p <- ggplot(
          data_group,
          aes(
            x = period,
            y = value,
            fill = Variables
          )
        ) +
          geom_area(
            position = "stack",
            alpha = 0.85,
            colour = "white",
            linewidth = 0.15
          ) +
          facet_wrap(
            ~region,
            nrow = 1,
            scales = "free_y"
          ) +
          labs(
            title = group_titles[as.character(current_group)],
            x = "Year",
            y = unit_label,
            fill = NULL
          ) +
          scale_fill_manual(
            values = group_colors,
            breaks = variable_order,
            drop = FALSE
          ) +
          scale_x_continuous(
            breaks = scales::pretty_breaks(6)
          ) +
          scale_y_continuous(
            labels = scales::label_number(big.mark = ","),
            expand = expansion(mult = c(0, 0.05))
          ) +
          theme_bw(base_size = 15) +
          theme(
            plot.title = element_text(
              face = "bold",
              size = 18,
              hjust = 0.5
            ),
            strip.text = element_text(
              face = "bold",
              size = 13
            ),
            strip.background = element_rect(fill = "grey90"),
            axis.title = element_text(
              face = "bold",
              size = 13
            ),
            axis.text = element_text(size = 11),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 11),
            legend.key.width = unit(1.2, "cm"),
            plot.margin = ggplot2::margin(10, 10, 10, 10)
          ) +
          guides(
            fill = guide_legend(
              nrow = ceiling(length(variable_order) / 5),
              byrow = TRUE
            )
          )
        
        file_name <- paste0(
          sprintf("%02d", current_group),
          "_",
          str_replace_all(
            group_titles[as.character(current_group)],
            "[^A-Za-z0-9]+",
            "_"
          ),
          ".png"
        )
        
        ggsave(
          filename = file.path(output_dir, file_name),
          plot = p,
          width = 18,
          height = 6,
          dpi = 300,
          bg = "white"
        )
      }
    }
  }
  invisible(NULL)
}
  