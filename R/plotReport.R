#' Generate a Plot from a MAgPIE Object
#'
#' This function generates a plot for a MAgPIE object using specified settings.
#' It supports both bar and area plot types and handles color mappings for variables.
#'
#' @param magpie_obj A MAgPIE object containing the data to be plotted.
#' @param plot_type A string specifying the type of plot. Options are `"bar"` or `"area"`. Defaults to `"bar"`.
#' @param label An optional string to label the plot legend. Defaults to `NULL`.
#' @param save_name An optional string specifying the file name to save the plot.
#' If `NULL`, the plot is not saved. Defaults to `NULL`.
#'
#' @return A ggplot object representing the plot.
#' @details
#' - The function extracts variable names from the `magpie_obj` and matches them with color mappings.
#' - A warning is issued if any variables do not have corresponding color mappings.
#' - If `save_name` is provided, the plot is saved to the specified file.
#'
#' @examples
#' \dontrun{
#' plotReport(magpie_obj = example_magpie, plot_type = "bar", save_name = "example_plot.png")
#' }
#' @importFrom ggplot2 ggplot aes geom_area geom_bar geom_line facet_wrap scale_fill_manual
#' @importFrom ggplot2 scale_color_manual labs theme_bw theme ggsave element_text
#' @importFrom rlang enquo as_label
#' @importFrom dplyr filter arrange %>% anti_join
#' @importFrom quitte as.quitte
#' @importFrom magclass getSets getNames getItems
#' @export
plotReport <- function(magpie_obj, plot_style = "bar",
                       label = NULL, save_name = NULL) {
  variable_name <- getSets(magpie_obj)["d3.1"]
  vars <- getNames(magpie_obj, 3.1)[[variable_name]]

  colors_vars <- getColorMappings() %>%
    filter(X %in% vars)

  # Throw warning if missing color mappings are found
  missing_colors <- setdiff(vars, colors_vars$X)
  if (length(missing_colors) > 0) {
    warning(
      sprintf(
        "No color available for the following variables: %s",
        paste(missing_colors, collapse = ", ")
      )
    )
  }

  items <- as.character(unique(getItems(magpie_obj, 3.1)))
  units <- unique(getItems(magpie_obj, 3.2))
  y_label <- strsplit(items, "\\|")[[1]][1]
  y_label <- paste0(y_label," [", units, "]")
  
  
  plot <- plotTool(
    data = filter(as.quitte(magpie_obj), !is.na(value)),
    colors_vars = colors_vars,
    variable = variable_name,
    plot_style = plot_style,
    label = label,
    y_label = y_label
  )
  
  if (!is.null(save_name)) {
    print(paste0("Saving plot to ", save_name))
    ggsave(save_name, plot, units = "in", width = 5.5, height = 4, dpi = 1200)
  }
  return(plot)
}
# Helpers -------------------------------------------------------------
plotTool <- function(data, colors_vars, variable, plot_style,
                     label = NULL, text_size = 12,
                     legend_key_size = 0.5, legend_key_width = 0.5,
                     x_label = "period", y_label = NULL,
                     n_regions_per_plot = 8) {
  
  if (is.null(label)) label <- "Labels"
  if (is.null(y_label)) {
    y_label <- paste0("[", unique(data[["unit"]]), "]")
  }
  
  # Split regions into chunks
  unique_regions <- unique(data$region)
  region_chunks <- split(unique_regions, ceiling(seq_along(unique_regions) / n_regions_per_plot))
  
  # List to store plots
  plots <- list()
  
  for (i in seq_along(region_chunks)) {
    chunk_regions <- region_chunks[[i]]
    
    # Filter data for this chunk
    chunk_data <- data %>% filter(region %in% chunk_regions)
    
    # Skip this chunk if there’s no data
    if (nrow(chunk_data) == 0) next
    
    # Base plot for this chunk
    plot_chunk <- ggplot(chunk_data,
                         aes(y = value, x = period, color = .data[[variable]])) +
      scale_fill_manual(
        values = colors_vars$color,
        limits = colors_vars$X,
        labels = colors_vars$legend
      ) +
      scale_color_manual(
        values = as.character(colors_vars[, 3]),
        limits = as.character(colors_vars[, 1]),
        labels = as.character(colors_vars[, 2])
      ) +
      facet_wrap(~region, scales = "free_y") +
      labs(
        x = x_label,
        y = y_label,
        color = label,
        fill = label
      ) +
      theme_bw() +
      theme(
        text = element_text(size = text_size),
        legend.position = "right",
        axis.text.x = element_text(angle = 90)
      )
    
    # Ensure that the draw order is bars/areas first, lines last
    style_order <- intersect(c("bar", "area", "dLine", "solidLine"), unique(colors_vars$style))
    
    for (style in style_order) {
      vars_per_style <- colors_vars$X[colors_vars$style == style]
      data_sub <- chunk_data %>% filter(.data[[variable]] %in% vars_per_style)
      
      # Skip this geom if there’s no data
      if (nrow(data_sub) == 0) next
      
      plot_chunk <- plot_chunk + switch(style,
                                        "area"       = geom_area(data = data_sub, stat = "identity", aes(fill = .data[[variable]])),
                                        "bar"        = geom_bar(data = data_sub, stat = "identity", aes(fill = .data[[variable]])),
                                        "dLine"      = geom_line(data = data_sub, aes(color = .data[[variable]]), linetype = "twodash"),
                                        "solidLine"  = geom_line(data = data_sub, aes(color = .data[[variable]]), linetype = "solid"),
                                        stop("Invalid plot_type.")
      )
    }
    
    plots[[i]] <- plot_chunk
  }
  
  return(plots)
}

getColorMappings <- function(new_mappings = NULL) {
  mappings <- read.csv(system.file(package = "postprom", file.path("extdata", "plotstyle.csv")))

  if (!requireNamespace("mip", quietly = TRUE)) {
    message("⚠️ The 'mip' package is not installed.")
    message("To use mip features (plotstyle.csv)), install it with: install.packages('mip')")
    return(invisible(NULL))
  }

  extra_mappings <- read.csv(system.file(package = "mip", file.path("extdata", "plotstyle.csv")), sep = ";") %>%
    select(c("X", "legend", "color")) %>%
    distinct() %>%
    mutate(style = "bar")

  if (!is.null(new_mappings)) {
    extra_mappings <- new_mappings %>%
      filter(!(X %in% extra_mappings$X)) %>%
      bind_rows(extra_mappings)
  }

  color_mappings <- mappings %>%
    # bind only non-present mappings
    bind_rows(anti_join(extra_mappings, mappings, by = "X"))
  return(color_mappings)
}
