#' Generate Plots for MAgPIE Data
#'
#' This function generates plots for MAgPIE objects based on specified variables and plot types.
#'
#' @param magpie_obj A MAgPIE object containing the data to be plotted.
#' @param an A numeric vector specifying the analysis years.
#' @param vars A character vector or list of character vectors indicating the variables to plot.
#' @param plot_type A string specifying the plot type. Options are `"bar"` or `"area"`. Default is `"bar"`.
#' @param label An optional string to label the plot legend. Defaults to `NULL`.
#' @param save_name An optional string specifying the file name to save the plot. Defaults to `NULL`.
#'
#' @return A single plot object or a list of plot objects.
#' @examples
#' \dontrun{
#' plotReport(magpie_obj, an = c(2020, 2030), vars = "Capacity|Electricity", plot_type = "bar")
#' }
#' @importFrom ggplot2 ggplot aes geom_area geom_bar facet_wrap scale_fill_manual
#' @importFrom ggplot2 scale_color_manual labs theme_bw theme ggsave element_text
#' @importFrom rlang enquo as_label
#' @importFrom dplyr filter arrange %>%
#' @importFrom quitte as.quitte
#' @importFrom magclass getSets
#' @export
plotReport <- function(magpie_obj, an, vars, plot_type = "bar",
                              label = NULL, save_name = NULL) {
  if (is.character(vars)) {
    vars <- list(vars)
  }
  else if (!is.list(vars)) {
    stop("Argument 'vars' must be a character vector or a list of character vectors.")
  }

  plots <- lapply(vars, function(var) {
    plotSingleVectorVar(magpie_obj, an, var, plot_type, label, save_name)
  })

  if (length(plots) == 1) {
    return(plots[[1]])
  }

  return(plots)
}
# Helpers -------------------------------------------------------------
plotTool <- function(data, colors_vars, variable, plot_type, label) {
  if (is.null(label)) {
    label <- rlang::as_label(rlang::enquo(variable))
  }

  plot <- ggplot(data, aes(y = value, x = period, color = .data[[variable]])) +
    scale_fill_manual(
      values = as.character(colors_vars[, 3]),
      limits = as.character(colors_vars[, 1])
    ) +
    scale_color_manual(
      values = as.character(colors_vars[, 3]),
      limits = as.character(colors_vars[, 1])
    ) +
    switch(plot_type,
      "area" = geom_area(stat = "identity", aes(fill = .data[[variable]])),
      "bar" = geom_bar(stat = "identity", aes(fill = .data[[variable]])),
      stop("Invalid plot_type.")
    ) +
    facet_wrap("region", scales = "free_y") +
    labs(
      x = "period",
      y = paste0("Capacity|Electricity", " ", unique(data[["unit"]])),
      color = label,
      fill = label
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 4),
      strip.text.x = element_text(margin = margin(0.05, 0, 0.05, 0, "cm")),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      aspect.ratio = 1.5 / 2,
      plot.title = element_text(size = 4),
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm")
    )
  return(plot)
}

plotSingleVectorVar <- function(magpie_obj, an, vars, plot_type,
                                label, save_name) {
  variable_name = getSets(magpie_obj)["d3.1"]
  colors_vars <- getColorMappings() %>%
    filter(X %in% vars)

  # Throw warning if missing color mappings are found
  missing_colors <- setdiff(vars, colors_vars$X)
  if (length(missing_colors) > 0) {
    warning(sprintf(
      "No color available for the following variables: %s",
      paste(missing_colors, collapse = ", ")
    )
    )
  }

  data <- as.quitte(magpie_obj) %>%
    filter(.data[[variable_name]] %in% colors_vars[, 1], period <= max(an))

  plot <- plotTool(data, colors_vars, variable_name, plot_type, label)

  if (!is.null(save_name)) {
    print(paste0("Saving plot to ", save_name))
    ggsave(save_name, plot, units = "in", width = 5.5, height = 4, dpi = 1200)
  }
  return(plot)
}

getColorMappings <- function(new_mappings = NULL) {
  extra_mappings <- read.csv(system.file(package = "openprom", file.path("extdata", "plotstyle.csv")))

  if (!is.null(new_mappings)) {
    extra_mappings <- new_mappings %>%
      filter(!(X %in% extra_mappings$X)) %>%
      bind_rows(extra_mappings)
  }

  color_mappings <- read.csv(system.file(package = "mip", file.path("extdata", "plotstyle.csv")), sep = ";") %>%
    select(c("X", "legend", "color")) %>%
    distinct() %>%
    bind_rows(extra_mappings)
  return(color_mappings)
}
