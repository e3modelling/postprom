#' Plot Report from a Magpie Object
#'
#' This function generates a ggplot-based visualization from a `magpie` object,
#' using specified variables, colors, and plot types. Optionally, the plot can
#' be saved to a file.
#'
#' @param magpie_obj A `magpie` object to be converted and filtered for plotting.
#' @param variable_name The variable to plot, provided as an unquoted column name.
#' @param an GDX object. The maximum value of the `period` to include in the plot.
#' @param colors_vars A data frame containing color mapping. Expected columns:
#'   \describe{
#'     \item{1}{Variable names.}
#'     \item{2}{Corresponding categories.}
#'     \item{3}{Colors for each category.}
#'   }
#' @param plot_type A string specifying the type of plot to generate. Options are
#'   \code{"bar"} (default) and \code{"area"}.
#' @param label Optional. A string specifying a custom label for the legend.
#' @param save_name Optional. A string specifying the file name to save the plot.
#'   If \code{NULL}, the plot is not saved.
#'
#' @return A ggplot object containing the generated plot.
#'
#' @importFrom ggplot2 ggplot aes geom_area geom_bar facet_wrap scale_fill_manual
#' @importFrom ggplot2 scale_color_manual labs theme_bw theme ggsave element_text
#' @importFrom rlang enquo as_label
#' @importFrom dplyr filter arrange %>%
#' @importFrom quitte as.quitte
#' @export
plotReport <- function(magpie_obj, variable_name, an, colors_vars,
                       plot_type = "bar", label = NULL, save_name = NULL) {
  data <- magpie_obj %>%
    as.quitte() %>%
    # Filter data by variables and max period
    filter({{ variable_name }} %in% colors_vars[, 1], period <= max(an)) %>%
    # Arrange variables to match colors
    arrange(as.character({{ variable_name }}))
  plot <- .plotTool(data, colors_vars, {{ variable_name }}, plot_type, label)

  if (!is.null(save_name)) {
    ggsave(save_name, plot, units = "in", width = 5.5, height = 4, dpi = 1200)
  }
  return(plot)
}

# Helpers -------------------------------------------------------------
.plotTool <- function(data, colors_vars, variable, plot_type, label) {
  if (is.null(label)) {
    label <- rlang::as_label(rlang::enquo(variable))
  }

  plot <- ggplot(data, aes(y = value, x = period, color = {{ variable }})) +
    scale_fill_manual(
      values = as.character(colors_vars[, 3]),
      limits = as.character(colors_vars[, 1])
    ) +
    scale_color_manual(
      values = as.character(colors_vars[, 3]),
      limits = as.character(colors_vars[, 1])
    ) +
    switch(plot_type,
      "area" = geom_area(stat = "identity", aes(fill = {{ variable }})),
      "bar" = geom_bar(stat = "identity", aes(fill = {{ variable }})),
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
