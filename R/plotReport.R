#' Generate and Save Plots for MAGPIE Objects
#'
#' @description
#' This set of functions generates plots for MAGPIE objects or lists
#' of MAGPIE objects, based on the specified parameters.
#' The plots can be saved individually or combined into a PDF file.
#'
#' @details
#' The generic \code{plotReport} function delegates to specific methods based on the class of \code{obj}. Supported
#' methods include \code{plotReport.list} for lists of MAGPIE objects and \code{plotReport.magpie} for individual
#' MAGPIE objects.
#'
#' @examples
#' \dontrun{
#' magpie_obj <- read.magpie("example_file.mgp")
#' plotReport(magpie_obj, an = 2050, vars = c("variable1", "variable2"), plot_type = "bar")
#' }
#' @importFrom ggplot2 ggplot aes geom_area geom_bar facet_wrap scale_fill_manual
#' @importFrom ggplot2 scale_color_manual labs theme_bw theme ggsave element_text
#' @importFrom rlang enquo as_label
#' @importFrom dplyr filter arrange %>%
#' @importFrom quitte as.quitte
#' @importFrom magclass getSets
#' @export
plotReport <- function(obj, ...) {
  UseMethod("plotReport")
}

#' @export
plotReport.list <- function(magpie_list, an, plot_info,
                            plot_type = "bar", save_pdf = NULL) {
  magpie_list <- magpie_list[names(magpie_list) %in% names(plot_info)]

  mapply_fun <- function(name, obj) {
    lapply(
      plot_info[[name]],
      function(vars) {plotReport(obj, an, vars, label=name)}
    )
  }

  plots_list <- unlist(
    mapply(
      mapply_fun,
      names(magpie_list), magpie_list,
      SIMPLIFY = FALSE
    ),
    recursive = FALSE
  )

  if (!is.null(save_pdf)) {
    pdf(save_pdf, width = 8, height = 6)
    for (plot in plots_list) print(plot)
    dev.off()
  }
}

#' @export
plotReport.magpie <- function(magpie_obj, an, vars, plot_type = "bar",
                              label = NULL, save_name = NULL) {
  colors_vars <- getColorMappings() %>%
    filter(X %in% vars)
  variable_name = getSets(magpie_obj)["d3.1"]

  data <- as.quitte(magpie_obj) %>%
    filter(.data[[variable_name]] %in% colors_vars[, 1], period <= max(an))

  plot <- plotTool(data, colors_vars, variable_name, plot_type, label)

  if (!is.null(save_name)) {
    print(paste0("Saving plot to ", save_name))
    ggsave(save_name, plot, units = "in", width = 5.5, height = 4, dpi = 1200)
  }
  return(plot)
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
