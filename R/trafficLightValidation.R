#' Read Policy Targets from Excel File
#'
#' Reads policy targets and comparison criteria from an Excel spreadsheet.
#' File should contain columns: Policy, Region, Year, Target, Variable, Unit
#'
#' @param targets_file Path to Excel file containing policy targets
#' @param target_sheet Sheet name in Excel file (default: "Targets")
#'
#' @return A data.frame with columns: Policy, Region, Year, Target, Variable, Unit
#'
#' @importFrom readxl read_excel
#' @export
readPolicyTargets <- function(targets_file, target_sheet = "Targets") {
  if (!file.exists(targets_file)) {
    stop("Targets file not found: ", targets_file)
  }

  targets <- read_excel(targets_file)
  
  # Ensure required columns exist
  required_cols <- c("Policy", "Region", "Year", "Target", "Variable", "Unit")
  missing_cols <- setdiff(required_cols, names(targets))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns in targets file: ", paste(missing_cols, collapse = ", "))
  }

  return(targets)
}

#' Calculate Traffic Light Status
#'
#' Determines Green/Yellow/Red status based on deviation between result and target
#'
#' @param target Numeric target value
#' @param result Numeric result/actual value
#' @param threshold_green Percentage threshold for Green status (default: 5)
#' @param threshold_yellow Percentage threshold for Yellow status (default: 15)
#' @param direction Character: "absolute" for absolute difference, "relative" for % of target
#'
#' @return Character: "Green", "Yellow", or "Red"
#'
#' @keywords internal
#' @noRd
calculateStatus <- function(target, result, threshold_green = 5, threshold_yellow = 15, 
                           direction = "relative") {
  if (is.na(target) || is.na(result)) {
    return(NA_character_)
  }

  if (direction == "relative") {
    # Percentage difference relative to target: ((result - target) / target) * 100
    deviation_pct <- abs((result - target) / target) * 100
  } else {
    # Absolute difference
    deviation_pct <- abs(result - target)
  }

  status <- case_when(
    deviation_pct <= threshold_green ~ "Green",
    deviation_pct <= threshold_yellow ~ "Yellow",
    TRUE ~ "Red"
  )

  attr(status, "deviation") <- deviation_pct
  return(status)
}

#' Compare model results with policy targets
#'
#' Extracts data from magpie object and compares with targets
#'
#' @param magpie_obj A magpie object with dimensions (region, year, data)
#' @param targets A data.frame from readPolicyTargets()
#' @param threshold_green Percentage for Green (default: 5)
#' @param threshold_yellow Percentage for Yellow (default: 15)
#'
#' @return A data.frame with comparison results and status
#'
#' @importFrom magclass as.data.frame
#' @importFrom dplyr left_join mutate case_when filter select
#' @importFrom rlang .data
#' @export
compareWithTargets <- function(magpie_obj, targets, threshold_green = 5, threshold_yellow = 15) {
  
  # Convert magpie object to quitte for easier comparison
  result_df <- as.quitte(magpie_obj)

  if ("DSBSpEF" %in% names(result_df)) {
    result_df <- result_df %>%
      select(-variable) %>%
      rename(variable = DSBSpEF)
  }

  # Ensure all columns exist and have proper types
  result_df$Region <- as.character(result_df$region)
  result_df$Year <- as.numeric(result_df$period)
  result_df$Data <- as.character(result_df$variable)
  result_df$Value <- as.numeric(result_df$value)
  
  # Ensure targets has proper types too
  targets$Region <- as.character(targets$Region)
  targets$Year <- as.numeric(targets$Year)
  targets$Variable <- as.character(targets$Variable)
  targets$Target <- as.numeric(targets$Target)
  
  # Merge targets with results - use base R if dplyr fails
  tryCatch({
    # Merge targets with results
    comparison <- targets %>%
      left_join(
        result_df %>%
          filter(.data$Data %in% targets$Variable) %>%
          select(.data$Region, .data$Year, .data$Data, .data$Value),
        by = c("Region" = "Region", "Year" = "Year", "Variable" = "Data")
      ) %>%
      mutate(
        Value = as.numeric(.data$Value),
        Target = as.numeric(.data$Target),
        Deviation_pct = ifelse(
          !is.na(.data$Value) & !is.na(.data$Target),
          abs((.data$Value - .data$Target) / .data$Target) * 100,
          NA_real_
        ),
        Status = mapply(
          calculateStatus,
          target = .data$Target,
          result = .data$Value,
          threshold_green = threshold_green,
          threshold_yellow = threshold_yellow,
          USE.NAMES = FALSE
        )
      )
  }, error = function(e) {
    # Fallback to base R merge if dplyr fails
    message("Note: Using base R merge instead of dplyr")
    result_subset <- result_df[result_df$Data %in% targets$Variable, 
                               c("Region", "Year", "Data", "Value")]
    comparison <- merge(
      targets,
      result_subset,
      by.x = c("Region", "Year", "Variable"),
      by.y = c("Region", "Year", "Data"),
      all.x = TRUE
    )
    comparison$Value <- as.numeric(comparison$Value)
    comparison$Target <- as.numeric(comparison$Target)
    comparison$Deviation_pct <- ifelse(
      !is.na(comparison$Value) & !is.na(comparison$Target),
      abs((comparison$Value - comparison$Target) / comparison$Target) * 100,
      NA_real_
    )
    comparison$Status <- mapply(
      calculateStatus,
      target = comparison$Target,
      result = comparison$Value,
      threshold_green = threshold_green,
      threshold_yellow = threshold_yellow,
      USE.NAMES = FALSE
    )
  })

  return(comparison)
}

#' Generate LaTeX table with traffic light colors
#'
#' Creates a colored LaTeX table for inclusion in PDF reports
#'
#' @param comparison_df A data.frame from compareWithTargets()
#' @param title Table title
#' @param color_green LaTeX color for Green cells (default: "green!20")
#' @param color_yellow LaTeX color for Yellow cells (default: "yellow!40")
#' @param color_red LaTeX color for Red cells (default: "red!20")
#'
#' @return Character string with LaTeX table code
#'
#' @keywords internal
#' @noRd
escapeLaTeX <- function(text) {
  # Escape special LaTeX characters
  text <- as.character(text)
  text <- gsub("\\\\", "\\textbackslash{}", text)
  text <- gsub("&", "\\\\&", text)
  text <- gsub("%", "\\\\%", text)
  text <- gsub("\\$", "\\\\$", text)
  text <- gsub("#", "\\\\#", text)
  text <- gsub("_", "\\\\_", text)
  text <- gsub("\\^", "\\\\textasciicircum{}", text)
  text <- gsub("~", "\\\\textasciitilde{}", text)
  text <- gsub("\\{", "\\\\{", text)
  text <- gsub("\\}", "\\\\}", text)
  return(text)
}

generateTrafficLightLatex <- function(comparison_df, title = "Policy Targets Validation",
                                     color_green = "green!20", 
                                     color_yellow = "yellow!40", 
                                     color_red = "red!20") {
  
  # Create color mapping
  color_map <- list(
    "Green" = color_green,
    "Yellow" = color_yellow,
    "Red" = color_red
  )

  latex_lines <- c(
    "% Traffic Light Validation Table",
    sprintf("\\subsection*{%s}", escapeLaTeX(title)),
    "",
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    "\\begin{tabular}{|l|l|r|r|r|c|}",
    "\\hline",
    "\\textbf{Policy} & \\textbf{Region} & \\textbf{Target} & \\textbf{Result} & \\textbf{Dev. \\%} & \\textbf{Status} \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(comparison_df))) {
    row <- comparison_df[i, ]
    status <- row[["Status"]]
    color <- color_map[[status]]
    
    # Escape text fields - use [[ ]] to extract values from single-row df
    policy_text <- escapeLaTeX(as.character(row[["Policy"]]))
    region_text <- escapeLaTeX(as.character(row[["Region"]]))
    
    if (is.na(status)) {
      status_cell <- "\\cellcolor{gray!20}N/A"
    } else {
      status_cell <- sprintf("\\cellcolor{%s}%s", color, status)
    }

    latex_lines <- c(
      latex_lines,
      sprintf(
        "%s & %s & %.2f & %.2f & %.1f & %s \\\\",
        policy_text,
        region_text,
        as.numeric(row[["Target"]]),
        as.numeric(row[["Value"]]),
        as.numeric(row[["Deviation_pct"]]),
        status_cell
      )
    )
  }

  latex_lines <- c(
    latex_lines,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}",
    ""
  )

  return(paste(latex_lines, collapse = "\n"))
}

#' Create traffic light summary for PDF report
#'
#' Main function to generate the full traffic light section for inclusion in PDF
#'
#' @param magpie_obj A magpie object with model results
#' @param targets_file Path to Excel file with policy targets
#' @param title Table title (default: "Policy Targets Validation")
#'
#' @return List with:
#'   - summary: data.frame with comparison results
#'   - latex_code: Character string with LaTeX code
#'
#' @export
createTrafficLightReport <- function(magpie_obj, targets_file, 
                                    title = "Policy Targets Validation") {
  
  targets <- readPolicyTargets(targets_file)
  comparison <- compareWithTargets(magpie_obj, targets)
  latex_code <- generateTrafficLightLatex(comparison, title = title)

  return(list(
    summary = comparison,
    latex_code = latex_code
  ))
}
