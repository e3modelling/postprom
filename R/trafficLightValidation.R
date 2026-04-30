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

  targets_raw <- read_excel(targets_file, sheet = target_sheet)

  targets <- targets_raw |>
    dplyr::rename(
      Policy = Policy,
      Region = Region,
      Variable = Variable,
      Target = Target,
      Unit = Unit,
      TargetType = `Target type`,
      BaseYear = `Base Year`,
      TargetYear = `Target Year`
    ) |>
    dplyr::mutate(
      Policy = as.character(Policy),
      Region = as.character(Region),
      Variable = as.character(Variable),
      Target = as.numeric(Target),
      TargetType = tolower(as.character(TargetType)),
      BaseYear = suppressWarnings(as.numeric(BaseYear)),
      Year = suppressWarnings(as.numeric(TargetYear)),
      Unit = as.character(Unit)
    )

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
  

  # Ensure targets types
  targets$Region <- as.character(targets$Region)
  targets$Year <- as.numeric(targets$Year)      # Target Year
  targets$Variable <- as.character(targets$Variable)
  targets$Target <- as.numeric(targets$Target)
  targets$TargetType <- tolower(as.character(targets$TargetType))

  # Build target-year and base-year value tables
  target_vals <- result_df |>
    dplyr::select(Region, Year, Data, Value) |>
    dplyr::rename(Variable = Data, Value_TY = Value)

  base_vals <- result_df |>
    dplyr::select(Region, Year, Data, Value) |>
    dplyr::rename(Variable = Data, BaseYear = Year, Value_BY = Value)

  comparison <- targets |>
    dplyr::left_join(target_vals, by = c("Region", "Year", "Variable")) |>
    dplyr::left_join(base_vals,   by = c("Region", "BaseYear", "Variable")) |>
    dplyr::mutate(
      # Default missing types to absolute
      TargetType = dplyr::if_else(is.na(TargetType) | TargetType == "", "absolute", TargetType),

      # Relative achieved percent of base (TargetYear/BaseYear * 100)
      AchievedPct = dplyr::if_else(
        TargetType == "relative" &
          !is.na(Value_TY) & !is.na(Value_BY) & Value_BY != 0,
        (Value_TY / Value_BY) * 100,
        NA_real_
      ),

      # ---- Standardized "display" columns ----
      Target_display = dplyr::if_else(
        TargetType == "relative",
        Target,         # percent target (e.g., 80)
        Target          # absolute target level
      ),

      Result_display = dplyr::if_else(
        TargetType == "relative",
        AchievedPct,    # achieved percent of base
        Value_TY        # absolute result at TargetYear
      ),

      Deviation_display = dplyr::case_when(
        TargetType == "relative" & !is.na(AchievedPct) ~ abs(AchievedPct - Target),  # pp
        TargetType == "absolute" & !is.na(Value_TY) & !is.na(Target) & Target != 0 ~ abs((Value_TY - Target) / Target) * 100, # %
        TargetType == "absolute" & !is.na(Value_TY) & !is.na(Target) & Target == 0 ~ abs(Value_TY - Target), # fallback
        TRUE ~ NA_real_
      ),

      Deviation_label = dplyr::if_else(TargetType == "relative", "pp", "%"),

      # Status uses your existing calculateStatus() with type-specific direction
      Status = mapply(
        function(t, r, type) {
          if (is.na(t) || is.na(r)) return(NA_character_)
          if (type == "relative") {
            # thresholds interpreted as percentage points
            calculateStatus(t, r, threshold_green, threshold_yellow, direction = "absolute")
          } else {
            # thresholds interpreted as % of target
            calculateStatus(t, r, threshold_green, threshold_yellow, direction = "relative")
          }
        },
        t = Target_display,
        r = Result_display,
        type = TargetType,
        USE.NAMES = FALSE
      ),
    )

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

  color_map <- list("Green" = color_green, "Yellow" = color_yellow, "Red" = color_red)

  latex_lines <- c(
    "% Traffic Light Validation Table",
    sprintf("\\subsection*{%s}", escapeLaTeX(title)),
    "",
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    "\\begin{tabular}{|l|l|r|r|r|c|}",
    "\\hline",
    "\\textbf{Policy} & \\textbf{Region} & \\textbf{Target} & \\textbf{Result} & \\textbf{Dev.} & \\textbf{Status} \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(comparison_df))) {
    row <- comparison_df[i, ]
    status <- row[["Status"]]
    color  <- color_map[[status]]

    policy_text <- escapeLaTeX(as.character(row[["Policy"]]))
    region_text <- escapeLaTeX(as.character(row[["Region"]]))

    if (is.na(status)) {
      status_cell <- "\\cellcolor{gray!20}N/A"
    } else {
      status_cell <- sprintf("\\cellcolor{%s}%s", color, status)
    }

    dev_label <- as.character(row[["Deviation_label"]])

    latex_lines <- c(
      latex_lines,
      sprintf(
        "%s & %s & %.2f & %.2f & %.1f\\,%s & %s \\\\",
        policy_text,
        region_text,
        as.numeric(row[["Target_display"]]),
        as.numeric(row[["Result_display"]]),
        as.numeric(row[["Deviation_display"]]),
        escapeLaTeX(dev_label),
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

  paste(latex_lines, collapse = "\n")
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
