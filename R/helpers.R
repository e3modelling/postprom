# Helpers functions -------------------------------------------------

#' @export
helperRenameItems <- function(magpie, mapping, prefix, dim = 3.1) {
  # Rename magpie from OPEN-PROM Efs to labels (e.g. Gas -> Secondary Energy|Electricity|)
  # and aggregate in case of duplicates (Coal from lignite and Hard coal to single Coal)
  magpie <- toolAggregate(
    magpie,
    dim = 3, rel = mapping, from = "Tech", to = "EF"
  )

  name <- getItems(magpie, dim)
  title <- if (!is.null(prefix)) paste0(prefix, name) else name
  getItems(magpie, 3) <- title
  return(magpie)
}

#' Aggregate magpie dimension by hierarchical name levels
#'
#' Aggregates a magpie object along a given dimension by truncating
#' pipe-separated (\code{"|"}) item names to a specified hierarchical level.
#'
#' For example, variables such as
#' \code{"Secondary Energy|Electricity|Gas|w/o CCS"} and
#' \code{"Secondary Energy|Electricity|Gas|w/ CCS"} can be aggregated to
#' \code{"Secondary Energy|Electricity|Gas"} by choosing \code{level = 3}.
#'
#' The hierarchy is inferred from the number of \code{"|"} separators in the
#' item names. Aggregation is performed using \code{\link{toolAggregate}}.
#'
#' @param magpie_object A \code{magpie} object to be aggregated.
#' @param level Integer specifying the hierarchical depth to retain. All
#'   name components below this level are aggregated. Must be smaller than the
#'   maximum depth present in the data.
#' @param dim Numeric dimension identifier of the magpie object to aggregate
#'   (e.g. \code{3.1} for variables). Defaults to \code{3.1}.
#' @param recursive Logical. If \code{FALSE} (default), aggregation is performed
#'   directly to the specified level. If \code{TRUE}, aggregation maps for all
#'   intermediate levels between the maximum depth and \code{level} are combined
#'   before aggregation.
#'
#' @return A \code{magpie} object aggregated to the specified hierarchical level.
#'   If \code{level} is greater than or equal to the maximum depth of the item
#'   names, the original object is returned unchanged.
#'
#' @details
#' Item names are assumed to follow a hierarchical structure separated by
#' \code{"|"} characters. The function truncates names to the requested level
#' and aggregates all matching entries accordingly.
#'
#' Recursive aggregation is mathematically equivalent to direct aggregation and
#' is mainly intended for compatibility with workflows that explicitly require
#' multi-level mapping.
#'
#' @examples
#' \dontrun{
#' helperAggregateLevel(x, level = 3)
#' helperAggregateLevel(x, level = 2, recursive = TRUE)
#' }
#'
#' @importFrom magclass is.magpie getItems
#' @importFrom madrat toolAggregate
#' @importFrom dplyr bind_rows filter
#' @importFrom stringr str_extract
#' @export
helperAggregateLevel <- function(magpie_object, level, dim = 3.1, recursive = FALSE) {
  if (!is.magpie(magpie_object)) stop("Input must be a magpie object")
  if (!is.numeric(level) || level < 1) stop("Level must be a positive integer")

  full_names <- getItems(magpie_object, dim = dim)
  max_depth <- max(sapply(strsplit(full_names, "\\|"), length))

  if (level >= max_depth) {
    return(magpie_object)
  }

  if (recursive == TRUE) {
    map <- NULL
    for (lvl in seq(max_depth, level, -1)) {
      pattern <- paste0("^([^|]+\\|){", lvl - 1, "}[^|]+")
      new_names <- str_extract(full_names, pattern)
      temp_map <- data.frame(names = full_names, parent = new_names)
      map <- bind_rows(map, temp_map)
    }
  } else {
    # keep name up to the level of depth
    pattern <- paste0("^([^|]+\\|){", level - 1, "}[^|]+")
    new_names <- str_extract(full_names, pattern)

    # Aggregate based on new level structure
    map <- data.frame(names = full_names, parent = new_names)
  }

  map <- map %>% filter(!is.na(parent))
  x <- toolAggregate(
    magpie_object,
    dim = dim, rel = map, from = "names", to = "parent"
  )
  return(x)
}

#' Convert units in a magpie object to expected units
#'
#' Converts variables in a magpie object (dimension 3 items) from the units found
#' in the object to the units expected by a provided unit table. Conversions are
#' applied variable-by-variable using audited, hard-coded conversion factors for
#' a limited set of supported unit pairs (energy quantities, selected prices,
#' population scaling, and macro aggregates).
#'
#' The function updates both the numeric values (by multiplying with a conversion
#' factor) and the unit labels embedded in the third-dimension item names
#' (e.g. `"GDP (billion USD_2010/yr)"`). Unsupported conversions either stop with an 
#' error (default) or are skipped with a warning if `allowUnrecognized = TRUE`.
#'
#' @param magpieObj A magpie object whose third-dimension item names follow the
#'   pattern `"variable (unit)"`. Values will be scaled in-place for variables
#'   requiring conversion.
#' @param unitTable A data.frame with (at least) the columns:
#'   `variable`, `magpieUnit`, `expectedUnit`, and `unitMatches`.
#'   Rows with `unitMatches` not `TRUE` are treated as mismatches to convert.
#'   `variable` should match the variable name *without* the trailing `" (unit)"`
#'   part in the magpie object.
#' @param usd2015to2010 Numeric scalar deflator to convert values from USD_2015 to
#'   USD_2010 (multiplicative factor). Used for currency and price conversions.
#' @param allowUnrecognized Logical. If `FALSE` (default), unsupported unit pairs
#'   cause an error and the function stops. If `TRUE`, unsupported conversions are
#'   skipped (with a warning) and the variable name is recorded in `skipped`.
#' @param quiet Logical. If `FALSE` (default), prints a short summary of how many
#'   variables were converted and shows the first rows of the conversion log.
#'
#' @return A list with:
#' \describe{
#'   \item{object}{The converted magpie object (same class as input).}
#'   \item{log}{A data.frame recording each successful conversion with columns
#'     `variable`, `fromUnit`, `toUnit`, `factorUsed`.}
#'   \item{skipped}{Character vector of variables skipped due to unsupported unit
#'     pairs (only non-empty when `allowUnrecognized = TRUE`).}
#' }
#' @examples
#' \dontrun{
#' # unitTable is expected to have: variable, magpieUnit, expectedUnit, unitMatches
#' res <- convertUnitsToExpected(
#'   magpieObj        = x,
#'   unitTable        = unitTable,
#'   usd2015to2010    = 0.93,
#'   allowUnrecognized = TRUE
#' )}
#' @importFrom magclass getItems
#' @export
convertUnitsToExpected <- function(magpieObj, unitTable, usd2015to2010, 
                          allowUnrecognized = FALSE, quiet = FALSE) {

  trim <- function(x) trimws(as.character(x))
  cleanVar <- function(x) sub("\\s*\\(.*\\)$", "", x) # drop trailing " (unit)"
  safeEq <- function(a, b) trim(a) == trim(b)

  # normalize unit strings (case-insensitive)
  normUnit <- function(u) {
    u <- trim(u)
    u <- gsub("KWh", "kWh", u, fixed = TRUE) # harmonize capitalization
    u <- gsub("US\\$","USD_", u)             # normalize currency prefix
    u <- gsub("USD__", "USD_", u)            # double underscore guard
    u <- gsub("tn CO2", "t CO2", u, fixed = TRUE) # treat tn and t as same
    u
  }

  # exact constants (audited)
  TOE_PER_GJ   <- 1/41.868             # toe per GJ (for per-energy prices)
  GJ_PER_TOE   <- 41.868               # GJ per toe
  EJ_PER_MTOE  <- 0.041868             # EJ per Mtoe
  EJ_PER_TWH   <- 0.0036               # EJ per TWh
  GJ_PER_KWH   <- 0.0036               # GJ per kWh
  KWH_PER_GJ   <- 1 / GJ_PER_KWH       # 277.7777777778
  BILLION_TO_MILLION <- 1000

  # compute factor for one pair of units
  computeFactor <- function(fromU, toU) {
    fromU0 <- normUnit(fromU)
    toU0   <- normUnit(toU)

    # identical after normalization -> factor 1
    if (fromU0 == toU0) return(1)

    # ---- energy quantity flows ----
    # Mtoe -> EJ(/yr)
    if (fromU0 == "Mtoe" && grepl("^EJ", toU0)) return(EJ_PER_MTOE)

    # TWh -> EJ(/yr)
    if (fromU0 == "TWh" && grepl("^EJ", toU0)) return(EJ_PER_TWH)

    # ---- population ----
    if (fromU0 == "billion" && toU0 == "million") return(BILLION_TO_MILLION)

    # ---- prices ----
    # KUSD_2015/toe -> USD_2010/GJ
    if (fromU0 == "KUSD_2015/toe" && toU0 == "USD_2010/GJ")
      return(1000 * usd2015to2010 * (1 / GJ_PER_TOE))  # ×1000 (KUSD->USD) × deflator × 1/41.868

    # USD_2015/kWh -> USD_2010/GJ
    if (fromU0 == "USD_2015/kWh" && toU0 == "USD_2010/GJ")
      return(usd2015to2010 * KWH_PER_GJ)               # × deflator × 277.777...

    # US$2015/KWh cases may have been normalized above
    if (fromU0 == "USD_2015/kWh" && toU0 == "USD_2010/GJ")
      return(usd2015to2010 * KWH_PER_GJ)

    # Carbon price: USD_2015/t CO2 -> USD_2010/t CO2
    if (fromU0 == "USD_2015/t CO2" && toU0 == "USD_2010/t CO2")
      return(usd2015to2010)

    # Some sources write USD_2015/tn CO2; normalized to t CO2 above
    if (fromU0 == "USD_2015/t CO2" && toU0 == "USD_2010/t CO2")
      return(usd2015to2010)

    # ---- macro aggregates ----
    # GDP: billion USD_2015/yr -> billion USD_2010/yr
    if (fromU0 == "billion USD_2015/yr" && toU0 == "billion USD_2010/yr")
      return(usd2015to2010)

    # ---- unsupported pair ----
    return(NA_real_)
  }

  # current 3rd-dimension item names and their clean labels
  curNames <- magclass::getItems(magpieObj, dim = 3)
  curClean <- cleanVar(curNames)

  # keep only rows that actually differ and exist in the object
  unitTable$variable    <- trim(unitTable$variable)
  unitTable$magpieUnit  <- trim(unitTable$magpieUnit)
  unitTable$expectedUnit<- trim(unitTable$expectedUnit)

  toFix <- subset(unitTable, !isTRUE(unitMatches))
  toFix <- toFix[toFix$variable %in% curClean, , drop = FALSE]

  if (nrow(toFix) == 0L) {
    if (!quiet) message("No unit mismatches found to convert.")
    return(list(object = magpieObj, log = data.frame(), skipped = character()))
  }

  # containers for audit
  audit <- list()
  skipped <- character()

  # perform conversions variable by variable
  for (i in seq_len(nrow(toFix))) {
    v   <- toFix$variable[i]
    uIn <- toFix$magpieUnit[i]
    uEx <- toFix$expectedUnit[i]

    idx <- which(curClean == v)
    if (!length(idx)) next

    factor <- computeFactor(uIn, uEx)

    if (is.na(factor)) {
      msg <- paste0("Unsupported conversion: '", uIn, "' -> '", uEx, "' for variable '", v, "'.")
      if (allowUnrecognized) {
        warning(msg)
        skipped <- c(skipped, v)
        next
      } else {
        stop(msg, call. = FALSE)
      }
    }

    # scale values
    magpieObj[,, idx] <- magpieObj[,, idx] * factor

    # update unit label in the 3rd-dimension name(s)
    curNames[idx] <- paste0(v, " (", uEx, ")")

    # add to audit
    audit[[length(audit) + 1]] <- data.frame(
      variable    = v,
      fromUnit    = uIn,
      toUnit      = uEx,
      factorUsed  = factor,
      stringsAsFactors = FALSE
    )
  }
  getItems(magpieObj, dim = 3) <- curNames

  auditDf <- if (length(audit)) do.call(rbind, audit) else data.frame()

  if (!quiet && nrow(auditDf)) {
    message("Converted ", nrow(auditDf), " variables. Example:\n",
            utils::capture.output(print(utils::head(auditDf), row.names = FALSE)) |> paste(collapse = "\n"))
    if (length(skipped)) message("Skipped (unsupported): ", paste(unique(skipped), collapse = "; "))
  }

  return(list(object = magpieObj, log = auditDf, skipped = unique(skipped)))
}

#' Check run folders and generate status list
#'
#' Scans the \code{runs} directory inside a given base path and evaluates
#' each subfolder for required files such as \code{main.gms}, \code{main.log},
#' \code{main.lst}, and optional files like \code{git_diff.txt}. The function
#' determines run status (FAILED, COMPLETED, PENDING), extracts the final year,
#' checks calibration settings, and flags modified runs.
#'
#' @param base_path Character path to the root directory containing \code{runs/}.
#' @param flag Logical, reserved for extended modelstat checks. Default: FALSE.
#'
#' @return A list where each element contains:
#'   \itemize{
#'     \item \code{message} — formatted status message
#'     \item \code{folder}  — full path of the run folder
#'   }
#'
#' @import fs
#' @import stringr
#' @export
check_files_and_list_subfolders <- function(base_path, flag = FALSE) {
  runs_path <- file.path(base_path, "runs")
  subfolders <- dir(runs_path, full.names = TRUE, recursive = FALSE)
  subfolders <- subfolders[file.info(subfolders)$isdir]
  
  folder_names <- basename(subfolders)
  max_folder_name_length <- max(nchar(folder_names))
  max_status_length <- 35
  max_year_length <- 4
  
  current_time <- as.numeric(Sys.time())
  max_modification_threshold <- 120
  max_mod_time <- current_time - max_modification_threshold
  
  subfolder_status_list <- list()
  
  for (folder in subfolders) {
    main_gms_path <- file.path(folder, "main.gms")
    main_lst_path <- file.path(folder, "main.lst")
    main_log_path <- file.path(folder, "main.log")
    git_diff_path <- file.path(folder, "git_diff.txt")
    
    folder_name <- basename(folder)
    status <- ""
    run_type <- "Run: Vanilla"
    
    # Determine calibration mode
    if (file.exists(main_gms_path)) {
      gms_txt <- readLines(main_gms_path, warn = FALSE)
      if (any(grepl("\\$setGlobal Calibration on", gms_txt)))
        run_type <- "Run: Calibration"
    }
    
    modified_status <- ifelse(file.exists(git_diff_path), "Yes", "No")
    
    if (!file.exists(main_gms_path)) {
      status <- sprintf("%-*s", max_status_length,
                        "Missing: main.gms  Status: NOT A RUN")
      
    } else if (!file.exists(main_lst_path) || !file.exists(main_log_path)) {
      
      if (current_time > max_mod_time) {
        status <- sprintf("%-*s", max_status_length,
                          "main.lst or main.log missing -> FAILED Year: None Horizon: None")
      } else {
        status <- sprintf("%-*s", max_status_length,
                          "Missing: main.lst or main.log Status: PENDING")
      }
      
    } else {
      # Extract end year
      gms_lines <- readLines(main_gms_path, warn = FALSE)
      end_line <- gms_lines[grepl("\\$evalGlobal fEndY", gms_lines)]
      end_horizon_year <- ifelse(length(end_line) > 0,
                                 tail(str_split(end_line, "\\s+")[[1]], 1), "NA")
      
      log_lines <- readLines(main_log_path, warn = FALSE)
      
      year <- NA
      running_year <- NA
      
      loop_lines <- grep("--- LOOPS an =", log_lines, value = TRUE)
      if (length(loop_lines) > 0) {
        year <- str_trim(str_split(loop_lines[length(loop_lines)], "=")[[1]][2])
        running_year <- year
      }
      
      log_time <- as.numeric(file.info(main_log_path)$mtime)
      time_diff <- current_time - log_time
      modification_threshold <- 15
      completed <- any(grepl("\\*\\*\\* Status: Normal completion", log_lines))
      
      if (completed) {
        status <- sprintf("%-*s", max_status_length,
                          paste("Missing: NONE Status: COMPLETED Year:", year,
                                "Horizon:", end_horizon_year))
        
      } else if (!completed && time_diff < modification_threshold) {
        status <- sprintf("%-*s", max_status_length,
                          paste("Missing: NONE Status: PENDING Running_Year:",
                                running_year, "Horizon:", end_horizon_year))
        
      } else {
        status <- sprintf("%-*s", max_status_length,
                          paste("main.log -> FAILED Status: FAILED Year:", year,
                                "Horizon:", end_horizon_year))
      }
    }
    
    msg <- sprintf("%-*s %s %s Modified: %s",
                   max_folder_name_length, folder_name,
                   status, run_type, modified_status)
    
    subfolder_status_list[[length(subfolder_status_list) + 1]] <-
      list(message = msg, folder = folder)
  }
  
  subfolder_status_list <- subfolder_status_list[
    order(sapply(subfolder_status_list, function(x) file.info(x$folder)$ctime))
  ]
  
  return(subfolder_status_list)
}

#' Display numbered, color-coded subfolder status list
#'
#' Prints the list produced by \code{check_files_and_list_subfolders()} in
#' reverse order, applying color coding:
#' \itemize{
#'   \item Red — FAILED
#'   \item Blue — PENDING
#'   \item Green — COMPLETED
#' }
#'
#' @param subfolder_status_list List output from
#'   \code{\link{check_files_and_list_subfolders}}.
#'
#' @return The input list, invisibly.
#'
#' @import crayon
#' @export
list_subfolders <- function(subfolder_status_list) {
  if (length(subfolder_status_list) == 0) {
    cat("No subfolders found in the 'runs' directory.\n")
    return(list())
  }
  
  cat(yellow("List of subfolders...\n\n"))
  
  n <- length(subfolder_status_list)
  
  for (i in seq_along(subfolder_status_list)) {
    num <- n - i + 1
    msg <- subfolder_status_list[[i]]$message
    
    colored_msg <- if (grepl("FAILED", msg)) red(msg)
    else if (grepl("PENDING", msg)) blue(msg)
    else if (grepl("COMPLETED", msg)) green(msg)
    else msg
    
    cat(sprintf("%2d. %s\n", num, colored_msg))
  }
  
  return(subfolder_status_list)
}

#' Get the directory of the currently running script
#'
#' Works in Rscript, RStudio, and interactive sessions.
#'
#' @return Character path of the script directory
#' @importFrom rstudioapi isAvailable getSourceEditorContext
#' @export
get_script_directory <- function() {
  # Rscript
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_index <- grep(file_arg, cmd_args)
  
  if (length(file_index) > 0) {
    script_path <- sub(file_arg, "", cmd_args[file_index])
    return(dirname(normalizePath(script_path)))
  }
  
  # RStudio
  if (rstudioapi::isAvailable()) {
    script_path <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(script_path)) return(dirname(normalizePath(script_path)))
  }
  
  # Sourced or interactive
  if (!is.null(sys.frame(1)$ofile)) {
    return(dirname(normalizePath(sys.frame(1)$ofile)))
  }
  
  # Fallback
  return(normalizePath(getwd()))
}

#' VS Code–friendly wrapper to select run folders
#'
#' Runs \code{check_files_and_list_subfolders()} and
#' \code{list_subfolders()} and selects
#' one or more subfolders automatically.
#'
#' @param preselect Optional numeric vector (indexes) or character vector (folder names) to select subfolders.
#' If NULL, all subfolders are selected.
#'
#' @return A list of selected subfolder entries.
#'
#' @export
#' Interactive folder selection (Python-style)
#'
#' Lists available subfolders and allows the user to select
#' one or more subfolders by number, similar to the Python version.
#'
#' @return A list of selected subfolder entries.
#' @export
call_functions <- function() {
  
  # Path to the script directory
  script_directory <- get_script_directory()
  print(script_directory)
  
  # Get subfolders and their status
  subfolder_status_list <- check_files_and_list_subfolders(script_directory)
  selected_subfolders <- list_subfolders(subfolder_status_list)
  
  if (length(selected_subfolders) == 0) {
    cat("No subfolders found in the 'runs' directory.\n")
    return(NULL)
  }
  
  # Display numbered list of available subfolders
  folder_names <- sapply(selected_subfolders, `[[`, "folder")
  # cat("\nAvailable subfolders:\n")
  # for (i in seq_along(folder_names)) {
  #   cat(i, ". ", folder_names[i], "\n", sep = "")
  # }
  
  # Ask user to input numbers (all at once, like Python input)
  cat("\nEnter the numbers of the subfolders (e.g., 1 2 3): ")
  # choices <- scan(what = integer(), quiet = TRUE)
  input <- readLines("stdin", n = 1)
  choices <- as.integer(strsplit(input, "\\s+")[[1]])
  
  # Validate selections
  choices <- choices[choices >= 1 & choices <= length(selected_subfolders)]
  if (length(choices) == 0) {
    cat("No valid selections made. Returning NULL.\n")
    return(NULL)
  }
  
  # Select subfolders based on user input
  n <- length(selected_subfolders)
  chosen <- lapply(choices, function(choice) selected_subfolders[[n - choice + 1]])
  
  return(chosen)
}
