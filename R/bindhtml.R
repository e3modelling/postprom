#' bindhtml
#' 
#' This script looks for the most recent HTML validation report in a folder
#' by searching for files whose names start with validation_cfg.
#' It then builds a combined dashboard.html page. If no validation file exists,
#' the output contains only a single “Dashboard” tab showing the main dashboard
#' HTML file. If a validation file is found, the script reads both the dashboard
#' and validation HTML files and creates a two-tab interface: one tab for the
#' dashboard and another for the validation report.
#' 
#' @return This function does not return a value but it binds dashboard.html
#' and a validation html.
#'
#' @examples
#' \dontrun{
#' bindhtml(.path = "path/to/run")
#' }
#' 
#' @export 
bindhtml <- function(.path) {
  
  files <- list.files(
    path = .path,
    pattern = "^validation_cfg",
    full.names = TRUE
  )
  
  latest_file <- files[
    which.max(file.info(files)$mtime)
  ]
  
  if (length(latest_file) == 0) {
    file1 <- paste0(.path,"/dashboard.html")
    output_file <- "dashboard.html"
    
    # Read entire HTML files
    content1 <- paste(readLines(file1, warn = FALSE), collapse = "\n")
    
    # Escape quotes for srcdoc
    escape_html <- function(x) {
      x <- gsub("&", "&amp;", x)
      x <- gsub('"', "&quot;", x)
      x <- gsub("<", "&lt;", x)
      x <- gsub(">", "&gt;", x)
      x
    }
    
    content1_escaped <- escape_html(content1)
    # content2_escaped <- escape_html(content2)
    
    # Create combined HTML
    html_content <- paste0(
      '<!DOCTYPE html>
<html>
<head>
  <title>Combined Report</title>

  <link rel="stylesheet"
        href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">

  <style>
    body {
      margin: 10px;
    }

    .nav-tabs {
      margin-bottom: 15px;
    }

    .tab-pane {
      padding: 20px;
    }

    iframe {
      border: none;
      margin-top: 10px;
      width: 100%;
      height: 1200px;
    }
  </style>
</head>

<body>

<h2>Combined Reports</h2>

<ul class="nav nav-tabs" role="tablist">
  <li class="active">
    <a href="#tab1" data-toggle="tab">Dashboard</a>
  </li>

</ul>

<div class="tab-content">

  <div id="tab1" class="tab-pane active">
    <iframe srcdoc="',
      content1_escaped,
      '"></iframe>
  </div>

</div>

<script src="https://code.jquery.com/jquery-1.11.3.min.js"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>

</body>
</html>'
    )
    
    # Write file
    writeLines(html_content, output_file)
    
    # Show resulting size
    cat("Created:", output_file, "\n")
    
  } else {
    
    
    file1 <- paste0(.path,"/dashboard.html")
    file2 <- latest_file
    output_file <- "dashboard.html"
    
    # Read entire HTML files
    content1 <- paste(readLines(file1, warn = FALSE), collapse = "\n")
    content2 <- paste(readLines(file2, warn = FALSE), collapse = "\n")
    
    # Escape quotes for srcdoc
    escape_html <- function(x) {
      x <- gsub("&", "&amp;", x)
      x <- gsub('"', "&quot;", x)
      x <- gsub("<", "&lt;", x)
      x <- gsub(">", "&gt;", x)
      x
    }
    
    content1_escaped <- escape_html(content1)
    content2_escaped <- escape_html(content2)
    
    # Create combined HTML
    html_content <- paste0(
      '<!DOCTYPE html>
<html>
<head>
  <title>Combined Report</title>

  <link rel="stylesheet"
        href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">

  <style>
    body {
      margin: 10px;
    }

    .nav-tabs {
      margin-bottom: 15px;
    }

    .tab-pane {
      padding: 20px;
    }

    iframe {
      border: none;
      margin-top: 10px;
      width: 100%;
      height: 1200px;
    }
  </style>
</head>

<body>

<h2>Combined Reports</h2>

<ul class="nav nav-tabs" role="tablist">
  <li class="active">
    <a href="#tab1" data-toggle="tab">Dashboard</a>
  </li>

  <li>
    <a href="#tab2" data-toggle="tab">Validation</a>
  </li>
</ul>

<div class="tab-content">

  <div id="tab1" class="tab-pane active">
    <iframe srcdoc="',
      content1_escaped,
      '"></iframe>
  </div>

  <div id="tab2" class="tab-pane">
    <iframe srcdoc="',
      content2_escaped,
      '"></iframe>
  </div>

</div>

<script src="https://code.jquery.com/jquery-1.11.3.min.js"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>

</body>
</html>'
    )
    
    # Write file
    writeLines(html_content, output_file)
    
    cat("Created:", output_file, "\n")
    
  }
  
}