# Loads the packages and functions of the dream journal project.
# Usage (from the project root): source("src/setup.R")

required_packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "ggplot2",
  "readr", "stringr", "lubridate", "purrr", "tibble",
  "plotly", "htmltools", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "), "\n",
    "Install them with: install.packages(c(",
    paste0('"', missing_packages, '"', collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages(
  invisible(lapply(required_packages, library, character.only = TRUE))
)

#' Source every R file in a folder (except this setup file).
#'
#' @param src_dir Folder that holds the project's R scripts.
#' @return The sourced file paths, invisibly.
source_project <- function(src_dir = "src") {
  if (!dir.exists(src_dir)) {
    stop("Folder '", src_dir, "' not found. Run this from the project root.",
         call. = FALSE)
  }
  files <- list.files(src_dir, pattern = "\\.R$", full.names = TRUE)
  files <- files[basename(files) != "setup.R"]
  for (file in files) source(file, local = globalenv())
  invisible(files)
}

source_project()
