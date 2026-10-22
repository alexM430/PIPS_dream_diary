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

# Personal and demo journals share a schema, but never share diary rows.

personal_copy_lexicon <- function(s, reference) {
  for (field in c("vad", "vad_limits", "vad_source", "vad_version")) {
    s[[field]] <- reference[[field]]
  }
  s
}

personal_empty_state <- function(reference) {
  s <- review_initial_state(journal_template())
  s <- personal_copy_lexicon(s, reference)
  s$simulation <- list(synthetic = FALSE)
  review_validate_state(s)
  s
}

# Read the bundled demo without writing or replacing its source file.
# The bundled snapshot is assumed to contain simulation, not personal data.
personal_demo_state <- function(state_path, simulate_if_missing = TRUE) {
  if (file.exists(state_path)) {
    s <- readRDS(state_path)
    review_validate_state(s)
    return(s)
  }
  if (!isTRUE(simulate_if_missing)) {
    stop("Demo snapshot is missing and simulation is disabled.")
  }
  s <- review_initial_state(simulate_dream_journal())
  s$reviews$during <- s$journal$emotions
  s$reviews$during_status <- ifelse(
    nzchar(s$reviews$during), "reported", "unanswered"
  )
  # Do not invent waking responses or claim human entity review.
  s$simulation <- list(synthetic = TRUE)
  review_validate_state(s)
  s
}

personal_export_table <- function(s) {
  review_validate_state(s)
  dplyr::left_join(s$journal, s$reviews, by = "date")
}

# Import the CSV produced by this app. Read text explicitly so that
# dates, clock times, Unicode and labels do not depend on type guessing.
personal_read_csv <- function(path, reference) {
  x <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(),
    name_repair = "minimal",
    show_col_types = FALSE
  )
  if (nrow(readr::problems(x))) stop("The CSV has malformed rows.")
  if (anyDuplicated(names(x))) stop("The CSV has duplicate column names.")
  
  required <- unique(c(names(journal_template()), names(review_empty())))
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Missing CSV columns: ", paste(missing, collapse = ", "),
         ". Upload a reviewed CSV exported by this app.")
  }
  
  dates <- suppressWarnings(as.Date(x$date, format = "%Y-%m-%d"))
  if (anyNA(dates) || any(format(dates, "%Y-%m-%d") != x$date)) {
    stop("Dates must use YYYY-MM-DD.")
  }
  if (anyDuplicated(dates)) stop("The CSV has more than one entry for a date.")
  x$date <- dates
  
  for (field in c("sleep_quality", "stress")) {
    value <- suppressWarnings(as.numeric(x[[field]]))
    if (any(!is.finite(value) | value < 1 | value > 10 |
            value != floor(value))) {
      stop(field, " must contain whole numbers from 1 to 10.")
    }
    x[[field]] <- as.integer(value)
  }
  if (any(!is_valid_clock(x$bedtime)) ||
      any(!is_valid_clock(x$wake_time))) {
    stop("Bedtime and wake time must use 24-hour HH:MM format.")
  }
  # Recalculate derived hours rather than trusting an edited CSV value.
  x$sleep_hours <- round(sleep_duration_hours(x$bedtime, x$wake_time), 2)
  
  flag <- toupper(trimws(x$entities_reviewed))
  if (any(!flag %in% c("TRUE", "FALSE"))) {
    stop("entities_reviewed must contain TRUE or FALSE.")
  }
  x$entities_reviewed <- flag == "TRUE"
  
  label_columns <- c("themes", "emotions", "codes", "during", "after",
                     "people", "animals", "objects")
  for (field in label_columns) {
    x[[field]] <- vapply(x[[field]], function(value) {
      review_pack(review_unpack(value))
    }, character(1))
  }
  for (i in seq_len(nrow(x))) {
    review_validate_emotions(x$during_status[i], review_unpack(x$during[i]))
    review_validate_emotions(x$after_status[i], review_unpack(x$after[i]))
  }
  if (any(x$themes != x$codes)) {
    stop("The themes and reviewed codes disagree. Correct the CSV first.")
  }
  
  s <- personal_empty_state(reference)
  s$journal <- dplyr::arrange(
    dplyr::select(x, dplyr::all_of(names(journal_template()))), date
  )
  s$reviews <- dplyr::arrange(
    dplyr::select(x, dplyr::all_of(names(review_empty()))), date
  )
  codes <- review_unpack(s$reviews$codes)
  s$definitions <- tibble::tibble(
    code = codes, definition = rep("", length(codes))
  )
  review_validate_state(s)
  s
}