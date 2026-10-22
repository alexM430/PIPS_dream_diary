# Demo mode and the personal diary (by Farah).
#
# Demo mode shows the bundled example journal, which is read-only. With demo
# mode off, the user writes a personal diary that only lives in this session:
# nothing is written to the project folder, so private dreams can never end up
# on GitHub. Users download their diary as a CSV and upload it next time.

#' Copy the emotion lexicon (VAD) from another journal state.
#'
#' @param s Journal state to update.
#' @param reference State that holds the lexicon (e.g. the demo journal).
#' @return `s` with the lexicon fields of `reference`.
personal_copy_lexicon <- function(s, reference) {
  for (field in c("vad", "vad_limits", "vad_source", "vad_version")) {
    s[[field]] <- reference[[field]]
  }
  s
}

#' An empty personal diary that keeps the emotion lexicon of `reference`, so
#' the emotion map and the analysis also work for personal entries.
#'
#' @param reference State that holds the lexicon (e.g. the demo journal).
#' @return A valid journal state without nights.
personal_empty_state <- function(reference) {
  s <- review_initial_state(journal_template())
  s <- personal_copy_lexicon(s, reference)
  s$simulation <- list(synthetic = FALSE)
  review_validate_state(s)
  s
}

#' The personal diary as one table (journal and reviewed labels), for download.
#'
#' @param s Journal state.
#' @return A tibble with one row per night.
personal_export_table <- function(s) {
  review_validate_state(s)
  dplyr::left_join(s$journal, s$reviews, by = "date")
}

#' Read a diary CSV downloaded from this app.
#'
#' Columns are read as text first, so dates, clock times and labels do not
#' depend on type guessing; then every column is checked.
#'
#' @param path Path to the CSV file.
#' @param reference State that holds the lexicon to keep.
#' @return A valid journal state. Errors with a readable message when the file
#'   is not a diary from this app or contains invalid values.
personal_read_csv <- function(path, reference) {
  x <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(),
    name_repair = "minimal",
    show_col_types = FALSE
  )
  if (nrow(readr::problems(x))) stop("The CSV has malformed rows.", call. = FALSE)
  if (anyDuplicated(names(x))) stop("The CSV has duplicate column names.", call. = FALSE)
  
  required <- unique(c(names(journal_template()), names(review_empty())))
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Missing CSV columns: ", paste(missing, collapse = ", "),
         ". Upload a diary CSV downloaded from this app.", call. = FALSE)
  }
  
  dates <- suppressWarnings(as.Date(x$date, format = "%Y-%m-%d"))
  if (anyNA(dates) || any(format(dates, "%Y-%m-%d") != x$date)) {
    stop("Dates must use YYYY-MM-DD.", call. = FALSE)
  }
  if (anyDuplicated(dates)) stop("The CSV has more than one entry for a date.", call. = FALSE)
  x$date <- dates
  
  for (field in c("sleep_quality", "stress")) {
    value <- suppressWarnings(as.numeric(x[[field]]))
    if (any(!is.finite(value) | value < 1 | value > 10 | value != floor(value))) {
      stop(field, " must contain whole numbers from 1 to 10.", call. = FALSE)
    }
    x[[field]] <- as.integer(value)
  }
  if (any(!is_valid_clock(x$bedtime)) || any(!is_valid_clock(x$wake_time))) {
    stop("Bedtime and wake time must use 24-hour HH:MM format.", call. = FALSE)
  }
  # Recalculate derived hours rather than trusting an edited CSV value.
  x$sleep_hours <- round(sleep_duration_hours(x$bedtime, x$wake_time), 2)
  
  flag <- toupper(trimws(x$entities_reviewed))
  if (any(!flag %in% c("TRUE", "FALSE"))) {
    stop("entities_reviewed must contain TRUE or FALSE.", call. = FALSE)
  }
  x$entities_reviewed <- flag == "TRUE"
  
  label_columns <- c("themes", "emotions", "codes", "during", "after",
                     "people", "animals", "objects")
  for (field in label_columns) {
    x[[field]] <- vapply(x[[field]], function(value) review_pack(review_unpack(value)),
                         character(1), USE.NAMES = FALSE)
  }
  for (i in seq_len(nrow(x))) {
    review_validate_emotions(x$during_status[i], review_unpack(x$during[i]))
    review_validate_emotions(x$after_status[i], review_unpack(x$after[i]))
  }
  if (any(x$themes != x$codes)) {
    stop("The themes and reviewed codes disagree. Correct the CSV first.", call. = FALSE)
  }
  
  s <- personal_empty_state(reference)
  s$journal <- dplyr::arrange(dplyr::select(x, dplyr::all_of(names(journal_template()))), date)
  s$reviews <- dplyr::arrange(dplyr::select(x, dplyr::all_of(names(review_empty()))), date)
  codes <- review_unpack(s$reviews$codes)
  s$definitions <- tibble::tibble(code = codes, definition = rep("", length(codes)))
  review_validate_state(s)
  s
}

#' Sidebar controls: the demo-mode switch and, with demo mode off, the buttons
#' to download and upload the personal diary.
#'
#' @return An htmltools tag list.
personal_mode_sidebar <- function() {
  shiny::tagList(
    bslib::input_switch("demo_mode", "\U0001F9EA Demo mode (off = my own diary)", value = TRUE),
    shiny::uiOutput("data_mode_status"),
    shiny::conditionalPanel(
      condition = "input.demo_mode === false",
      shiny::downloadButton("download_personal_csv", "Download my diary (CSV)"),
      shiny::fileInput("upload_personal_csv", "Upload my previous diary CSV",
                       accept = c(".csv", "text/csv")),
      shiny::checkboxInput("confirm_diary_replace",
                           "Replace my current session diary with this CSV", FALSE),
      shiny::actionButton("load_personal_csv", "Load diary CSV")
    )
  )
}

#' Server part of demo mode and the personal diary.
#'
#' @param input,output,session Shiny objects of the app server.
#' @param personal `reactiveVal` holding the personal diary state.
#' @param demo_state The demo journal state (provides the lexicon).
#' @param demo_mode Reactive that is TRUE while demo mode is on.
#' @param on_loaded Function called after a diary CSV was loaded.
personal_mode_server <- function(input, output, session, personal, demo_state,
                                 demo_mode, on_loaded) {
  output$data_mode_status <- shiny::renderUI({
    if (demo_mode()) {
      shiny::p(class = "dream-hint",
               shiny::strong("Demo mode: "),
               "an example diary with simulated dreams. It is read-only; switch demo mode ",
               "off to write your own diary.")
    } else {
      shiny::p(class = "dream-hint",
               shiny::strong("My own diary: "),
               "your entries only live in this session and are never saved in the project ",
               "folder. Download your diary before closing the app, and upload it next ",
               "time to continue. Keep the CSV private and outside GitHub.")
    }
  })
  
  output$download_personal_csv <- shiny::downloadHandler(
    filename = function() paste0("my-dream-diary-", Sys.Date(), ".csv"),
    contentType = "text/csv; charset=utf-8",
    content = function(file) readr::write_csv(personal_export_table(personal()), file, na = "")
  )
  
  shiny::observeEvent(input$load_personal_csv, {
    if (demo_mode()) {
      shiny::showNotification("Switch demo mode off before uploading.", type = "warning")
      return()
    }
    if (is.null(input$upload_personal_csv)) {
      shiny::showNotification("Choose a CSV first.", type = "warning")
      return()
    }
    if (!isTRUE(input$confirm_diary_replace)) {
      shiny::showNotification("Tick the box to confirm replacing your current session diary.",
                              type = "warning")
      return()
    }
    imported <- tryCatch(
      personal_read_csv(input$upload_personal_csv$datapath, demo_state),
      error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(imported)) return()
    # Replace only after the whole file was checked.
    personal(imported)
    shiny::updateCheckboxInput(session, "confirm_diary_replace", value = FALSE)
    on_loaded()
    shiny::showNotification(
      sprintf("Loaded %d night%s. Add more, then download your updated diary.",
              nrow(imported$journal), if (nrow(imported$journal) == 1) "" else "s"),
      type = "message")
  }, ignoreInit = TRUE)
}
