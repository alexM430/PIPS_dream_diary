# Entry point for the dream journal app.

#' Create the dream journal Shiny app.
#'
#' @param journal_path CSV the journal is read from and saved to. If it does
#'   not exist, it is created from `simulate_dream_journal()` when
#'   `simulate_if_missing` is TRUE, otherwise the app starts empty.
#' @param lexicon_dir Folder with the word lists.
#' @param simulate_if_missing Seed a missing journal with simulated nights.
#' @return A shiny.appobj; print it or pass it to `shiny::runApp()`.
dream_app <- function(journal_path = "data/dream_journal.csv",
                      lexicon_dir = "data/lexicons",
                      simulate_if_missing = TRUE) {
  if (!file.exists(journal_path) && simulate_if_missing) {
    message("No journal found at ", journal_path, "; creating one from simulated data.")
    write_journal(simulate_dream_journal(), journal_path)
  }
  lexicons <- load_lexicons(lexicon_dir)
  shinyApp(dream_app_ui(lexicons),
           dream_app_server(normalizePath(journal_path, mustWork = FALSE), lexicons))
}

#' Launch the dream journal app in the browser.
#'
#' @inheritParams dream_app
#' @param ... Passed on to `shiny::runApp()`, e.g. `port` or `launch.browser`.
run_dream_app <- function(journal_path = "data/dream_journal.csv",
                          lexicon_dir = "data/lexicons",
                          simulate_if_missing = TRUE, ...) {
  runApp(dream_app(journal_path, lexicon_dir, simulate_if_missing), ...)
}
