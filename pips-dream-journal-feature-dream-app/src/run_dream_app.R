# Entry point for the dream journal app.

#' Create the dream journal Shiny app.
#'
#' @param journal_path Personal CSV, loaded if present and created on first save.
#' @param lexicon_dir Folder with the word lists.
#' @return A shiny.appobj.
dream_app <- function(
    journal_path = "data/personal_dream_journal.csv",
    lexicon_dir = "data/lexicons") {
  
  lexicons <- load_lexicons(lexicon_dir)
  
  shiny::shinyApp(
    ui = dream_app_ui(lexicons),
    server = dream_app_server(
      normalizePath(journal_path, mustWork = FALSE),
      lexicons
    )
  )
}

#' Launch the dream journal app in the browser.
#'
#' @param journal_path Personal CSV, loaded if present and created on first save.
#' @param lexicon_dir Folder with the word lists.
#' @param ... Passed to shiny::runApp().
run_dream_app <- function(
    journal_path = "data/personal_dream_journal.csv",
    lexicon_dir = "data/lexicons", ...) {
  
  shiny::runApp(
    dream_app(
      journal_path = journal_path,
      lexicon_dir = lexicon_dir
    ),
    ...
  )
}
