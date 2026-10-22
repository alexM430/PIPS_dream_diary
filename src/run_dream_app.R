# Entry point for the dream journal app.

#' Create the dream journal Shiny app.
#'
#' The demo journal and its reviewed labels are read from an RDS snapshot
#' (`state_path`). If the snapshot does not exist, it is created from the CSV
#' journal; if that does not exist either, the CSV is first made from
#' `simulate_dream_journal()` when `simulate_if_missing` is TRUE. Personal
#' entries are never written to the project (see src/personal_journal.R).
#'
#' @param journal_path CSV journal used to start a new snapshot.
#' @param state_path RDS snapshot with the demo journal.
#' @param lexicon_dir Folder with the word lists.
#' @param simulate_if_missing Seed a missing CSV journal with simulated nights.
#' @param splash_seconds Minimum seconds the cloudy loading screen is shown;
#'   0 turns it off.
#' @return A shiny.appobj; print it or pass it to `shiny::runApp()`.
dream_app <- function(journal_path = "data/dream_journal.csv",
                      state_path = "data/reviewed_dream_journal.rds",
                      lexicon_dir = "data/lexicons",
                      simulate_if_missing = TRUE,
                      splash_seconds = 3) {
  if (!file.exists(state_path) && !file.exists(journal_path) && simulate_if_missing) {
    message("No journal found at ", journal_path, "; creating one from simulated data.")
    write_journal(simulate_dream_journal(), journal_path)
  }
  state <- review_read_state(state_path, journal_path)
  if (!nrow(state$vad)) {
    warning("No VAD lexicon is saved in this snapshot, so the emotion map stays empty.",
            call. = FALSE)
  }
  
  ui <- dream_app_ui(splash_seconds = splash_seconds)
  if (isTRUE(state$simulation$synthetic)) {
    ui <- htmltools::tagList(
      shiny::conditionalPanel("input.demo_mode", shiny::tags$div(
        style = "padding:10px 18px;background:#fff3cd;color:#664d03;border-bottom:1px solid #e6d49a;font-size:14px;",
        shiny::tags$strong("SYNTHETIC DATA \u2014 "),
        "Dreaming emotions come from the simulator. Waking responses are simulated separately. Entity confirmations are automatic, not human review."
      )),
      ui
    )
  }
  shinyApp(ui, dream_app_server(normalizePath(journal_path, mustWork = FALSE),
                                normalizePath(state_path, mustWork = FALSE),
                                lexicon_dir))
}

#' Launch the dream journal app in the browser.
#'
#' @inheritParams dream_app
#' @param ... Passed on to `shiny::runApp()`, e.g. `port` or `launch.browser`.
run_dream_app <- function(journal_path = "data/dream_journal.csv",
                          state_path = "data/reviewed_dream_journal.rds",
                          lexicon_dir = "data/lexicons",
                          simulate_if_missing = TRUE, splash_seconds = 3, ...) {
  runApp(dream_app(journal_path, state_path, lexicon_dir, simulate_if_missing,
                   splash_seconds), ...)
}
