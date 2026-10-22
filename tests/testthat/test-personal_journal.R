# Tests for demo mode and the personal diary (src/personal_journal.R).

demo_reference <- function() {
  s <- review_initial_state(journal_template())
  s$vad <- tibble::tibble(term = "happy", valence = 0.8, arousal = 0.4, dominance = 0.2)
  s
}

personal_with_nights <- function() {
  s <- personal_empty_state(demo_reference())
  entry <- new_entry("2026-10-01", "23:30", "07:00", 7, 4, "I was flying.", "flying", character())
  s$journal <- upsert_entry(s$journal, entry)
  row <- review_row("2026-10-01", "flying")
  row$during <- "happy"
  row$during_status <- "reported"
  row$people <- "sister"
  row$entities_reviewed <- TRUE
  s$reviews <- row
  s$definitions <- tibble::tibble(code = "flying", definition = "")
  s
}

test_that("an empty personal diary keeps the emotion lexicon", {
  s <- personal_empty_state(demo_reference())
  expect_equal(nrow(s$journal), 0)
  expect_equal(s$vad$term, "happy")
  expect_false(s$simulation$synthetic)
})

test_that("a downloaded diary can be uploaded again", {
  path <- tempfile(fileext = ".csv")
  original <- personal_with_nights()
  readr::write_csv(personal_export_table(original), path, na = "")
  loaded <- personal_read_csv(path, demo_reference())
  expect_equal(loaded$journal, original$journal, ignore_attr = TRUE)
  expect_equal(loaded$reviews$during, "happy")
  expect_true(loaded$reviews$entities_reviewed)
  expect_equal(loaded$vad$term, "happy")
})

test_that("invalid diary files give clear errors", {
  path <- tempfile(fileext = ".csv")
  table <- personal_export_table(personal_with_nights())
  readr::write_csv(dplyr::select(table, -stress), path, na = "")
  expect_error(personal_read_csv(path, demo_reference()), "Missing CSV columns: stress")
  table$date <- "01/10/2026"
  readr::write_csv(table, path, na = "")
  expect_error(personal_read_csv(path, demo_reference()), "YYYY-MM-DD")
})

test_that("demo mode is read-only and personal entries stay in the session", {
  dir <- tempfile("app-")
  dir.create(dir)
  file.copy(file.path(project_root, "data", c("reviewed_dream_journal.rds", "dream_journal.csv")), dir)
  state_path <- file.path(dir, "reviewed_dream_journal.rds")
  before <- tools::md5sum(state_path)
  server <- dream_app_server(file.path(dir, "dream_journal.csv"), state_path,
                             file.path(project_root, "data", "lexicons"))
  shiny::testServer(server, {
    write_night <- function() {
      session$setInputs(r_date = as.Date("2026-10-05"), r_bed = "23:00", r_wake = "07:00",
                        r_quality = 6, r_stress = 5, r_text = "A test dream.",
                        r_codes = "test", r_during_status = "unanswered",
                        r_after_status = "unanswered", r_note = "")
      session$setInputs(r_save = 1)
    }
    session$setInputs(demo_mode = TRUE)
    n_demo <- nrow(journal())
    write_night()
    expect_equal(nrow(journal()), n_demo)  # blocked in demo mode
    
    session$setInputs(demo_mode = FALSE)
    expect_equal(nrow(journal()), 0)
    write_night()
    expect_equal(nrow(journal()), 1)
    expect_equal(journal()$dream_text, "A test dream.")
    
    session$setInputs(demo_mode = TRUE)
    expect_equal(nrow(journal()), n_demo)  # the demo diary is unchanged
  })
  expect_equal(tools::md5sum(state_path), before)  # nothing written to the project
})
