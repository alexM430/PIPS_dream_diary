# The combined app starts and its colour-theme switch works.

# Copies the saved snapshot and journal to a temporary folder so tests never
# change the project's data files.
temp_app_files <- function() {
  dir <- tempfile("app-")
  dir.create(dir)
  file.copy(file.path(project_root, "data", c("reviewed_dream_journal.rds", "dream_journal.csv")), dir)
  list(state = file.path(dir, "reviewed_dream_journal.rds"),
       journal = file.path(dir, "dream_journal.csv"))
}

test_that("the app is built from the saved snapshot", {
  files <- temp_app_files()
  old_wd <- setwd(project_root)
  on.exit(setwd(old_wd))
  app <- suppressWarnings(dream_app(files$journal, files$state, splash_seconds = 0))
  expect_s3_class(app, "shiny.appobj")
})

test_that("a missing snapshot is created from the journal", {
  files <- temp_app_files()
  file.remove(files$state)
  old_wd <- setwd(project_root)
  on.exit(setwd(old_wd))
  suppressWarnings(dream_app(files$journal, files$state, splash_seconds = 0))
  expect_true(file.exists(files$state))
  expect_equal(nrow(readRDS(files$state)$journal), nrow(read_journal(files$journal)))
})

test_that("every colour preset can be selected", {
  files <- temp_app_files()
  server <- dream_app_server(files$journal, files$state, file.path(project_root, "data", "lexicons"))
  shiny::testServer(server, {
    for (preset in names(dream_themes)) {
      expect_no_error(session$setInputs(app_colour_theme = preset))
    }
  })
})

test_that("the UI has the loading screen and colour menu", {
  old_wd <- setwd(project_root)
  on.exit(setwd(old_wd))
  html <- as.character(dream_app_ui(splash_seconds = 3))
  expect_match(html, "dream-splash")
  expect_match(html, "app_colour_theme")
  expect_false(grepl("dream-splash", as.character(dream_app_ui(splash_seconds = 0))))
})
