# Dream Journal

Edit test

A Shiny app to log dreams and explore their themes, emotions, recurring people
and objects, and how they relate to sleep and stress. It ships with a simulated
journal of one dreamer (180 nights) so every view works straight away.

## Install

Requires R ≥ 4.1 and, to render `main.qmd`, [Quarto](https://quarto.org).

```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "readr",
                   "stringr", "lubridate", "purrr", "tibble", "testthat"))
```

## Run

Open `main.qmd` in RStudio and run the chunks, or from an R console in the
project root:

```r
source("src/setup.R")
run_dream_app()
```

Run the tests with `testthat::test_dir("tests/testthat")`.

## Structure

```
main.qmd                  main deliverable
data/
  dream_journal.csv       journal the app reads and writes (created on first run)
  lexicons/               word lists (themes, emotions, people, objects) + colours
src/
  setup.R                 loads packages and all functions below
  journal_io.R            read/validate/save entries, time helpers
  simulate_journal.R      simulated dream journal
  text_features.R         word-list detection and valence/arousal scores
  plots.R                 ggplot2 figures used by the app
  sleep_recommendations.R rule-based sleep advice with references
  app_ui.R, app_server.R  Shiny interface and logic
  run_dream_app.R         dream_app() / run_dream_app()
tests/testthat/           unit tests
```

To add a theme, emotion, person or object, add rows to the CSVs in
`data/lexicons/`; no code changes are needed.
