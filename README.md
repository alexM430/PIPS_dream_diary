# Dream Journal

A Shiny app to log dreams and explore content codes, emotions, recurring
people, animals and objects, and sleep and stress patterns.

Includes a synthetic journal of 180 nights, simulated reviewed labels,
and an imported emotion lexicon.

## Install

Requires R ≥ 4.1. Quarto is needed only to render `main.qmd`.

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "ggplot2", "readr",
  "stringr", "lubridate", "purrr", "tibble", "plotly",
  "htmltools", "scales", "testthat"
))
```

## Run

Open the `pips-dream-journal-feature-dream-app` folder in RStudio and run:

```r
renv::restore()   # first time only: installs the recorded package versions
source("src/setup.R")
run_dream_app()
```

`main.qmd` starts the same app.

Keep `data/reviewed_dream_journal.rds` included: it contains the saved
entries, reviewed labels and emotion lexicon. No demo generation or
lexicon import is required.

## Structure

```text
main.qmd                         project report
src/run_dream_app.R              run_dream_app(): starts the app
src/app_ui.R, src/app_server.R   app layout and logic
src/review_*.R                   reviewed labels, entities, charts, VAD map
src/                             other supporting R scripts
data/reviewed_dream_journal.rds   saved entries, reviews and lexicon
data/dream_journal.csv            journal CSV
data/lexicons/                    dictionaries and metadata
www/                             web assets, where supplied
tests/testthat/                  unit tests
```

Keep supporting files in their existing folders.

## Tests

```r
testthat::test_dir("tests/testthat")
```

## Notes

The app is intended for a single user. Dictionary matches require review
before they count as confirmed labels. Check the NRC lexicon's
redistribution terms before sharing the embedded ratings.