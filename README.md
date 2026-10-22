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

Open the project in RStudio and run from the project root:

```r
source("src/reviewed_app.R")
run_fixed_reviewed_app()
```

Keep `data/reviewed_dream_journal.rds` included: it contains the saved
entries, reviewed labels and emotion lexicon. No demo generation or
lexicon import is required.

## Structure

```text
main.qmd                         project report
src/reviewed_app.R               consolidated app and launcher
src/                             supporting R scripts
data/reviewed_dream_journal.rds   saved entries, reviews and lexicon
data/dream_journal.csv            journal CSV
data/lexicons/                    dictionaries and metadata
www/                             web assets, where supplied
tests/testthat/                  unit tests
```

Keep supporting files in their existing folders. The old
`annas_revision.R` and `reviewed_demo.R` files are not required.

## Tests

```r
testthat::test_dir("tests/testthat")
```

## Notes

The app is intended for a single user. Dictionary matches require review
before they count as confirmed labels. Check the NRC lexicon's
redistribution terms before sharing the embedded ratings.