# Dream Journal

## Project description

This project is a Shiny app in which one could log their dreams, and explore the themes which appear in their dreams, recurring people, animals and objects, as well as emotions, sleep and stress patterns.
The app has a demo-mode, in which one can see a logged journal of 180 nights, with logged emotions and themes. The app also produces a recommendation for the user based on their sleep pattern and stress levels.
To use the app in its personal mode, one can flip the toggle and start filling in their sleep pattern. In order to avoid the hassle of making an account, the user-entered data is stored locally, and users are asked to download their diary.
Users can reupload their diary at a later moment to continue filling it in. 

## Requirements

- R ≥ 4.4.1 (the lockfile was made with R 4.6.1; we also tested the project
  with R 4.4.1). If `renv::status()` says the R version differs from the
  lockfile, you can ignore that message as long as no packages are listed.
- RStudio (recent versions include Quarto, which is needed to render
  `main.qmd`)

## Features

| Tab | What it shows |
|-----|---------------|
| ✍️ New dream | Write or edit a night step by step: sleep, dream text, feelings while dreaming and after waking, themes, and the people, animals and objects that appeared |
| 🔮 Dream themes | Themes as clouds (bigger = more frequent) for the last week, month, year, all nights or a custom period, plus stress per theme |
| 💖 Emotions | Feelings while dreaming vs. after waking, and an optional 3D valence–arousal–dominance (VAD) map |
| 📊 Your patterns | The stress/sleep → negative-dream analysis from `main.qmd`, run on your own journal for any period |
| 👥 People & things | Who and what recur in confirmed entries |
| 📅 Calendar | Day, week or month view, coloured by sleep quality, stress or hours slept |
| 🌙 Sleep | Sleep ring, stats, charts, tips and a shareable story picture |
| 🔎 Search | Search dream text and all labels |
| ⚙️ Settings | Theme definitions, renaming labels, remembered names, export and backup |

The sidebar also has an *App colour theme* menu (lavender, ocean, forest).

## Run


### 1. Clone the repository
```bash
git clone https://github.com/alexM430/PIPS_dream_diary
```

Or download it on GitHub with **Code → Download ZIP** and unzip it.

### 2. Open the project 
Double-click **`PIPS_dream_diary.Rproj`**. RStudio opens in the right folder,
and renv sets itself up automatically (the first time this can take a
minute).

Not using RStudio? Start R in the `PIPS_dream_diary` folder instead, or run
`setwd("path/to/PIPS_dream_diary")`.

### 3. Install packages

``` r
install.packages("renv") # Only the first time, run in the console
renv::restore() # run in the console
renv::status() # run in the console and check if the packages are installed. if not, run renv::restore() again.

```
If the error *Error: attempt to use zero-length variable name*, run renv::restore in the console.

### 4. Read the report

Open `main.qmd` and click **Render**. The report opens as a web page.

### 5. Open the app

Rendering does not start the app. In the R console, run:

```r
source("src/setup.R")
run_dream_app()
```

The app opens in your browser or the RStudio viewer. Stop it with the red **Stop** button in the console.

## Structure

``` text
├── main.qmd                    project report (research question + analysis)
├── renv.lock, renv/, .Rprofile pinned package versions
├── renv_dependencies.R         package list for renv (never run)
├── data/
│   ├── reviewed_dream_journal.rds
│   ├── dream_journal.csv
│   └── lexicons/
├── src/
│   ├── setup.R                 loads packages and sources src/*.R
│   ├── run_dream_app.R         dream_app() / run_dream_app() entry points
│   ├── app_ui.R, app_server.R  app layout and logic
│   ├── personal_journal.R      demo mode and session-only personal diary
│   ├── review_*.R              reviewed labels, entities, charts, VAD map
│   ├── analysis.R              logistic regression for "Your patterns"
│   ├── simulate_journal.R      synthetic journal generator
│   ├── journal_io.R            reading and writing journal CSVs
│   ├── text_features.R         lexicon matching on dream text
│   ├── *_view.R, *_plots.R     tab views and charts
│   └── www/                    CSS
└── tests/testthat/             unit tests
```

Keep supporting files in their existing folders.

## Tests

The project has unit tests (51 tests in 13 files) in `tests/testthat/`,
written with the [testthat](https://testthat.r-lib.org) package. They check
that the analysis, the simulation and the app's building blocks work as
intended, and that the app handles missing or wrong input with clear
messages instead of crashing.

### Run the tests

Open the project (`PIPS_dream_diary.Rproj`), make sure the packages are
installed (`renv::restore()`), and run in the R console:

```r
testthat::test_dir("tests/testthat")
```

All tests pass when the summary at the end shows `FAIL 0`.

### What the tests check

| File | What it tests |
|------|---------------|
| `test-analysis.R` | The logistic regression: data preparation, finding a built-in stress effect, friendly errors with too little data, APA-style p-values |
| `test-simulate_and_recommend.R` | The simulation is reproducible and stays in valid ranges, stress makes negative dreams more likely, and sleep tips react to short or irregular sleep |
| `test-personal_journal.R` | Privacy and demo mode: the demo journal is read-only, personal entries stay in the session, and a downloaded diary can be uploaded again |
| `test-journal_io.R` | Saving and reading journal entries, readable errors for invalid entries, and bedtimes after midnight |
| `test-review.R` | Reviewed labels: emotions while dreaming and after waking, people/animals/objects, and the emotion (VAD) map |
| `test-text_features.R` | Word-list matching: themes, emotions, people and objects are found as whole words only, and every word in the simulated dreams is in the word lists |
| `test-app.R` | The app starts from the saved snapshot, rebuilds a missing snapshot, and has its loading screen and colour menu |
| `test-period_tabs.R` | Period choices on the Dream themes and Your patterns tabs work independently and handle periods without nights |
| `test-theme_clouds.R` | Theme clouds count per night and grow with frequency |
| `test-calendar_widget.R` | The calendar shows the right days in day, week and month view |
| `test-sleep_view.R` | Sleep statistics, tips and the shareable story picture |
| `test-dream_themes.R` | The colour themes (lavender, ocean, forest) and charts with or without data |
| `test-splash_screen.R` | The cloudy loading screen |

`tests/testthat/helper-load.R` loads the project before the tests run.


## Dependencies

### Software

| Software | Version | Needed for |
|----------|---------|------------|
| [R](https://cran.r-project.org) | ≥ 4.4.1 (lockfile made with 4.6.1) | Running the app and the analysis |
| [RStudio](https://posit.co/download/rstudio-desktop/) | recent | Opening the project (`PIPS_dream_diary.Rproj`) |
| [Quarto](https://quarto.org) | included with recent RStudio | Rendering `main.qmd` |

### R packages

All package versions are recorded in `renv.lock` and installed with
`renv::restore()` (see *Run*). The packages the project uses directly are:

| Package | Used for |
|---------|----------|
| `shiny` | The app itself |
| `bslib` | App layout (tabs, cards, sidebar) and the colour themes |
| `dplyr`, `tidyr`, `tibble` | Preparing and reshaping the journal data |
| `readr` | Reading and writing journal CSVs and the word lists |
| `stringr` | Matching words in dream texts |
| `lubridate` | Dates and weekdays (e.g. the weekly stress rhythm in the simulation) |
| `ggplot2`, `scales` | Charts in the app and the report |
| `plotly` | The interactive 3D emotion map |
| `htmltools` | Building the calendar, theme clouds and other HTML parts of the app |
| `purrr` | Helper functions for working with lists |
| `knitr`, `rmarkdown` | Rendering `main.qmd` and its tables |
| `testthat` | Unit tests in `tests/testthat/` |
| `renv` | Installing the exact package versions from `renv.lock` |

The lockfile lists more packages than this (about 128): these are the
packages above plus the packages *they* depend on, which renv installs
automatically.

`src/setup.R` checks at start-up that the app's packages are installed and
names any that are missing.

## Limitations

- The app is meant for a single user on their own machine.
- The demo data are simulated, so results in `main.qmd` show that the method
  works rather than saying anything about real dreams.
- Labels found by dictionary matching depend on the word lists and need
  review before they count.
- Sleep tips are general guidance, not medical advice.


## References
References can be found in the main.qmd file.

