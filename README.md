# Dream Journal

## Project description

This project is a Shiny app in which one could log their dreams, and explore the themes which appear in their dreams, recurring people, animals and objects, as well as emotions, sleep and stress patterns.
The app has a demo-mode, in which one can see a logged journal of 180 nights, with logged emotions and themes. The app also produces a recommendation for the user based on their sleep pattern and stress levels.
To use the app in its personal mode, one can flip the toggle and start filling in their sleep pattern. In order to avoid the hassle of making an account, the user-entered data is stored locally, and users are asked to download their diary.
Users can reupload their diary at a later moment to continue filling it in. 

## Requirements

Requires R 4.6.1 to render `main.qmd`.

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
install.packages("renv") # Only the first time
renv::activate()
renv::restore()


**If `renv::restore()` fails**, install the packages directly instead:

install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "ggplot2", "readr",
  "stringr", "lubridate", "purrr", "tibble", "plotly",
  "htmltools", "scales", "knitr", "rmarkdown", "testthat"
))

```
### 4. Read the report

Open `main.qmd` and click **Render**. The report opens as a web page.

### 5. Open the app

Rendering does not start the app. In the R console, run:

```r
source("src/setup.R")
run_dream_app()
```

The app opens in your browser or the RStudio viewer. Stop it with the red
**Stop** button in the console.

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

``` r
testthat::test_dir("tests/testthat")
```

## Limitations

- The app is meant for a single user on their own machine.
- The demo data are simulated, so results in `main.qmd` show that the method
  works rather than saying anything about real dreams.
- Labels found by dictionary matching depend on the word lists and need
  review before they count.
- Sleep tips are general guidance, not medical advice.


## References
References can be found in the main.qmd file.

