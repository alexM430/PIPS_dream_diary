# Package list for renv. src/setup.R loads the packages from a vector, which
# renv's code scan cannot see, so they are named here explicitly. This file is
# never run; keep it in sync with `required_packages` in src/setup.R.
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(testthat)  # unit tests in tests/testthat
library(rmarkdown) # rendering main.qmd
library(plotly)    # 3D emotion map in src/review_vad.R
library(htmltools)
library(scales)
