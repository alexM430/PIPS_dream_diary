# Reading, validating and saving dream journal entries.
#
# One row per night. `date` is the date of the evening the dreamer went to bed.
# `themes` and `emotions` hold semicolon-separated tags, e.g. "falling;work".

#' An empty journal with the expected columns and types.
#'
#' @return A zero-row tibble.
journal_template <- function() {
  tibble(
    date = as.Date(character()),
    bedtime = character(),
    wake_time = character(),
    sleep_hours = double(),
    sleep_quality = integer(),
    stress = integer(),
    dream_text = character(),
    themes = character(),
    emotions = character()
  )
}

journal_col_types <- function() {
  cols(
    date = col_date(), bedtime = col_character(), wake_time = col_character(),
    sleep_hours = col_double(), sleep_quality = col_integer(),
    stress = col_integer(), dream_text = col_character(),
    themes = col_character(), emotions = col_character()
  )
}

#' Read a journal CSV.
#'
#' @param path Path to the CSV file.
#' @return A journal tibble sorted by date; an empty journal if the file does
#'   not exist yet.
read_journal <- function(path) {
  if (!file.exists(path)) return(journal_template())
  journal <- read_csv(path, col_types = journal_col_types(), na = "")
  missing_cols <- setdiff(names(journal_template()), names(journal))
  if (length(missing_cols) > 0) {
    stop("Journal file '", path, "' is missing columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  journal |>
    mutate(across(c(dream_text, themes, emotions), \(x) coalesce(x, ""))) |>
    arrange(date)
}

#' Write a journal to CSV, creating the folder if needed.
#'
#' @param journal A journal tibble.
#' @param path Destination CSV path.
#' @return `path`, invisibly.
write_journal <- function(journal, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write_csv(arrange(journal, date), path, na = "")
  invisible(path)
}

#' Convert "HH:MM" clock times to hours after 18:00.
#'
#' Bedtimes cross midnight, so 23:30 becomes 5.5 and 00:30 becomes 6.5. This
#' keeps late bedtimes on one continuous scale for averages and plots.
#'
#' @param clock Character vector of "HH:MM" times.
#' @return Numeric vector in [0, 24).
clock_to_evening_hours <- function(clock) {
  if (!all(is_valid_clock(clock))) {
    stop("Times must be in 24-hour HH:MM format, e.g. 23:30.", call. = FALSE)
  }
  parts <- str_split_fixed(clock, ":", 2)
  hours <- as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
  (hours - 18) %% 24
}

#' Convert hours after 18:00 back to "HH:MM".
#'
#' @param evening_hours Numeric vector as returned by `clock_to_evening_hours()`.
#' @return Character vector of "HH:MM" times.
evening_hours_to_clock <- function(evening_hours) {
  minutes <- round(((evening_hours + 18) %% 24) * 60) %% (24 * 60)
  sprintf("%02d:%02d", minutes %/% 60, minutes %% 60)
}

is_valid_clock <- function(clock) {
  str_detect(coalesce(clock, ""), "^([01]?[0-9]|2[0-3]):[0-5][0-9]$")
}

#' Hours slept between a bedtime and a wake time (crossing midnight is fine).
#'
#' @param bedtime,wake_time Character vectors of "HH:MM" times.
#' @return Numeric vector of hours in (0, 24].
sleep_duration_hours <- function(bedtime, wake_time) {
  duration <- (clock_to_evening_hours(wake_time) -
                 clock_to_evening_hours(bedtime)) %% 24
  ifelse(duration == 0, 24, duration)
}

#' Build and validate a single journal entry.
#'
#' @param date Night of the entry (Date or "YYYY-MM-DD").
#' @param bedtime,wake_time "HH:MM" times.
#' @param sleep_quality,stress Ratings from 1 (low) to 10 (high).
#' @param dream_text Free text; may be empty when no dream was recalled.
#' @param themes,emotions Character vectors of tags.
#' @return A one-row journal tibble. Errors with a readable message on
#'   invalid input.
new_entry <- function(date, bedtime, wake_time, sleep_quality, stress,
                      dream_text = "", themes = character(),
                      emotions = character()) {
  date <- as.Date(date)
  if (length(date) != 1 || is.na(date)) stop("Please give a valid date.", call. = FALSE)
  if (!is_valid_clock(bedtime)) stop("Bedtime must look like 23:30.", call. = FALSE)
  if (!is_valid_clock(wake_time)) stop("Wake time must look like 07:15.", call. = FALSE)
  check_rating(sleep_quality, "Sleep quality")
  check_rating(stress, "Stress")

  tibble(
    date = date,
    bedtime = bedtime,
    wake_time = wake_time,
    sleep_hours = round(sleep_duration_hours(bedtime, wake_time), 2),
    sleep_quality = as.integer(sleep_quality),
    stress = as.integer(stress),
    dream_text = str_squish(coalesce(dream_text, "")),
    themes = join_tags(themes),
    emotions = join_tags(emotions)
  )
}

check_rating <- function(value, label) {
  if (length(value) != 1 || is.na(value) || value < 1 || value > 10) {
    stop(label, " must be a number from 1 to 10.", call. = FALSE)
  }
}

#' Add an entry, replacing any existing entry for the same night.
#'
#' @param journal A journal tibble.
#' @param entry A one-row tibble from `new_entry()`.
#' @return The updated journal, sorted by date.
upsert_entry <- function(journal, entry) {
  journal |>
    filter(date != entry$date) |>
    bind_rows(entry) |>
    arrange(date)
}

#' Split semicolon-separated tags into a list of character vectors.
split_tags <- function(tags) {
  lapply(str_split(coalesce(tags, ""), ";"), \(x) x[x != ""])
}

#' Collapse a character vector of tags into one semicolon-separated string.
join_tags <- function(tags) {
  paste(unique(tags[tags != ""]), collapse = ";")
}

#' Long format: one row per (night, tag) for the given tag column.
#'
#' @param journal A journal tibble.
#' @param column "themes" or "emotions".
#' @return Tibble with `date` and `tag`.
unnest_tags <- function(journal, column) {
  journal |>
    transmute(date, tag = split_tags(.data[[column]])) |>
    unnest_longer(tag) |>
    filter(!is.na(tag))
}
