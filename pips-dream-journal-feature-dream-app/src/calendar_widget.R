# The dreamy HTML calendar on the Calendar tab. Styling lives in
# src/www/dream_calendar.css; this file builds the HTML.

# Pastel colour scale for each "Colour days by" option.
calendar_palettes <- list(
  valence = list(colours = c("#A9C6F5", "#F4ECFF", "#FFB0D5"),
                 limits = c(-1, 1), low = "unpleasant", high = "pleasant"),
  sleep_quality = list(colours = c("#FBF3FF", "#D8B4FE", "#A78BFA"),
                       limits = c(1, 10), low = "restless", high = "restful"),
  stress = list(colours = c("#FFF5F7", "#FBCFE8", "#F472B6"),
                limits = c(1, 10), low = "relaxed", high = "stressed"),
  sleep_hours = list(colours = c("#F0F7FF", "#C7D2FE", "#A5B4FC"),
                     limits = c(4, 10), low = "4 h", high = "10 h")
)

#' Colour scale for a calendar variable, optionally in a colour theme.
#'
#' @param fill_by Name of a palette in `calendar_palettes`.
#' @param theme Theme name (see `dream_themes`), or NULL for the default
#'   pastels. Valence keeps its own blue-to-pink scale in every theme.
#' @return A palette list with `colours`, `limits`, `low` and `high`.
calendar_palette <- function(fill_by, theme = NULL) {
  palette <- calendar_palettes[[fill_by]]
  if (is.null(palette)) stop("Unknown fill_by: ", fill_by, call. = FALSE)
  if (!is.null(theme) && fill_by != "valence") {
    t <- get_dream_theme(theme)
    palette$colours <- switch(fill_by,
      sleep_quality = c("#FFFFFF", t$soft[1], t$charts[1]),
      stress = c("#FFFFFF", t$soft[2], t$charts[2]),
      sleep_hours = c("#FFFFFF", t$sky[1], t$charts[3]))
  }
  palette
}

#' Pastel colour for each value of a calendar variable.
#'
#' @param value Numeric vector (NA allowed).
#' @param fill_by Name of a palette in `calendar_palettes`.
#' @param theme Theme name, or NULL for the default pastels.
#' @return Character vector of hex colours, NA where `value` is NA. Values
#'   outside the palette limits get the end colours.
pastel_colour <- function(value, fill_by, theme = NULL) {
  palette <- calendar_palette(fill_by, theme)
  limits <- palette$limits
  colours <- rep(NA_character_, length(value))
  known <- !is.na(value)
  scaled <- (pmin(pmax(value[known], limits[1]), limits[2]) - limits[1]) /
    diff(limits)
  colours[known] <- rgb(colorRamp(palette$colours)(scaled), maxColorValue = 255)
  colours
}

#' Date range covered by a calendar view.
#'
#' @param day Selected date.
#' @param view "day", "week" or "month".
#' @return Date vector of length 2: first and last day.
calendar_range <- function(day, view) {
  day <- as.Date(day)
  switch(view,
         day = c(day, day),
         week = floor_date(day, "week", week_start = 1) + c(0, 6),
         month = c(floor_date(day, "month"), ceiling_date(day, "month") - 1),
         stop("Unknown view: ", view, call. = FALSE))
}

#' Move a selected date one view step back or forward.
#'
#' @param day Selected date.
#' @param view "day", "week" or "month".
#' @param direction -1 for back, 1 for forward.
#' @return The new date (month steps keep valid month ends, e.g. 31 Jan -> 28 Feb).
shift_calendar_date <- function(day, view, direction) {
  step <- switch(view, day = days(1), week = weeks(1), month = months(1))
  if (direction > 0) as.Date(day) %m+% step else as.Date(day) %m-% step
}

#' Heading for a calendar view, e.g. "October 2026".
calendar_title <- function(day, view) {
  day <- as.Date(day)
  switch(view,
         day = format(day, "%A %d %B %Y"),
         week = paste("Week of", format(floor_date(day, "week", week_start = 1),
                                        "%d %B %Y")),
         month = format(day, "%B %Y"))
}

#' Month/week grid of pastel day tiles.
#'
#' Each tile shows the day number, the emoji of the night's first emotion (a
#' moon if no dream was recalled) and a dot per dream theme. Clicking a tile
#' sends its date to the Shiny input `click_input`.
#'
#' @param journal Journal with `valence` (see `add_affect_scores()`).
#' @param start,end First and last day of the view; the grid is padded to
#'   whole weeks and padding days are faded.
#' @param fill_by Name of a palette in `calendar_palettes`.
#' @param selected Date to highlight.
#' @param lexicons Output of `load_lexicons()` (theme colours, emotion emoji).
#' @param click_input Name of the Shiny input that receives clicked dates.
#' @param theme Theme name for the tile colours, or NULL for the default pastels.
#' @return An htmltools tag.
dream_calendar <- function(journal, start, end, fill_by, selected, lexicons,
                           click_input = "cal_click", theme = NULL) {
  start <- as.Date(start)
  end <- as.Date(end)
  grid_start <- floor_date(start, "week", week_start = 1)
  grid_end <- floor_date(end, "week", week_start = 1) + 6

  days <- tibble(date = seq(grid_start, grid_end, by = "day")) |>
    left_join(journal, by = "date") |>
    mutate(fill = pastel_colour(.data[[fill_by]], fill_by, theme))

  weekday_names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  tiles <- lapply(seq_len(nrow(days)), function(i) {
    calendar_tile(days[i, ], in_range = days$date[i] >= start & days$date[i] <= end,
                  selected = identical(days$date[i], as.Date(selected)),
                  lexicons = lexicons, click_input = click_input)
  })

  div(
    class = "dream-cal",
    div(class = "dream-cal-weekdays",
        lapply(weekday_names, \(name) div(class = "dream-cal-weekday", name))),
    div(class = "dream-cal-grid", tiles),
    calendar_legend(fill_by, theme)
  )
}

# One day tile.
calendar_tile <- function(night, in_range, selected, lexicons, click_input) {
  logged <- !is.na(night$bedtime)
  dreamed <- logged && night$dream_text != ""
  emotions <- if (logged) split_tags(night$emotions)[[1]] else character()
  themes <- if (logged) split_tags(night$themes)[[1]] else character()

  emoji <- if (length(emotions) > 0) {
    lexicons$emotion_meta$emoji[match(emotions[1], lexicons$emotion_meta$emotion)]
  } else if (dreamed) {
    "\u2601\uFE0F"  # cloud: a dream was recalled
  } else if (logged) {
    "\U0001F319"  # crescent moon: slept, no dream recalled
  } else {
    ""
  }
  emoji <- coalesce(emoji, "")
  theme_colours <- lexicons$theme_meta$colour[match(themes, lexicons$theme_meta$theme)]

  tooltip <- if (!logged) "Not logged" else paste0(
    format(night$date, "%a %d %b"), " - ", sprintf("%.1f", night$sleep_hours),
    " h, quality ", night$sleep_quality, "/10, stress ", night$stress, "/10",
    if (dreamed) paste0(" - ", str_replace_all(night$themes, ";", ", ")) else
      " - no dream recalled"
  )

  classes <- paste(c("dream-day",
                     if (!in_range) "is-outside",
                     if (!logged) "is-empty",
                     if (selected) "is-selected"), collapse = " ")
  div(
    class = classes,
    style = if (logged && !is.na(night$fill)) paste0("--day-fill:", night$fill, ";"),
    title = tooltip,
    role = "button",
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                      click_input, format(night$date)),
    span(class = "dream-day-num", day(night$date)),
    span(class = "dream-day-emoji", emoji),
    div(class = "dream-day-dots",
        lapply(theme_colours, \(colour) span(class = "dream-dot",
                                              style = paste0("background:", colour))))
  )
}

# Colour key below the grid.
calendar_legend <- function(fill_by, theme = NULL) {
  palette <- calendar_palette(fill_by, theme)
  div(
    class = "dream-legend",
    span(palette$low),
    div(class = "dream-legend-bar",
        style = paste0("background: linear-gradient(90deg, ",
                       paste(palette$colours, collapse = ", "), ");")),
    span(palette$high),
    span(class = "dream-legend-note", "\u2601\uFE0F dream \u00B7 \U0001F319 no dream recalled"),
    span(class = "dream-legend-note", "dots = dream themes")
  )
}

#' Pastel summary pills for the nights in a calendar view.
#'
#' @param entries Journal rows in the view.
#' @return An htmltools tag.
calendar_stats <- function(entries) {
  pill <- function(emoji, value, label) {
    div(class = "dream-stat",
        span(class = "dream-stat-emoji", emoji),
        div(div(class = "dream-stat-value", value),
            div(class = "dream-stat-label", label)))
  }
  if (nrow(entries) == 0) {
    return(div(class = "dream-stats",
               pill("\U0001F4AD", "0", "nights logged in this period")))
  }
  div(
    class = "dream-stats",
    pill("\U0001F319", nrow(entries), "nights logged"),
    pill("\u2601\uFE0F", sum(entries$dream_text != ""), "dreams recalled"),
    pill("\U0001F4A4", sprintf("%.1f h", mean(entries$sleep_hours)), "avg. sleep"),
    pill("\u2B50", sprintf("%.1f", mean(entries$sleep_quality)), "avg. quality"),
    pill("\u26A1", sprintf("%.1f", mean(entries$stress)), "avg. stress")
  )
}

#' Large card for the selected night.
#'
#' @param entry One journal row, or zero rows if the night is not logged.
#' @param day The selected date.
#' @param lexicons Output of `load_lexicons()`.
#' @return An htmltools tag.
dream_night_card <- function(entry, day, lexicons) {
  heading <- div(class = "dream-night-date", format(as.Date(day), "%A %d %B"))
  if (nrow(entry) == 0) {
    return(div(class = "dream-night", heading,
               p(class = "dream-night-text", "No entry for this night yet.")))
  }

  emotions <- split_tags(entry$emotions)[[1]]
  themes <- split_tags(entry$themes)[[1]]
  emotion_rows <- match(emotions, lexicons$emotion_meta$emotion)
  theme_rows <- match(themes, lexicons$theme_meta$theme)

  div(
    class = "dream-night",
    heading,
    div(class = "dream-night-facts",
        span(paste0("\U0001F6CF\uFE0F ", entry$bedtime, " \u2192 ", entry$wake_time)),
        span(sprintf("\U0001F4A4 %.1f h", entry$sleep_hours)),
        span(paste0("\u2B50 ", entry$sleep_quality, "/10")),
        span(paste0("\u26A1 ", entry$stress, "/10"))),
    p(class = "dream-night-text",
      if (entry$dream_text == "") "No dream recalled \U0001F319" else entry$dream_text),
    div(class = "dream-chips",
        lapply(seq_along(emotions), \(i) span(
          class = "dream-chip",
          paste(lexicons$emotion_meta$emoji[emotion_rows[i]],
                lexicons$emotion_meta$label[emotion_rows[i]]))),
        lapply(seq_along(themes), \(i) span(
          class = "dream-chip",
          style = paste0("border-color:", lexicons$theme_meta$colour[theme_rows[i]]),
          lexicons$theme_meta$label[theme_rows[i]])))
  )
}
