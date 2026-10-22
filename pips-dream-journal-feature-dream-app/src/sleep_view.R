# Overview pieces of the Sleep tab: summary statistics, the sleep ring, stat
# bubbles and recommendation cards. Styling lives in src/www/sleep_view.css.

#' The most recent nights of a journal.
#'
#' @param journal A journal tibble.
#' @param n_nights Number of most recent nights; 0 keeps all nights.
#' @return The matching rows, sorted by date.
last_nights <- function(journal, n_nights) {
  if (nrow(journal) == 0 || n_nights == 0) return(arrange(journal, date))
  journal |>
    filter(date > max(date) - n_nights) |>
    arrange(date)
}

#' Summary statistics of a set of nights.
#'
#' @param nights Journal rows (e.g. from `last_nights()`).
#' @param target_hours Recommended sleep range in hours (Hirshkowitz et al., 2015).
#' @return A named list, or NULL when there are no nights.
sleep_summary <- function(nights, target_hours = c(7, 9)) {
  if (nrow(nights) == 0) return(NULL)
  bedtime <- clock_to_evening_hours(nights$bedtime)
  best <- nights |>
    arrange(desc(sleep_quality), desc(sleep_hours)) |>
    slice(1)

  list(
    first = min(nights$date),
    last = max(nights$date),
    n_nights = nrow(nights),
    avg_hours = mean(nights$sleep_hours),
    avg_quality = mean(nights$sleep_quality),
    avg_stress = mean(nights$stress),
    avg_bedtime = evening_hours_to_clock(mean(bedtime)),
    avg_wake = evening_hours_to_clock(mean(bedtime + nights$sleep_hours)),
    target_hours = target_hours,
    zone_nights = sum(nights$sleep_hours >= target_hours[1] &
                        nights$sleep_hours <= target_hours[2]),
    dreams_recalled = sum(nights$dream_text != ""),
    top_emotion = most_common_tag(nights, "emotions"),
    top_theme = most_common_tag(nights, "themes"),
    best_date = best$date,
    best_hours = best$sleep_hours,
    best_quality = best$sleep_quality
  )
}

# Most frequent tag in a tag column, NA if there are none.
most_common_tag <- function(nights, column) {
  tags <- unnest_tags(nights, column)$tag
  if (length(tags) == 0) return(NA_character_)
  names(sort(table(tags), decreasing = TRUE))[1]
}

#' Label (with emoji for emotions) of a tag, or a fallback when it is NA.
tag_label <- function(tag, meta, key, fallback = "\u2014") {
  if (is.na(tag)) return(fallback)
  row <- match(tag, meta[[key]])
  if (is.na(row)) return(tag)
  if ("emoji" %in% names(meta)) paste(meta$emoji[row], meta$label[row]) else meta$label[row]
}

#' Ring showing average sleep against a goal.
#'
#' @param avg_hours Average hours slept.
#' @param goal_hours Hours that fill the ring completely.
#' @return An htmltools tag containing an inline SVG.
sleep_ring <- function(avg_hours, goal_hours = 8) {
  radius <- 54
  circumference <- 2 * pi * radius
  filled <- min(avg_hours / goal_hours, 1) * circumference
  div(
    class = "sleep-ring",
    HTML(sprintf(
      '<svg viewBox="0 0 140 140" role="img" aria-label="Average sleep %.1f of %s hours">
         <defs><linearGradient id="sleepRingGradient" x1="0" y1="0" x2="1" y2="1">
           <stop offset="0%%" stop-color="#A78BFA"/><stop offset="100%%" stop-color="#F472B6"/>
         </linearGradient></defs>
         <circle cx="70" cy="70" r="%s" class="sleep-ring-track"/>
         <circle cx="70" cy="70" r="%s" class="sleep-ring-fill"
                 stroke-dasharray="%.1f %.1f" transform="rotate(-90 70 70)"/>
       </svg>',
      avg_hours, goal_hours, radius, radius, filled, circumference)),
    div(class = "sleep-ring-label",
        div(class = "sleep-ring-value", sprintf("%.1f h", avg_hours)),
        div(class = "sleep-ring-caption", paste("avg. sleep \u00B7 goal", goal_hours, "h")))
  )
}

#' Bubbles with the main sleep and dream statistics.
#'
#' @param summary Output of `sleep_summary()`.
#' @param lexicons Output of `load_lexicons()`.
#' @return An htmltools tag.
sleep_stat_bubbles <- function(summary, lexicons) {
  bubble <- function(emoji, value, label) {
    div(class = "sleep-bubble",
        span(class = "sleep-bubble-emoji", emoji),
        div(div(class = "sleep-bubble-value", value),
            div(class = "sleep-bubble-label", label)))
  }
  div(
    class = "sleep-bubbles",
    bubble("\U0001F319", summary$avg_bedtime, "avg. bedtime"),
    bubble("\u2600\uFE0F", summary$avg_wake, "avg. wake-up"),
    bubble("\u2728", sprintf("%d / %d", summary$zone_nights, summary$n_nights),
           sprintf("nights in the %g\u2013%g h dream zone",
                   summary$target_hours[1], summary$target_hours[2])),
    bubble("\u2B50", sprintf("%.1f / 10", summary$avg_quality), "avg. sleep quality"),
    bubble("\u26A1", sprintf("%.1f / 10", summary$avg_stress), "avg. stress"),
    bubble("\u2601\uFE0F", summary$dreams_recalled, "dreams remembered"),
    bubble("\U0001F496", tag_label(summary$top_emotion, lexicons$emotion_meta, "emotion"),
           "most common feeling"),
    bubble("\U0001F52E", tag_label(summary$top_theme, lexicons$theme_meta, "theme"),
           "most common theme"),
    bubble("\U0001F3C6", format(summary$best_date, "%a %d %b"),
           sprintf("best night \u00B7 %.1f h \u00B7 quality %d",
                   summary$best_hours, summary$best_quality))
  )
}

#' Cards for sleep recommendations.
#'
#' @param advice Output of `sleep_recommendations()`.
#' @return An htmltools tag.
recommendation_cards <- function(advice) {
  div(
    class = "sleep-tips",
    lapply(seq_len(nrow(advice)), function(i) {
      div(class = "sleep-tip",
          span(class = "sleep-tip-emoji", advice$emoji[i]),
          div(div(class = "sleep-tip-title", advice$title[i]),
              div(class = "sleep-tip-text", advice$message[i]),
              if (advice$source[i] != "")
                div(class = "sleep-tip-source", advice$source[i])))
    })
  )
}
