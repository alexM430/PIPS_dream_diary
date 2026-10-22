# Overview bubbles at the top of the Emotions, Themes and People tabs. They
# reuse the bubble styling of the Sleep tab (src/www/sleep_view.css).

#' Row of stat bubbles.
#'
#' @param items List of `c(emoji, value, label)` character vectors.
#' @return An htmltools tag.
overview_bubbles <- function(items) {
  div(
    class = "sleep-bubbles dream-overview",
    lapply(items, function(item) {
      div(class = "sleep-bubble",
          span(class = "sleep-bubble-emoji", item[[1]]),
          div(div(class = "sleep-bubble-value", item[[2]]),
              div(class = "sleep-bubble-label", item[[3]])))
    })
  )
}

# Most frequent label and its number of nights, e.g. "calm (12)", or a dash.
top_label <- function(long) {
  if (!nrow(long)) return("\u2014")
  counts <- dplyr::count(long, label, name = "nights")
  counts <- counts[order(-counts$nights, counts$label), ]
  sprintf("%s (%d)", counts$label[1], counts$nights[1])
}

#' Friendly card with the reviewed labels of one night (calendar side panel).
#'
#' @param review One review row.
#' @return An htmltools tag.
review_night_details <- function(review) {
  status_text <- c(unanswered = "not answered yet", reported = "", none = "no emotion noticed",
                   dont_remember = "don't remember")
  feeling_row <- function(emoji, title, status, labels) {
    labels <- review_unpack(labels)
    div(class = "mb-2", strong(paste(emoji, title)), br(),
        if (length(labels)) div(class = "dream-chips", lapply(labels, \(x) span(class = "dream-chip", x)))
        else span(class = "dream-hint", status_text[[status]]))
  }
  entities <- c(review_unpack(review$people), review_unpack(review$animals),
                review_unpack(review$objects))
  div(
    class = "dream-night mt-3",
    feeling_row("\U0001F4AD", "While dreaming", review$during_status, review$during),
    feeling_row("\u2600\uFE0F", "After waking", review$after_status, review$after),
    if (length(entities))
      div(strong("\U0001F465 Who and what appeared"),
          div(class = "dream-chips", lapply(entities, \(x) span(class = "dream-chip is-soft", x))))
  )
}

#' Overview of reported emotions.
#'
#' @param reviews Review rows in the selected period.
#' @return An htmltools tag.
emotion_overview <- function(reviews) {
  during <- review_long(dplyr::filter(reviews, during_status == "reported"), "during")
  after <- review_long(dplyr::filter(reviews, after_status == "reported"), "after")
  both <- dplyr::filter(reviews, during_status == "reported", after_status == "reported")
  same <- vapply(seq_len(nrow(both)), function(i) {
    length(intersect(review_unpack(both$during[i]), review_unpack(both$after[i]))) > 0
  }, logical(1))
  overview_bubbles(list(
    c("\U0001F319", sprintf("%d / %d", sum(reviews$during_status == "reported"), nrow(reviews)),
      "nights with feelings in the dream"),
    c("\U0001F4AD", top_label(during), "top feeling while dreaming"),
    c("\u2600\uFE0F", top_label(after), "top feeling after waking"),
    c("\U0001F3A8", length(unique(c(during$label, after$label))), "different feelings named"),
    c("\U0001F501", if (nrow(both)) sprintf("%.0f%%", 100 * mean(same)) else "\u2014",
      "nights the feeling stayed after waking")
  ))
}

#' Overview of dream themes (content codes).
#'
#' @param journal Journal rows in the selected period.
#' @param reviews Review rows in the selected period.
#' @param min_nights Minimum nights a theme needs to count as "most stressful".
#' @return An htmltools tag.
theme_overview <- function(journal, reviews, min_nights = 3) {
  codes <- review_long(reviews, "codes")
  stressful <- codes |>
    dplyr::inner_join(dplyr::select(journal, date, stress), by = "date") |>
    dplyr::group_by(label) |>
    dplyr::summarise(nights = dplyr::n(), stress = mean(stress), .groups = "drop") |>
    dplyr::filter(nights >= min_nights) |>
    dplyr::arrange(dplyr::desc(stress))
  overview_bubbles(list(
    c("\U0001F3F7\uFE0F", length(unique(codes$label)), "different themes"),
    c("\u2B50", top_label(codes), "most common theme"),
    c("\U0001F4D3", sprintf("%d / %d", length(unique(codes$date)), nrow(journal)),
      "nights with a theme"),
    c("\u26A1", if (nrow(stressful)) sprintf("%s (%.1f)", stressful$label[1], stressful$stress[1])
      else "\u2014", "theme on the most stressful days")
  ))
}

#' Overview of confirmed people, animals and objects.
#'
#' @param confirmed Review rows whose entity lists were confirmed.
#' @param n_logged Number of logged nights in the period.
#' @return An htmltools tag.
entity_overview <- function(confirmed, n_logged) {
  overview_bubbles(list(
    c("\u2705", sprintf("%d / %d", nrow(confirmed), n_logged), "nights confirmed"),
    c("\U0001F465", top_label(review_long(confirmed, "people")), "most frequent person"),
    c("\U0001F43E", top_label(review_long(confirmed, "animals")), "most frequent animal"),
    c("\U0001F511", top_label(review_long(confirmed, "objects")), "most frequent object")
  ))
}
