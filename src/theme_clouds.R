# Dream themes drawn as clouds: one cloud per theme, in its own theme colour,
# bigger when the theme came up on more nights. Styling lives in
# src/www/dream_app.css (.theme-cloud).

#' Date range for a period choice on the Dream themes tab.
#'
#' Relative periods count back from the latest logged night, so the demo
#' journal (which ends in June 2026) still shows data.
#'
#' @param dates Dates of all logged nights.
#' @param window "7", "30" or "365" (days), "all" or "custom".
#' @param custom Two dates used when `window` is "custom".
#' @return Date vector of length 2 (first and last day).
theme_period_range <- function(dates, window, custom = NULL) {
  if (!length(dates)) return(rep(Sys.Date(), 2))
  last <- max(dates)
  if (identical(window, "all")) return(c(min(dates), last))
  if (identical(window, "custom")) {
    if (length(custom) != 2 || anyNA(custom)) return(c(min(dates), last))
    return(sort(as.Date(custom)))
  }
  days <- suppressWarnings(as.integer(window))
  if (is.na(days) || days < 1) stop("Unknown period: ", window, call. = FALSE)
  c(last - days + 1, last)
}

#' Nights per theme, most frequent first.
#'
#' @param reviews Review rows in the chosen period.
#' @param top Maximum number of themes ("all" keeps every theme).
#' @return Tibble with `label` and `nights`.
theme_counts <- function(reviews, top = "all") {
  counts <- dplyr::count(review_long(reviews, "codes"), label, name = "nights")
  counts <- counts[order(-counts$nights, counts$label), ]
  if (!identical(top, "all")) counts <- utils::head(counts, as.integer(top))
  counts
}

#' One cloud per theme, sized by how often the theme came up.
#'
#' Clicking a cloud sends its label to the Shiny input `click_input`.
#'
#' @param counts Output of `theme_counts()`.
#' @param n_logged Number of logged nights in the period (for percentages).
#' @param theme Colour theme name (see `dream_themes`).
#' @param selected Label of the highlighted cloud, if any.
#' @param click_input Name of the Shiny input that receives clicked labels.
#' @param size_range Smallest and largest cloud width in pixels.
#' @return An htmltools tag.
theme_clouds <- function(counts, n_logged, theme = "lavender", selected = NULL,
                         click_input = "r_cloud_click", size_range = c(90, 260)) {
  if (!nrow(counts)) {
    return(div(class = "dream-hint text-center p-4",
               "No dream themes in this period yet \u2014 add some in the New dream tab."))
  }
  colours <- label_colours(counts$label, theme)
  # Area grows with the number of nights, so width grows with its square root.
  sizes <- size_range[1] + diff(size_range) * sqrt(counts$nights / max(counts$nights))

  clouds <- lapply(seq_len(nrow(counts)), function(i) {
    label <- counts$label[i]
    share <- if (n_logged > 0) sprintf(" (%.0f%% of nights)", 100 * counts$nights[i] / n_logged) else ""
    div(
      class = paste("theme-cloud", if (identical(label, selected)) "is-selected"),
      style = sprintf("--cloud-size:%.0fpx; --cloud-colour:%s; --cloud-delay:-%.1fs;",
                      sizes[i], colours[[label]], (i * 1.7) %% 6),
      title = sprintf("%s: %d night%s%s", label, counts$nights[i],
                      if (counts$nights[i] == 1) "" else "s", share),
      role = "button",
      onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", click_input,
                        jsonlite::toJSON(label, auto_unbox = TRUE)),
      span(class = "theme-cloud-label", label),
      span(class = "theme-cloud-count", sprintf("%d night%s", counts$nights[i],
                                                if (counts$nights[i] == 1) "" else "s"))
    )
  })
  # Biggest cloud in the middle, smaller ones drifting out to both sides.
  middle_out <- unlist(lapply(seq_along(clouds), function(i) if (i %% 2) i else -i))
  order <- c(rev(-middle_out[middle_out < 0]), middle_out[middle_out > 0])
  div(class = "theme-clouds", clouds[order])
}
