# ggplot2 figures used by the app. Every function takes a journal tibble and
# returns a ggplot; with no data it returns a placeholder plot instead of
# failing, so the app never shows an error panel.

dream_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          legend.position = "bottom")
}

#' Placeholder plot with a centred message.
empty_plot <- function(message = "No entries in this period yet.") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 5, colour = "grey45") +
    theme_void()
}

#' Heat map of dream themes over time; each theme has its own colour.
#'
#' @param journal A journal tibble.
#' @param theme_meta Tibble with `theme`, `label` and `colour`.
#' @param period "week" or "month".
#' @return A ggplot. Darker tiles mean the theme occurred on more nights.
plot_theme_heatmap <- function(journal, theme_meta, period = "week") {
  themes <- unnest_tags(journal, "themes")
  if (nrow(themes) == 0) return(empty_plot())

  counts <- themes |>
    mutate(period_start = floor_date(date, period, week_start = 1)) |>
    count(period_start, tag) |>
    complete(period_start, tag = theme_meta$theme, fill = list(n = 0)) |>
    left_join(theme_meta, by = c("tag" = "theme"))
  theme_order <- counts |>
    group_by(label) |>
    summarise(total = sum(n)) |>
    arrange(total) |>
    pull(label)

  ggplot(counts, aes(period_start, factor(label, levels = theme_order))) +
    geom_tile(aes(fill = tag, alpha = n), colour = "white", linewidth = 0.6) +
    geom_text(aes(label = ifelse(n > 0, n, "")), size = 3.2) +
    scale_fill_manual(values = setNames(theme_meta$colour, theme_meta$theme),
                      guide = "none") +
    scale_alpha_continuous(range = c(0.06, 1), name = "Nights") +
    scale_x_date(expand = c(0, 0)) +
    labs(x = NULL, y = NULL,
         title = paste("Dream themes per", period)) +
    dream_theme()
}

#' Affective map: each dream on the valence-arousal circumplex.
#'
#' @param journal A journal tibble with `valence` and `arousal`
#'   (see `add_affect_scores()`).
#' @param emotion_meta Tibble with `emotion`, `label`, `valence`, `arousal`.
#' @param colour_by Journal column used for point colour.
#' @return A ggplot.
plot_affect_map <- function(journal, emotion_meta, colour_by = "stress") {
  dreams <- filter(journal, !is.na(valence))
  if (nrow(dreams) == 0) return(empty_plot("No dreams with emotion tags in this period."))

  circle <- tibble(angle = seq(0, 2 * pi, length.out = 200),
                   x = cos(angle), y = sin(angle))
  quadrants <- tibble(
    x = c(0.95, -0.95, -0.95, 0.95), y = c(1.05, 1.05, -1.05, -1.05),
    hjust = c(1, 0, 0, 1),
    label = c("Pleasant & intense", "Unpleasant & intense",
              "Unpleasant & low energy", "Pleasant & low energy")
  )

  ggplot() +
    geom_path(data = circle, aes(x, y), colour = "grey80") +
    geom_hline(yintercept = 0, colour = "grey80") +
    geom_vline(xintercept = 0, colour = "grey80") +
    geom_text(data = quadrants, aes(x, y, label = label, hjust = hjust),
              colour = "grey55", size = 3.3) +
    geom_jitter(data = dreams,
                aes(valence, arousal, colour = .data[[colour_by]]),
                width = 0.05, height = 0.05, size = 2.6, alpha = 0.8) +
    geom_label(data = emotion_meta, aes(valence, arousal, label = label),
               size = 3, label.size = 0, fill = alpha("white", 0.7)) +
    scale_colour_viridis_c(option = "magma", end = 0.9, direction = -1,
                           name = str_to_sentence(str_replace(colour_by, "_", " "))) +
    coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
    labs(x = "Valence (unpleasant → pleasant)",
         y = "Arousal (calm → intense)",
         title = "Affective map of your dreams") +
    dream_theme()
}

#' Share of each emotion per period (stacked bars).
#'
#' @param journal A journal tibble.
#' @param emotion_meta Tibble with `emotion`, `label` and `colour`.
#' @param period "week" or "month".
#' @return A ggplot.
plot_emotion_timeline <- function(journal, emotion_meta, period = "week") {
  emotions <- unnest_tags(journal, "emotions")
  if (nrow(emotions) == 0) return(empty_plot())

  counts <- emotions |>
    mutate(period_start = floor_date(date, period, week_start = 1)) |>
    count(period_start, tag) |>
    mutate(tag = factor(tag, levels = emotion_meta$emotion))

  ggplot(counts, aes(period_start, n, fill = tag)) +
    geom_col(position = "fill", width = if (period == "week") 6 else 25) +
    scale_fill_manual(values = setNames(emotion_meta$colour, emotion_meta$emotion),
                      labels = setNames(emotion_meta$label, emotion_meta$emotion),
                      name = NULL) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = "Share of emotions",
         title = paste("Emotions per", period)) +
    dream_theme()
}

#' Calendar grid (Mon-Sun columns) coloured by one variable.
#'
#' @param journal A journal tibble with `valence` (see `add_affect_scores()`).
#' @param start,end First and last date shown.
#' @param fill_by One of "valence", "sleep_quality", "stress", "sleep_hours".
#' @return A ggplot. Days without an entry are grey.
plot_calendar <- function(journal, start, end, fill_by = "valence") {
  days <- tibble(date = seq(as.Date(start), as.Date(end), by = "day")) |>
    left_join(journal, by = "date") |>
    mutate(
      weekday = wday(date, label = TRUE, week_start = 1, abbr = TRUE,
                     locale = "C"),
      week_start = floor_date(date, "week", week_start = 1),
      # Discrete y axis with the earliest week on top.
      week = factor(format(week_start, "%d %b"),
                    levels = rev(unique(format(sort(week_start), "%d %b")))),
      day_label = day(date)
    )

  fill_scale <- switch(
    fill_by,
    valence = scale_fill_gradient2(low = "#4575B4", mid = "#F7F7F7",
                                   high = "#E69F00", limits = c(-1, 1),
                                   na.value = "grey92", name = "Dream valence"),
    stress = scale_fill_distiller(palette = "Reds", direction = 1,
                                  limits = c(1, 10), na.value = "grey92",
                                  name = "Stress"),
    sleep_quality = scale_fill_distiller(palette = "Greens", direction = 1,
                                         limits = c(1, 10), na.value = "grey92",
                                         name = "Sleep quality"),
    sleep_hours = scale_fill_distiller(palette = "Blues", direction = 1,
                                       na.value = "grey92", name = "Hours slept"),
    stop("Unknown fill_by: ", fill_by, call. = FALSE)
  )

  ggplot(days, aes(weekday, week)) +
    geom_tile(aes(fill = .data[[fill_by]]), colour = "white", linewidth = 1.2) +
    geom_text(aes(label = day_label), size = 3.6, colour = "grey20") +
    fill_scale +
    labs(x = NULL, y = "Week of") +
    dream_theme() +
    theme(panel.grid = element_blank())
}

#' Horizontal bar chart of the most frequently mentioned categories.
#'
#' @param mentions Output of `count_mentions()`.
#' @param top_n Number of bars.
#' @param title Plot title.
#' @param fill Bar colour.
#' @return A ggplot.
plot_mentions <- function(mentions, top_n = 10, title = "", fill = "#5B4B8A") {
  if (nrow(mentions) == 0) return(empty_plot("Nothing recognised in this period."))
  totals <- mentions |>
    group_by(category) |>
    summarise(nights = n_distinct(date), .groups = "drop") |>
    slice_max(nights, n = top_n, with_ties = FALSE)

  ggplot(totals, aes(nights, reorder(str_to_sentence(category), nights))) +
    geom_col(fill = fill, width = 0.7) +
    geom_text(aes(label = nights), hjust = -0.3, size = 3.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Nights mentioned", y = NULL, title = title) +
    dream_theme()
}

#' Sleep hours, quality, stress and bedtime over time with smoothed trends.
#'
#' @param journal A journal tibble.
#' @return A ggplot with one panel per measure.
plot_sleep_trends <- function(journal) {
  if (nrow(journal) == 0) return(empty_plot())
  long <- journal |>
    mutate(bedtime_hour = clock_to_evening_hours(bedtime) + 18) |>
    select(date, `Hours slept` = sleep_hours, `Sleep quality` = sleep_quality,
           Stress = stress, `Bedtime (hour, 25 = 01:00)` = bedtime_hour) |>
    pivot_longer(-date, names_to = "measure")

  ggplot(long, aes(date, value)) +
    geom_line(colour = "grey70") +
    geom_point(size = 1, colour = "grey45") +
    { if (nrow(journal) >= 7) geom_smooth(method = "loess", formula = y ~ x,
                                          se = FALSE, colour = "#5B4B8A",
                                          linewidth = 1.1) } +
    facet_wrap(~measure, ncol = 2, scales = "free_y") +
    labs(x = NULL, y = NULL, title = "Sleep routine over time") +
    dream_theme()
}

#' Stress on nights with each theme (box plots), to spot stress-linked themes.
#'
#' @param journal A journal tibble.
#' @param theme_meta Tibble with `theme`, `label` and `colour`.
#' @return A ggplot. The dashed line is the average stress over all nights.
plot_theme_stress <- function(journal, theme_meta) {
  themed <- unnest_tags(journal, "themes") |>
    inner_join(select(journal, date, stress), by = "date") |>
    left_join(theme_meta, by = c("tag" = "theme"))
  if (nrow(themed) == 0) return(empty_plot())

  ggplot(themed, aes(reorder(label, stress, FUN = median), stress, fill = tag)) +
    geom_hline(yintercept = mean(journal$stress), linetype = "dashed",
               colour = "grey50") +
    geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.6) +
    scale_fill_manual(values = setNames(theme_meta$colour, theme_meta$theme),
                      guide = "none") +
    scale_y_continuous(limits = c(0.5, 10.5), breaks = c(1, 4, 7, 10)) +
    coord_flip() +
    labs(x = NULL, y = "Stress the day before (1-10)") +
    dream_theme()
}
