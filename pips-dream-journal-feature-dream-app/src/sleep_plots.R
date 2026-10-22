# Pastel ggplot2 charts for the Sleep tab. Backgrounds are transparent so the
# dreamy card behind them shows through.

dream_ink <- "#4A3B5C"

#' Soft, minimal ggplot theme used by the sleep charts.
dreamy_plot_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(colour = dream_ink),
      axis.text = element_text(colour = "#9A8AAE"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "#E9DDF7", linetype = "dashed"),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = base_size * 1.15),
      plot.subtitle = element_text(colour = "#9A8AAE")
    )
}

# Bar thickness that suits the number of nights shown.
bar_linewidth <- function(n_nights) min(6, 55 / max(n_nights, 1))

#' Hours slept per night as rounded "cloud" bars over the dream zone.
#'
#' @param nights Journal rows to show.
#' @param target_hours Recommended sleep range in hours.
#' @param theme Colour theme name (see `dream_themes`).
#' @return A ggplot.
plot_sleep_hours <- function(nights, target_hours = c(7, 9), theme = "lavender") {
  t <- get_dream_theme(theme)
  if (nrow(nights) == 0) return(empty_plot("No nights logged yet."))
  width <- bar_linewidth(nrow(nights))
  nights <- nights |>
    mutate(zone = case_when(
      sleep_hours < target_hours[1] ~ "a bit short",
      sleep_hours > target_hours[2] ~ "extra long",
      TRUE ~ "in the dream zone"
    ))
  zone_colours <- c("in the dream zone" = t$charts[1], "a bit short" = t$charts[2],
                    "extra long" = t$charts[3])

  ggplot(nights, aes(date, sleep_hours, colour = zone)) +
    annotate("rect", xmin = min(nights$date) - 1, xmax = max(nights$date) + 1,
             ymin = target_hours[1],
             ymax = target_hours[2], fill = t$sky[2], alpha = 0.9) +
    annotate("text", x = min(nights$date), y = target_hours[2], vjust = -0.5,
             hjust = 0, label = "dream zone", colour = t$accent[2], size = 3.6,
             fontface = "bold") +
    geom_segment(aes(xend = date, y = 0, yend = sleep_hours),
                 linewidth = width, lineend = "round") +
    geom_point(shape = 21, fill = "white", size = width * 0.9, stroke = 1) +
    scale_colour_manual(values = zone_colours, name = NULL) +
    guides(colour = guide_legend(override.aes = list(linewidth = 3, size = 2))) +
    scale_y_continuous(limits = c(0, max(10, max(nights$sleep_hours) + 0.5)),
                       breaks = seq(0, 12, 2), expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "hours slept",
         title = "How long you slept",
         subtitle = "Each bar is one night") +
    dreamy_plot_theme()
}

#' Sleep windows: a bar from bedtime to wake-up for each night.
#'
#' @param nights Journal rows to show.
#' @return A ggplot with evening at the top and morning at the bottom.
plot_sleep_windows <- function(nights, theme = "lavender") {
  t <- get_dream_theme(theme)
  if (nrow(nights) == 0) return(empty_plot("No nights logged yet."))
  width <- bar_linewidth(nrow(nights))
  nights <- nights |>
    mutate(bed = clock_to_evening_hours(bedtime), wake = bed + sleep_hours)
  hour_breaks <- seq(floor(min(nights$bed)), ceiling(max(nights$wake)), by = 2)

  ggplot(nights, aes(date)) +
    geom_segment(aes(xend = date, y = bed, yend = wake, colour = sleep_quality),
                 linewidth = width, lineend = "round") +
    geom_point(aes(y = bed), colour = t$accent[1], size = width * 0.6) +
    geom_point(aes(y = wake), colour = "#F5B700", size = width * 0.6) +
    scale_colour_gradient(low = t$soft[1], high = t$accent[2], limits = c(1, 10),
                          name = "sleep quality") +
    scale_y_reverse(breaks = hour_breaks, labels = evening_hours_to_clock(hour_breaks)) +
    labs(x = NULL, y = NULL,
         title = "When you slept",
         subtitle = "Dark dot = bedtime, yellow dot = wake-up") +
    dreamy_plot_theme() +
    guides(colour = guide_colourbar(barwidth = 10, barheight = 0.6))
}
