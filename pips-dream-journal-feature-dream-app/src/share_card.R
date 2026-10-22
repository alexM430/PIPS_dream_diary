# Instagram-story-sized (1080 x 1920 px) picture of someone's sleep and dream
# stats. Drawn with ggplot2 on a 9 x 16 canvas so it looks the same everywhere.

story_canvas <- c(width = 9, height = 16)

# Polygon points of a circle.
circle_points <- function(x, y, r, n = 60) {
  angle <- seq(0, 2 * pi, length.out = n)
  tibble(x = x + r * cos(angle), y = y + r * sin(angle))
}

# A cloud made of overlapping circles; `s` scales its size.
cloud_points <- function(x, y, s = 1, id = 1) {
  puffs <- tibble(dx = c(-1.1, -0.25, 0.75, 1.5), dy = c(0, 0.45, 0.25, -0.05),
                  r = c(0.75, 1.0, 0.85, 0.6))
  bind_rows(lapply(seq_len(nrow(puffs)), function(i) {
    circle_points(x + puffs$dx[i] * s, y + puffs$dy[i] * s, puffs$r[i] * s) |>
      mutate(group = paste(id, i))
  }))
}

# Polygon points of a rectangle with rounded corners.
rounded_rect_points <- function(xmin, xmax, ymin, ymax, r = 0.3) {
  corner <- function(cx, cy, from) {
    angle <- seq(from, from + pi / 2, length.out = 10)
    tibble(x = cx + r * cos(angle), y = cy + r * sin(angle))
  }
  bind_rows(corner(xmax - r, ymax - r, 0), corner(xmin + r, ymax - r, pi / 2),
            corner(xmin + r, ymin + r, pi), corner(xmax - r, ymin + r, 3 * pi / 2))
}

#' Story card with sleep and dream stats.
#'
#' @param nights Journal rows to summarise (e.g. from `last_nights()`).
#' @param lexicons Output of `load_lexicons()` (labels for emotions and themes).
#' @param target_hours Recommended sleep range in hours.
#' @param max_bars Maximum number of most recent nights in the mini chart.
#' @return A ggplot meant to be saved at 1080 x 1920 px (see `save_story_card()`).
plot_story_card <- function(nights, lexicons, target_hours = c(7, 9), max_bars = 14) {
  summary <- sleep_summary(nights, target_hours)
  if (is.null(summary)) stop("There are no nights to share yet.", call. = FALSE)
  w <- story_canvas[["width"]]
  h <- story_canvas[["height"]]

  sky <- as.raster(matrix(colorRampPalette(c("#D9C8FF", "#F6D3EC", "#FFE3D3"))(200),
                          ncol = 1))
  clouds <- bind_rows(
    cloud_points(0.9, 15.3, 0.9, 1), cloud_points(8.2, 14.6, 0.8, 2),
    cloud_points(1.0, -0.2, 1.4, 3), cloud_points(5.0, -0.6, 1.6, 4),
    cloud_points(8.6, 0.0, 1.3, 5)
  )

  # Mini bar chart of the most recent nights.
  bars <- tail(nights, max_bars) |>
    mutate(x = seq(1.2, w - 1.2, length.out = n()),
           top = 7.3 + pmin(sleep_hours, 11) / 11 * 2.2,
           colour = case_when(sleep_hours < target_hours[1] ~ "#F9A8D4",
                              sleep_hours > target_hours[2] ~ "#93C5FD",
                              TRUE ~ "#B794F6"))
  bar_width <- min(7, 70 / nrow(bars))

  stats <- tibble(
    value = c(sprintf("%d / %d", summary$zone_nights, summary$n_nights),
              as.character(summary$dreams_recalled),
              sprintf("%.1f / 10", summary$avg_quality),
              summary$avg_bedtime,
              tag_label(summary$top_emotion, select(lexicons$emotion_meta, -emoji),
                        "emotion"),
              tag_label(summary$top_theme, lexicons$theme_meta, "theme")),
    caption = c("nights in the dream zone", "dreams remembered", "sleep quality",
                "avg. bedtime", "top feeling", "top dream theme"),
    x = rep(c(2.45, 6.55), 3),
    y = rep(c(5.9, 4.3, 2.7), each = 2)
  )
  cards <- bind_rows(lapply(seq_len(nrow(stats)), function(i) {
    rounded_rect_points(stats$x[i] - 1.95, stats$x[i] + 1.95,
                        stats$y[i] - 0.7, stats$y[i] + 0.7) |>
      mutate(group = i)
  }))
  date_range <- paste(format(summary$first, "%d %b"), "–",
                      format(summary$last, "%d %b %Y"))

  ggplot() +
    annotation_raster(sky, xmin = 0, xmax = w, ymin = 0, ymax = h, interpolate = TRUE) +
    geom_polygon(data = clouds, aes(x, y, group = group), fill = "#FFFDFE") +
    annotate("text", x = w / 2, y = 14.3, label = "MY DREAM STATS", size = 4.2,
             fontface = "bold", colour = "#9A7FD1") +
    annotate("text", x = w / 2, y = 13.4, label = "Dream Journal", size = 11,
             fontface = "bold", colour = "#8B5CF6") +
    annotate("text", x = w / 2, y = 12.6, label = date_range, size = 4.4,
             colour = dream_ink) +
    annotate("text", x = w / 2, y = 11.45, label = sprintf("%.1f h", summary$avg_hours),
             size = 24, fontface = "bold", colour = "#D946A0") +
    annotate("text", x = w / 2, y = 10.05, label = "average sleep per night",
             size = 4.6, colour = dream_ink) +
    geom_segment(data = bars, aes(x = x, xend = x, y = 7.3, yend = top, colour = colour),
                 linewidth = bar_width, lineend = "round") +
    geom_polygon(data = cards, aes(x, y, group = group), fill = "white", alpha = 0.78) +
    geom_text(data = stats, aes(x, y + 0.18, label = value), size = 6.2,
              fontface = "bold", colour = "#7C3AED") +
    geom_text(data = stats, aes(x, y - 0.38, label = caption), size = 3.3,
              colour = dream_ink) +
    annotate("text", x = w / 2, y = 0.6, label = "made with Dream Journal",
             size = 3.6, fontface = "bold", colour = "#9A7FD1") +
    scale_colour_identity() +
    coord_fixed(xlim = c(0, w), ylim = c(0, h), expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

#' Save the story card as a 1080 x 1920 px PNG.
#'
#' @param path Destination file ending in .png.
#' @inheritParams plot_story_card
#' @return `path`, invisibly.
save_story_card <- function(path, nights, lexicons, target_hours = c(7, 9)) {
  ggsave(path, plot_story_card(nights, lexicons, target_hours),
         width = 1080, height = 1920, units = "px", dpi = 270, bg = "#F6D3EC")
  invisible(path)
}

# Browser code for the "Share to Instagram" button. Instagram has no web link
# for posting, so the picture goes through the phone's share menu (Web Share
# API) where Instagram is one of the choices; elsewhere it downloads instead.
share_story_js <- "
async function shareDreamStory() {
  var link = document.getElementById('story_png');
  if (!link || !link.href) return;
  try {
    var blob = await (await fetch(link.href)).blob();
    var file = new File([blob], 'my-dream-stats.png', { type: 'image/png' });
    if (navigator.canShare && navigator.canShare({ files: [file] })) {
      await navigator.share({ files: [file], title: 'My dream stats' });
      return;
    }
  } catch (err) {
    if (err.name === 'AbortError') return;
  }
  link.click();
}
"
