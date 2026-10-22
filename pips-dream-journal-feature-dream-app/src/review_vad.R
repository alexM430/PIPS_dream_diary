# review_vad.R

# Optional valence-arousal-dominance (VAD) emotion map, based on the NRC VAD
# v2 lexicon (Mohammad, 2025) saved in the journal snapshot. Each reported feeling is placed by how
# pleasant (valence), how intense (arousal) and how in control (dominance) it
# is. Feelings are grouped into five regions of this space, following the
# circumplex model of affect (Russell, 1980), with dominance separating anger
# (in control) from fear (powerless).

# Emotion regions: name and which chart colour of the theme they use.
vad_regions <- c(
  joy = "\U0001F604 Joy & excitement",
  calm = "\U0001F60C Calm & contentment",
  anger = "\U0001F620 Anger & frustration",
  fear = "\U0001F628 Fear & anxiety",
  sadness = "\U0001F622 Sadness & tiredness"
)
vad_region_colour_index <- c(joy = 4, calm = 5, anger = 6, fear = 1, sadness = 3)
# Where each region's name tag sits relative to its cluster on screen (pixels),
# so tags spread out towards their side of the map instead of overlapping.
vad_tag_offsets <- list(joy = c(80, -60), calm = c(90, 45), anger = c(-90, -70),
                        fear = c(-110, -15), sadness = c(-90, 55))

#' Emotion region of points in VAD space.
#'
#' @param valence,arousal,dominance Numeric vectors of lexicon scores.
#' @param mid Middle of the score scale (0 for -1 to 1, 0.5 for 0 to 1).
#' @return Character vector of region keys (names of `vad_regions`).
vad_region <- function(valence, arousal, dominance, mid = 0) {
  dplyr::case_when(
    valence >= mid & arousal >= mid ~ "joy",
    valence >= mid ~ "calm",
    arousal >= mid & dominance >= mid ~ "anger",
    arousal >= mid ~ "fear",
    TRUE ~ "sadness"
  )
}

review_vad_matches <- function(reviews, vad) {
  x <- dplyr::count(
    review_emotion_long(reviews),
    label,
    stage,
    name = "nights"
  )
  dplyr::left_join(x, vad, by = c(label = "term"))
}

# Message shown instead of the map when there is nothing to draw.
vad_empty_plot <- function() {
  plotly::config(
    plotly::layout(
      plotly::plot_ly(type = "scatter", mode = "markers"),
      xaxis = list(visible = FALSE, range = c(0, 1)),
      yaxis = list(visible = FALSE, range = c(0, 1)),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      annotations = list(list(
        text = paste0(
          "Your emotion map appears here once you report feelings<br>",
          "that are in the emotion lexicon. The chart above does not need it."
        ),
        x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
        font = list(family = "Quicksand, sans-serif", size = 14, color = "#9A8AAE")
      )),
      margin = list(l = 15, r = 15, b = 15, t = 15)
    ),
    displayModeBar = FALSE
  )
}

#' 3D map of reported feelings in valence-arousal-dominance space.
#'
#' Bubbles are feelings (bigger = reported on more nights), coloured by emotion
#' region; a floating label names each region that has feelings.
#'
#' @param matches Output of `review_vad_matches()`.
#' @param stage "during" (while dreaming) or "after" (after waking).
#' @param limits Score scale of the lexicon, e.g. `c(-1, 1)`.
#' @param theme Colour theme name (see `dream_themes`).
#' @param n_labels Number of most frequent feelings that get a text label.
#' @return A plotly widget.
review_vad_plot <- function(matches, stage, limits, theme = "lavender", n_labels = 15) {
  x <- dplyr::filter(
    matches,
    .data$stage == .env$stage,
    is.finite(valence),
    is.finite(arousal),
    is.finite(dominance)
  )
  if (!nrow(x)) return(vad_empty_plot())

  t <- get_dream_theme(theme)
  mid <- mean(limits)
  span <- diff(limits)
  region_colours <- stats::setNames(t$charts[vad_region_colour_index], names(vad_regions))
  x <- dplyr::mutate(
    x,
    region = vad_region(valence, arousal, dominance, mid),
    size = 10 + 20 * sqrt(nights / max(nights)),
    hover = sprintf(
      "<b>%s</b><br>%s<br>%d night%s<br>pleasant %.2f \u00B7 intense %.2f \u00B7 in control %.2f",
      htmltools::htmlEscape(label), vad_regions[region], nights,
      ifelse(nights == 1, "", "s"), valence, arousal, dominance
    )
  )

  p <- plotly::plot_ly()
  # Faint centre lines through the middle of each axis, for orientation.
  centre <- rep(mid, 2)
  for (axis in c("x", "y", "z")) {
    line <- list(x = centre, y = centre, z = centre)
    line[[axis]] <- limits
    p <- plotly::add_trace(p, type = "scatter3d", mode = "lines",
                           x = line$x, y = line$y, z = line$z,
                           line = list(color = t$soft[1], width = 3, dash = "dot"),
                           hoverinfo = "none", showlegend = FALSE)
  }
  # One bubble trace per region, so the legend shows the regions.
  for (key in names(vad_regions)) {
    r <- x[x$region == key, ]
    if (!nrow(r)) next
    p <- plotly::add_trace(
      p, type = "scatter3d", mode = "markers",
      x = r$valence, y = r$arousal, z = r$dominance,
      text = r$hover, hoverinfo = "text", name = vad_regions[[key]],
      marker = list(size = r$size, color = region_colours[[key]], opacity = 0.9,
                    line = list(color = "#FFFFFF", width = 2))
    )
  }
  # Names of the most frequent feelings next to their bubbles.
  labelled <- utils::head(x[order(-x$nights), ], n_labels)
  p <- plotly::add_trace(
    p, type = "scatter3d", mode = "text",
    x = labelled$valence, y = labelled$arousal, z = labelled$dominance + 0.06 * span,
    text = labelled$label, textposition = "top center", hoverinfo = "none",
    textfont = list(family = "Quicksand, sans-serif", size = 12, color = t$ink),
    showlegend = FALSE
  )

  # A floating name tag above the centre of each region's cluster.
  clusters <- x |>
    dplyr::group_by(region) |>
    dplyr::summarise(valence = stats::weighted.mean(valence, nights),
                     arousal = stats::weighted.mean(arousal, nights),
                     dominance = stats::weighted.mean(dominance, nights), .groups = "drop")
  tags <- lapply(seq_len(nrow(clusters)), function(i) {
    key <- clusters$region[i]
    list(
      x = clusters$valence[i], y = clusters$arousal[i],
      z = clusters$dominance[i],
      text = paste0("<b>", vad_regions[[key]], "</b>"),
      showarrow = TRUE, arrowhead = 0, arrowwidth = 1.5, arrowcolor = region_colours[[key]],
      ax = vad_tag_offsets[[key]][1], ay = vad_tag_offsets[[key]][2],
      bgcolor = "rgba(255,255,255,0.9)", bordercolor = region_colours[[key]],
      borderwidth = 2, borderpad = 6,
      font = list(family = "Quicksand, sans-serif", size = 13, color = t$ink)
    )
  })

  axis <- function(title) {
    list(
      title = list(text = title, font = list(family = "Quicksand, sans-serif",
                                             size = 13, color = t$ink)),
      range = limits, gridcolor = "#EDE4F7", zerolinecolor = "#EDE4F7",
      showbackground = TRUE, backgroundcolor = "rgba(255,255,255,0.35)",
      tickfont = list(size = 10, color = t$muted), showspikes = FALSE
    )
  }
  p |>
    plotly::layout(
      scene = list(
        xaxis = axis("pleasant \u2192"),
        yaxis = axis("intense \u2192"),
        zaxis = axis("in control \u2192"),
        aspectmode = "cube",
        annotations = tags,
        camera = list(eye = list(x = 1.15, y = -1.25, z = 0.6))
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Quicksand, sans-serif", color = t$ink),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.02,
                    font = list(size = 12)),
      hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = t$soft[1],
                        font = list(family = "Quicksand, sans-serif", color = t$ink)),
      margin = list(l = 0, r = 0, b = 0, t = 10)
    ) |>
    # No scroll-zoom, so scrolling the page never gets stuck in the map.
    plotly::config(displayModeBar = FALSE, scrollZoom = FALSE)
}
