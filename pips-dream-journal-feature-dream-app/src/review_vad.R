# Optional valence-arousal-dominance (VAD) emotion map, based on the NRC VAD
# v2 lexicon (Mohammad, 2025) saved in the journal snapshot.

.review_base_vad <- function(matches, stage, limits, colour = "#76559A") {
    x <- dplyr::filter(matches, .data$stage == .env$stage, is.finite(valence), is.finite(arousal), is.finite(dominance))
    if (!nrow(x)) {
        return(plotly::layout(plotly::plot_ly(), annotations = list(list(text = "No matched emotion labels for this stage.", 
            x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))))
    }
    safe_labels <- htmltools::htmlEscape(x$label)
    x$hover <- sprintf("%s<br>Nights: %d<br>Valence: %.3f<br>Arousal: %.3f<br>Dominance: %.3f", safe_labels, 
        x$nights, x$valence, x$arousal, x$dominance)
    plotly::layout(plotly::plot_ly(x, x = ~valence, y = ~arousal, z = ~dominance, type = "scatter3d", 
        mode = "markers", text = ~hover, hoverinfo = "text", size = ~nights, sizes = c(7, 22), marker = list(color = colour, 
            opacity = 0.8)), scene = list(xaxis = list(title = "Valence: unpleasant \u2192 pleasant", range = limits), 
        yaxis = list(title = "Arousal", range = limits), zaxis = list(title = "Dominance: low \u2192 high control", 
            range = limits), aspectmode = "cube"), margin = list(l = 0, r = 0, b = 0, t = 15))
}

review_vad_matches <- function(reviews, vad) {
    x <- dplyr::count(review_emotion_long(reviews), label, stage, name = "nights")
    dplyr::left_join(x, vad, by = c(label = "term"))
}

review_vad_plot <- function(matches, stage, limits, colour = "#76559A") {
    x <- dplyr::filter(matches, .data$stage == .env$stage, is.finite(valence), is.finite(arousal), is.finite(dominance))
    if (nrow(x)) 
        # No scroll-zoom, so scrolling the page never gets stuck in the 3D map.
        return(plotly::config(.review_base_vad(matches, stage, limits, colour), scrollZoom = FALSE))
    plotly::config(plotly::layout(plotly::plot_ly(), xaxis = list(visible = FALSE, range = c(0, 1)), 
        yaxis = list(visible = FALSE, range = c(0, 1)), annotations = list(list(text = paste0("Affective-space plot unavailable.<br>", 
            "Requires reported emotions and matching terms<br>", "from an imported VAD lexicon. Frequency charts are separate."), 
            x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE)), margin = list(l = 15, 
            r = 15, b = 15, t = 15)), displayModeBar = FALSE)
}
