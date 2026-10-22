# Charts for reviewed content codes and emotion frequencies.

.review_select_chart_codes <- function(reviews, top = "all") {
    if (is.null(top) || identical(top, "all")) 
        return(reviews)
    limit <- suppressWarnings(as.integer(top))
    if (!length(limit) || is.na(limit) || limit < 1) {
        stop("Invalid maximum-label setting.")
    }
    order <- dplyr::pull(dplyr::arrange(dplyr::count(review_long(reviews, "codes"), label, name = "nights"), 
        dplyr::desc(nights), label), label)
    keep <- utils::head(order, limit)
    reviews$codes <- vapply(reviews$codes, function(value) {
        review_pack(intersect(review_unpack(value), keep))
    }, character(1))
    reviews
}

# Pastel colours per label, in the order given, from the current theme.
label_colours <- function(labels, theme = "lavender") {
    stats::setNames(dream_chart_colours(length(labels), theme), labels)
}

#' Heat map of content codes over time: each code has its own colour and
#' darker tiles mean the code appeared on a larger share of logged nights.
.review_base_heatmap <- function(journal, reviews, period = "week", theme = "lavender") {
    period <- match.arg(period, c("week", "month"))
    x <- review_long(reviews, "codes")
    if (!nrow(x)) {
        return(empty_plot("Add your own content codes to entries first."))
    }
    periods <- seq(lubridate::floor_date(min(journal$date), period, week_start = 1),
                   lubridate::floor_date(max(journal$date), period, week_start = 1), by = period)
    denominators <- dplyr::count(dplyr::mutate(journal, period_start = lubridate::floor_date(date,
        .env$period, week_start = 1)), period_start, name = "logged")
    counts <- x |>
        dplyr::mutate(period_start = lubridate::floor_date(date, .env$period, week_start = 1)) |>
        dplyr::count(period_start, label, name = "nights") |>
        tidyr::complete(period_start = .env$periods, label, fill = list(nights = 0L)) |>
        dplyr::left_join(denominators, by = "period_start") |>
        dplyr::mutate(rate = nights / logged)
    order <- counts |>
        dplyr::group_by(label) |>
        dplyr::summarise(total = sum(nights), .groups = "drop") |>
        dplyr::arrange(total, dplyr::desc(label)) |>
        dplyr::pull(label)
    counts$label <- factor(counts$label, levels = order)
    ggplot2::ggplot(counts, ggplot2::aes(period_start, label)) +
        ggplot2::geom_tile(ggplot2::aes(fill = label, alpha = rate), colour = "white",
                           linewidth = 1.2, na.rm = TRUE) +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(logged), "\u00B7",
            ifelse(nights > 0, as.character(nights), ""))), size = 3.2, colour = dream_ink) +
        ggplot2::scale_fill_manual(values = label_colours(rev(order), theme), guide = "none") +
        # Shade relative to the busiest tile so typical weeks are clearly visible.
        ggplot2::scale_alpha_continuous(range = c(0.12, 1), limits = c(0, max(c(counts$rate, 0.01), na.rm = TRUE)),
            na.value = 0, guide = "none") +
        ggplot2::scale_x_date(expand = c(0, 0)) +
        ggplot2::labs(x = NULL, y = NULL, title = paste("Your dream themes per", period),
            subtitle = "Numbers = nights with that theme. Darker = more of that week's nights.") +
        dreamy_plot_theme()
}

#' Stress the day before, on nights with each content code.
.review_base_stress <- function(journal, reviews, theme = "lavender") {
    x <- dplyr::filter(dplyr::left_join(review_long(reviews, "codes"),
        dplyr::select(journal, date, stress), by = "date"), is.finite(stress))
    if (!nrow(x))
        return(empty_plot("No content codes with stress ratings."))
    counts <- dplyr::count(x, label, name = "n")
    x <- dplyr::mutate(dplyr::left_join(x, counts, by = "label"),
                       display = paste0(label, "  (", n, " nights)"))
    order <- x |>
        dplyr::group_by(display) |>
        dplyr::summarise(mid = stats::median(stress), .groups = "drop") |>
        dplyr::arrange(mid) |>
        dplyr::pull(display)
    x$display <- factor(x$display, levels = order)
    ggplot2::ggplot(x, ggplot2::aes(stress, display, fill = display)) +
        ggplot2::geom_vline(xintercept = mean(journal$stress, na.rm = TRUE), linetype = "dashed",
                            colour = "#B9A9CF") +
        ggplot2::geom_boxplot(outlier.shape = NA, width = 0.6, colour = "#FFFFFF", alpha = 0.95) +
        ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.15, height = 0.12, seed = 7),
                            colour = dream_ink, alpha = 0.25, size = 1.2) +
        ggplot2::scale_fill_manual(values = label_colours(order, theme), guide = "none") +
        ggplot2::scale_x_continuous(limits = c(0.5, 10.5), breaks = c(1, 4, 7, 10)) +
        ggplot2::labs(x = "stress the day before (1\u201310)", y = NULL,
            title = "Stress before each theme",
            subtitle = "Dashed line = your average stress") +
        dreamy_plot_theme()
}

#' Bar chart of how many nights each label appeared.
#'
#' @param data Tibble with `label` and `nights`.
#' @param title Chart title.
#' @param fill Single bar colour, used when `theme` is NULL.
#' @param order Labels in display order (most frequent first).
#' @param limit Upper end of the x axis, so related charts share a scale.
#' @param theme Theme name; when given, every bar gets its own theme colour.
#' @return A ggplot.
review_frequency_plot <- function(data, title, fill = "#76559A", order = NULL, limit = NULL,
                                  theme = NULL) {
    if (!nrow(data))
        return(empty_plot(paste0(title, "\nNo reviewed labels for this chart in the selected period.")))
    if (is.null(order))
        order <- dplyr::pull(dplyr::arrange(data, dplyr::desc(nights), label), label)
    data <- dplyr::filter(data, label %in% order)
    data$label <- factor(data$label, levels = rev(order))
    if (is.null(limit))
        limit <- max(c(1, data$nights))
    colours <- if (is.null(theme)) stats::setNames(rep(fill, length(order)), order) else
        label_colours(order, theme)
    ggplot2::ggplot(data, ggplot2::aes(nights, label, fill = label)) +
        # Thinner bars when there are only a few labels.
        ggplot2::geom_col(width = min(0.7, 0.15 + 0.1 * length(order))) +
        ggplot2::geom_text(ggplot2::aes(label = nights), hjust = -0.35, size = 3.8,
                           fontface = "bold", colour = dream_ink) +
        ggplot2::scale_fill_manual(values = colours, guide = "none") +
        ggplot2::scale_y_discrete(drop = FALSE) +
        ggplot2::scale_x_continuous(limits = c(0, limit * 1.18), breaks = function(limits) {
            unique(round(pretty(c(0, limit), n = 5)))
        }, expand = c(0, 0)) +
        ggplot2::labs(x = "nights", y = NULL, title = title) +
        dreamy_plot_theme() +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_line(colour = "#E9DDF7",
                                                                  linetype = "dashed"))
}

#' Feelings while dreaming vs. after waking, as a mirrored ("butterfly") bar
#' chart: dreaming to the left, waking to the right, one row per emotion.
#'
#' @param counts Output of `review_emotion_counts()`.
#' @param order Emotions to show, most frequent first.
#' @param theme Theme name.
#' @return A ggplot.
plot_emotion_butterfly <- function(counts, order = NULL, theme = "lavender") {
    if (!nrow(counts) || all(counts$nights == 0))
        return(empty_plot("No reported emotions in this period yet."))
    if (is.null(order)) {
        order <- counts |>
            dplyr::group_by(label) |>
            dplyr::summarise(total = sum(nights), .groups = "drop") |>
            dplyr::arrange(dplyr::desc(total), label) |>
            dplyr::pull(label)
    }
    t <- get_dream_theme(theme)
    x <- dplyr::filter(counts, label %in% order) |>
        dplyr::mutate(label = factor(label, levels = rev(order)),
                      side = ifelse(stage == "during", -nights, nights),
                      stage = factor(stage, levels = c("during", "after"),
                                     labels = c("while dreaming", "after waking")))
    top <- max(c(1, x$nights))
    ggplot2::ggplot(x, ggplot2::aes(side, label, fill = stage)) +
        ggplot2::geom_col(width = 0.72) +
        ggplot2::geom_vline(xintercept = 0, colour = "white", linewidth = 1.5) +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(nights > 0, nights, ""),
            hjust = ifelse(side < 0, 1.3, -0.3)), size = 3.5, fontface = "bold", colour = dream_ink) +
        ggplot2::scale_fill_manual(values = c("while dreaming" = t$accent[1],
                                              "after waking" = t$charts[2]), name = NULL) +
        ggplot2::scale_x_continuous(limits = c(-top, top) * 1.25,
            labels = function(v) abs(v), breaks = function(l) pretty(c(-top, top), n = 6)) +
        ggplot2::labs(x = "nights", y = NULL, title = "While dreaming vs. after waking",
            subtitle = "\u2190 nights you felt it in the dream  \u00B7  nights you felt it after waking \u2192") +
        dreamy_plot_theme() +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_line(colour = "#E9DDF7", linetype = "dashed"))
}

review_heatmap <- function(journal, reviews, period = "week", top = "all", theme = "lavender") {
    if (!nrow(journal))
        return(empty_plot("No logged nights in this period."))
    .review_base_heatmap(journal, .review_select_chart_codes(reviews, top), period, theme)
}

review_stress_plot <- function(journal, reviews, top = "all", theme = "lavender") {
    .review_base_stress(journal, .review_select_chart_codes(reviews, top), theme)
}
