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

.review_base_heatmap <- function(journal, reviews, period = "week") {
    period <- match.arg(period, c("week", "month"))
    x <- review_long(reviews, "codes")
    if (!nrow(x)) {
        return(empty_plot("Add your own content codes to entries first."))
    }
    periods <- seq(lubridate::floor_date(min(journal$date), period, week_start = 1), lubridate::floor_date(max(journal$date), 
        period, week_start = 1), by = period)
    denominators <- dplyr::count(dplyr::mutate(journal, period_start = lubridate::floor_date(date, .env$period, 
        week_start = 1)), period_start, name = "logged")
    counts <- dplyr::mutate(dplyr::left_join(tidyr::complete(dplyr::count(dplyr::mutate(x, period_start = lubridate::floor_date(date, 
        .env$period, week_start = 1)), period_start, label, name = "nights"), period_start = .env$periods, 
        label, fill = list(nights = 0L)), denominators, by = "period_start"), rate = nights/logged)
    ggplot2::ggplot(counts, ggplot2::aes(period_start, label, fill = rate)) + ggplot2::geom_tile(colour = "white") + 
        ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(logged), "\u2014", ifelse(nights > 0, as.character(nights), 
            ""))), size = 3) + ggplot2::scale_fill_gradient(low = "#F3EEF8", high = "#76559A", limits = c(0, 
        1), na.value = "#DDDDDD", labels = scales::percent, name = "% of logged nights") + ggplot2::labs(x = NULL, 
        y = NULL, title = paste("User-defined content codes per", period), caption = paste("Numbers = nights coded. Grey / \u2014 = no entries.", 
            "Whole-entry labels support coding, not automated thematic analysis.")) + dream_theme()
}

.review_base_stress <- function(journal, reviews) {
    x <- dplyr::filter(dplyr::left_join(review_long(reviews, "codes"), dplyr::select(journal, date, stress), 
        by = "date"), is.finite(stress))
    if (!nrow(x)) 
        return(empty_plot("No content codes with stress ratings."))
    counts <- dplyr::count(x, label, name = "n")
    x <- dplyr::mutate(dplyr::left_join(x, counts, by = "label"), display = paste0(label, " (n=", n, 
        ")"))
    ggplot2::ggplot(x, ggplot2::aes(reorder(display, stress, FUN = median), stress)) + ggplot2::geom_hline(yintercept = mean(journal$stress, 
        na.rm = TRUE), linetype = "dashed", colour = "grey50") + ggplot2::geom_boxplot(fill = "#B8A1D1", 
        outlier.shape = NA) + ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.1, height = 0, 
        seed = 7), alpha = 0.4) + ggplot2::coord_flip(ylim = c(0.5, 10.5)) + ggplot2::labs(x = NULL, 
        y = "Stress the day before (1\u201310)", caption = "Dots = nights. Dashed line = overall mean. Association, not causation.") + 
        dream_theme()
}

review_frequency_plot <- function(data, title, fill = "#76559A", order = NULL, limit = NULL) {
    if (!nrow(data)) 
        return(empty_plot(paste0(title, "\nNo reviewed labels for this chart in the selected period.")))
    if (is.null(order)) 
        order <- dplyr::pull(dplyr::arrange(data, dplyr::desc(nights), label), label)
    data <- dplyr::filter(data, label %in% order)
    data$label <- factor(data$label, levels = rev(order))
    if (is.null(limit)) 
        limit <- max(c(1, data$nights))
    ggplot2::ggplot(data, ggplot2::aes(nights, label)) + ggplot2::geom_col(fill = fill, width = 0.65) + 
        ggplot2::geom_text(ggplot2::aes(label = nights), hjust = -0.25, size = 3.8) + ggplot2::scale_y_discrete(drop = FALSE) + 
        ggplot2::scale_x_continuous(limits = c(0, limit * 1.18), breaks = function(limits) {
            unique(round(pretty(c(0, limit), n = 5)))
        }, expand = c(0, 0)) + ggplot2::labs(x = "Nights reported", y = NULL, title = title) + dream_theme()
}

review_heatmap <- function(journal, reviews, period = "week", top = "all") {
    if (!nrow(journal)) 
        return(empty_plot("No logged nights in this period."))
    p <- .review_base_heatmap(journal, .review_select_chart_codes(reviews, top), period)
    if (length(p$layers)) {
        p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top", barwidth = grid::unit(9, 
            "cm"), barheight = grid::unit(0.4, "cm"))) + ggplot2::theme(legend.position = "bottom", plot.caption = ggplot2::element_text(size = 9, 
            hjust = 0), plot.margin = ggplot2::margin(10, 15, 10, 10)) + ggplot2::labs(caption = paste("Numbers = nights coded; colour = share of logged nights. Grey / \u2014 = no entries.", 
            "Whole-entry labels support coding, not automated thematic analysis.", sep = "\n"))
    }
    p
}

review_stress_plot <- function(journal, reviews, top = "all") {
    .review_base_stress(journal, .review_select_chart_codes(reviews, top))
}
