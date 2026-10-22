# Analysis: do stress and short sleep make negative dreams more likely?
#
# Each night with feelings reported "while dreaming" becomes one observation.
# A dream counts as negative when the mean valence of its feelings in the NRC
# VAD lexicon (Mohammad, 2025) is below 0. A logistic regression then models
# the chance of a negative dream from the stress of the day before and the
# hours slept. Used by both main.qmd and the app's "Your patterns" tab.

#' One row per night with the variables of the analysis.
#'
#' @param journal Journal tibble (`date`, `stress`, `sleep_hours`).
#' @param reviews Review rows (`date`, `during`, `during_status`).
#' @param vad Lexicon with `term` and `valence` (scale -1 to 1).
#' @return Tibble with `date`, `stress`, `sleep_hours`, `valence`,
#'   `n_feelings` (feelings found in the lexicon) and `negative` (0/1).
#'   Nights without matched feelings are left out.
dream_analysis_data <- function(journal, reviews, vad) {
  if (!all(c("term", "valence") %in% names(vad))) {
    stop("The lexicon needs the columns 'term' and 'valence'.", call. = FALSE)
  }
  feelings <- review_long(dplyr::filter(reviews, during_status == "reported"), "during")
  feelings |>
    dplyr::inner_join(dplyr::select(vad, term, valence), by = c("label" = "term")) |>
    dplyr::group_by(date) |>
    dplyr::summarise(valence = mean(valence), n_feelings = dplyr::n(), .groups = "drop") |>
    dplyr::inner_join(dplyr::select(journal, date, stress, sleep_hours), by = "date") |>
    dplyr::mutate(negative = as.integer(valence < 0)) |>
    dplyr::select(date, stress, sleep_hours, valence, n_feelings, negative)
}

#' Logistic regression: negative dream ~ stress + sleep hours.
#'
#' @param data Output of `dream_analysis_data()`.
#' @param min_nights Minimum number of nights needed to fit the model.
#' @return A `glm` object. Errors with a readable message when there are too
#'   few nights or no variation to learn from.
fit_negative_dream_model <- function(data, min_nights = 20) {
  if (nrow(data) < min_nights) {
    stop(sprintf("Only %d night%s with feelings in this period; at least %d are needed to look for patterns.",
                 nrow(data), if (nrow(data) == 1) "" else "s", min_nights), call. = FALSE)
  }
  if (length(unique(data$negative)) < 2) {
    stop("All dreams in this period were equally pleasant or unpleasant, so there is nothing to compare yet.",
         call. = FALSE)
  }
  if (length(unique(data$stress)) < 2 || length(unique(data$sleep_hours)) < 2) {
    stop("Stress or sleep did not vary in this period, so their effect cannot be estimated.",
         call. = FALSE)
  }
  stats::glm(negative ~ stress + sleep_hours, family = stats::binomial(), data = data)
}

#' Odds ratios with 95% confidence intervals and p-values.
#'
#' @param model Output of `fit_negative_dream_model()`.
#' @return Tibble with `term`, `odds_ratio`, `ci_low`, `ci_high`, `p_value`
#'   for stress and sleep hours (Wald intervals).
negative_dream_results <- function(model) {
  estimates <- summary(model)$coefficients
  ci <- stats::confint.default(model)
  terms <- c("stress", "sleep_hours")
  tibble::tibble(
    term = terms,
    odds_ratio = exp(estimates[terms, "Estimate"]),
    ci_low = exp(ci[terms, 1]),
    ci_high = exp(ci[terms, 2]),
    p_value = estimates[terms, "Pr(>|z|)"]
  )
}

#' Predicted chance of a negative dream across stress levels.
#'
#' One line (with 95% confidence band) per sleep duration.
#'
#' @param model Output of `fit_negative_dream_model()`.
#' @param sleep_levels Hours of sleep to draw a line for.
#' @param theme Colour theme name (see `dream_themes`).
#' @return A ggplot.
plot_negative_dream_model <- function(model, sleep_levels = c(6, 8), theme = "lavender") {
  t <- get_dream_theme(theme)
  grid <- tidyr::expand_grid(stress = seq(1, 10, by = 0.1), sleep_hours = sleep_levels)
  link <- stats::predict(model, newdata = grid, type = "link", se.fit = TRUE)
  inverse <- stats::family(model)$linkinv
  grid <- grid |>
    dplyr::mutate(fit = inverse(link$fit),
                  low = inverse(link$fit - 1.96 * link$se.fit),
                  high = inverse(link$fit + 1.96 * link$se.fit),
                  sleep = factor(paste(sleep_hours, "h sleep"),
                                 levels = paste(sleep_levels, "h sleep")))
  colours <- stats::setNames(c(t$charts[2], t$accent[1], t$charts[3])[seq_along(sleep_levels)],
                             levels(grid$sleep))

  ggplot2::ggplot(grid, ggplot2::aes(stress, fit)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high, fill = sleep), alpha = 0.18) +
    ggplot2::geom_line(ggplot2::aes(colour = sleep), linewidth = 1.4) +
    ggplot2::scale_colour_manual(values = colours, name = NULL) +
    ggplot2::scale_fill_manual(values = colours, name = NULL) +
    ggplot2::scale_x_continuous(breaks = c(1, 4, 7, 10)) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                                breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    ggplot2::labs(x = "stress the day before (1\u201310)", y = "chance of a negative dream",
                  title = "Stress, sleep and negative dreams",
                  subtitle = "Lines = model prediction, shaded band = 95% confidence interval") +
    dreamy_plot_theme()
}

# "p < .001" style p-values.
format_p <- function(p) {
  if (p < 0.001) "p < .001" else sprintf("p = %s", sub("^0", "", sprintf("%.3f", p)))
}

#' Plain-language summary of the model.
#'
#' @param model Output of `fit_negative_dream_model()`.
#' @param alpha Significance level.
#' @return Character vector with one sentence per predictor.
describe_negative_dream_model <- function(model, alpha = 0.05) {
  results <- negative_dream_results(model)
  sentence <- function(term, name, what, unit) {
    row <- results[results$term == term, ]
    stats <- sprintf("(odds ratio %.2f, 95%% CI %.2f\u2013%.2f, %s)", row$odds_ratio,
                     row$ci_low, row$ci_high, format_p(row$p_value))
    if (row$p_value >= alpha) {
      return(sprintf("%s showed no clear link with negative dreams %s.", name, stats))
    }
    sprintf("%s went with %s negative dreams: each %s multiplied the odds of a negative dream by %.2f %s.",
            what, if (row$odds_ratio > 1) "more" else "fewer", unit, row$odds_ratio, stats)
  }
  c(sentence("stress", "Stress", "More stress", "extra stress point"),
    sentence("sleep_hours", "Hours of sleep", "Longer sleep", "extra hour of sleep"))
}
