# A small journal where the dream tone depends on stress.
analysis_fixture <- function(n = 60, seed = 1) {
  set.seed(seed)
  dates <- as.Date("2026-01-01") + seq_len(n) - 1
  stress <- sample(1:10, n, replace = TRUE)
  negative <- stats::rbinom(n, 1, stats::plogis(-3 + 0.6 * stress))
  journal <- bind_rows(lapply(seq_len(n), function(i) {
    new_entry(dates[i], "23:00", sprintf("0%d:00", sample(5:8, 1)), 5, stress[i], "A dream.")
  }))
  reviews <- bind_rows(lapply(dates, review_row))
  reviews$during <- ifelse(negative == 1, "fear", "joy")
  reviews$during_status <- "reported"
  vad <- tibble(term = c("fear", "joy"), valence = c(-0.85, 0.96))
  list(journal = journal, reviews = reviews, vad = vad)
}

test_that("each night with matched feelings becomes one row", {
  x <- analysis_fixture(10)
  x$reviews$during[1] <- "fear;joy"            # mean valence 0.055 -> not negative
  x$reviews$during_status[2] <- "dont_remember" # dropped
  x$reviews$during[3] <- "wobbly"              # not in lexicon -> dropped
  d <- dream_analysis_data(x$journal, x$reviews, x$vad)
  expect_equal(nrow(d), 8)
  expect_equal(d$negative[d$date == as.Date("2026-01-01")], 0L)
  expect_false(any(as.Date(c("2026-01-02", "2026-01-03")) %in% d$date))
  expect_error(dream_analysis_data(x$journal, x$reviews, tibble(word = "joy")), "term")
})

test_that("the model finds a built-in stress effect", {
  x <- analysis_fixture(200)
  model <- fit_negative_dream_model(dream_analysis_data(x$journal, x$reviews, x$vad))
  results <- negative_dream_results(model)
  expect_equal(results$term, c("stress", "sleep_hours"))
  expect_gt(results$odds_ratio[1], 1)
  expect_lt(results$p_value[1], 0.05)
  expect_true(all(results$ci_low < results$odds_ratio & results$odds_ratio < results$ci_high))
  expect_match(describe_negative_dream_model(model)[1], "More stress went with more negative dreams")
  plot <- plot_negative_dream_model(model, theme = "forest")
  expect_no_error(ggplot2::ggplot_build(plot))
  expect_false(any(vapply(plot$layers, \(l) inherits(l$geom, "GeomPoint"), logical(1))))
})

test_that("too little data gives a friendly error instead of a model", {
  x <- analysis_fixture(10)
  d <- dream_analysis_data(x$journal, x$reviews, x$vad)
  expect_error(fit_negative_dream_model(d), "at least 20")
  d <- dream_analysis_data(analysis_fixture(40)$journal, analysis_fixture(40)$reviews, analysis_fixture(40)$vad)
  d$negative <- 1L
  expect_error(fit_negative_dream_model(d), "nothing to compare")
})

test_that("p-values are formatted APA-style", {
  expect_equal(format_p(0.0004), "p < .001")
  expect_equal(format_p(0.0312), "p = .031")
})
