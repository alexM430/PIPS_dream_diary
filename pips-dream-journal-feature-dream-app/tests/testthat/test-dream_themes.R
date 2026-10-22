test_that("every theme defines the colours the app needs", {
  needed <- c("label", "primary", "bg", "fg", "sky", "accent", "soft", "ink", "muted", "charts")
  for (theme in dream_themes) {
    expect_true(all(needed %in% names(theme)))
    expect_length(theme$sky, 3)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", unlist(theme[needed[-1]]))))
  }
})

test_that("theme CSS has a block per theme and unknown themes fall back", {
  css <- dream_theme_css()
  for (name in names(dream_themes)) expect_match(css, sprintf('data-dream-theme="%s"', name))
  expect_identical(get_dream_theme("rainbow"), dream_themes$lavender)
  expect_length(dream_chart_colours(13, "ocean"), 13)
})

test_that("theme-coloured charts build, also with no data", {
  counts <- tibble(label = c("happy", "happy", "scared", "scared"),
                   stage = c("during", "after", "during", "after"), nights = c(3L, 1L, 0L, 2L))
  expect_no_error(ggplot2::ggplot_build(plot_emotion_butterfly(counts, theme = "forest")))
  expect_s3_class(plot_emotion_butterfly(counts[0, ]), "ggplot")
  bars <- tibble(label = c("mother", "friend"), nights = c(4L, 2L))
  expect_no_error(ggplot2::ggplot_build(review_frequency_plot(bars, "People", theme = "ocean")))
})

test_that("overview bubbles summarise reviews", {
  r <- bind_rows(review_row("2026-01-01", "falling"), review_row("2026-01-02", c("falling", "school")))
  r$during <- c("scared", "scared;happy")
  r$after <- c("scared", "relieved")
  r$during_status <- r$after_status <- "reported"
  html <- as.character(emotion_overview(r))
  expect_match(html, "2 / 2")
  expect_match(html, "scared \\(2\\)")
  expect_match(html, "50%")  # night 1 kept "scared" after waking, night 2 did not
  j <- bind_rows(new_entry("2026-01-01", "23:00", "07:00", 6, 8), new_entry("2026-01-02", "23:00", "07:00", 6, 2))
  expect_match(as.character(theme_overview(j, r, min_nights = 1)), "falling \\(5.0\\)")
})
