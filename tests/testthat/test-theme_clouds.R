test_that("periods count back from the latest logged night", {
  dates <- as.Date("2026-01-01") + 0:179  # last night: 2026-06-29
  expect_equal(theme_period_range(dates, "7"), as.Date(c("2026-06-23", "2026-06-29")))
  expect_equal(theme_period_range(dates, "30"), as.Date(c("2026-05-31", "2026-06-29")))
  expect_equal(theme_period_range(dates, "all"), range(dates))
  expect_equal(theme_period_range(dates, "custom", as.Date(c("2026-03-10", "2026-03-01"))),
               as.Date(c("2026-03-01", "2026-03-10")))
  expect_equal(theme_period_range(dates, "custom", NULL), range(dates))
  expect_error(theme_period_range(dates, "decade"), "Unknown period")
})

test_that("theme counts are per night, most frequent first, and can be limited", {
  r <- bind_rows(review_row("2026-01-01", c("flying", "water")),
                 review_row("2026-01-02", "flying"),
                 review_row("2026-01-03", "school"))
  counts <- theme_counts(r)
  expect_equal(counts$label, c("flying", "school", "water"))
  expect_equal(counts$nights, c(2L, 1L, 1L))
  expect_equal(nrow(theme_counts(r, top = "1")), 1)
})

test_that("bigger themes get bigger clouds", {
  counts <- tibble(label = c("flying", "water"), nights = c(9L, 1L))
  html <- as.character(theme_clouds(counts, n_logged = 10, theme = "ocean", selected = "water"))
  sizes <- as.numeric(regmatches(html, gregexpr("(?<=--cloud-size:)[0-9]+", html, perl = TRUE))[[1]])
  labels <- regmatches(html, gregexpr('(?<=theme-cloud-label">)[a-z]+', html, perl = TRUE))[[1]]
  expect_gt(sizes[labels == "flying"], sizes[labels == "water"])
  expect_equal(max(sizes), 260)
  expect_match(html, "flying: 9 nights \\(90% of nights\\)")
  expect_match(html, "theme-cloud is-selected")
  expect_match(as.character(theme_clouds(counts[0, ], 0)), "No dream themes")
})
