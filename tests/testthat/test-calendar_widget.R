test_that("pastel colours cover the palette and keep missing days empty", {
  colours <- pastel_colour(c(-1, 1, NA, 5), "valence")
  expect_equal(colours[1:2], toupper(c("#A9C6F5", "#FFB0D5")))
  expect_true(is.na(colours[3]))
  expect_equal(colours[4], colours[2])  # values beyond the limits are clamped
  expect_error(pastel_colour(1, "mood"), "Unknown fill_by")
})

test_that("calendar views cover the right days", {
  day <- as.Date("2026-10-21")  # a Wednesday
  expect_equal(calendar_range(day, "week"), as.Date(c("2026-10-19", "2026-10-25")))
  expect_equal(calendar_range(day, "month"), as.Date(c("2026-10-01", "2026-10-31")))
  expect_equal(shift_calendar_date(as.Date("2026-01-31"), "month", 1), as.Date("2026-02-28"))
  expect_equal(shift_calendar_date(day, "week", -1), as.Date("2026-10-14"))
})

test_that("the calendar renders a tile per day of whole weeks", {
  journal <- add_affect_scores(simulate_dream_journal(n_nights = 40), lexicons$emotion_meta)
  html <- as.character(dream_calendar(journal, "2026-01-01", "2026-01-31",
                                      "valence", as.Date("2026-01-10"), lexicons))
  # January 2026 starts on a Thursday, so the grid runs 29 Dec - 1 Feb (5 weeks).
  expect_equal(lengths(regmatches(html, gregexpr('class="dream-day[ "]', html))), 35)
  expect_match(html, "is-selected")
})
