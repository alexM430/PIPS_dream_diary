test_that("last_nights keeps the right nights", {
  journal <- simulate_dream_journal(n_nights = 30)
  expect_equal(nrow(last_nights(journal, 7)), 7)
  expect_equal(nrow(last_nights(journal, 0)), 30)
  expect_equal(nrow(last_nights(journal_template(), 7)), 0)
})

test_that("sleep_summary computes the overview numbers", {
  nights <- bind_rows(
    new_entry("2026-01-01", "23:00", "07:00", 8, 3, "I felt happy.", "flying", "joy"),
    new_entry("2026-01-02", "01:00", "06:00", 4, 7, "", character(), character())
  )
  summary <- sleep_summary(nights)
  expect_equal(summary$avg_hours, 6.5)
  expect_equal(summary$avg_bedtime, "00:00")
  expect_equal(summary$zone_nights, 1)
  expect_equal(summary$dreams_recalled, 1)
  expect_equal(summary$top_emotion, "joy")
  expect_equal(summary$best_date, as.Date("2026-01-01"))
  expect_null(sleep_summary(journal_template()))
})

test_that("every recommendation has an emoji", {
  advice <- sleep_recommendations(simulate_dream_journal(n_nights = 30),
                                  lexicons$emotion_meta)
  expect_true(all(nzchar(advice$emoji)))
})

test_that("the story card is saved as a 1080 x 1920 picture", {
  path <- tempfile(fileext = ".png")
  save_story_card(path, simulate_dream_journal(n_nights = 14), lexicons)
  header <- readBin(path, "raw", 24)
  to_int <- function(bytes) sum(as.integer(bytes) * 256^(3:0))
  expect_equal(c(to_int(header[17:20]), to_int(header[21:24])), c(1080, 1920))
  expect_error(plot_story_card(journal_template(), lexicons), "no nights")
})
