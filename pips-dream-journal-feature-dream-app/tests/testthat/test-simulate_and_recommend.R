test_that("simulation is reproducible and within valid ranges", {
  a <- simulate_dream_journal(n_nights = 60, seed = 1)
  b <- simulate_dream_journal(n_nights = 60, seed = 1)
  expect_identical(a, b)
  expect_true(all(a$stress %in% 1:10 & a$sleep_quality %in% 1:10))
  expect_true(all(is_valid_clock(c(a$bedtime, a$wake_time))))
  expect_error(simulate_dream_journal(n_nights = 0), "n_nights")
})

test_that("stress makes negative dreams more likely in the simulation", {
  journal <- simulate_dream_journal(n_nights = 400, seed = 3)
  dreams <- subset(journal, emotions != "")
  dreams$negative <- grepl("fear|anger|sadness|shame|confusion", dreams$emotions)
  fit <- glm(negative ~ stress, family = binomial, data = dreams)
  expect_gt(coef(fit)[["stress"]], 0)
})

test_that("recommendations react to short, irregular sleep", {
  nights <- as.Date("2026-01-01") + 0:6
  journal <- bind_rows(lapply(seq_along(nights), function(i) {
    new_entry(nights[i], c("22:00", "02:00")[i %% 2 + 1], "06:00", 4, 8)
  }))
  advice <- sleep_recommendations(journal, lexicons$emotion_meta)
  expect_true(all(c("Sleep a little longer", "Keep a regular bedtime") %in% advice$title))
  expect_equal(sleep_recommendations(journal_template(), lexicons$emotion_meta)$title,
               "Keep logging")
})
