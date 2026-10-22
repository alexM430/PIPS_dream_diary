test_that("word lists find themes, emotions, people and objects", {
  text <- "I was falling and my Mom couldn't reach my phone. I felt scared."
  expect_equal(detect_categories(text, lexicons$themes), "falling")
  expect_equal(detect_categories(text, lexicons$emotions), "fear")
  expect_equal(detect_categories(text, lexicons$people), "mother")
  expect_equal(detect_categories(text, lexicons$objects), "phone")
  expect_equal(detect_categories("", lexicons$themes), character())
})

test_that("only whole words match", {
  expect_equal(detect_categories("a carpet and a scarf", lexicons$objects), character())
})

test_that("affect scores average the emotion coordinates", {
  journal <- tibble(date = as.Date("2026-01-01") + 0:1, emotions = c("joy;calm", ""))
  scored <- add_affect_scores(journal, lexicons$emotion_meta)
  expect_equal(scored$valence[1], mean(c(0.8, 0.6)))
  expect_true(is.na(scored$valence[2]))
})

test_that("every word used in simulated dreams is in the word lists", {
  expect_true(all(unlist(emotion_words) %in% lexicons$emotions$word))
  expect_true(all(names(dream_people) %in% lexicons$people$category))
  expect_true(all(names(dream_objects) %in% lexicons$objects$category))
})
