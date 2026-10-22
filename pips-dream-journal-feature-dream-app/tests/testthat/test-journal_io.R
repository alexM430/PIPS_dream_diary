test_that("bedtimes after midnight stay on one continuous scale", {
  expect_equal(clock_to_evening_hours(c("23:30", "00:30")), c(5.5, 6.5))
  expect_equal(evening_hours_to_clock(c(5.5, 6.5)), c("23:30", "00:30"))
  expect_equal(sleep_duration_hours("23:30", "07:00"), 7.5)
})

test_that("invalid entries give readable errors", {
  expect_error(new_entry("2026-01-01", "25:00", "07:00", 5, 5), "Bedtime")
  expect_error(new_entry("2026-01-01", "23:00", "07:00", 11, 5), "Sleep quality")
  expect_error(new_entry("not a date", "23:00", "07:00", 5, 5))
})

test_that("saving a night twice replaces the first entry", {
  first <- new_entry("2026-01-01", "23:00", "07:00", 5, 5, "I fell.", "falling")
  second <- new_entry("2026-01-01", "22:00", "06:00", 8, 2, "I flew.", "flying")
  journal <- upsert_entry(upsert_entry(journal_template(), first), second)
  expect_equal(nrow(journal), 1)
  expect_equal(journal$themes, "flying")
})

test_that("a journal survives a write/read round trip", {
  path <- tempfile(fileext = ".csv")
  journal <- simulate_dream_journal(n_nights = 10)
  # The simulation's provenance attribute is metadata, not diary data; CSV
  # files do not store it.
  attr(journal, "simulation") <- NULL
  write_journal(journal, path)
  expect_equal(read_journal(path), journal, ignore_attr = TRUE)
  expect_equal(nrow(read_journal(tempfile())), 0)
})
