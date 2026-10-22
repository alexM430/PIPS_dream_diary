# Tests for the reviewed-journal logic (moved from Anna's run_reviewed_dream_tests()).

test_that("labels are normalised without merging synonyms", {
    expect_identical(review_normalise(c(" Happy ", "happy", "VERY  SAD", "worried", "anxious")), 
        c("happy", "very sad", "worried", "anxious"))
    expect_error(review_pack("bad;label"))
})

test_that("legacy emotions are not assigned a stage", {
    j <- new_entry("2026-01-01", "23:00", "07:00", 6, 5, "I saw Paul.", "work", "fear")
    s <- review_initial_state(j)
    expect_equal(s$reviews$during, "")
    expect_equal(s$reviews$after_status, "unanswered")
    expect_equal(s$journal$emotions, "fear")
})

test_that("entity matches respect boundaries and remembered names", {
    d <- tibble::tibble(category = c("car", "friend"), word = c("car", "best friend"))
    expect_identical(review_detect("My best friend stood on a carpet.", d), "friend")
    expect_length(review_detect("best. Friend", d), 0)
    n <- tibble::tibble(alias = "paul", person = "paul smith")
    result <- review_suggest_entities("Paul arrived.", list(people = d, animals = d[0, ], objects = d[0, 
        ]), n)
    expect_identical(result$people, "paul smith")
})

test_that("emotion counts are per night and preserve stage zeros", {
    r <- review_row("2026-01-01")
    r$during <- "happy;happy"
    r$during_status <- "reported"
    x <- review_emotion_counts(r)
    expect_equal(x$nights[x$stage == "during"], 1L)
    expect_equal(x$nights[x$stage == "after"], 0L)
    expect_error(review_validate_emotions("none", "happy"))
    expect_error(review_validate_emotions("reported", character()))
})

test_that("VAD uses complete labels and excludes unmatched terms only from scoring", {
    r <- review_row("2026-01-01")
    r$during <- "happy;very happy"
    r$during_status <- "reported"
    vad <- tibble::tibble(term = "happy", valence = 0.8, arousal = 0.4, dominance = 0.2)
    x <- review_vad_matches(r, vad)
    expect_equal(nrow(x), 2L)
    expect_true(is.na(x$valence[x$label == "very happy"]))
    file <- tempfile(fileext = ".csv")
    on.exit(unlink(file), add = TRUE)
    readr::write_csv(vad, file)
    expect_equal(nrow(review_import_vad(file, c(-1, 1))), 1L)
    vad$dominance <- 5
    readr::write_csv(vad, file)
    expect_error(review_import_vad(file, c(-1, 1)))
})

test_that("snapshot round-trip preserves original journal fields", {
    s <- review_initial_state(new_entry("2026-01-01", "23:00", "07:00", 6, 5))
    file <- tempfile(fileext = ".rds")
    on.exit(unlink(c(file, paste0(file, ".previous"))), add = TRUE)
    review_write_state(s, file)
    expect_equal(readRDS(file), s)
    review_write_state(s, file)
    expect_true(file.exists(paste0(file, ".previous")))
})

test_that("frequency plot keeps common order and zero bars", {
    x <- tibble::tibble(label = c("happy", "sad"), nights = c(2L, 0L))
    p <- review_frequency_plot(x, "Test", order = c("happy", "sad"), limit = 3)
    expect_no_error(ggplot2::ggplot_build(p))
    expect_s3_class(review_frequency_plot(x[0, ], "Empty"), "ggplot")
})
