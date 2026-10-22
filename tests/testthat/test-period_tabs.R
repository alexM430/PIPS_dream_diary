# The Dream themes and Your patterns tabs each have their own period picker
# (period_nights() in src/app_server.R). Tests asked for in the review of PR 14.

# The app server, running on copies of the demo data.
period_test_server <- function() {
  dir <- tempfile("app-")
  dir.create(dir)
  file.copy(file.path(project_root, "data", c("reviewed_dream_journal.rds", "dream_journal.csv")), dir)
  dream_app_server(file.path(dir, "dream_journal.csv"),
                   file.path(dir, "reviewed_dream_journal.rds"),
                   file.path(project_root, "data", "lexicons"))
}

test_that("changing the period on one tab does not change the other tab", {
  shiny::testServer(period_test_server(), {
    session$setInputs(demo_mode = TRUE, r_theme_window = "7", r_pattern_window = "all")
    n_all <- nrow(journal())
    expect_equal(nrow(theme_journal()), 7)
    expect_equal(nrow(pattern_journal()), n_all)

    session$setInputs(r_pattern_window = "custom",
                      r_pattern_range = as.Date(c("2026-02-01", "2026-02-14")))
    expect_equal(nrow(pattern_journal()), 14)
    expect_equal(nrow(theme_journal()), 7)  # Dream themes tab unchanged

    session$setInputs(r_theme_window = "365")
    expect_equal(nrow(pattern_journal()), 14)  # Your patterns tab unchanged
  })
})

test_that("a theme shows only its dreams within the chosen period", {
  shiny::testServer(period_test_server(), {
    session$setInputs(demo_mode = TRUE, r_theme_window = "30", r_top = "all")
    theme <- theme_counts(theme_reviews())$label[1]
    session$setInputs(r_support_code = theme)

    in_period <- review_long(theme_reviews(), "codes")
    expected <- sum(in_period$label == theme)
    all_nights <- sum(review_long(reviews(), "codes")$label == theme)
    expect_lt(expected, all_nights)  # the period really narrows it down
    html <- as.character(output$r_support$html)
    expect_equal(lengths(regmatches(html, gregexpr('class="dream-result"', html))), expected)

    # Clicking a cloud does not change the chosen period.
    session$setInputs(r_cloud_click = theme)
    expect_equal(nrow(theme_journal()), 30)
  })
})

test_that("periods without logged nights show messages instead of errors", {
  shiny::testServer(period_test_server(), {
    empty <- as.Date(c("2025-01-01", "2025-01-31"))  # before the demo diary starts
    session$setInputs(demo_mode = TRUE,
                      r_theme_window = "custom", r_theme_range = empty,
                      r_pattern_window = "custom", r_pattern_range = empty,
                      r_support_code = "flying")
    expect_equal(nrow(theme_journal()), 0)
    expect_match(as.character(output$r_theme_clouds$html), "No dream themes")
    expect_match(as.character(output$r_support$html), "No dreams with this theme")
    expect_match(as.character(output$r_pattern_text$html), "at least 20 are needed")
    expect_no_error(output$r_pattern_plot)
    expect_no_error(output$r_theme_overview)
  })
})

test_that("an empty personal diary works on both tabs", {
  shiny::testServer(period_test_server(), {
    session$setInputs(demo_mode = FALSE, r_theme_window = "all", r_pattern_window = "all")
    expect_equal(nrow(journal()), 0)
    expect_match(as.character(output$r_theme_clouds$html), "No dream themes")
    expect_match(as.character(output$r_pattern_text$html), "at least 20 are needed")
    expect_no_error(output$r_pattern_plot)
  })
})
