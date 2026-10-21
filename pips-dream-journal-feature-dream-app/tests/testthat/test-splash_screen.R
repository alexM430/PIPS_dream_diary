test_that("the loading screen has its clouds and waits the minimum time", {
  css <- file.path(project_root, "src", "www", "splash_screen.css")
  html <- as.character(splash_screen(min_seconds = 2.5, css_path = css))
  expect_equal(lengths(regmatches(html, gregexpr('class="splash-cloud ', html))),
               nrow(splash_clouds))
  expect_match(html, "var minMs = 2500")
  expect_error(splash_screen(min_seconds = -1, css_path = css), "min_seconds")
})
