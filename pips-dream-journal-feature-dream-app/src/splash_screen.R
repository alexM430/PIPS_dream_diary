# Cloudy loading screen shown while the app starts. The clouds part to the
# sides once the app is ready and `min_seconds` have passed. Clicking skips it.
# Styling lives in src/www/splash_screen.css.

# Cloud layout: position (% of screen), width (px), layer (1 = far, 3 = near)
# and drift delay (s). Clouds left of centre leave to the left, the rest right.
splash_clouds <- tribble(
  ~x, ~y, ~size, ~layer, ~delay,
   8,  12,  260,  1,  0.0,
  30,   6,  220,  1,  1.2,
  62,   9,  280,  1,  0.6,
  88,  14,  240,  1,  1.8,
  -2,  38,  380,  2,  0.4,
  24,  30,  300,  2,  2.0,
  76,  32,  340,  2,  1.0,
 102,  44,  360,  2,  0.2,
  12,  62,  420,  3,  1.4,
  38,  78,  380,  3,  0.8,
  66,  70,  440,  3,  0.0,
  94,  80,  400,  3,  1.6,
  -6,  92,  480,  3,  0.3,
  50,  98,  520,  3,  1.1,
 106,  96,  460,  3,  0.7
)

#' Full-screen cloudy loading screen.
#'
#' @param min_seconds Minimum number of seconds the clouds stay, even if the
#'   app is ready sooner.
#' @param css_path Stylesheet for the loading screen.
#' @return An htmltools tag list to place in the page.
splash_screen <- function(min_seconds = 3,
                          css_path = "src/www/splash_screen.css") {
  if (!is.numeric(min_seconds) || length(min_seconds) != 1 || min_seconds < 0) {
    stop("min_seconds must be a single number of 0 or more.", call. = FALSE)
  }

  clouds <- lapply(seq_len(nrow(splash_clouds)), function(i) {
    cloud <- splash_clouds[i, ]
    side <- if (cloud$x < 50) "is-left" else "is-right"
    div(
      class = paste("splash-cloud", side, paste0("layer-", cloud$layer)),
      style = sprintf("left:%s%%; top:%s%%; --size:%spx; --delay:%ss;",
                      cloud$x, cloud$y, cloud$size, cloud$delay),
      div(class = "splash-puff")
    )
  })

  tagList(
    includeCSS(css_path),
    div(
      id = "dream-splash",
      role = "status",
      `aria-label` = "Loading Dream Journal",
      div(class = "splash-stars"),
      clouds,
      div(class = "splash-center",
          div(class = "splash-moon", "\U0001F319"),
          h1(class = "splash-title", "Dream Journal"),
          p(class = "splash-subtitle", "drifting into your dreams",
            span(class = "splash-dots", span("."), span("."), span("."))),
          p(class = "splash-skip", "tap to skip"))
    ),
    tags$script(HTML(sprintf(splash_script, round(min_seconds * 1000))))
  )
}

# Clears the clouds once Shiny is idle and the minimum time has passed. A
# fallback clears them anyway after 10 extra seconds so the app never stays
# hidden.
splash_script <- "
document.addEventListener('DOMContentLoaded', function () {
  var splash = document.getElementById('dream-splash');
  if (!splash) return;
  var minMs = %d, started = Date.now(), cleared = false;
  function clear() {
    if (cleared) return;
    cleared = true;
    splash.classList.add('is-clearing');
    setTimeout(function () { splash.remove(); }, 1800);
  }
  function clearWhenReady() {
    setTimeout(clear, Math.max(0, minMs - (Date.now() - started)));
  }
  $(document).one('shiny:idle', clearWhenReady);
  setTimeout(clear, minMs + 10000);
  splash.addEventListener('click', clear);
});
"
