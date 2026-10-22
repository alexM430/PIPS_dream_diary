# Colour themes for the whole app. Each theme sets the Bootstrap colours (used
# by Shiny's inputs), the dreamy CSS variables (sky, cards, accents) and the
# colours of the charts. The "App colour theme" menu switches between them.

dream_themes <- list(
  lavender = list(
    label = "Lavender (original)",
    primary = "#7C5CC4", bg = "#FBFAFD", fg = "#4A3B5C",
    sky = c("#EDE4FF", "#FCE7F3", "#FFF1E6"),
    accent = c("#8B5CF6", "#EC4899"),
    soft = c("#C9A7EB", "#F7A8C8"),
    ink = "#4A3B5C", muted = "#9A8AAE",
    charts = c("#A78BFA", "#F472B6", "#60A5FA", "#FBBF24", "#34D399",
               "#FB7185", "#C084FC", "#2DD4BF", "#FDBA74", "#93C5FD")
  ),
  ocean = list(
    label = "Ocean blue",
    primary = "#0369A1", bg = "#F5FBFF", fg = "#1E3A5F",
    sky = c("#E0F2FE", "#E0E7FF", "#ECFEFF"),
    accent = c("#0284C7", "#6366F1"),
    soft = c("#93C5FD", "#A5B4FC"),
    ink = "#1E3A5F", muted = "#7C93AE",
    charts = c("#38BDF8", "#818CF8", "#2DD4BF", "#F472B6", "#FBBF24",
               "#60A5FA", "#A78BFA", "#34D399", "#FB923C", "#67E8F9")
  ),
  forest = list(
    label = "Forest green",
    primary = "#15803D", bg = "#F7FDF8", fg = "#1F3B2D",
    sky = c("#DCFCE7", "#ECFCCB", "#FEF9C3"),
    accent = c("#16A34A", "#0D9488"),
    soft = c("#86EFAC", "#99F6E4"),
    ink = "#1F3B2D", muted = "#7F9A88",
    charts = c("#34D399", "#FBBF24", "#60A5FA", "#F472B6", "#A3E635",
               "#2DD4BF", "#FB923C", "#A78BFA", "#F87171", "#86EFAC")
  )
)

#' A theme by name, falling back to lavender for unknown names.
#'
#' @param name Theme name, e.g. "ocean".
#' @return A theme list (see `dream_themes`).
get_dream_theme <- function(name) {
  if (is.null(name) || !name %in% names(dream_themes)) name <- "lavender"
  dream_themes[[name]]
}

#' `n` chart colours from a theme, repeating its palette when needed.
#'
#' @param n Number of colours.
#' @param theme A theme list, or a theme name.
#' @return Character vector of hex colours.
dream_chart_colours <- function(n, theme = "lavender") {
  if (is.character(theme)) theme <- get_dream_theme(theme)
  rep_len(theme$charts, n)
}

#' CSS variables for every theme, switched by `data-dream-theme` on <html>.
#'
#' @return A single CSS string.
dream_theme_css <- function() {
  blocks <- vapply(names(dream_themes), function(name) {
    t <- dream_themes[[name]]
    selector <- if (name == "lavender") {
      ":root, :root[data-dream-theme=\"lavender\"]"
    } else {
      sprintf(":root[data-dream-theme=\"%s\"]", name)
    }
    sprintf(paste(
      "%s {",
      "  --dream-sky-1: %s; --dream-sky-2: %s; --dream-sky-3: %s;",
      "  --dream-accent-1: %s; --dream-accent-2: %s;",
      "  --dream-lilac: %s; --dream-pink: %s;",
      "  --dream-ink: %s; --dream-muted: %s;",
      "}", sep = "\n"),
      selector, t$sky[1], t$sky[2], t$sky[3], t$accent[1], t$accent[2],
      t$soft[1], t$soft[2], t$ink, t$muted)
  }, character(1))
  paste(blocks, collapse = "\n")
}

# Sets the theme attribute on <html> as soon as the menu changes, so the
# whole page recolours instantly; Shiny's server updates Bootstrap colours.
dream_theme_js <- "
(function () {
  function apply(name) {
    document.documentElement.setAttribute('data-dream-theme', name || 'lavender');
  }
  apply('lavender');
  $(document).on('change', '#app_colour_theme', function () { apply(this.value); });
  $(document).on('shiny:inputchanged', function (event) {
    if (event.name === 'app_colour_theme') apply(event.value);
  });
})();
"
