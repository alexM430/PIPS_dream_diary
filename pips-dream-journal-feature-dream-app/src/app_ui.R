# User interface of the dream journal app.

#' Build the app UI.
#'
#' @param lexicons Output of `load_lexicons()`; used for the theme and emotion
#'   choices in the entry form.
#' @param css_paths Stylesheets for the dreamy calendar and Sleep tab.
#' @param splash_seconds Minimum seconds the cloudy loading screen is shown;
#'   0 turns it off.
#' @return A bslib page.
dream_app_ui <- function(lexicons,
                         css_paths = c("src/www/dream_calendar.css",
                                       "src/www/sleep_view.css"),
                         splash_seconds = 3) {
  theme_choices <- setNames(lexicons$theme_meta$theme, lexicons$theme_meta$label)
  emotion_choices <- setNames(lexicons$emotion_meta$emotion,
                              lexicons$emotion_meta$label)

  page_navbar(
    title = "Dream Journal",
    theme = bs_theme(version = 5, primary = "#5B4B8A", bg = "#FBFAFD",
                     fg = "#222222"),
    header = tagList(
      tags$head(
        tags$link(rel = "stylesheet",
                  href = "https://fonts.googleapis.com/css2?family=Quicksand:wght@500;600;700&display=swap"),
        lapply(css_paths, includeCSS)
      ),
      if (splash_seconds > 0) splash_screen(splash_seconds)
    ),
    sidebar = sidebar(
      width = 260,
      dateRangeInput("period", "Period shown in the charts",
                     weekstart = 1, format = "d M yyyy"),
      radioButtons("bin", "Group charts by", inline = TRUE,
                   choices = c(Week = "week", Month = "month")),
      helpText("The calendar and new-entry tabs have their own date pickers.")
    ),

    nav_panel(
      "New entry", icon = icon("pen"),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("How did you sleep?"),
          dateInput("entry_date", "Night of (the evening you went to bed)",
                    weekstart = 1, format = "d M yyyy"),
          layout_columns(
            fill = FALSE,
            textInput("entry_bedtime", "Went to bed", value = "23:30",
                      placeholder = "HH:MM"),
            textInput("entry_wake", "Woke up", value = "07:30",
                      placeholder = "HH:MM")
          ),
          sliderInput("entry_quality", "Sleep quality", 1, 10, 6, step = 1),
          sliderInput("entry_stress", "Stress the day before", 1, 10, 5, step = 1),
          textAreaInput("entry_text", "What did you dream?", rows = 5,
                        placeholder = "Leave empty if you don't remember a dream."),
          actionButton("detect", "Suggest themes & emotions from my text",
                       icon = icon("wand-magic-sparkles"),
                       class = "btn-outline-primary btn-sm"),
          checkboxGroupInput("entry_themes", "Themes", theme_choices, inline = TRUE),
          checkboxGroupInput("entry_emotions", "Emotions", emotion_choices,
                             inline = TRUE),
          actionButton("save", "Save entry", icon = icon("floppy-disk"),
                       class = "btn-primary")
        ),
        card(
          card_header("Latest entries"),
          tableOutput("recent_entries")
        )
      )
    ),

    nav_panel(
      "Calendar", icon = icon("calendar-days"),
      div(
        class = "dream-sky",
        div(
          class = "dream-cal-header",
          div(class = "dream-cal-nav",
              actionButton("cal_prev", "\u2039", class = "dream-arrow",
                           `aria-label` = "Previous"),
              h2(class = "dream-cal-title", textOutput("cal_title", inline = TRUE)),
              actionButton("cal_next", "\u203A", class = "dream-arrow",
                           `aria-label` = "Next")),
          div(class = "dream-cal-controls",
              radioButtons("cal_view", "View", inline = TRUE,
                           choices = c(Day = "day", Week = "week", Month = "month"),
                           selected = "month"),
              selectInput("cal_fill", "Colour days by", width = "190px",
                          c("\U0001F497 Dream mood" = "valence",
                            "\u2B50 Sleep quality" = "sleep_quality",
                            "\u26A1 Stress" = "stress",
                            "\U0001F4A4 Hours slept" = "sleep_hours")))
        ),
        uiOutput("cal_summary"),
        div(
          class = "dream-cal-body",
          uiOutput("cal_grid"),
          div(class = "dream-side",
              uiOutput("cal_night"),
              div(class = "dream-list",
                  h6("All dreams in this view"),
                  uiOutput("cal_entries")))
        )
      )
    ),

    nav_panel(
      "Themes", icon = icon("fire"),
      card(full_screen = TRUE, plotOutput("theme_heatmap", height = "480px")),
      card(full_screen = TRUE,
           card_header("Stress on nights with each theme"),
           plotOutput("theme_stress", height = "400px"))
    ),

    nav_panel(
      "Emotions", icon = icon("face-smile"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          selectInput("affect_colour", "Colour dreams by",
                      c(Stress = "stress", "Sleep quality" = "sleep_quality",
                        "Hours slept" = "sleep_hours"), width = "220px"),
          plotOutput("affect_map", height = "480px")
        ),
        card(full_screen = TRUE, plotOutput("emotion_timeline", height = "540px"))
      )
    ),

    nav_panel(
      "People & objects", icon = icon("users"),
      sliderInput("top_n", "Show top", 3, 20, 8, step = 1, width = "250px"),
      layout_columns(
        col_widths = c(6, 6),
        card(plotOutput("people_plot", height = "400px")),
        card(plotOutput("objects_plot", height = "400px"))
      ),
      helpText("Detected with the word lists in data/lexicons/people.csv and objects.csv.")
    ),

    nav_panel(
      "Sleep", icon = icon("moon"),
      div(
        class = "dream-sky sleep-sky",
        div(class = "sleep-header",
            h2(class = "sleep-title", "Your sleep, softly \u2601\uFE0F"),
            div(class = "dream-pills",
                radioButtons("sleep_window", NULL, inline = TRUE, selected = 14,
                             choices = c("7 nights" = 7, "14 nights" = 14,
                                         "30 nights" = 30, "All" = 0)))),
        div(class = "sleep-overview",
            uiOutput("sleep_ring"),
            uiOutput("sleep_bubbles")),
        div(class = "sleep-charts",
            div(class = "sleep-card", plotOutput("sleep_hours_plot", height = "330px")),
            div(class = "sleep-card", plotOutput("sleep_windows_plot", height = "330px"))),
        div(class = "sleep-bottom",
            div(class = "sleep-card",
                h3(class = "sleep-card-title", "\U0001F4AD Little sleep tips"),
                uiOutput("recommendations"),
                p(class = "sleep-note",
                  "General sleep-hygiene advice, not medical advice.")),
            div(class = "sleep-card sleep-share",
                h3(class = "sleep-card-title", "\U0001F4F8 Share your dream stats"),
                imageOutput("story_preview", height = "auto"),
                div(class = "sleep-share-buttons",
                    tags$button(id = "share_story", type = "button",
                                class = "btn dream-btn", onclick = "shareDreamStory()",
                                "\u2728 Share to Instagram"),
                    downloadButton("story_png", "Download picture",
                                   class = "dream-btn-outline")),
                p(class = "sleep-note",
                  "On your phone, Share opens the share menu: pick Instagram, then Story.",
                  "On a computer the picture downloads, so you can post it from the",
                  "Instagram app.")))
      ),
      tags$script(HTML(share_story_js))
    ),

    nav_panel(
      "Search", icon = icon("magnifying-glass"),
      textInput("search", NULL, placeholder = "Search your dreams, e.g. 'mother' or 'falling'",
                width = "100%"),
      uiOutput("search_results")
    )
  )
}
