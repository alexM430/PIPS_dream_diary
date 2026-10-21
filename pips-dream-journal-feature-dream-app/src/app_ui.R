# User interface of the dream journal app.

#' Build the app UI.
#'
#' @param lexicons Output of `load_lexicons()`; used for the theme and emotion
#'   choices in the entry form.
#' @return A bslib page.
dream_app_ui <- function(lexicons) {
  theme_choices <- setNames(lexicons$theme_meta$theme, lexicons$theme_meta$label)
  emotion_choices <- setNames(lexicons$emotion_meta$emotion,
                              lexicons$emotion_meta$label)

  page_navbar(
    title = "Dream Journal",
    theme = bs_theme(version = 5, primary = "#5B4B8A", bg = "#FBFAFD",
                     fg = "#222222"),
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
      layout_columns(
        col_widths = c(4, 4, 4), fill = FALSE,
        radioButtons("cal_view", "View", inline = TRUE,
                     choices = c(Day = "day", Week = "week", Month = "month"),
                     selected = "month"),
        dateInput("cal_date", "Date", weekstart = 1, format = "d M yyyy"),
        selectInput("cal_fill", "Colour days by",
                    c("Dream valence" = "valence", "Sleep quality" = "sleep_quality",
                      "Stress" = "stress", "Hours slept" = "sleep_hours"))
      ),
      uiOutput("cal_summary"),
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE, plotOutput("cal_plot", height = "420px")),
        card(card_header("Dreams in this period"), uiOutput("cal_entries"),
             max_height = "480px")
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
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE, plotOutput("sleep_trends", height = "520px")),
        card(
          card_header("Recommendations"),
          sliderInput("rec_window", "Based on the last ... nights", 7, 60, 14),
          uiOutput("recommendations"),
          tags$small(class = "text-muted",
                     "General sleep-hygiene advice, not medical advice.")
        )
      )
    ),

    nav_panel(
      "Search", icon = icon("magnifying-glass"),
      textInput("search", NULL, placeholder = "Search your dreams, e.g. 'mother' or 'falling'",
                width = "100%"),
      uiOutput("search_results")
    )
  )
}
