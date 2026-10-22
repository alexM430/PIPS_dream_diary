# User interface of the Dream Journal app. Every tab uses the dreamy look of
# src/www/dream_app.css; colours follow the selected theme (src/dream_themes.R).

# Multi-label input: type a label and press Enter; remove with x.
tags_input <- function(id, label, placeholder = "Type and press Enter") {
  shiny::selectizeInput(id, label, choices = character(), multiple = TRUE,
                        options = list(create = TRUE, persist = FALSE,
                                       plugins = list("remove_button"),
                                       placeholder = placeholder))
}

# How the user answers the emotion question for one stage.
status_input <- function(id) {
  shiny::selectInput(id, NULL, c("Not answered yet" = "unanswered",
                                 "I felt something" = "reported",
                                 "I don't remember" = "dont_remember",
                                 "No emotion noticed" = "none"))
}

# Card title with a numbered step bubble.
step_header <- function(number, title) {
  bslib::card_header(shiny::span(class = "dream-step", number), title)
}

# Big gradient title and subtitle at the top of a tab.
tab_title <- function(title, subtitle) {
  shiny::tagList(shiny::h2(class = "dream-tab-title", title),
                 shiny::p(class = "dream-tab-subtitle", subtitle))
}

# Low/high words under a 1-10 slider.
slider_scale <- function(low, high) {
  shiny::div(class = "dream-scale", shiny::span(low), shiny::span(high))
}

entry_tab <- function() {
  bslib::nav_panel(
    "\u270D\uFE0F New dream",
    tab_title("Tonight's dream", "Write it down while it's still fresh \u2014 everything except the dream text is optional."),
    shiny::div(class = "dream-actions",
               shiny::actionButton("r_load", "\U0001F4D6 Open a saved night"),
               shiny::actionButton("r_clear", "\u2728 Start a new night")),
    shiny::uiOutput("r_edit_notice"),
    bslib::layout_columns(
      col_widths = c(7, 5), fill = FALSE,
      shiny::div(
        bslib::card(fill = FALSE,
          step_header(1, "\U0001F319 Your night"),
          shiny::dateInput("r_date", "Night of (the evening you went to bed)", weekstart = 1),
          bslib::layout_columns(fill = FALSE,
            shiny::textInput("r_bed", "\U0001F6CF\uFE0F Went to bed", "23:30"),
            shiny::textInput("r_wake", "\u2600\uFE0F Woke up", "07:30")),
          shiny::sliderInput("r_quality", "\u2B50 How well did you sleep?", 1, 10, 6, ticks = FALSE),
          slider_scale("\U0001F635 restless", "restful \U0001F31F"),
          shiny::sliderInput("r_stress", "\u26A1 How stressful was your day?", 1, 10, 5, ticks = FALSE),
          slider_scale("\U0001F60C relaxed", "very stressed \U0001F62B")),
        bslib::card(fill = FALSE, class = "dream-diary",
          step_header(2, "\u2601\uFE0F Your dream"),
          shiny::textAreaInput("r_text", NULL, rows = 8, width = "100%",
            placeholder = "Where were you? Who was there? What happened? How did it end?"),
          shiny::div(class = "dream-word-count", shiny::textOutput("r_word_count", inline = TRUE))),
        bslib::card(fill = FALSE,
          step_header(3, "\U0001F496 How did it feel?"),
          shiny::p(class = "dream-hint", "Type any feeling and press Enter \u2014 you can add several."),
          shiny::div(class = "dream-feelings",
            shiny::div(class = "dream-feeling-box",
              shiny::h6("\U0001F4AD While dreaming"),
              status_input("r_during_status"),
              tags_input("r_during", NULL, "e.g. scared, happy")),
            shiny::div(class = "dream-feeling-box",
              shiny::h6("\u2600\uFE0F After waking up"),
              status_input("r_after_status"),
              tags_input("r_after", NULL, "e.g. relieved, confused")))),
        bslib::card(fill = FALSE,
          step_header(4, "\U0001F3F7\uFE0F Dream themes"),
          shiny::p(class = "dream-hint", "Your own labels for what the dream was about, e.g. falling, school, travel."),
          tags_input("r_codes", NULL, "Add a theme and press Enter"),
          shiny::textAreaInput("r_note", "Why do these themes fit? (optional)", rows = 2, width = "100%")),
        shiny::actionButton("r_save", "\u2728 Save this dream", class = "btn-primary dream-save")),
      shiny::div(
        bslib::card(fill = FALSE,
          step_header(5, "\U0001F465 Who and what appeared?"),
          shiny::actionButton("r_detect", "\U0001F50D Find them in my dream text"),
          shiny::p(class = "dream-hint mt-2",
                   "Remove wrong ones with \u00D7 and type any that were missed."),
          tags_input("r_people", "\U0001F9D1 People (including names)"),
          tags_input("r_animals", "\U0001F43E Animals"),
          tags_input("r_objects", "\U0001F511 Objects"),
          shiny::checkboxInput("r_entities_confirmed", "\u2705 These lists are correct", FALSE),
          shiny::p(class = "dream-hint", "Only confirmed lists are counted in the People tab.")),
        bslib::card(fill = FALSE,
          bslib::card_header("\U0001F4D3 Your latest nights"),
          shiny::tableOutput("r_recent")))
    )
  )
}

themes_tab <- function() {
  bslib::nav_panel(
    "\U0001F52E Dream themes",
    tab_title("Your dream themes", "What your dreams are about, and how that changes over time."),
    shiny::uiOutput("r_theme_overview"),
    bslib::card(fill = FALSE, full_screen = TRUE, shiny::plotOutput("r_heatmap", height = "430px")),
    bslib::layout_columns(
      col_widths = c(7, 5), fill = FALSE,
      bslib::card(fill = FALSE, full_screen = TRUE, shiny::plotOutput("r_code_stress", height = "430px")),
      bslib::card(fill = FALSE,
        bslib::card_header("\U0001F4D6 Dreams with this theme"),
        shiny::selectInput("r_support_code", NULL, choices = character(), width = "100%"),
        shiny::div(style = "max-height: 360px; overflow-y: auto;", shiny::uiOutput("r_support"))))
  )
}

emotions_tab <- function() {
  bslib::nav_panel(
    "\U0001F496 Emotions",
    tab_title("Your dream feelings", "How you felt inside your dreams, and how you felt after waking up."),
    shiny::uiOutput("r_emotion_overview"),
    bslib::card(fill = FALSE, full_screen = TRUE,
      shiny::plotOutput("r_butterfly", height = "460px"),
      shiny::div(class = "dream-hint",
                 shiny::p(shiny::textOutput("r_during_summary", inline = TRUE)),
                 shiny::p(shiny::textOutput("r_after_summary", inline = TRUE)))),
    bslib::card(fill = FALSE,
      bslib::card_header("\U0001F30C Map of your emotions (optional)"),
      shiny::p(class = "dream-hint",
               "Places each feeling by how pleasant, how intense and how in-control it is (NRC VAD lexicon)."),
      shiny::radioButtons("r_vad_stage", NULL,
                          c("\U0001F4AD While dreaming" = "during", "\u2600\uFE0F After waking" = "after"),
                          inline = TRUE),
      plotly::plotlyOutput("r_vad", height = "450px"),
      shiny::p(class = "dream-hint", shiny::textOutput("r_coverage", inline = TRUE)),
      shiny::tableOutput("r_unmatched"))
  )
}

people_tab <- function() {
  bslib::nav_panel(
    "\U0001F465 People & things",
    tab_title("Who and what shows up", "People, animals and objects from your confirmed dream entries."),
    shiny::uiOutput("r_entity_overview"),
    bslib::layout_columns(
      col_widths = c(4, 4, 4), fill = FALSE,
      bslib::card(fill = FALSE, shiny::plotOutput("r_people_plot", height = "420px")),
      bslib::card(fill = FALSE, shiny::plotOutput("r_animals_plot", height = "420px")),
      bslib::card(fill = FALSE, shiny::plotOutput("r_objects_plot", height = "420px"))),
    shiny::p(class = "dream-hint", shiny::textOutput("r_entity_summary", inline = TRUE))
  )
}

calendar_tab <- function() {
  bslib::nav_panel(
    "\U0001F4C5 Calendar",
    shiny::div(
      class = "dream-sky",
      shiny::div(class = "dream-cal-header",
        shiny::div(class = "dream-cal-nav",
          shiny::actionButton("r_cal_prev", "\u2039", class = "dream-arrow", `aria-label` = "Previous"),
          shiny::h2(class = "dream-cal-title", shiny::textOutput("r_cal_title", inline = TRUE)),
          shiny::actionButton("r_cal_next", "\u203A", class = "dream-arrow", `aria-label` = "Next")),
        shiny::div(class = "dream-cal-controls",
          shiny::div(class = "dream-pills",
            shiny::radioButtons("r_cal_view", NULL, c(Day = "day", Week = "week", Month = "month"),
                                selected = "month", inline = TRUE)),
          shiny::dateInput("r_cal_date", NULL, weekstart = 1, width = "140px"),
          shiny::selectInput("r_cal_fill", NULL, width = "190px",
                             c("\u2B50 Sleep quality" = "sleep_quality", "\u26A1 Stress" = "stress",
                               "\U0001F4A4 Hours slept" = "sleep_hours")))),
      shiny::uiOutput("r_cal_summary"),
      shiny::div(class = "dream-cal-body",
                 shiny::uiOutput("r_cal_grid"),
                 shiny::div(class = "dream-side", shiny::uiOutput("r_cal_night"))))
  )
}

sleep_tab <- function() {
  bslib::nav_panel(
    "\U0001F319 Sleep",
    shiny::div(
      class = "dream-sky sleep-sky",
      shiny::div(class = "sleep-header",
        shiny::h2(class = "sleep-title", "Your sleep, softly \u2601\uFE0F"),
        shiny::div(class = "dream-pills",
          shiny::radioButtons("r_sleep_window", NULL, inline = TRUE, selected = 14,
                              c("7 nights" = 7, "14 nights" = 14, "30 nights" = 30, "All" = 0)))),
      shiny::div(class = "sleep-overview",
                 shiny::uiOutput("r_sleep_ring"), shiny::uiOutput("r_sleep_bubbles")),
      shiny::div(class = "sleep-charts",
        shiny::div(class = "sleep-card", shiny::plotOutput("r_sleep_hours", height = "330px")),
        shiny::div(class = "sleep-card", shiny::plotOutput("r_sleep_windows", height = "330px"))),
      shiny::div(class = "sleep-bottom",
        shiny::div(class = "sleep-card",
          shiny::h3(class = "sleep-card-title", "\U0001F4AD Little sleep tips"),
          shiny::uiOutput("r_recommendations"),
          shiny::p(class = "sleep-note", "General sleep-hygiene advice, not medical advice.")),
        shiny::div(class = "sleep-card sleep-share",
          shiny::h3(class = "sleep-card-title", "\U0001F4F8 Share your dream stats"),
          shiny::imageOutput("r_story_preview", height = "auto"),
          shiny::div(class = "sleep-share-buttons",
            shiny::tags$button(id = "share_story", type = "button", class = "btn dream-btn",
                               onclick = "shareDreamStory()", "\u2728 Share to Instagram"),
            shiny::downloadButton("r_story", "Download picture", class = "dream-btn-outline")),
          shiny::p(class = "sleep-note",
                   "On your phone, Share opens the share menu: pick Instagram, then Story.",
                   "On a computer the picture downloads, so you can post it from the Instagram app.")))),
    shiny::tags$script(shiny::HTML(share_story_js))
  )
}

search_tab <- function() {
  bslib::nav_panel(
    "\U0001F50E Search",
    tab_title("Search your dreams", "Find dream text, themes, feelings, people, animals or objects."),
    shiny::textInput("r_search", NULL, width = "100%",
                     placeholder = "e.g. falling, mother, scared, dog"),
    shiny::uiOutput("r_search_results")
  )
}

settings_tab <- function() {
  bslib::nav_panel(
    "\u2699\uFE0F Settings",
    tab_title("Settings & export", "Tidy up your labels, teach the app names, and back up your journal."),
    bslib::layout_columns(
      col_widths = c(6, 6), fill = FALSE,
      bslib::card(fill = FALSE,
        bslib::card_header("\U0001F4DD Theme definitions"),
        shiny::selectizeInput("r_definition_code", "Theme", choices = character(),
                              options = list(create = TRUE)),
        shiny::textAreaInput("r_definition", "What counts as this theme?", rows = 3, width = "100%"),
        shiny::actionButton("r_define", "Save definition"),
        shiny::tableOutput("r_definitions")),
      bslib::card(fill = FALSE,
        bslib::card_header("\U0001F500 Rename or merge labels"),
        shiny::selectInput("r_rename_kind", "Label type",
                           c("Dream themes" = "codes", "Emotions (both stages)" = "emotion")),
        shiny::selectInput("r_rename_from", "Existing label", choices = character()),
        shiny::textInput("r_rename_to", "New label"),
        shiny::checkboxInput("r_rename_confirm", "Apply this change to all saved entries", FALSE),
        shiny::actionButton("r_rename", "Rename / merge"),
        shiny::p(class = "dream-hint mt-2",
                 "Merging combines counts without counting a night twice.")),
      bslib::card(fill = FALSE,
        bslib::card_header("\U0001F9D1 Remember names"),
        shiny::textInput("r_name_alias", "Name or nickname in your dreams, e.g. Paul"),
        shiny::textInput("r_name_person", "Person it refers to, e.g. Paul Smith"),
        shiny::actionButton("r_remember", "Remember name"),
        shiny::tableOutput("r_names"),
        shiny::selectInput("r_forget_alias", "Name to forget", choices = character()),
        shiny::actionButton("r_forget", "Forget name"),
        shiny::p(class = "dream-hint mt-2",
                 "Names only affect future suggestions; old entries are not changed.")),
      bslib::card(fill = FALSE,
        bslib::card_header("\U0001F30C Import the NRC VAD emotion lexicon"),
        shiny::tags$a(href = "https://saifmohammad.com/WebPages/nrc-vad.html", target = "_blank",
                      "Official NRC VAD download and usage terms"),
        shiny::p(class = "dream-hint mt-2",
                 "Prepare a UTF-8 CSV with columns term, valence, arousal, dominance (mean scores)."),
        shiny::fileInput("r_vad_file", "Prepared VAD CSV", accept = ".csv"),
        shiny::selectInput("r_vad_scale", "Score scale (check your file)",
                           stats::setNames(c("signed", "unit"), c("\u22121 to +1", "0 to 1"))),
        shiny::textInput("r_vad_source", "Source / citation", "NRC VAD Lexicon v2 \u2014 Mohammad (2025)"),
        shiny::checkboxInput("r_vad_confirm",
                             "I checked the file version, score scale, columns and usage terms", FALSE),
        shiny::actionButton("r_import_vad", "Import lexicon"),
        shiny::p(class = "dream-hint mt-2", shiny::textOutput("r_vad_info", inline = TRUE))),
      bslib::card(fill = FALSE,
        bslib::card_header("\U0001F4E6 Export"),
        shiny::div(class = "dream-actions",
                   shiny::downloadButton("r_export", "Export as CSV"),
                   shiny::downloadButton("r_snapshot", "Download full backup (.rds)")),
        shiny::p(class = "dream-hint",
                 "Exports contain your private dream texts \u2014 don't upload them to GitHub.")))
  )
}

#' Build the app UI.
#'
#' @param css_paths Stylesheets for the app, calendar and Sleep tab.
#' @param splash_seconds Minimum seconds the cloudy loading screen is shown;
#'   0 turns it off.
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
      # Unchecked means the app shows personal data.
      shiny::checkboxInput(
        inputId = "demo_mode",
        label = "Demo mode — show an example diary",
        value = FALSE
      ),
      shiny::uiOutput("data_mode_status"),
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
          shiny::uiOutput("save_entry_control")
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
