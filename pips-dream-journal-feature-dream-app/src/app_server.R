# Server logic of the dream journal app.

#' Build the app server.
#'
#' @param journal_path CSV file the journal is read from and saved to.
#' @param lexicons Output of `load_lexicons()`.
#' @return A Shiny server function.
dream_app_server <- function(journal_path, lexicons) {
  function(input, output, session) {
    # Each preset defines an accent, page background, and text colour.
    colour_presets &lt;- list(
      lavender = list(
        primary = "#5B4B8A",
        bg = "#FBFAFD",
        fg = "#222222"
      ),
      ocean = list(
        primary = "#075985",
        bg = "#F0F9FF",
        fg = "#172554"
      ),
      forest = list(
        primary = "#166534",
        bg = "#F0FDF4",
        fg = "#14291C"
      )
    )
    
    # Run once when the input arrives, and again whenever it changes.
    shiny::observeEvent(input$app_colour_theme, {
      shiny::req(input$app_colour_theme)
      
      # Ignore unexpected values rather than trying to use an invalid preset.
      shiny::req(input$app_colour_theme %in% names(colour_presets))
      colours &lt;- colour_presets[[input$app_colour_theme]]
      
      # Preserve the existing Bootstrap theme and update its colours.
      updated_theme &lt;- bslib::bs_theme_update(
        session$getCurrentTheme(),
        primary = colours$primary,
        bg = colours$bg,
        fg = colours$fg
      )
      
      # Send the updated theme to this user's browser.
      session$setCurrentTheme(updated_theme)
    })
    
    journal <- reactiveVal(read_journal(journal_path))
    scored <- reactive(add_affect_scores(journal(), lexicons$emotion_meta))

    # Start every date picker at the most recent entry.
    observe({
      dates <- journal()$date
      last <- if (length(dates) > 0) max(dates) else Sys.Date()
      first <- if (length(dates) > 0) min(dates) else Sys.Date() - 30
      updateDateRangeInput(session, "period", start = first, end = last)
      updateDateInput(session, "cal_date", value = last)
      updateDateInput(session, "entry_date", value = last + 1)
    }) |> bindEvent(TRUE, once = TRUE)

    in_period <- reactive({
      req(input$period)
      filter(scored(), date >= input$period[1], date <= input$period[2])
    })

    # New entry -----------------------------------------------------------
    observeEvent(input$detect, {
      themes <- detect_categories(input$entry_text, lexicons$themes)
      emotions <- detect_categories(input$entry_text, lexicons$emotions)
      updateCheckboxGroupInput(session, "entry_themes", selected = themes)
      updateCheckboxGroupInput(session, "entry_emotions", selected = emotions)
      if (length(themes) + length(emotions) == 0) {
        showNotification("No known theme or emotion words found. Tick them yourself.",
                         type = "warning")
      }
    })

    observeEvent(input$save, {
      entry <- tryCatch(
        new_entry(input$entry_date, input$entry_bedtime, input$entry_wake,
                  input$entry_quality, input$entry_stress, input$entry_text,
                  input$entry_themes, input$entry_emotions),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      req(entry)
      replaced <- entry$date %in% journal()$date
      journal(upsert_entry(journal(), entry))
      write_journal(journal(), journal_path)

      showNotification(
        if (replaced) "Entry updated." else "Entry saved.", type = "message"
      )
      if (entry$date > input$period[2] || entry$date < input$period[1]) {
        updateDateRangeInput(session, "period",
                             start = min(input$period[1], entry$date),
                             end = max(input$period[2], entry$date))
      }
      updateDateInput(session, "entry_date", value = entry$date + 1)
      updateTextAreaInput(session, "entry_text", value = "")
      updateCheckboxGroupInput(session, "entry_themes", selected = character())
      updateCheckboxGroupInput(session, "entry_emotions", selected = character())
    })

    output$recent_entries <- renderTable({
      journal() |>
        arrange(desc(date)) |>
        head(8) |>
        transmute(Night = format(date, "%a %d %b"), Bed = bedtime, Up = wake_time,
                  Hours = sleep_hours, Quality = sleep_quality, Stress = stress,
                  Themes = str_replace_all(themes, ";", ", "),
                  Emotions = str_replace_all(emotions, ";", ", "))
    }, digits = 1)

    # Calendar ------------------------------------------------------------
    cal_range <- reactive({
      req(input$cal_date)
      day <- as.Date(input$cal_date)
      switch(input$cal_view,
             day = c(day, day),
             week = floor_date(day, "week", week_start = 1) + c(0, 6),
             month = c(floor_date(day, "month"),
                       ceiling_date(day, "month") - 1))
    })
    cal_entries <- reactive({
      filter(scored(), date >= cal_range()[1], date <= cal_range()[2])
    })

    output$cal_summary <- renderUI({
      entries <- cal_entries()
      boxes <- if (nrow(entries) == 0) {
        list(value_box("Nights logged", 0))
      } else {
        list(
          value_box("Nights logged", nrow(entries), showcase = icon("book")),
          value_box("Dreams recalled", sum(entries$dream_text != ""),
                    showcase = icon("cloud")),
          value_box("Avg. hours slept", sprintf("%.1f", mean(entries$sleep_hours)),
                    showcase = icon("bed")),
          value_box("Avg. sleep quality", sprintf("%.1f / 10", mean(entries$sleep_quality)),
                    showcase = icon("star")),
          value_box("Avg. stress", sprintf("%.1f / 10", mean(entries$stress)),
                    showcase = icon("bolt"))
        )
      }
      do.call(layout_columns, c(list(fill = FALSE), boxes))
    })

    output$cal_plot <- renderPlot({
      plot_calendar(scored(), cal_range()[1], cal_range()[2], input$cal_fill)
    }, res = 96)

    output$cal_entries <- renderUI({
      entries <- cal_entries() |> arrange(desc(date))
      if (nrow(entries) == 0) return(p(class = "text-muted", "No entries."))
      tagList(lapply(seq_len(nrow(entries)), \(i) entry_card(entries[i, ])))
    })

    # Themes and emotions -------------------------------------------------
    output$theme_heatmap <- renderPlot({
      plot_theme_heatmap(in_period(), lexicons$theme_meta, input$bin)
    }, res = 96)
    output$theme_stress <- renderPlot({
      plot_theme_stress(in_period(), lexicons$theme_meta)
    }, res = 96)
    output$affect_map <- renderPlot({
      plot_affect_map(in_period(), lexicons$emotion_meta, input$affect_colour)
    }, res = 96)
    output$emotion_timeline <- renderPlot({
      plot_emotion_timeline(in_period(), lexicons$emotion_meta, input$bin)
    }, res = 96)

    # People and objects --------------------------------------------------
    output$people_plot <- renderPlot({
      plot_mentions(count_mentions(in_period(), lexicons$people), input$top_n,
                    "Recurring people", fill = "#5B4B8A")
    }, res = 96)
    output$objects_plot <- renderPlot({
      plot_mentions(count_mentions(in_period(), lexicons$objects), input$top_n,
                    "Recurring objects", fill = "#D58936")
    }, res = 96)

    # Sleep ---------------------------------------------------------------
    output$sleep_trends <- renderPlot(plot_sleep_trends(in_period()), res = 96)

    output$recommendations <- renderUI({
      advice <- sleep_recommendations(journal(), lexicons$emotion_meta,
                                      input$rec_window)
      tagList(lapply(seq_len(nrow(advice)), function(i) {
        div(class = "mb-3",
            strong(advice$title[i]), br(), advice$message[i],
            if (advice$source[i] != "")
              tags$small(class = "text-muted", paste0(" (", advice$source[i], ")")))
      }))
    })

    # Search --------------------------------------------------------------
    output$search_results <- renderUI({
      query <- str_trim(coalesce(input$search, ""))
      if (query == "") return(p(class = "text-muted", "Type a word to search all entries."))
      hits <- search_journal(scored(), query)
      if (nrow(hits) == 0) return(p("No dreams match ", strong(query), "."))
      tagList(
        p(nrow(hits), if (nrow(hits) == 1) "dream matches." else "dreams match."),
        lapply(seq_len(nrow(hits)), \(i) entry_card(hits[i, ], highlight = query))
      )
    })
  }
}

#' Entries whose text, themes or emotions contain a search term.
#'
#' @param journal A journal tibble.
#' @param query Search term (case-insensitive, matched literally).
#' @return The matching rows, newest first.
search_journal <- function(journal, query) {
  pattern <- fixed(query, ignore_case = TRUE)
  journal |>
    filter(str_detect(dream_text, pattern) | str_detect(themes, pattern) |
             str_detect(emotions, pattern)) |>
    arrange(desc(date))
}

# A card showing one night, optionally with a search term highlighted.
entry_card <- function(entry, highlight = NULL) {
  text <- if (entry$dream_text == "") "No dream recalled." else entry$dream_text
  text <- htmltools::htmlEscape(text)
  if (!is.null(highlight)) {
    text <- str_replace_all(
      text, regex(str_escape(htmltools::htmlEscape(highlight)), ignore_case = TRUE),
      "<mark>\\0</mark>")
  }
  tags_line <- paste(c(split_tags(entry$themes)[[1]], split_tags(entry$emotions)[[1]]),
                     collapse = " · ")
  div(
    class = "border-bottom pb-2 mb-2",
    strong(format(entry$date, "%A %d %B %Y")),
    tags$small(class = "text-muted ms-2",
               sprintf("%s–%s · %.1f h · quality %d · stress %d",
                       entry$bedtime, entry$wake_time, entry$sleep_hours,
                       entry$sleep_quality, entry$stress)),
    p(class = "mb-1", HTML(text)),
    if (tags_line != "") tags$small(class = "text-primary", tags_line)
  )
}
