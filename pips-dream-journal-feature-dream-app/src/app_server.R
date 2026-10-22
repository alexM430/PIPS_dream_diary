# Server logic of the dream journal app.

#' Build the app server.
#'
#' @param journal_path CSV file the journal is read from and saved to.
#' @param lexicons Output of `load_lexicons()`.
#' @return A Shiny server function.
dream_app_server <- function(journal_path, lexicons) {
  function(input, output, session) {
    journal <- reactiveVal(read_journal(journal_path))
    scored <- reactive(add_affect_scores(journal(), lexicons$emotion_meta))

    # Start every date picker at the most recent entry.
    observe({
      dates <- journal()$date
      last <- if (length(dates) > 0) max(dates) else Sys.Date()
      first <- if (length(dates) > 0) min(dates) else Sys.Date() - 30
      updateDateRangeInput(session, "period", start = first, end = last)
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
    # The selected night; starts at the most recent entry.
    cal_day <- reactiveVal(
      if (nrow(isolate(journal())) > 0) max(isolate(journal())$date) else Sys.Date()
    )
    observeEvent(input$cal_click, cal_day(as.Date(input$cal_click)))
    observeEvent(input$cal_prev, cal_day(shift_calendar_date(cal_day(), input$cal_view, -1)))
    observeEvent(input$cal_next, cal_day(shift_calendar_date(cal_day(), input$cal_view, 1)))

    cal_range <- reactive(calendar_range(cal_day(), input$cal_view))
    cal_entries <- reactive({
      filter(scored(), date >= cal_range()[1], date <= cal_range()[2])
    })

    output$cal_title <- renderText(calendar_title(cal_day(), input$cal_view))
    output$cal_summary <- renderUI(calendar_stats(cal_entries()))
    output$cal_grid <- renderUI({
      dream_calendar(scored(), cal_range()[1], cal_range()[2], input$cal_fill,
                     selected = cal_day(), lexicons = lexicons)
    })
    output$cal_night <- renderUI({
      dream_night_card(filter(scored(), date == cal_day()), cal_day(), lexicons)
    })

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
    sleep_nights <- reactive(last_nights(journal(), as.numeric(input$sleep_window)))
    sleep_stats <- reactive(sleep_summary(sleep_nights()))

    output$sleep_ring <- renderUI({
      if (is.null(sleep_stats())) return(p("No nights logged yet."))
      sleep_ring(sleep_stats()$avg_hours)
    })
    output$sleep_bubbles <- renderUI({
      req(sleep_stats())
      sleep_stat_bubbles(sleep_stats(), lexicons)
    })
    output$sleep_hours_plot <- renderPlot(plot_sleep_hours(sleep_nights()),
                                          res = 96, bg = "transparent")
    output$sleep_windows_plot <- renderPlot(plot_sleep_windows(sleep_nights()),
                                            res = 96, bg = "transparent")

    output$recommendations <- renderUI({
      nights <- sleep_nights()
      window_days <- if (nrow(nights) == 0) 14 else
        as.numeric(max(nights$date) - min(nights$date)) + 1
      recommendation_cards(
        sleep_recommendations(journal(), lexicons$emotion_meta, window_days)
      )
    })

    # The shareable story picture, made once per change of nights.
    story_file <- reactive({
      req(nrow(sleep_nights()) > 0)
      save_story_card(tempfile(fileext = ".png"), sleep_nights(), lexicons)
    })
    output$story_preview <- renderImage({
      list(src = story_file(), contentType = "image/png",
           alt = "Preview of your shareable dream stats picture")
    }, deleteFile = FALSE)
    output$story_png <- downloadHandler(
      filename = function() paste0("my-dream-stats-", Sys.Date(), ".png"),
      content = function(file) file.copy(story_file(), file),
      contentType = "image/png"
    )

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
                     collapse = " \u00B7 ")
  div(
    class = "border-bottom pb-2 mb-2",
    strong(format(entry$date, "%A %d %B %Y")),
    tags$small(class = "text-muted ms-2",
               sprintf("%s\u2013%s \u00B7 %.1f h \u00B7 quality %d \u00B7 stress %d",
                       entry$bedtime, entry$wake_time, entry$sleep_hours,
                       entry$sleep_quality, entry$stress)),
    p(class = "mb-1", HTML(text)),
    if (tags_line != "") tags$small(class = "text-primary", tags_line)
  )
}
