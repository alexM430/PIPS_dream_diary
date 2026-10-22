# Server logic of the Dream Journal app (originally Anna's reviewed app),
# plus the colour-theme switch.

#' Build the app server.
#'
#' @param journal_path CSV journal, read only when no state snapshot exists yet.
#' @return A Shiny server function.
dream_app_server <- function(journal_path, lexicons) {
  function(input, output, session) {
    # Create one simulated diary per session, not on every toggle change.
    demo_journal <- simulate_dream_journal()
    
    # Personal entries are session-local; users upload a CSV to resume.
    personal_journal <- shiny::reactiveVal(journal_template())
      # Prevents app from loading a shared CSV project as someone's personal diary.
  
    # Existing charts/search already read journal(), so they automatically
    # switch datasets without rewriting each individual output.
    journal <- shiny::reactive({
      if (isTRUE(input$demo_mode)) {
        demo_journal
      } else {
        personal_journal()
      }
    })
    
    scored <- shiny::reactive({
      add_affect_scores(journal(), lexicons$emotion_meta)
    })
    
    output$data_mode_status <- shiny::renderUI({
      if (isTRUE(input$demo_mode)) {
        shiny::tagList(
          shiny::strong("Demo mode"),
          shiny::helpText(
            "This diary is simulated and read-only. Switch Demo mode off ",
            "to enter your own dreams or upload your earlier diary CSV."
          )
        )
      } else {
        shiny::tagList(
          shiny::strong("Self-analysis: your personal diary"),
          
          shiny::helpText(
            "Start empty or upload your previous diary CSV. Add each night ",
            "and click Save entry. You can add several nights in one session."
          ),
          
          shiny::helpText(
            "Entries are kept in this session, not automatically saved to ",
            "a permanent diary file. Download your updated CSV before closing ",
            "or refreshing. Next time, upload it to continue adding nights."
          ),
          
          shiny::helpText(
            "Uploading replaces the current personal diary; it does not merge ",
            "files. Download your current diary first if you want to keep it."
          ),
          
          shiny::helpText(
            "Downloaded CSVs contain private, unencrypted dream text. Keep ",
            "them outside GitHub. For more private use, run locally in RStudio. ",
            "Hosted apps process your entries and uploads on the server."
          )
        )
      }
    })
    
    output$save_entry_control <- shiny::renderUI({
      if (isTRUE(input$demo_mode)) {
        shiny::tagList(
          shiny::tags$button(
            type = "button",
            class = "btn btn-primary",
            disabled = "disabled",
            "Save unavailable in Demo mode"
          ),
          shiny::helpText("Switch Demo mode off before saving an entry.")
        )
      } else {
        shiny::actionButton(
          "save", "Save entry",
          icon = shiny::icon("floppy-disk"),
          class = "btn-primary"
        )
      }
    })

    # Reset dates at startup and whenever the user changes data mode.
    # Otherwise the personal diary could inherit the demo's date filters.
    shiny::observeEvent(input$demo_mode, {
      dates <- journal()$date
      dates <- dates[!is.na(dates)]
      
      last <- if (length(dates) > 0) max(dates) else Sys.Date()
      first <- if (length(dates) > 0) min(dates) else Sys.Date() - 30
      next_entry <- if (length(dates) > 0) last + 1 else Sys.Date()
      
      shiny::updateDateRangeInput(
        session, "period", start = first, end = last
      )
      shiny::updateDateInput(session, "cal_date", value = last)
      shiny::updateDateInput(session, "entry_date", value = next_entry)
    }, ignoreNULL = FALSE)

    
    output$download_personal_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("personal-dream-journal-", Sys.Date(), ".csv")
      },
      contentType = "text/csv; charset=utf-8",
      content = function(file) {
        shiny::req(!isTRUE(input$demo_mode))
        readr::write_csv(personal_journal(), file, na = "")
      }
    )
    
    shiny::observeEvent(input$load_personal_csv, {
      if (isTRUE(input$demo_mode)) {
        shiny::showNotification(
          "Switch to Self-analysis before uploading.",
          type = "warning"
        )
        return()
      }
      
      if (is.null(input$upload_personal_csv)) {
        shiny::showNotification("Choose a CSV first.", type = "warning")
        return()
      }
      
      if (!isTRUE(input$confirm_diary_replace)) {
        shiny::showNotification(
          "Confirm replacement of the current session diary first.",
          type = "warning"
        )
        return()
      }
      
      imported <- tryCatch(
        read_personal_diary_csv(input$upload_personal_csv$datapath),
        error = function(e) {
          shiny::showNotification(
            conditionMessage(e), type = "error", duration = 10
          )
          NULL
        }
      )
      
      if (is.null(imported)) return()
      
      # Replace only after validation succeeds.
      personal_journal(imported)
      
      dates <- imported$date
      first <- if (length(dates)) min(dates) else Sys.Date() - 30
      last <- if (length(dates)) max(dates) else Sys.Date()
      
      shiny::updateDateRangeInput(
        session, "period", start = first, end = last
      )
      shiny::updateDateInput(session, "cal_date", value = last)
      shiny::updateDateInput(
        session, "entry_date",
        value = if (length(dates)) last + 1 else Sys.Date()
      )
      
      shiny::updateTextAreaInput(session, "entry_text", value = "")
      shiny::updateCheckboxGroupInput(
        session, "entry_themes", selected = character()
      )
      shiny::updateCheckboxGroupInput(
        session, "entry_emotions", selected = character()
      )
      shiny::updateCheckboxInput(
        session, "confirm_diary_replace", value = FALSE
      )
      
      shiny::showNotification(
        "Diary loaded. Add more nights, then download the updated CSV.",
        type = "message"
      )
    }, ignoreInit = TRUE)
    
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

    shiny::observeEvent(input$save, {
      # Guard the write operation even if a stale/manual event is received.
      if (isTRUE(input$demo_mode)) {
        shiny::showNotification(
          "Demo data is read-only. Switch Demo mode off to save your entry.",
          type = "warning"
        )
        return()
      }
      
      entry <- tryCatch(
        new_entry(
          input$entry_date, input$entry_bedtime, input$entry_wake,
          input$entry_quality, input$entry_stress, input$entry_text,
          input$entry_themes, input$entry_emotions
        ),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      shiny::req(entry)
      
      # Always save to the personal dataset, never the displayed demo.
      replaced <- entry$date %in% personal_journal()$date
      updated_personal <- upsert_entry(personal_journal(), entry)
      personal_journal(updated_personal)
      
      shiny::showNotification(
        if (replaced)
          "Night updated in this session. Download your CSV before leaving."
        else
          "Night added to this session. Download your CSV before leaving.",
        type = "message"
      )
      
      if (!is.null(input$period) && length(input$period) == 2) {
        if (entry$date > input$period[2] || entry$date < input$period[1]) {
          shiny::updateDateRangeInput(
            session, "period",
            start = min(input$period[1], entry$date),
            end = max(input$period[2], entry$date)
          )
        }
      }
      
      shiny::updateDateInput(session, "entry_date", value = entry$date + 1)
      shiny::updateTextAreaInput(session, "entry_text", value = "")
      shiny::updateCheckboxGroupInput(
        session, "entry_themes", selected = character()
      )
      shiny::updateCheckboxGroupInput(
        session, "entry_emotions", selected = character()
      )
    }, ignoreInit = TRUE)

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

# Uses eisting journal functions and preserves dream text, themes and emotions
read_personal_diary_csv <- function(path) {
  x <- readr::read_csv(
    path,
    col_types = journal_col_types(),
    na = character(),
    name_repair = "minimal",
    show_col_types = FALSE
  )
  
  if (nrow(readr::problems(x)) > 0) {
    stop("The CSV contains invalid dates, numbers or malformed rows.")
  }
  
  if (anyDuplicated(names(x))) {
    stop("The CSV has duplicate column names.")
  }
  
  required <- names(journal_template())
  missing <- setdiff(required, names(x))
  
  if (length(missing)) {
    stop(
      "Missing columns: ", paste(missing, collapse = ", "),
      ". Upload a diary CSV downloaded from this app."
    )
  }
  
  if (length(setdiff(names(x), required))) {
    stop("This CSV uses a different diary format. Upload a CSV from this app.")
  }
  
  if (anyNA(x$date) || anyDuplicated(x$date)) {
    stop("Each night must have one valid, unique date.")
  }
  
  if (any(!is_valid_clock(x$bedtime)) ||
      any(!is_valid_clock(x$wake_time))) {
    stop("Bedtime and wake time must use HH:MM, such as 23:30.")
  }
  
  for (field in c("sleep_quality", "stress")) {
    value <- x[[field]]
    if (any(!is.finite(value) | value < 1 | value > 10 |
            value != floor(value))) {
      stop(field, " must contain whole numbers from 1 to 10.")
    }
  }
  
  for (field in c("dream_text", "themes", "emotions")) {
    x[[field]][is.na(x[[field]])] <- ""
  }
  
  # Recalculate derived hours from the validated times.
  x$sleep_hours <- round(
    sleep_duration_hours(x$bedtime, x$wake_time), 2
  )
  
  dplyr::arrange(
    dplyr::select(x, dplyr::all_of(required)),
    date
  )
}
