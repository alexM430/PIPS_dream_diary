# Server logic of the Dream Journal app (originally Anna's reviewed app),
# plus the colour-theme switch.

#' Build the app server.
#'
#' @param journal_path CSV journal, read only when no state snapshot exists yet.
#' @param state_path RDS snapshot with the demo journal and reviewed labels;
#'   it is only read. Personal entries stay in the session (see
#'   src/personal_journal.R).
#' @param lexicon_dir Folder with the word lists.
#' @return A Shiny server function.
dream_app_server <- function(journal_path, state_path, lexicon_dir) {
  dictionaries <- review_entity_dictionaries(lexicon_dir)
  function(input, output, session) {
    # Colour theme (Farah): runs once when the input arrives, and again
    # whenever it changes.
    shiny::observeEvent(input$app_colour_theme, {
      # Ignore unexpected values rather than trying to use an invalid preset.
      shiny::req(input$app_colour_theme %in% names(dream_themes))
      colours <- dream_themes[[input$app_colour_theme]]
      # Preserve the existing Bootstrap theme and update its colours.
      # (There is no theme yet when the page has not been sent.)
      current_theme <- session$getCurrentTheme()
      shiny::req(current_theme)
      updated_theme <- bslib::bs_theme_update(current_theme,
                                              primary = colours$primary, bg = colours$bg, fg = colours$fg)
      # Send the updated theme to this user's browser.
      session$setCurrentTheme(updated_theme)
    })
    # Chart colours follow the selected colour theme.
    theme_name <- shiny::reactive(input$app_colour_theme %||% "lavender")
    # Demo mode shows the read-only demo journal; otherwise the personal
    # diary, which only lives in this session (Farah's privacy design).
    demo_state <- review_read_state(state_path, journal_path)
    personal <- shiny::reactiveVal(personal_empty_state(demo_state))
    demo_mode <- shiny::reactive(!isFALSE(input$demo_mode))
    state <- shiny::reactive(if (demo_mode()) demo_state else personal())
    editing <- shiny::reactiveVal(NULL)
    notify_error <- function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
    persist <- function(s) {
      if (demo_mode()) {
        stop("The demo diary is read-only. Switch demo mode off to write your own diary.",
             call. = FALSE)
      }
      personal(s)
    }
    journal <- shiny::reactive(state()$journal)
    reviews <- shiny::reactive(state()$reviews)
    in_period <- shiny::reactive({
      shiny::req(input$r_period)
      dplyr::filter(journal(), date >= input$r_period[1], date <= input$r_period[2])
    })
    in_reviews <- shiny::reactive(dplyr::filter(reviews(), date %in% in_period()$date))
    top_labels <- function(x) {
      if (identical(input$r_top, "all")) 
        return(x)
      utils::head(x, as.integer(input$r_top %||% "10"))
    }
    fill_tags <- function(id, values, choices = values) {
      shiny::updateSelectizeInput(session, id, choices = unique(c(choices, values)), selected = values)
    }
    clear_form <- function() {
      editing(NULL)
      shiny::updateTextAreaInput(session, "r_text", value = "")
      shiny::updateTextAreaInput(session, "r_note", value = "")
      for (id in c("r_codes", "r_during", "r_after", "r_people", "r_animals", "r_objects")) fill_tags(id, 
                                                                                                      character())
      shiny::updateSelectInput(session, "r_during_status", selected = "unanswered")
      shiny::updateSelectInput(session, "r_after_status", selected = "unanswered")
      shiny::updateCheckboxInput(session, "r_entities_confirmed", value = FALSE)
    }
    # Show the dates of the current diary at start, when switching demo
    # mode and after loading a diary CSV.
    reset_view <- function() {
      clear_form()
      dates <- shiny::isolate(journal())$date
      first <- if (length(dates)) 
        min(dates)
      else Sys.Date() - 30
      last <- if (length(dates)) 
        max(dates)
      else Sys.Date()
      shiny::updateDateRangeInput(session, "r_period", start = first, end = last)
      shiny::updateDateInput(session, "r_date", value = last + 1)
      shiny::updateDateInput(session, "r_cal_date", value = last)
    }
    shiny::observeEvent(demo_mode(), reset_view())
    personal_mode_server(input, output, session, personal, demo_state, demo_mode,
                         on_loaded = reset_view)
    shiny::observe({
      s <- state()
      choices <- list(r_codes = unique(c(review_unpack(s$reviews$codes), s$definitions$code)), 
                      r_during = review_unpack(c(s$reviews$during, s$reviews$after)), r_after = review_unpack(c(s$reviews$during, 
                                                                                                                s$reviews$after)), r_people = unique(c(review_unpack(s$reviews$people), s$names$person)), 
                      r_animals = review_unpack(s$reviews$animals), r_objects = review_unpack(s$reviews$objects))
      for (id in names(choices)) {
        selected <- shiny::isolate(input[[id]])
        fill_tags(id, selected, choices[[id]])
      }
      support <- shiny::isolate(input$r_support_code)
      if (!length(support) || !nzchar(support) || !support %in% choices$r_codes) {
        support <- dplyr::pull(dplyr::count(review_long(s$reviews, "codes"), label, sort = TRUE), label)[1]
      }
      shiny::updateSelectInput(session, "r_support_code", choices = choices$r_codes, selected = support)
      shiny::updateSelectizeInput(session, "r_definition_code", choices = choices$r_codes, selected = shiny::isolate(input$r_definition_code))
      shiny::updateSelectInput(session, "r_forget_alias", choices = s$names$alias)
    })
    shiny::observe({
      choices <- if (identical(input$r_rename_kind, "emotion")) 
        review_unpack(c(reviews()$during, reviews()$after))
      else review_unpack(reviews()$codes)
      shiny::updateSelectInput(session, "r_rename_from", choices = choices)
    })
    shiny::observeEvent(input$r_definition_code, {
      row <- dplyr::filter(state()$definitions, code == input$r_definition_code)
      shiny::updateTextAreaInput(session, "r_definition", value = if (nrow(row)) 
        row$definition[1]
        else "")
    })
    shiny::observeEvent(input$r_clear, clear_form())
    shiny::observeEvent(input$r_load, {
      date <- as.Date(input$r_date)
      j <- dplyr::filter(journal(), .data$date == .env$date)
      r <- dplyr::filter(reviews(), .data$date == .env$date)
      if (!nrow(j)) {
        shiny::showNotification("No saved entry on this date. Clear the form to write a new one.")
        return()
      }
      editing(date)
      shiny::updateTextInput(session, "r_bed", value = j$bedtime)
      shiny::updateTextInput(session, "r_wake", value = j$wake_time)
      shiny::updateSliderInput(session, "r_quality", value = j$sleep_quality)
      shiny::updateSliderInput(session, "r_stress", value = j$stress)
      shiny::updateTextAreaInput(session, "r_text", value = j$dream_text)
      shiny::updateTextAreaInput(session, "r_note", value = r$note)
      for (column in c("codes", "during", "after", "people", "animals", "objects")) fill_tags(paste0("r_", 
                                                                                                     column), review_unpack(r[[column]]))
      shiny::updateSelectInput(session, "r_during_status", selected = r$during_status)
      shiny::updateSelectInput(session, "r_after_status", selected = r$after_status)
      shiny::updateCheckboxInput(session, "r_entities_confirmed", value = r$entities_reviewed)
    })
    output$r_edit_notice <- shiny::renderUI({
      date <- as.Date(input$r_date)
      if (!is.null(editing())) {
        j <- dplyr::filter(journal(), .data$date == editing())
        shiny::tagList(shiny::p("Editing saved night: ", as.character(editing())), if (nzchar(j$emotions)) 
          shiny::p("Legacy emotion tags (timing unspecified): ", j$emotions, ". Assign stages yourself if known."))
      }
      else if (date %in% journal()$date) 
        shiny::p("This night already exists. Load it before editing; saving an unloaded existing night is blocked.")
    })
    shiny::observeEvent(input$r_detect, {
      tryCatch({
        suggestions <- review_suggest_entities(input$r_text, dictionaries, state()$names)
        for (column in names(suggestions)) {
          id <- paste0("r_", column)
          fill_tags(id, unique(c(shiny::isolate(input[[id]]), suggestions[[column]])))
        }
        shiny::updateCheckboxInput(session, "r_entities_confirmed", value = FALSE)
        shiny::showNotification("Suggestions added. Correct the lists, then confirm you reviewed them.")
      }, error = notify_error)
    })
    output$r_word_count <- shiny::renderText({
      words <- length(strsplit(stringr::str_squish(input$r_text %||% ""), " ")[[1]])
      if (!nzchar(stringr::str_squish(input$r_text %||% ""))) "" else
        sprintf("%d word%s", words, if (words == 1) "" else "s")
    })
    for (stage in c("during", "after")) local({
      id <- paste0("r_", stage)
      status_id <- paste0("r_", stage, "_status")
      shiny::observeEvent(input[[id]], {
        if (length(input[[id]]) && !identical(input[[status_id]], "reported"))
          shiny::updateSelectInput(session, status_id, selected = "reported")
      }, ignoreNULL = TRUE)
    })
    shiny::observeEvent(input$r_text, {
      shiny::updateCheckboxInput(session, "r_entities_confirmed", value = FALSE)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$r_save, {
      tryCatch({
        date <- as.Date(input$r_date)
        if (date %in% journal()$date && (is.null(editing()) || date != editing())) 
          stop("Load the existing night before editing it.")
        if (!is.null(editing()) && date != editing()) 
          stop("Keep the loaded entry's date, or clear the form to start a new night.")
        during <- review_validate_emotions(input$r_during_status, input$r_during)
        after <- review_validate_emotions(input$r_after_status, input$r_after)
        codes <- review_normalise(input$r_codes)
        s <- state()
        old <- dplyr::filter(s$journal, .data$date == .env$date)
        legacy <- if (nrow(old)) 
          review_unpack(old$emotions)
        else character()
        entry <- new_entry(date, input$r_bed, input$r_wake, input$r_quality, input$r_stress, 
                           input$r_text, codes, legacy)
        row <- review_row(date, codes)
        row$note <- input$r_note %||% ""
        row$during <- review_pack(during)
        row$after <- review_pack(after)
        row$during_status <- input$r_during_status
        row$after_status <- input$r_after_status
        row$people <- review_pack(input$r_people)
        row$animals <- review_pack(input$r_animals)
        row$objects <- review_pack(input$r_objects)
        row$entities_reviewed <- isTRUE(input$r_entities_confirmed)
        s$journal <- upsert_entry(s$journal, entry)
        s$reviews <- dplyr::arrange(dplyr::bind_rows(dplyr::filter(s$reviews, .data$date != .env$date), 
                                                     row), date)
        persist(s)
        if (date < input$r_period[1] || date > input$r_period[2]) 
          shiny::updateDateRangeInput(session, "r_period", start = min(date, input$r_period[1]), 
                                      end = max(date, input$r_period[2]))
        clear_form()
        shiny::updateDateInput(session, "r_date", value = date + 1)
        shiny::showNotification("Entry and reviewed labels saved.")
      }, error = notify_error)
    })
    output$r_recent <- shiny::renderTable({
      dplyr::transmute(utils::head(dplyr::arrange(journal(), dplyr::desc(date)), 8),
                       Night = format(date, "%a %d %b"), Sleep = sprintf("%.1f h", sleep_hours),
                       Quality = sleep_quality, Stress = stress, Themes = gsub(";", ", ", themes))
    })
    output$r_theme_overview <- shiny::renderUI(theme_overview(in_period(), in_reviews()))
    output$r_heatmap <- shiny::renderPlot(review_heatmap(in_period(), in_reviews(), input$r_bin,
                                                         top = input$r_top, theme = theme_name()), res = 96, bg = "transparent")
    output$r_code_stress <- shiny::renderPlot(review_stress_plot(in_period(), in_reviews(),
                                                                 top = input$r_top, theme = theme_name()), res = 96, bg = "transparent")
    output$r_support <- shiny::renderUI({
      shiny::req(input$r_support_code)
      dates <- dplyr::pull(dplyr::filter(review_long(in_reviews(), "codes"), label == input$r_support_code),
                           date)
      x <- dplyr::left_join(dplyr::filter(in_period(), date %in% dates),
                            dplyr::select(in_reviews(), date, note), by = "date")
      x <- dplyr::arrange(x, dplyr::desc(date))
      if (!nrow(x)) return(shiny::p(class = "dream-hint", "No dreams with this theme in the chosen dates."))
      shiny::tagList(lapply(seq_len(nrow(x)), function(i) {
        shiny::div(class = "dream-result",
                   shiny::div(class = "dream-result-date", format(x$date[i], "%A %d %B")),
                   shiny::div(class = "dream-result-text", x$dream_text[i]),
                   if (nzchar(x$note[i] %||% "")) shiny::div(class = "dream-hint", "\U0001F4DD ", x$note[i]))
      }))
    })
    counts <- shiny::reactive(review_emotion_counts(in_reviews()))
    emotion_order <- shiny::reactive({
      top_labels(dplyr::pull(dplyr::arrange(dplyr::summarise(dplyr::group_by(counts(), label), 
                                                             total = sum(nights), .groups = "drop"), dplyr::desc(total), label), label))
    })
    output$r_emotion_overview <- shiny::renderUI(emotion_overview(in_reviews()))
    output$r_butterfly <- shiny::renderPlot(plot_emotion_butterfly(counts(), emotion_order(),
                                                                   theme = theme_name()), res = 96, bg = "transparent")
    output$r_during_summary <- shiny::renderText(review_status_summary(in_reviews(), "during"))
    output$r_after_summary <- shiny::renderText(review_status_summary(in_reviews(), "after"))
    matches <- shiny::reactive(review_vad_matches(in_reviews(), state()$vad))
    output$r_vad <- plotly::renderPlotly(review_vad_plot(matches(), input$r_vad_stage, state()$vad_limits,
                                                         colour = get_dream_theme(theme_name())$accent[1]))
    output$r_coverage <- shiny::renderText({
      x <- dplyr::filter(matches(), stage == input$r_vad_stage)
      if (!nrow(state()$vad)) 
        return("VAD is optional and has not been imported. Emotion frequency charts do not require it. Import a documented NRC VAD v2 CSV under Settings / export to enable the 3D plot.")
      sprintf("%d / %d distinct labels matched; %d / %d label-night occurrences matched. Source: %s. Scale: %g to %g.", 
              sum(is.finite(x$valence)), nrow(x), sum(x$nights[is.finite(x$valence)]), sum(x$nights), 
              state()$vad_source, state()$vad_limits[1], state()$vad_limits[2])
    })
    output$r_unmatched <- shiny::renderTable({
      x <- dplyr::transmute(dplyr::filter(matches(), stage == input$r_vad_stage, !is.finite(valence)),
                            `Feeling not in the lexicon` = label, Nights = nights)
      if (nrow(x)) x else NULL
    })
    entity_reviews <- shiny::reactive(dplyr::filter(in_reviews(), entities_reviewed))
    output$r_entity_overview <- shiny::renderUI(entity_overview(entity_reviews(), nrow(in_period())))
    output$r_entity_summary <- shiny::renderText(sprintf("Charts use %d confirmed nights out of %d logged nights. Each label counts once per night; confirm the lists in the New dream tab. In the demo, confirmations are synthetic.",
                                                         nrow(entity_reviews()), nrow(in_period())))
    for (column in c("people", "animals", "objects")) local({
      kind <- column
      # Plain titles: the chart font cannot draw emoji.
      title <- c(people = "People", animals = "Animals", objects = "Objects")[[kind]]
      output[[paste0("r_", kind, "_plot")]] <- shiny::renderPlot({
        x <- dplyr::count(review_long(entity_reviews(), kind), label, name = "nights")
        order <- top_labels(dplyr::pull(dplyr::arrange(x, dplyr::desc(nights), label), label))
        review_frequency_plot(x, title, order = order, theme = theme_name())
      }, res = 96, bg = "transparent")
    })
    display_meta <- shiny::reactive({
      codes <- review_unpack(reviews()$codes)
      list(theme_meta = tibble::tibble(theme = codes, label = codes,
                                       colour = unname(label_colours(codes, theme_name()))),
           emotion_meta = tibble::tibble(emotion = character(), label = character(), emoji = character(), 
                                         valence = double(), arousal = double()))
    })
    display_journal <- shiny::reactive({
      j <- journal()
      j$emotions <- rep("", nrow(j))
      j$valence <- rep(NA_real_, nrow(j))
      j$arousal <- rep(NA_real_, nrow(j))
      j
    })
    shiny::observeEvent(input$r_cal_click, shiny::updateDateInput(session, "r_cal_date", value = as.Date(input$r_cal_click)))
    cal_entries <- shiny::reactive({
      range <- calendar_range(input$r_cal_date, input$r_cal_view)
      dplyr::filter(display_journal(), date >= range[1], date <= range[2])
    })
    shiny::observeEvent(input$r_cal_prev, shiny::updateDateInput(session, "r_cal_date",
                                                                 value = shift_calendar_date(input$r_cal_date, input$r_cal_view, -1)))
    shiny::observeEvent(input$r_cal_next, shiny::updateDateInput(session, "r_cal_date",
                                                                 value = shift_calendar_date(input$r_cal_date, input$r_cal_view, 1)))
    output$r_cal_title <- shiny::renderText({
      shiny::req(input$r_cal_date)
      calendar_title(input$r_cal_date, input$r_cal_view)
    })
    output$r_cal_summary <- shiny::renderUI(calendar_stats(cal_entries()))
    output$r_cal_grid <- shiny::renderUI({
      range <- calendar_range(input$r_cal_date, input$r_cal_view)
      dream_calendar(display_journal(), range[1], range[2], input$r_cal_fill, as.Date(input$r_cal_date),
                     display_meta(), click_input = "r_cal_click", theme = theme_name())
    })
    output$r_cal_night <- shiny::renderUI({
      date <- as.Date(input$r_cal_date)
      r <- dplyr::filter(reviews(), .data$date == .env$date)
      shiny::tagList(dream_night_card(dplyr::filter(display_journal(), .data$date == .env$date),
                                      date, display_meta()), if (nrow(r)) review_night_details(r))
    })
    sleep_nights <- shiny::reactive(last_nights(display_journal(), as.numeric(input$r_sleep_window)))
    output$r_sleep_ring <- shiny::renderUI({
      summary <- sleep_summary(sleep_nights())
      if (is.null(summary)) 
        shiny::p("No nights logged.")
      else sleep_ring(summary$avg_hours)
    })
    output$r_sleep_bubbles <- shiny::renderUI({
      summary <- sleep_summary(sleep_nights())
      if (!is.null(summary)) {
        # Feelings live in the reviews (while dreaming), not in the journal.
        nights_reviews <- dplyr::filter(reviews(), date %in% sleep_nights()$date,
                                        during_status == "reported")
        feelings <- review_long(nights_reviews, "during")
        summary$top_emotion <- if (nrow(feelings))
          dplyr::pull(dplyr::count(feelings, label, sort = TRUE), label)[1] else NA_character_
        sleep_stat_bubbles(summary, display_meta())
      }
    })
    output$r_sleep_hours <- shiny::renderPlot(plot_sleep_hours(sleep_nights(), theme = theme_name()), res = 96,
                                              bg = "transparent")
    output$r_sleep_windows <- shiny::renderPlot(plot_sleep_windows(sleep_nights(), theme = theme_name()), res = 96,
                                                bg = "transparent")
    output$r_recommendations <- shiny::renderUI({
      n <- as.numeric(input$r_sleep_window)
      if (n == 0) 
        n <- if (nrow(journal())) 
          as.numeric(max(journal()$date) - min(journal()$date)) + 1
      else 14
      recommendation_cards(sleep_recommendations(display_journal(), display_meta()$emotion_meta, 
                                                 window_days = n))
    })
    # The shareable story picture, made once per change of nights.
    story_file <- shiny::reactive({
      shiny::req(nrow(sleep_nights()) > 0)
      save_story_card(tempfile(fileext = ".png"), sleep_nights(), display_meta())
    })
    output$r_story_preview <- shiny::renderImage({
      list(src = story_file(), contentType = "image/png",
           alt = "Preview of your shareable dream stats picture")
    }, deleteFile = FALSE)
    output$r_story <- shiny::downloadHandler(filename = function() paste0("my-dream-stats-", Sys.Date(),
                                                                          ".png"), content = function(file) file.copy(story_file(), file), contentType = "image/png")
    output$r_search_results <- shiny::renderUI({
      query <- stringr::str_trim(input$r_search %||% "")
      if (!nzchar(query))
        return(shiny::p(class = "dream-hint", "Type a word to search all your dreams."))
      x <- dplyr::left_join(journal(), reviews(), by = "date")
      columns <- c("dream_text", "codes", "during", "after", "people", "animals", "objects", "note")
      found <- rep(FALSE, nrow(x))
      for (column in columns) found <- found | stringr::str_detect(x[[column]], stringr::fixed(query, 
                                                                                               ignore_case = TRUE))
      x <- dplyr::arrange(x[found, ], dplyr::desc(date))
      chips <- function(values, prefix) {
        lapply(review_unpack(values), function(v) shiny::span(class = "dream-chip is-soft", prefix, v))
      }
      shiny::tagList(shiny::p(class = "dream-hint", nrow(x), if (nrow(x) == 1) "dream found" else "dreams found"),
                     lapply(seq_len(nrow(x)), function(i) {
                       shiny::div(class = "dream-result",
                                  shiny::div(class = "dream-result-date", format(x$date[i], "%A %d %B %Y")),
                                  shiny::div(class = "dream-result-text", x$dream_text[i]),
                                  shiny::div(class = "dream-chips",
                                             chips(x$codes[i], "\U0001F3F7\uFE0F "), chips(x$during[i], "\U0001F4AD "),
                                             chips(x$after[i], "\u2600\uFE0F "), chips(x$people[i], "\U0001F9D1 "),
                                             chips(x$animals[i], "\U0001F43E "), chips(x$objects[i], "\U0001F511 ")))
                     }))
    })
    shiny::observeEvent(input$r_define, tryCatch({
      code <- review_normalise(input$r_definition_code)
      if (length(code) != 1) 
        stop("Enter one code.")
      s <- state()
      s$definitions <- dplyr::bind_rows(dplyr::filter(s$definitions, .data$code != .env$code), 
                                        tibble::tibble(code = code, definition = input$r_definition %||% ""))
      persist(s)
    }, error = notify_error))
    output$r_definitions <- shiny::renderTable(state()$definitions)
    shiny::observeEvent(input$r_rename, tryCatch({
      if (!isTRUE(input$r_rename_confirm)) 
        stop("Confirm the global rename first.")
      from <- review_normalise(input$r_rename_from)
      to <- review_normalise(input$r_rename_to)
      if (length(from) != 1 || length(to) != 1) 
        stop("Choose one existing label and one replacement.")
      if (identical(from, to)) 
        stop("The replacement is unchanged.")
      s <- state()
      columns <- if (input$r_rename_kind == "emotion") 
        c("during", "after")
      else "codes"
      for (column in columns) s$reviews[[column]] <- vapply(s$reviews[[column]], function(value) {
        labels <- review_unpack(value)
        labels[labels == from] <- to
        review_pack(labels)
      }, character(1))
      if (identical(columns, "codes")) {
        s$journal$themes <- s$reviews$codes[match(s$journal$date, s$reviews$date)]
        old_definition <- dplyr::filter(s$definitions, code == from)
        if (!to %in% s$definitions$code && nrow(old_definition)) {
          old_definition$code <- to
          s$definitions <- dplyr::bind_rows(s$definitions, old_definition)
        }
        s$definitions <- dplyr::filter(s$definitions, code != from)
      }
      persist(s)
      shiny::updateCheckboxInput(session, "r_rename_confirm", value = FALSE)
      shiny::showNotification("Labels renamed / merged. Reload any entry currently open before saving it.")
      clear_form()
    }, error = notify_error))
    shiny::observeEvent(input$r_remember, tryCatch({
      alias <- review_normalise(input$r_name_alias)
      person <- review_normalise(input$r_name_person)
      if (length(alias) != 1 || length(person) != 1) 
        stop("Enter one alias and one person label.")
      s <- state()
      s$names <- dplyr::bind_rows(dplyr::filter(s$names, .data$alias != .env$alias), tibble::tibble(alias = alias, 
                                                                                                    person = person))
      persist(s)
      shiny::showNotification("Name remembered for future suggestions.")
    }, error = notify_error))
    output$r_names <- shiny::renderTable(state()$names)
    shiny::observeEvent(input$r_forget, tryCatch({
      s <- state()
      s$names <- dplyr::filter(s$names, alias != input$r_forget_alias)
      persist(s)
    }, error = notify_error))
    shiny::observeEvent(input$r_import_vad, tryCatch({
      shiny::req(input$r_vad_file)
      if (!isTRUE(input$r_vad_confirm)) 
        stop("Check the version, score scale, column mapping and usage terms first.")
      source <- stringr::str_squish(input$r_vad_source)
      if (!nzchar(source)) 
        stop("Provide a source citation.")
      limits <- if (input$r_vad_scale == "signed") 
        c(-1, 1)
      else c(0, 1)
      vad <- review_import_vad(input$r_vad_file$datapath, limits)
      s <- state()
      s$vad <- vad
      s$vad_limits <- limits
      s$vad_source <- source
      s$vad_version <- "v2 (user-confirmed)"
      persist(s)
      shiny::showNotification(sprintf("Imported %d VAD terms.", nrow(vad)))
    }, error = notify_error))
    output$r_vad_info <- shiny::renderText(sprintf("%d terms stored locally. %s \u00B7 %s", nrow(state()$vad), 
                                                   state()$vad_version, state()$vad_source))
    output$r_export <- shiny::downloadHandler(filename = function() paste0("reviewed-journal-", Sys.Date(), 
                                                                           ".csv"), content = function(file) {
                                                                             readr::write_csv(dplyr::left_join(journal(), reviews(), by = "date"), file)
                                                                           })
    output$r_snapshot <- shiny::downloadHandler(filename = function() paste0("reviewed-journal-", 
                                                                             Sys.Date(), ".rds"), content = function(file) saveRDS(state(), file))
  }
}
