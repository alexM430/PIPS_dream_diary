# Server logic of the Dream Journal app (originally Anna's reviewed app),
# plus the colour-theme switch.

#' Build the app server.
#'
#' @param journal_path CSV journal, read only when no state snapshot exists yet.
#' @param state_path RDS snapshot with the journal and reviewed labels; every
#'   save is written here.
#' @param lexicon_dir Folder with the word lists.
#' @return A Shiny server function.
dream_app_server <- function(journal_path, state_path, lexicon_dir) {
    dictionaries <- review_entity_dictionaries(lexicon_dir)
    function(input, output, session) {
        # Colour theme (Farah): runs once when the input arrives, and again
        # whenever it changes.
        shiny::observeEvent(input$app_colour_theme, {
            # Ignore unexpected values rather than trying to use an invalid preset.
            shiny::req(input$app_colour_theme %in% names(app_colour_presets))
            colours <- app_colour_presets[[input$app_colour_theme]]
            # Preserve the existing Bootstrap theme and update its colours.
            # (There is no theme yet when the page has not been sent.)
            current_theme <- session$getCurrentTheme()
            shiny::req(current_theme)
            updated_theme <- bslib::bs_theme_update(current_theme,
                primary = colours$primary, bg = colours$bg, fg = colours$fg)
            # Send the updated theme to this user's browser.
            session$setCurrentTheme(updated_theme)
        })
        state <- shiny::reactiveVal(review_read_state(state_path, journal_path))
        editing <- shiny::reactiveVal(NULL)
        notify_error <- function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
        persist <- function(s) {
            review_write_state(s, state_path)
            state(s)
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
        shiny::observeEvent(TRUE, {
            dates <- journal()$date
            first <- if (length(dates)) 
                min(dates)
            else Sys.Date() - 30
            last <- if (length(dates)) 
                max(dates)
            else Sys.Date()
            shiny::updateDateRangeInput(session, "r_period", start = first, end = last)
            shiny::updateDateInput(session, "r_date", value = last + 1)
            shiny::updateDateInput(session, "r_cal_date", value = last)
        }, once = TRUE)
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
            shiny::updateSelectInput(session, "r_support_code", choices = choices$r_codes, selected = shiny::isolate(input$r_support_code))
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
            dplyr::transmute(utils::head(dplyr::arrange(journal(), dplyr::desc(date)), 8), Night = date, 
                Quality = sleep_quality, Stress = stress, Codes = themes)
        })
        output$r_heatmap <- shiny::renderPlot(review_heatmap(in_period(), in_reviews(), input$r_bin, 
            top = input$r_top))
        output$r_code_stress <- shiny::renderPlot(review_stress_plot(in_period(), in_reviews(), top = input$r_top))
        output$r_support <- shiny::renderTable({
            shiny::req(input$r_support_code)
            dates <- dplyr::pull(dplyr::filter(review_long(in_reviews(), "codes"), label == input$r_support_code), 
                date)
            dplyr::transmute(dplyr::left_join(dplyr::filter(in_period(), date %in% dates), dplyr::select(in_reviews(), 
                date, note), by = "date"), Night = date, Dream = dream_text, Coding_note = note)
        })
        counts <- shiny::reactive(review_emotion_counts(in_reviews()))
        emotion_order <- shiny::reactive({
            top_labels(dplyr::pull(dplyr::arrange(dplyr::summarise(dplyr::group_by(counts(), label), 
                total = sum(nights), .groups = "drop"), dplyr::desc(total), label), label))
        })
        draw_emotions <- function(stage, title) {
            review_frequency_plot(dplyr::filter(counts(), .data$stage == .env$stage), title, fill = if (stage == 
                "during") 
                "#76559A"
            else "#277F8E", order = emotion_order(), limit = max(c(1, counts()$nights)))
        }
        output$r_during_plot <- shiny::renderPlot(draw_emotions("during", "During Dreaming"))
        output$r_after_plot <- shiny::renderPlot(draw_emotions("after", "After Waking"))
        output$r_during_summary <- shiny::renderText(review_status_summary(in_reviews(), "during"))
        output$r_after_summary <- shiny::renderText(review_status_summary(in_reviews(), "after"))
        matches <- shiny::reactive(review_vad_matches(in_reviews(), state()$vad))
        output$r_vad <- plotly::renderPlotly(review_vad_plot(matches(), input$r_vad_stage, state()$vad_limits))
        output$r_coverage <- shiny::renderText({
            x <- dplyr::filter(matches(), stage == input$r_vad_stage)
            if (!nrow(state()$vad)) 
                return("VAD is optional and has not been imported. Emotion frequency charts do not require it. Import a documented NRC VAD v2 CSV under Settings / export to enable the 3D plot.")
            sprintf("%d / %d distinct labels matched; %d / %d label-night occurrences matched. Source: %s. Scale: %g to %g.", 
                sum(is.finite(x$valence)), nrow(x), sum(x$nights[is.finite(x$valence)]), sum(x$nights), 
                state()$vad_source, state()$vad_limits[1], state()$vad_limits[2])
        })
        output$r_unmatched <- shiny::renderTable({
            dplyr::transmute(dplyr::filter(matches(), stage == input$r_vad_stage, !is.finite(valence)), 
                Unmatched_label = label, Nights = nights)
        })
        entity_reviews <- shiny::reactive(dplyr::filter(in_reviews(), entities_reviewed))
        output$r_entity_summary <- shiny::renderText(sprintf("Charts use %d confirmed nights out of %d logged nights in this period. Each label counts once per night. Unconfirmed entries are excluded; use Entry / review to confirm lists. In the demo, confirmations are synthetic.", 
            nrow(entity_reviews()), nrow(in_period())))
        for (column in c("people", "animals", "objects")) local({
            kind <- column
            output[[paste0("r_", kind, "_plot")]] <- shiny::renderPlot({
                x <- dplyr::count(review_long(entity_reviews(), kind), label, name = "nights")
                order <- top_labels(dplyr::pull(dplyr::arrange(x, dplyr::desc(nights), label), label))
                review_frequency_plot(x, stringr::str_to_sentence(kind), order = order)
            })
        })
        display_meta <- shiny::reactive({
            codes <- review_unpack(reviews()$codes)
            list(theme_meta = tibble::tibble(theme = codes, label = codes, colour = rep("#76559A", length(codes))), 
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
        output$r_cal_summary <- shiny::renderUI(calendar_stats(cal_entries()))
        output$r_cal_grid <- shiny::renderUI({
            range <- calendar_range(input$r_cal_date, input$r_cal_view)
            dream_calendar(display_journal(), range[1], range[2], input$r_cal_fill, as.Date(input$r_cal_date), 
                display_meta(), click_input = "r_cal_click")
        })
        output$r_cal_night <- shiny::renderUI({
            date <- as.Date(input$r_cal_date)
            r <- dplyr::filter(reviews(), .data$date == .env$date)
            shiny::tagList(dream_night_card(dplyr::filter(display_journal(), .data$date == .env$date), 
                date, display_meta()), if (nrow(r)) 
                shiny::tagList(shiny::p("During Dreaming: ", r$during_status, " \u00B7 ", r$during), shiny::p("After Waking: ", 
                  r$after_status, " \u00B7 ", r$after), shiny::p("People: ", r$people, " \u00B7 Animals: ", r$animals, 
                  " \u00B7 Objects: ", r$objects)))
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
            if (!is.null(summary)) 
                sleep_stat_bubbles(summary, display_meta())
        })
        output$r_sleep_hours <- shiny::renderPlot(plot_sleep_hours(sleep_nights()))
        output$r_sleep_windows <- shiny::renderPlot(plot_sleep_windows(sleep_nights()))
        output$r_recommendations <- shiny::renderUI({
            n <- as.numeric(input$r_sleep_window)
            if (n == 0) 
                n <- if (nrow(journal())) 
                  as.numeric(max(journal()$date) - min(journal()$date)) + 1
                else 14
            recommendation_cards(sleep_recommendations(display_journal(), display_meta()$emotion_meta, 
                window_days = n))
        })
        output$r_story <- shiny::downloadHandler(filename = function() paste0("sleep-stats-", Sys.Date(), 
            ".png"), content = function(file) {
            shiny::req(nrow(sleep_nights()) > 0)
            save_story_card(file, sleep_nights(), display_meta())
        })
        output$r_search_results <- shiny::renderUI({
            query <- stringr::str_trim(input$r_search %||% "")
            if (!nzchar(query)) 
                return(shiny::p("Search dream text, content codes, emotions or entities."))
            x <- dplyr::left_join(journal(), reviews(), by = "date")
            columns <- c("dream_text", "codes", "during", "after", "people", "animals", "objects", "note")
            found <- rep(FALSE, nrow(x))
            for (column in columns) found <- found | stringr::str_detect(x[[column]], stringr::fixed(query, 
                ignore_case = TRUE))
            x <- dplyr::arrange(x[found, ], dplyr::desc(date))
            shiny::tagList(shiny::p(nrow(x), "matching nights"), lapply(seq_len(nrow(x)), function(i) {
                shiny::div(class = "border-bottom mb-3", shiny::strong(as.character(x$date[i])), shiny::p(x$dream_text[i]), 
                  shiny::p("Codes: ", x$codes[i]), shiny::p("During: ", x$during[i], " \u00B7 After: ", x$after[i]), 
                  shiny::p("People: ", x$people[i], " \u00B7 Animals: ", x$animals[i], " \u00B7 Objects: ", x$objects[i]))
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
