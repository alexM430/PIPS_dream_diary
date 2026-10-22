# reviewed_app.R — consolidated reviewed journal app
# Generated from the current app code and applied chart/layout fixes.
# Launch from the project root:
#   source("src/reviewed_app.R")
#   run_fixed_reviewed_app()
# Saved journal and imported lexicon remain in the main RDS snapshot.
# No dependency on annas_revision.R or reviewed_demo.R at runtime.

load_reviewed_app_support <- function(src_dir = "src") {
  packages <- c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "readr",
                "stringr", "lubridate", "purrr", "tibble", "plotly",
                "htmltools", "scales")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install missing packages: ", paste(missing, collapse = ", "))
  suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
  files <- c("journal_io.R", "text_features.R", "plots.R", "calendar_widget.R",
             "sleep_view.R", "sleep_plots.R", "sleep_recommendations.R",
             "share_card.R", "splash_screen.R", "simulate_journal.R")
  paths <- file.path(src_dir, files)
  if (any(!file.exists(paths))) stop("Missing supporting scripts: ",
                                    paste(paths[!file.exists(paths)], collapse = ", "))
  for (path in paths) source(path, local = .GlobalEnv)
  invisible(TRUE)
}

.review_select_chart_codes <- function(reviews, top = "all") {
    if (is.null(top) || identical(top, "all")) 
        return(reviews)
    limit <- suppressWarnings(as.integer(top))
    if (!length(limit) || is.na(limit) || limit < 1) {
        stop("Invalid maximum-label setting.")
    }
    order <- dplyr::pull(dplyr::arrange(dplyr::count(review_long(reviews, "codes"), label, name = "nights"), 
        dplyr::desc(nights), label), label)
    keep <- utils::head(order, limit)
    reviews$codes <- vapply(reviews$codes, function(value) {
        review_pack(intersect(review_unpack(value), keep))
    }, character(1))
    reviews
}

.review_base_heatmap <- function(journal, reviews, period = "week") {
    period <- match.arg(period, c("week", "month"))
    x <- review_long(reviews, "codes")
    if (!nrow(x)) {
        return(empty_plot("Add your own content codes to entries first."))
    }
    periods <- seq(lubridate::floor_date(min(journal$date), period, week_start = 1), lubridate::floor_date(max(journal$date), 
        period, week_start = 1), by = period)
    denominators <- dplyr::count(dplyr::mutate(journal, period_start = lubridate::floor_date(date, .env$period, 
        week_start = 1)), period_start, name = "logged")
    counts <- dplyr::mutate(dplyr::left_join(tidyr::complete(dplyr::count(dplyr::mutate(x, period_start = lubridate::floor_date(date, 
        .env$period, week_start = 1)), period_start, label, name = "nights"), period_start = .env$periods, 
        label, fill = list(nights = 0L)), denominators, by = "period_start"), rate = nights/logged)
    ggplot2::ggplot(counts, ggplot2::aes(period_start, label, fill = rate)) + ggplot2::geom_tile(colour = "white") + 
        ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(logged), "—", ifelse(nights > 0, as.character(nights), 
            ""))), size = 3) + ggplot2::scale_fill_gradient(low = "#F3EEF8", high = "#76559A", limits = c(0, 
        1), na.value = "#DDDDDD", labels = scales::percent, name = "% of logged nights") + ggplot2::labs(x = NULL, 
        y = NULL, title = paste("User-defined content codes per", period), caption = paste("Numbers = nights coded. Grey / — = no entries.", 
            "Whole-entry labels support coding, not automated thematic analysis.")) + dream_theme()
}

.review_base_stress <- function(journal, reviews) {
    x <- dplyr::filter(dplyr::left_join(review_long(reviews, "codes"), dplyr::select(journal, date, stress), 
        by = "date"), is.finite(stress))
    if (!nrow(x)) 
        return(empty_plot("No content codes with stress ratings."))
    counts <- dplyr::count(x, label, name = "n")
    x <- dplyr::mutate(dplyr::left_join(x, counts, by = "label"), display = paste0(label, " (n=", n, 
        ")"))
    ggplot2::ggplot(x, ggplot2::aes(reorder(display, stress, FUN = median), stress)) + ggplot2::geom_hline(yintercept = mean(journal$stress, 
        na.rm = TRUE), linetype = "dashed", colour = "grey50") + ggplot2::geom_boxplot(fill = "#B8A1D1", 
        outlier.shape = NA) + ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.1, height = 0, 
        seed = 7), alpha = 0.4) + ggplot2::coord_flip(ylim = c(0.5, 10.5)) + ggplot2::labs(x = NULL, 
        y = "Stress the day before (1–10)", caption = "Dots = nights. Dashed line = overall mean. Association, not causation.") + 
        dream_theme()
}

.review_base_vad <- function(matches, stage, limits) {
    x <- dplyr::filter(matches, .data$stage == .env$stage, is.finite(valence), is.finite(arousal), is.finite(dominance))
    if (!nrow(x)) {
        return(plotly::layout(plotly::plot_ly(), annotations = list(list(text = "No matched emotion labels for this stage. Import NRC VAD v2 under Settings.", 
            x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))))
    }
    safe_labels <- htmltools::htmlEscape(x$label)
    x$hover <- sprintf("%s<br>Nights: %d<br>Valence: %.3f<br>Arousal: %.3f<br>Dominance: %.3f", safe_labels, 
        x$nights, x$valence, x$arousal, x$dominance)
    plotly::layout(plotly::plot_ly(x, x = ~valence, y = ~arousal, z = ~dominance, type = "scatter3d", 
        mode = "markers", text = ~hover, hoverinfo = "text", size = ~nights, sizes = c(7, 22), marker = list(color = "#76559A", 
            opacity = 0.8)), scene = list(xaxis = list(title = "Valence: unpleasant → pleasant", range = limits), 
        yaxis = list(title = "Arousal", range = limits), zaxis = list(title = "Dominance: low → high control", 
            range = limits), aspectmode = "cube"), margin = list(l = 0, r = 0, b = 0, t = 15))
}

review_normalise <- function(x) {
    x <- stringr::str_to_lower(stringr::str_squish(x))
    x <- x[!is.na(x) & nzchar(x)]
    if (any(grepl(";", x, fixed = TRUE))) 
        stop("Labels cannot contain semicolons.")
    unique(x)
}

review_pack <- function(x) paste(review_normalise(x), collapse = ";")

review_unpack <- function(x) review_normalise(unlist(split_tags(x), use.names = FALSE))

review_empty <- function() {
    tibble::tibble(date = as.Date(character()), codes = character(), note = character(), during = character(), 
        after = character(), during_status = character(), after_status = character(), people = character(), 
        animals = character(), objects = character(), entities_reviewed = logical())
}

review_row <- function(date, codes = character()) {
    tibble::tibble(date = as.Date(date), codes = review_pack(codes), note = "", during = "", after = "", 
        during_status = "unanswered", after_status = "unanswered", people = "", animals = "", objects = "", 
        entities_reviewed = FALSE)
}

review_initial_state <- function(journal) {
    reviews <- review_empty()
    for (i in seq_len(nrow(journal))) {
        reviews <- dplyr::bind_rows(reviews, review_row(journal$date[i], split_tags(journal$themes[i])[[1]]))
    }
    codes <- review_unpack(reviews$codes)
    list(version = 1L, journal = journal, reviews = reviews, definitions = tibble::tibble(code = codes, 
        definition = rep("Imported legacy content category; review its meaning.", length(codes))), names = tibble::tibble(alias = character(), 
        person = character()), vad = tibble::tibble(term = character(), valence = double(), arousal = double(), 
        dominance = double()), vad_limits = c(-1, 1), vad_source = "", vad_version = "")
}

review_validate_state <- function(s) {
    if (!is.list(s) || !identical(s$version, 1L)) 
        stop("Unsupported reviewed-journal format.")
    if (!all(c("journal", "reviews", "definitions", "names", "vad", "vad_limits", "vad_source", "vad_version") %in% 
        names(s))) 
        stop("Incomplete reviewed-journal state.")
    if (!all(names(journal_template()) %in% names(s$journal)) || !all(names(review_empty()) %in% names(s$reviews))) 
        stop("Invalid journal columns.")
    if (anyNA(s$journal$date) || anyDuplicated(s$journal$date) || anyDuplicated(s$reviews$date)) 
        stop("Invalid or duplicate dates.")
    if (!setequal(s$journal$date, s$reviews$date)) 
        stop("Journal and review dates do not agree.")
    valid <- c("unanswered", "reported", "dont_remember", "none")
    if (any(!s$reviews$during_status %in% valid) || any(!s$reviews$after_status %in% valid)) 
        stop("Invalid emotion status.")
    invisible(s)
}

review_write_state <- function(s, path) {
    review_validate_state(s)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile("reviewed-", tmpdir = dirname(path))
    on.exit(unlink(temporary), add = TRUE)
    saveRDS(s, temporary)
    previous <- paste0(path, ".previous")
    if (file.exists(path)) {
        if (!file.copy(path, previous, overwrite = TRUE)) 
            stop("Could not back up the reviewed journal.")
        if (!file.remove(path)) 
            stop("Could not replace reviewed journal.")
    }
    if (!file.rename(temporary, path)) {
        if (file.exists(previous)) 
            file.copy(previous, path, overwrite = TRUE)
        stop("Save failed; previous snapshot was restored when available.")
    }
    invisible(path)
}

review_read_state <- function(path, journal_path) {
    if (file.exists(path)) {
        s <- readRDS(path)
        review_validate_state(s)
        return(s)
    }
    s <- review_initial_state(read_journal(journal_path))
    review_write_state(s, path)
    s
}

review_entity_dictionaries <- function(lexicon_dir) {
    read_dictionary <- function(name) {
        path <- file.path(lexicon_dir, paste0(name, ".csv"))
        if (!file.exists(path)) 
            return(tibble::tibble(category = character(), word = character()))
        x <- readr::read_csv(path, show_col_types = FALSE)
        if (!all(c("category", "word") %in% names(x)) || anyNA(x[c("category", "word")])) 
            stop("Invalid dictionary: ", name)
        dplyr::transmute(x, category = stringr::str_to_lower(category), word = stringr::str_to_lower(word))
    }
    from_groups <- function(groups) {
        dplyr::bind_rows(lapply(names(groups), function(category) {
            tibble::tibble(category = category, word = strsplit(groups[[category]], "|", fixed = TRUE)[[1]])
        }))
    }
    animals <- from_groups(c(dog = "dog|dogs", cat = "cat|cats", snake = "snake|snakes", bird = "bird|birds", 
        horse = "horse|horses", spider = "spider|spiders", wolf = "wolf|wolves", bear = "bear|bears", 
        fish = "fish", shark = "shark|sharks", lion = "lion|lions", animal = "animal|animals"))
    people <- from_groups(c(mother = "mother|mothers|mom|mum|mommy|mummy", father = "father|fathers|dad|daddy", 
        parent = "parent|parents", son = "son|sons", daughter = "daughter|daughters", child = "child|children|baby|babies|kid|kids", 
        sister = "sister|sisters", brother = "brother|brothers", friend = "friend|friends|best friend|best friends", 
        aunt = "aunt|aunts", uncle = "uncle|uncles", cousin = "cousin|cousins", neighbour = "neighbour|neighbours|neighbor|neighbors", 
        doctor = "doctor|doctors", nurse = "nurse|nurses", teacher = "teacher|teachers|professor|professors", 
        partner = "partner|partners|boyfriend|girlfriend|husband|wife|spouse|fiancé|fiancée|fiance|fiancee", 
        ex_partner = "ex boyfriend|ex girlfriend|ex husband|ex wife|ex partner", colleague = "colleague|colleagues|coworker|coworkers|co worker|co workers", 
        man = "man|men", woman = "woman|women", boy = "boy|boys", girl = "girl|girls"))
    objects <- from_groups(c(phone = "phone|phones|telephone|telephones|smartphone|smartphones|mobile phone", 
        door = "door|doors", house = "house|houses", car = "car|cars", key = "key|keys", bag = "bag|bags|suitcase|suitcases|backpack|backpacks", 
        money = "money|cash|wallet|wallets", clock = "clock|clocks|watch|watches", mirror = "mirror|mirrors", 
        train = "train|trains", plane = "plane|planes|airplane|airplanes|aeroplane|aeroplanes", stairs = "stairs|staircase|staircases", 
        knife = "knife|knives", book = "book|books", bed = "bed|beds", window = "window|windows", elevator = "elevator|elevators|lift|lifts", 
        computer = "computer|computers|laptop|laptops", ladder = "ladder|ladders", bridge = "bridge|bridges", 
        ball = "ball|balls", gun = "gun|guns|pistol|pistols", boat = "boat|boats|ship|ships", bike = "bike|bikes|bicycle|bicycles", 
        umbrella = "umbrella|umbrellas", photo = "photo|photos|photograph|photographs", table = "table|tables", 
        chair = "chair|chairs", passport = "passport|passports", clothes = "clothes|clothing|shirt|shirts|dress|dresses|trousers|pants"))
    original_objects <- read_dictionary("objects")
    animal_categories <- unique(c(animals$category, "pet", "pets", "rabbit", "mouse", "rat", "insect"))
    animals <- dplyr::bind_rows(animals, dplyr::filter(original_objects, category %in% animal_categories), 
        read_dictionary("animals"))
    list(people = dplyr::distinct(dplyr::bind_rows(read_dictionary("people"), people)), animals = dplyr::distinct(animals), 
        objects = dplyr::distinct(dplyr::bind_rows(dplyr::filter(original_objects, !category %in% animal_categories), 
            objects)))
}

review_detect <- function(text, dictionary) {
    text <- stringr::str_to_lower(stringr::str_replace_all(dplyr::coalesce(text, ""), "[’']", ""))
    clauses <- stringr::str_split(text, "[.!?;,:\\n]+")[[1]]
    tokens <- lapply(clauses, function(x) stringr::str_extract_all(x, "\\p{L}+")[[1]])
    hit <- vapply(dictionary$word, function(word) {
        phrase <- stringr::str_extract_all(stringr::str_to_lower(stringr::str_remove_all(word, "[’']")), 
            "\\p{L}+")[[1]]
        k <- length(phrase)
        if (!k) 
            return(FALSE)
        any(vapply(tokens, function(t) {
            if (length(t) < k) return(FALSE)
            any(vapply(seq_len(length(t) - k + 1L), function(i) identical(t[i:(i + k - 1L)], phrase), 
                logical(1)))
        }, logical(1)))
    }, logical(1))
    review_normalise(dictionary$category[hit])
}

review_suggest_entities <- function(text, dictionaries, names) {
    people <- review_detect(text, dictionaries$people)
    if (nrow(names)) 
        people <- unique(c(people, review_detect(text, dplyr::transmute(names, category = person, word = alias))))
    list(people = people, animals = review_detect(text, dictionaries$animals), objects = review_detect(text, 
        dictionaries$objects))
}

review_validate_emotions <- function(status, labels) {
    if (!status %in% c("unanswered", "reported", "dont_remember", "none")) 
        stop("Choose an emotion response status.")
    labels <- review_normalise(labels)
    if (status == "reported" && !length(labels)) 
        stop("Add at least one emotion, or select another response status.")
    if (status != "reported" && length(labels)) 
        stop("Emotion labels require the 'Emotions reported' status. Remove them or change the status.")
    labels
}

review_long <- function(reviews, column) {
    empty <- tibble::tibble(date = as.Date(character()), label = character())
    if (!nrow(reviews)) 
        return(empty)
    result <- dplyr::bind_rows(lapply(seq_len(nrow(reviews)), function(i) {
        labels <- review_unpack(reviews[[column]][i])
        if (!length(labels)) 
            return(empty)
        tibble::tibble(date = reviews$date[i], label = labels)
    }))
    dplyr::distinct(result, date, label)
}

review_emotion_long <- function(reviews) {
    dplyr::bind_rows(dplyr::mutate(review_long(dplyr::filter(reviews, during_status == "reported"), "during"), 
        stage = "during"), dplyr::mutate(review_long(dplyr::filter(reviews, after_status == "reported"), 
        "after"), stage = "after"))
}

review_emotion_counts <- function(reviews) {
    x <- review_emotion_long(reviews)
    if (!nrow(x)) 
        return(tibble::tibble(label = character(), stage = character(), nights = integer()))
    tidyr::complete(dplyr::count(x, label, stage, name = "nights"), label, stage = c("during", "after"), 
        fill = list(nights = 0L))
}

review_frequency_plot <- function(data, title, fill = "#76559A", order = NULL, limit = NULL) {
    if (!nrow(data)) 
        return(empty_plot(paste0(title, "\nNo reviewed labels for this chart in the selected period.")))
    if (is.null(order)) 
        order <- dplyr::pull(dplyr::arrange(data, dplyr::desc(nights), label), label)
    data <- dplyr::filter(data, label %in% order)
    data$label <- factor(data$label, levels = rev(order))
    if (is.null(limit)) 
        limit <- max(c(1, data$nights))
    ggplot2::ggplot(data, ggplot2::aes(nights, label)) + ggplot2::geom_col(fill = fill, width = 0.65) + 
        ggplot2::geom_text(ggplot2::aes(label = nights), hjust = -0.25, size = 3.8) + ggplot2::scale_y_discrete(drop = FALSE) + 
        ggplot2::scale_x_continuous(limits = c(0, limit * 1.18), breaks = function(limits) {
            unique(round(pretty(c(0, limit), n = 5)))
        }, expand = c(0, 0)) + ggplot2::labs(x = "Nights reported", y = NULL, title = title) + dream_theme()
}

review_status_summary <- function(reviews, stage) {
    x <- reviews[[paste0(stage, "_status")]]
    sprintf("%d logged nights · %d answered (%d with emotions; %d no emotion noticed; %d don't remember) · %d unanswered", 
        length(x), sum(x != "unanswered"), sum(x == "reported"), sum(x == "none"), sum(x == "dont_remember"), 
        sum(x == "unanswered"))
}

review_heatmap <- function(journal, reviews, period = "week", top = "all") {
    if (!nrow(journal)) 
        return(empty_plot("No logged nights in this period."))
    p <- .review_base_heatmap(journal, .review_select_chart_codes(reviews, top), period)
    if (length(p$layers)) {
        p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top", barwidth = grid::unit(9, 
            "cm"), barheight = grid::unit(0.4, "cm"))) + ggplot2::theme(legend.position = "bottom", plot.caption = ggplot2::element_text(size = 9, 
            hjust = 0), plot.margin = ggplot2::margin(10, 15, 10, 10)) + ggplot2::labs(caption = paste("Numbers = nights coded; colour = share of logged nights. Grey / — = no entries.", 
            "Whole-entry labels support coding, not automated thematic analysis.", sep = "\n"))
    }
    p
}

review_stress_plot <- function(journal, reviews, top = "all") {
    .review_base_stress(journal, .review_select_chart_codes(reviews, top))
}

review_import_vad <- function(path, limits) {
    x <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
    names(x) <- stringr::str_to_lower(stringr::str_trim(names(x)))
    needed <- c("term", "valence", "arousal", "dominance")
    if (!all(needed %in% names(x))) 
        stop("Prepare a CSV with columns: term,valence,arousal,dominance. Follow the lexicon documentation for the correct source columns.")
    if (anyNA(x[needed])) 
        stop("VAD table contains missing values.")
    x$term <- stringr::str_to_lower(stringr::str_squish(x$term))
    if (any(!nzchar(x$term))) 
        stop("Empty VAD terms.")
    for (column in needed[-1]) {
        value <- suppressWarnings(as.numeric(x[[column]]))
        if (any(!is.finite(value) | value < limits[1] | value > limits[2])) 
            stop("Invalid or out-of-range ", column, " scores. Check the documented scale.")
        x[[column]] <- value
    }
    x <- dplyr::distinct(dplyr::select(x, dplyr::all_of(needed)))
    if (anyDuplicated(x$term)) 
        stop("Conflicting duplicate VAD terms; resolve them before import.")
    x
}

review_vad_matches <- function(reviews, vad) {
    x <- dplyr::count(review_emotion_long(reviews), label, stage, name = "nights")
    dplyr::left_join(x, vad, by = c(label = "term"))
}

review_vad_plot <- function(matches, stage, limits) {
    x <- dplyr::filter(matches, .data$stage == .env$stage, is.finite(valence), is.finite(arousal), is.finite(dominance))
    if (nrow(x)) 
        return(.review_base_vad(matches, stage, limits))
    plotly::config(plotly::layout(plotly::plot_ly(), xaxis = list(visible = FALSE, range = c(0, 1)), 
        yaxis = list(visible = FALSE, range = c(0, 1)), annotations = list(list(text = paste0("Affective-space plot unavailable.<br>", 
            "Requires reported emotions and matching terms<br>", "from an imported VAD lexicon. Frequency charts are separate."), 
            x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE)), margin = list(l = 15, 
            r = 15, b = 15, t = 15)), displayModeBar = FALSE)
}

reviewed_dream_ui <- function(css_paths = c("src/www/dream_calendar.css", "src/www/sleep_view.css")) {
    tags_input <- function(id, label) shiny::selectizeInput(id, label, choices = character(), multiple = TRUE, 
        options = list(create = TRUE, persist = FALSE, plugins = list("remove_button"), placeholder = "Type a label and press Enter"))
    status_input <- function(id) shiny::selectInput(id, "Response", c(Unanswered = "unanswered", `Emotions reported` = "reported", 
        `I don't remember` = "dont_remember", `No emotion noticed` = "none"))
    bslib::page_navbar(fillable = FALSE, title = "Dream Journal · reviewed labels", theme = bslib::bs_theme(version = 5, 
        primary = "#5B4B8A", bg = "#FBFAFD", fg = "#222222"), header = shiny::tags$head(lapply(css_paths[file.exists(css_paths)], 
        shiny::includeCSS)), sidebar = bslib::sidebar(width = 320, shiny::dateRangeInput("r_period", 
        "Chart date range", weekstart = 1), shiny::radioButtons("r_bin", "Group content-code heatmap by", 
        choices = c(Week = "week", Month = "month"), inline = TRUE), shiny::selectInput("r_top", "Maximum labels per chart", 
        c(`Top 10` = "10", `Top 20` = "20", All = "all")), shiny::helpText("All processing is local. This app is intended for a single user, not simultaneous multi-user editing.")), 
        bslib::nav_panel("Entry / review", bslib::layout_columns(fill = FALSE, col_widths = c(6, 6), 
            bslib::card(fill = FALSE, bslib::card_header("Write or edit a night"), shiny::dateInput("r_date", 
                "Night of (evening you went to bed)", weekstart = 1), shiny::actionButton("r_load", "Load saved night for editing"), 
                shiny::actionButton("r_clear", "Clear form for a new entry"), shiny::uiOutput("r_edit_notice"), 
                bslib::layout_columns(fill = FALSE, shiny::textInput("r_bed", "Went to bed", "23:30"), 
                  shiny::textInput("r_wake", "Woke up", "07:30")), shiny::sliderInput("r_quality", "Sleep quality", 
                  1, 10, 6), shiny::sliderInput("r_stress", "Stress the day before", 1, 10, 5), shiny::textAreaInput("r_text", 
                  "Dream text", rows = 6), tags_input("r_codes", "Your content codes / labels"), shiny::textAreaInput("r_note", 
                  "Optional coding note: why do these labels fit?", rows = 2), shiny::helpText("Create your own labels. Imported legacy categories are retained as editable codes. The app does not infer themes or psychological meanings."), 
                shiny::h5("During Dreaming"), status_input("r_during_status"), tags_input("r_during", 
                  "Emotions during the dream"), shiny::h5("After Waking"), status_input("r_after_status"), 
                tags_input("r_after", "Emotions after waking"), shiny::helpText("Multiple custom labels are allowed. Use × to remove a label; type its replacement to edit it. Existing emotion words are suggestions, not restrictions."), 
                shiny::actionButton("r_save", "Save reviewed entry", class = "btn-primary")), bslib::card(fill = FALSE, 
                bslib::card_header("Review people, animals and objects"), shiny::actionButton("r_detect", 
                  "Suggest entities from current text"), shiny::helpText("Suggestions are added to the lists below. Remove incorrect items with ×; type missed items and press Enter. To move an item, remove it here and add it to the correct group."), 
                tags_input("r_people", "People (including names)"), tags_input("r_animals", "Animals"), 
                tags_input("r_objects", "Inanimate objects"), shiny::checkboxInput("r_entities_confirmed", 
                  "I have reviewed these lists (including if empty)", FALSE), shiny::helpText("Unreviewed lists are excluded from entity charts. Unseen names may be missed: add them manually and optionally remember them under Settings."), 
                shiny::h5("Recent nights"), shiny::tableOutput("r_recent")))), bslib::nav_panel("Content codes", 
            bslib::card(fill = FALSE, shiny::plotOutput("r_heatmap", height = "420px")), bslib::card(fill = FALSE, 
                shiny::plotOutput("r_code_stress", height = "450px")), bslib::card(fill = FALSE, bslib::card_header("Supporting entries"), 
                shiny::selectInput("r_support_code", "Content code", choices = character()), shiny::tableOutput("r_support"))), 
        bslib::nav_panel("Emotions", bslib::layout_columns(fill = FALSE, col_widths = c(6, 6), bslib::card(fill = FALSE, 
            shiny::plotOutput("r_during_plot", height = "380px"), shiny::textOutput("r_during_summary")), 
            bslib::card(fill = FALSE, shiny::plotOutput("r_after_plot", height = "380px"), shiny::textOutput("r_after_summary"))), 
            bslib::card(fill = FALSE, bslib::card_header("A map of your emotions"), shiny::radioButtons("r_vad_stage", 
                "Stage", c(`During Dreaming` = "during", `After Waking` = "after"), inline = TRUE), plotly::plotlyOutput("r_vad", 
                height = "450px"), shiny::tableOutput("r_unmatched"))), bslib::nav_panel("People, animals & objects", 
            shiny::textOutput("r_entity_summary"), bslib::layout_columns(fill = FALSE, col_widths = c(4, 
                4, 4), bslib::card(fill = FALSE, shiny::plotOutput("r_people_plot", height = "420px")), 
                bslib::card(fill = FALSE, shiny::plotOutput("r_animals_plot", height = "420px")), bslib::card(fill = FALSE, 
                  shiny::plotOutput("r_objects_plot", height = "420px")))), bslib::nav_panel("Calendar", 
            shiny::dateInput("r_cal_date", "Selected date", weekstart = 1), shiny::selectInput("r_cal_view", 
                "View", c(Day = "day", Week = "week", Month = "month"), selected = "month"), shiny::selectInput("r_cal_fill", 
                "Colour by", c(`Sleep quality` = "sleep_quality", Stress = "stress", `Hours slept` = "sleep_hours")), 
            shiny::div(class = "dream-sky", shiny::uiOutput("r_cal_summary"), shiny::uiOutput("r_cal_grid"), 
                shiny::uiOutput("r_cal_night"))), bslib::nav_panel("Sleep", shiny::radioButtons("r_sleep_window", 
            "Window", c(`7 nights` = 7, `14 nights` = 14, `30 nights` = 30, All = 0), selected = 14, 
            inline = TRUE), shiny::div(class = "sleep-sky", shiny::uiOutput("r_sleep_ring"), shiny::uiOutput("r_sleep_bubbles")), 
            bslib::layout_columns(fill = FALSE, bslib::card(fill = FALSE, shiny::plotOutput("r_sleep_hours")), 
                bslib::card(fill = FALSE, shiny::plotOutput("r_sleep_windows"))), shiny::uiOutput("r_recommendations"), 
            shiny::helpText("General sleep-hygiene advice, not medical advice."), shiny::downloadButton("r_story", 
                "Download sleep / dream stats picture")), bslib::nav_panel("Search", shiny::textInput("r_search", 
            "Search text or reviewed labels"), shiny::uiOutput("r_search_results")), bslib::nav_panel("Settings / export", 
            bslib::card(fill = FALSE, bslib::card_header("Content-code definitions"), shiny::selectizeInput("r_definition_code", 
                "Code", choices = character(), options = list(create = TRUE)), shiny::textAreaInput("r_definition", 
                "Definition / inclusion guidance", rows = 3), shiny::actionButton("r_define", "Save definition"), 
                shiny::tableOutput("r_definitions")), bslib::card(fill = FALSE, bslib::card_header("Rename or merge labels across all entries"), 
                shiny::selectInput("r_rename_kind", "Label type", c(`Content codes` = "codes", `Emotions (both stages)` = "emotion")), 
                shiny::selectInput("r_rename_from", "Existing label", choices = character()), shiny::textInput("r_rename_to", 
                  "Replacement label"), shiny::checkboxInput("r_rename_confirm", "Apply this change to all saved entries", 
                  FALSE), shiny::actionButton("r_rename", "Rename / merge"), shiny::helpText("Merging combines counts without counting a night twice. The replacement code's existing definition is kept; otherwise the old definition is transferred.")), 
            bslib::card(fill = FALSE, bslib::card_header("Remember names locally"), shiny::textInput("r_name_alias", 
                "Name or alias appearing in text, e.g. Paul"), shiny::textInput("r_name_person", "Person label, e.g. Paul Smith"), 
                shiny::actionButton("r_remember", "Remember name / alias"), shiny::tableOutput("r_names"), 
                shiny::selectInput("r_forget_alias", "Alias to remove", choices = character()), shiny::actionButton("r_forget", 
                  "Forget alias"), shiny::helpText("Remembering an alias affects future suggestions only; it does not silently rewrite old entries.")), 
            bslib::card(fill = FALSE, bslib::card_header("Import NRC VAD v2"), shiny::tags$a(href = "https://saifmohammad.com/WebPages/nrc-vad.html", 
                target = "_blank", "Official NRC VAD download and usage terms"), shiny::helpText("Download v2 yourself and follow its documentation. Prepare a UTF-8 CSV with term,valence,arousal,dominance. Use the documented mean scores, not rating counts or standard deviations. No lexicon data is bundled."), 
                shiny::fileInput("r_vad_file", "Prepared VAD CSV", accept = ".csv"), shiny::selectInput("r_vad_scale", 
                  "Documented score scale (verify against your file)", c(`−1 to +1` = "signed", `0 to 1` = "unit")), 
                shiny::textInput("r_vad_source", "Source / citation", "NRC VAD Lexicon v2 — Mohammad (2025)"), 
                shiny::checkboxInput("r_vad_confirm", "I checked the file version, score scale, column mapping and usage terms", 
                  FALSE), shiny::actionButton("r_import_vad", "Import lexicon locally"), shiny::textOutput("r_vad_info")), 
            bslib::card(fill = FALSE, bslib::card_header("Export"), shiny::downloadButton("r_export", 
                "Export combined CSV"), shiny::downloadButton("r_snapshot", "Download full reviewed-journal backup (.rds)"), 
                shiny::helpText("The reviewed app uses its own RDS snapshot. Your original CSV is read only on first launch and never overwritten. Restart with a new state_path to import a different original CSV. Export contains sensitive dream text; do not commit it to GitHub."))))
}

reviewed_dream_server <- function(journal_path, state_path, lexicon_dir) {
    dictionaries <- review_entity_dictionaries(lexicon_dir)
    function(input, output, session) {
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
                shiny::tagList(shiny::p("During Dreaming: ", r$during_status, " · ", r$during), shiny::p("After Waking: ", 
                  r$after_status, " · ", r$after), shiny::p("People: ", r$people, " · Animals: ", r$animals, 
                  " · Objects: ", r$objects)))
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
                  shiny::p("Codes: ", x$codes[i]), shiny::p("During: ", x$during[i], " · After: ", x$after[i]), 
                  shiny::p("People: ", x$people[i], " · Animals: ", x$animals[i], " · Objects: ", x$objects[i]))
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
        output$r_vad_info <- shiny::renderText(sprintf("%d terms stored locally. %s · %s", nrow(state()$vad), 
            state()$vad_version, state()$vad_source))
        output$r_export <- shiny::downloadHandler(filename = function() paste0("reviewed-journal-", Sys.Date(), 
            ".csv"), content = function(file) {
            readr::write_csv(dplyr::left_join(journal(), reviews(), by = "date"), file)
        })
        output$r_snapshot <- shiny::downloadHandler(filename = function() paste0("reviewed-journal-", 
            Sys.Date(), ".rds"), content = function(file) saveRDS(state(), file))
    }
}

`%||%` <- function(x, fallback) if (is.null(x)) fallback else x

run_reviewed_dream_tests <- function() {
    if (!requireNamespace("testthat", quietly = TRUE)) 
        stop("Install testthat first: install.packages(\"testthat\")")
    testthat::test_that("labels are normalised without merging synonyms", {
        testthat::expect_identical(review_normalise(c(" Happy ", "happy", "VERY  SAD", "worried", "anxious")), 
            c("happy", "very sad", "worried", "anxious"))
        testthat::expect_error(review_pack("bad;label"))
    })
    testthat::test_that("legacy emotions are not assigned a stage", {
        j <- new_entry("2026-01-01", "23:00", "07:00", 6, 5, "I saw Paul.", "work", "fear")
        s <- review_initial_state(j)
        testthat::expect_equal(s$reviews$during, "")
        testthat::expect_equal(s$reviews$after_status, "unanswered")
        testthat::expect_equal(s$journal$emotions, "fear")
    })
    testthat::test_that("entity matches respect boundaries and remembered names", {
        d <- tibble::tibble(category = c("car", "friend"), word = c("car", "best friend"))
        testthat::expect_identical(review_detect("My best friend stood on a carpet.", d), "friend")
        testthat::expect_length(review_detect("best. Friend", d), 0)
        n <- tibble::tibble(alias = "paul", person = "paul smith")
        result <- review_suggest_entities("Paul arrived.", list(people = d, animals = d[0, ], objects = d[0, 
            ]), n)
        testthat::expect_identical(result$people, "paul smith")
    })
    testthat::test_that("emotion counts are per night and preserve stage zeros", {
        r <- review_row("2026-01-01")
        r$during <- "happy;happy"
        r$during_status <- "reported"
        x <- review_emotion_counts(r)
        testthat::expect_equal(x$nights[x$stage == "during"], 1L)
        testthat::expect_equal(x$nights[x$stage == "after"], 0L)
        testthat::expect_error(review_validate_emotions("none", "happy"))
        testthat::expect_error(review_validate_emotions("reported", character()))
    })
    testthat::test_that("VAD uses complete labels and excludes unmatched terms only from scoring", {
        r <- review_row("2026-01-01")
        r$during <- "happy;very happy"
        r$during_status <- "reported"
        vad <- tibble::tibble(term = "happy", valence = 0.8, arousal = 0.4, dominance = 0.2)
        x <- review_vad_matches(r, vad)
        testthat::expect_equal(nrow(x), 2L)
        testthat::expect_true(is.na(x$valence[x$label == "very happy"]))
        file <- tempfile(fileext = ".csv")
        on.exit(unlink(file), add = TRUE)
        readr::write_csv(vad, file)
        testthat::expect_equal(nrow(review_import_vad(file, c(-1, 1))), 1L)
        vad$dominance <- 5
        readr::write_csv(vad, file)
        testthat::expect_error(review_import_vad(file, c(-1, 1)))
    })
    testthat::test_that("snapshot round-trip preserves original journal fields", {
        s <- review_initial_state(new_entry("2026-01-01", "23:00", "07:00", 6, 5))
        file <- tempfile(fileext = ".rds")
        on.exit(unlink(c(file, paste0(file, ".previous"))), add = TRUE)
        review_write_state(s, file)
        testthat::expect_equal(readRDS(file), s)
        review_write_state(s, file)
        testthat::expect_true(file.exists(paste0(file, ".previous")))
    })
    testthat::test_that("frequency plot keeps common order and zero bars", {
        x <- tibble::tibble(label = c("happy", "sad"), nights = c(2L, 0L))
        p <- review_frequency_plot(x, "Test", order = c("happy", "sad"), limit = 3)
        testthat::expect_no_error(ggplot2::ggplot_build(p))
        testthat::expect_s3_class(review_frequency_plot(x[0, ], "Empty"), "ggplot")
    })
    invisible(TRUE)
}

run_fixed_reviewed_app <- function(
    journal_path = "data/dream_journal.csv",
    state_path = "data/reviewed_dream_journal.rds",
    lexicon_dir = "data/lexicons", src_dir = "src", ...) {
  load_reviewed_app_support(src_dir)
  if (!file.exists(state_path)) {
    stop("Main snapshot not found: ", state_path,
         "\nInclude the saved main RDS snapshot. No demo was generated or copied.")
  }
  s <- readRDS(state_path)
  review_validate_state(s)
  if (!nrow(s$vad)) warning("No VAD lexicon is saved in this snapshot. Import it under Settings / export.", call. = FALSE)
  ui <- reviewed_dream_ui()
  if (isTRUE(s$simulation$synthetic)) {
    ui <- htmltools::tagList(
      shiny::tags$div(
        style = "padding:10px 18px;background:#fff3cd;color:#664d03;border-bottom:1px solid #e6d49a;font-size:14px;",
        shiny::tags$strong("SYNTHETIC DATA — "),
        "Dreaming emotions come from the simulator. Waking responses are simulated separately. Entity confirmations are automatic, not human review."
      ), ui)
  }
  shiny::runApp(shiny::shinyApp(ui,
    reviewed_dream_server(journal_path, state_path, lexicon_dir)), ...)
}

