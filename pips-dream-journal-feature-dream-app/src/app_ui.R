# User interface of the Dream Journal app (originally Anna's reviewed app),
# plus the cloudy loading screen and the colour-theme menu.

#' Colour presets for the "App colour theme" menu: accent, page background
#' and text colour.
app_colour_presets <- list(
  lavender = list(label = "Lavender (original)", primary = "#5B4B8A", bg = "#FBFAFD", fg = "#222222"),
  ocean = list(label = "Ocean blue", primary = "#075985", bg = "#F0F9FF", fg = "#172554"),
  forest = list(label = "Forest green", primary = "#166534", bg = "#F0FDF4", fg = "#14291C")
)

#' Build the app UI.
#'
#' @param css_paths Stylesheets for the calendar and Sleep tab.
#' @param splash_seconds Minimum seconds the cloudy loading screen is shown;
#'   0 turns it off.
#' @return A bslib page.
dream_app_ui <- function(css_paths = c("src/www/dream_calendar.css", "src/www/sleep_view.css"),
                         splash_seconds = 3) {
    tags_input <- function(id, label) shiny::selectizeInput(id, label, choices = character(), multiple = TRUE, 
        options = list(create = TRUE, persist = FALSE, plugins = list("remove_button"), placeholder = "Type a label and press Enter"))
    status_input <- function(id) shiny::selectInput(id, "Response", c(Unanswered = "unanswered", `Emotions reported` = "reported", 
        `I don't remember` = "dont_remember", `No emotion noticed` = "none"))
    bslib::page_navbar(fillable = FALSE, title = "Dream Journal", theme = bslib::bs_theme(version = 5, 
        primary = "#5B4B8A", bg = "#FBFAFD", fg = "#222222"), header = shiny::tagList(shiny::tags$head(shiny::tags$link(rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Quicksand:wght@500;600;700&display=swap"),
        lapply(css_paths[file.exists(css_paths)], shiny::includeCSS)),
        if (splash_seconds > 0) splash_screen(splash_seconds)), sidebar = bslib::sidebar(width = 320,
        # The user sees labels; Shiny receives the values (see app_colour_presets).
        shiny::selectInput("app_colour_theme", "App colour theme",
            choices = stats::setNames(names(app_colour_presets), vapply(app_colour_presets, `[[`, "", "label")),
            selected = "lavender"),
        shiny::dateRangeInput("r_period", 
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
                tags_input("r_after", "Emotions after waking"), shiny::helpText("Multiple custom labels are allowed. Use \u00D7 to remove a label; type its replacement to edit it. Existing emotion words are suggestions, not restrictions."), 
                shiny::actionButton("r_save", "Save reviewed entry", class = "btn-primary")), bslib::card(fill = FALSE, 
                bslib::card_header("Review people, animals and objects"), shiny::actionButton("r_detect", 
                  "Suggest entities from current text"), shiny::helpText("Suggestions are added to the lists below. Remove incorrect items with \u00D7; type missed items and press Enter. To move an item, remove it here and add it to the correct group."), 
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
                  "Documented score scale (verify against your file)", stats::setNames(c("signed", "unit"), c("\u22121 to +1", "0 to 1"))), 
                shiny::textInput("r_vad_source", "Source / citation", "NRC VAD Lexicon v2 \u2014 Mohammad (2025)"), 
                shiny::checkboxInput("r_vad_confirm", "I checked the file version, score scale, column mapping and usage terms", 
                  FALSE), shiny::actionButton("r_import_vad", "Import lexicon locally"), shiny::textOutput("r_vad_info")), 
            bslib::card(fill = FALSE, bslib::card_header("Export"), shiny::downloadButton("r_export", 
                "Export combined CSV"), shiny::downloadButton("r_snapshot", "Download full reviewed-journal backup (.rds)"), 
                shiny::helpText("The reviewed app uses its own RDS snapshot. Your original CSV is read only on first launch and never overwritten. Restart with a new state_path to import a different original CSV. Export contains sensitive dream text; do not commit it to GitHub."))))
}
