# Reviewed journal state: the journal plus a review row per night (content
# codes, emotions during/after the dream, people/animals/objects). The state
# is saved as one RDS snapshot (data/reviewed_dream_journal.rds).

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

review_status_summary <- function(reviews, stage) {
    x <- reviews[[paste0(stage, "_status")]]
    sprintf("%d logged nights \u00B7 %d answered (%d with emotions; %d no emotion noticed; %d don't remember) \u00B7 %d unanswered", 
        length(x), sum(x != "unanswered"), sum(x == "reported"), sum(x == "none"), sum(x == "dont_remember"), 
        sum(x == "unanswered"))
}

# Fallback for NULL inputs (Shiny inputs are NULL until set).
`%||%` <- function(x, fallback) if (is.null(x)) fallback else x
