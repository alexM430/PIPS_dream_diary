# Word-list (dictionary) based features of dream texts.
#
# Each word list is a CSV with columns `category,word`. A dream mentions a
# category when any of its words appears as a whole word in the text. To add a
# theme, person or object, add rows to the CSVs in data/lexicons/.

#' Load all word lists and their metadata.
#'
#' @param lexicon_dir Folder with themes.csv, emotions.csv, people.csv,
#'   objects.csv, theme_meta.csv and emotion_meta.csv.
#' @return A named list of tibbles.
load_lexicons <- function(lexicon_dir = "data/lexicons") {
  files <- c("themes", "emotions", "people", "objects",
             "theme_meta", "emotion_meta")
  paths <- file.path(lexicon_dir, paste0(files, ".csv"))
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop("Word list files not found: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  lexicons <- lapply(paths, read_csv, show_col_types = FALSE)
  names(lexicons) <- files
  for (name in c("themes", "emotions", "people", "objects")) {
    lexicons[[name]] <- mutate(lexicons[[name]], word = str_to_lower(word))
  }
  lexicons
}

#' Split text into lowercase words ("couldn't" becomes "couldnt").
#'
#' @param text A single string.
#' @return Character vector of words.
tokenize_text <- function(text) {
  text <- str_to_lower(coalesce(text, ""))
  text <- str_remove_all(text, "['\u2019]")
  words <- str_split(text, "[^a-z]+")[[1]]
  words[words != ""]
}

#' Categories of a word list that occur in a text, most frequent first.
#'
#' @param text A single string.
#' @param lexicon Tibble with columns `category` and `word`.
#' @return Character vector of unique categories (empty if none match).
detect_categories <- function(text, lexicon) {
  words <- tokenize_text(text)
  hits <- lexicon$category[match(words, lexicon$word)]
  hits <- hits[!is.na(hits)]
  if (length(hits) == 0) return(character())
  names(sort(table(hits), decreasing = TRUE))
}

#' Count how often each category is mentioned per night.
#'
#' @param journal A journal tibble.
#' @param lexicon Tibble with columns `category` and `word`.
#' @return Tibble with `date`, `category` and `n` (mentions that night).
count_mentions <- function(journal, lexicon) {
  journal |>
    transmute(date, word = lapply(dream_text, tokenize_text)) |>
    unnest_longer(word) |>
    inner_join(lexicon, by = "word", relationship = "many-to-many") |>
    count(date, category, name = "n")
}

#' Valence and arousal of each night's dream from its emotion tags.
#'
#' Each emotion has a fixed position on Russell's (1980) circumplex
#' (emotion_meta.csv); a dream's position is the mean of its emotions.
#'
#' @param journal A journal tibble.
#' @param emotion_meta Tibble with `emotion`, `valence` and `arousal`.
#' @return The journal with `valence` and `arousal` added (NA when a night has
#'   no emotion tags).
add_affect_scores <- function(journal, emotion_meta) {
  scores <- unnest_tags(journal, "emotions") |>
    inner_join(emotion_meta, by = c("tag" = "emotion")) |>
    group_by(date) |>
    summarise(valence = mean(valence), arousal = mean(arousal),
              .groups = "drop")
  left_join(journal, scores, by = "date")
}
