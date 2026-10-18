# Simulates a dream journal for one dreamer.
#
# Data-generating model (one row per night):
#   stress        AR(1) around `stress_mean`, higher on weekdays (1-10)
#   bedtime       later on Friday/Saturday and on stressful days
#   sleep_hours   shorter after stressful days and late bedtimes
#   sleep_quality better with longer sleep, worse with stress (1-10)
#   dream tone    P(negative dream) = logistic(intercept
#                   + stress_on_negative * (stress - stress_mean)
#                   + sleep_on_negative * (sleep_hours - sleep_mean))
# Negative dreams draw "negative" themes and emotions, positive dreams the
# rest. Dream texts are built from sentence templates whose words are in the
# word lists, so the app's word-list detection can recover them.

negative_themes <- c("falling", "chased", "exams", "teeth", "lost", "death", "work")
positive_themes <- c("flying", "water", "romance", "family", "travel")
negative_emotions <- c("fear", "anger", "sadness", "shame", "confusion")
positive_emotions <- c("joy", "calm", "surprise")

theme_templates <- list(
  falling  = c("I was standing at the edge of a cliff with my {person} and then I fell.",
               "I kept falling down an endless staircase while holding a {object}."),
  chased   = c("Someone was chasing me through {place} and I tried to hide behind a {object}.",
               "I was running away from my {person} but my legs would not move."),
  flying   = c("I was flying high above {place}.",
               "I could float over the rooftops together with my {person}."),
  water    = c("I was swimming in a dark ocean next to my {person}.",
               "The streets were flooded and the water kept rising around my {object}."),
  exams    = c("I was back in school taking an exam I had not studied for.",
               "My {person} handed me a test in a classroom I did not recognise."),
  teeth    = c("My teeth started crumbling while I talked to my {person}.",
               "I looked in a mirror and my teeth were loose."),
  lost     = c("I was lost in {place} and could not find the way back.",
               "I wandered through a maze looking for my {object}."),
  death    = c("I went to a funeral and my {person} was there.",
               "There was a ghost standing next to the {object}."),
  romance  = c("I kissed my {person} at a wedding.",
               "I was on a date with my {person} in {place}."),
  family   = c("I had dinner with my family in my childhood home.",
               "My {person} and I were cleaning the old house."),
  travel   = c("I was at an airport and about to miss my plane.",
               "I was on a train going through {place} with a {object}."),
  work     = c("I was in a meeting and my boss asked for the report.",
               "I was at the office trying to finish a deadline on my computer.")
)

emotion_words <- list(
  joy = c("happy", "excited", "delighted"), calm = c("calm", "peaceful", "relaxed"),
  surprise = c("surprised", "amazed", "shocked"), fear = c("scared", "terrified", "afraid"),
  anger = c("angry", "furious", "frustrated"), sadness = c("sad", "lonely", "hopeless"),
  shame = c("embarrassed", "ashamed", "guilty"), confusion = c("confused", "puzzled", "unsure")
)

# This dreamer's recurring cast and props; weights make some recur more often.
dream_people <- c(mother = 5, friend = 4, boss = 3, sister = 2, partner = 2,
                  teacher = 1, stranger = 2, grandmother = 1)
dream_objects <- c(phone = 4, door = 3, car = 3, key = 2, dog = 2, bag = 2,
                   clock = 1, book = 1, snake = 1)
dream_places <- c("a dark forest", "a huge city", "a long hallway",
                  "a crowded market", "the beach", "an empty parking lot")

#' Simulate a dream journal for one dreamer.
#'
#' @param n_nights Number of consecutive nights.
#' @param start_date First night (fixed by default so results are reproducible).
#' @param seed Random seed.
#' @param stress_mean Average daily stress (1-10).
#' @param sleep_mean Average sleep duration in hours.
#' @param stress_on_sleep Change in sleep hours per stress point.
#' @param stress_on_negative,sleep_on_negative Log-odds change in a negative
#'   dream per stress point and per extra hour of sleep.
#' @param recall_prob Probability that a dream is remembered on a given night.
#' @return A journal tibble (see `journal_template()`).
simulate_dream_journal <- function(n_nights = 180,
                                   start_date = as.Date("2026-01-01"),
                                   seed = 2026,
                                   stress_mean = 5,
                                   sleep_mean = 7.4,
                                   stress_on_sleep = -0.18,
                                   stress_on_negative = 0.45,
                                   sleep_on_negative = -0.5,
                                   recall_prob = 0.8) {
  if (n_nights < 1) stop("n_nights must be at least 1.", call. = FALSE)
  if (recall_prob < 0 || recall_prob > 1) {
    stop("recall_prob must be between 0 and 1.", call. = FALSE)
  }
  set.seed(seed)

  dates <- start_date + seq_len(n_nights) - 1
  weekday <- wday(dates, week_start = 1)  # 1 = Monday
  is_weekend_night <- weekday %in% c(5, 6)  # Friday and Saturday night
  workday_bump <- ifelse(weekday <= 4, 0.7, -0.8)

  stress_raw <- numeric(n_nights)
  previous <- stress_mean
  for (i in seq_len(n_nights)) {
    previous <- stress_mean + 0.6 * (previous - stress_mean) +
      workday_bump[i] + rnorm(1, 0, 1.3)
    stress_raw[i] <- previous
  }
  stress <- clamp_round(stress_raw, 1, 10)

  bedtime_hours <- pmin(pmax(
    5.3 + 0.8 * is_weekend_night + 0.1 * (stress - stress_mean) +
      rnorm(n_nights, 0, 0.5), 2), 10)
  sleep_hours <- pmin(pmax(
    sleep_mean + stress_on_sleep * (stress - stress_mean) -
      0.4 * (bedtime_hours - 5.3) * (!is_weekend_night) +
      rnorm(n_nights, 0, 0.6), 3.5), 11)

  bedtime <- evening_hours_to_clock(bedtime_hours)
  wake_time <- evening_hours_to_clock(bedtime_hours + sleep_hours)
  sleep_hours <- round(sleep_duration_hours(bedtime, wake_time), 2)

  sleep_quality <- clamp_round(
    6 + 0.8 * (sleep_hours - sleep_mean) - 0.35 * (stress - stress_mean) +
      rnorm(n_nights, 0, 1), 1, 10)

  p_negative <- plogis(-0.2 + stress_on_negative * (stress - stress_mean) +
                         sleep_on_negative * (sleep_hours - sleep_mean))
  is_negative <- runif(n_nights) < p_negative
  recalled <- runif(n_nights) < recall_prob

  dreams <- lapply(seq_len(n_nights), function(i) {
    if (!recalled[i]) return(list(text = "", themes = "", emotions = ""))
    simulate_dream(is_negative[i])
  })

  tibble(
    date = dates,
    bedtime = bedtime,
    wake_time = wake_time,
    sleep_hours = sleep_hours,
    sleep_quality = as.integer(sleep_quality),
    stress = as.integer(stress),
    dream_text = vapply(dreams, `[[`, character(1), "text"),
    themes = vapply(dreams, `[[`, character(1), "themes"),
    emotions = vapply(dreams, `[[`, character(1), "emotions")
  )
}

# One dream: 1-2 themes and 1-2 emotions matching its tone, plus a text.
simulate_dream <- function(negative) {
  theme_pool <- if (negative) negative_themes else positive_themes
  emotion_pool <- if (negative) negative_emotions else positive_emotions
  themes <- sample(theme_pool, sample(1:2, 1, prob = c(0.6, 0.4)))
  emotions <- sample(emotion_pool, sample(1:2, 1, prob = c(0.6, 0.4)))

  theme_sentences <- vapply(themes, function(theme) {
    fill_template(sample(theme_templates[[theme]], 1))
  }, character(1))
  emotion_sentences <- vapply(emotions, function(emotion) {
    paste0("I felt ", sample(emotion_words[[emotion]], 1), ".")
  }, character(1))

  list(
    text = paste(c(theme_sentences, emotion_sentences), collapse = " "),
    themes = join_tags(themes),
    emotions = join_tags(emotions)
  )
}

fill_template <- function(template) {
  template |>
    str_replace("\\{person\\}", sample(names(dream_people), 1, prob = dream_people)) |>
    str_replace("\\{object\\}", sample(names(dream_objects), 1, prob = dream_objects)) |>
    str_replace("\\{place\\}", sample(dream_places, 1))
}

clamp_round <- function(x, lower, upper) {
  pmin(pmax(round(x), lower), upper)
}
