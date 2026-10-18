# Rule-based sleep routine recommendations. These are general sleep-hygiene
# suggestions from the literature, not medical advice.

recommendation_rules <- list(
  min_nights = 3,
  min_sleep_hours = 7,         # Hirshkowitz et al. (2015): adults 7-9 h
  max_bedtime_sd_hours = 1,    # Phillips et al. (2017): sleep regularity
  latest_mean_bedtime = "00:30",
  high_stress = 7,
  low_quality = 5,
  max_negative_share = 0.5
)

#' Recommendations based on the most recent nights.
#'
#' @param journal A journal tibble.
#' @param emotion_meta Tibble with `emotion` and `valence`; emotions with
#'   negative valence count as negative dreams.
#' @param window_days Number of most recent nights considered.
#' @param rules Thresholds; see `recommendation_rules`.
#' @return Tibble with `title`, `message` and `source`, one row per advice.
sleep_recommendations <- function(journal, emotion_meta, window_days = 14,
                                  rules = recommendation_rules) {
  recent <- if (nrow(journal) == 0) journal else
    filter(journal, date > max(date) - window_days)
  if (nrow(recent) < rules$min_nights) {
    return(tibble(
      title = "Keep logging",
      message = paste0("Log at least ", rules$min_nights,
                       " nights to get personal recommendations."),
      source = ""
    ))
  }

  bedtime_hours <- clock_to_evening_hours(recent$bedtime)
  negative_emotions <- emotion_meta$emotion[emotion_meta$valence < 0]
  dream_nights <- recent |> filter(emotions != "")
  negative_share <- if (nrow(dream_nights) == 0) 0 else
    mean(vapply(split_tags(dream_nights$emotions),
                \(x) any(x %in% negative_emotions), logical(1)))

  advice <- list()
  add <- function(title, message, source) {
    advice[[length(advice) + 1]] <<- tibble(title, message, source)
  }

  if (mean(recent$sleep_hours) < rules$min_sleep_hours) {
    add("Sleep a little longer",
        sprintf("You slept %.1f hours on average. Adults are advised to sleep 7-9 hours; try moving your bedtime 15 minutes earlier each week.",
                mean(recent$sleep_hours)),
        "Hirshkowitz et al. (2015)")
  }
  if (sd(bedtime_hours) > rules$max_bedtime_sd_hours) {
    add("Keep a regular bedtime",
        sprintf("Your bedtime varied by about %.1f hours (SD). Going to bed and getting up at similar times, also on weekends, supports better sleep.",
                sd(bedtime_hours)),
        "Phillips et al. (2017)")
  }
  if (mean(bedtime_hours) > clock_to_evening_hours(rules$latest_mean_bedtime)) {
    add("Wind down earlier",
        paste0("Your average bedtime was ", evening_hours_to_clock(mean(bedtime_hours)),
               ". Dim lights and put screens away an hour before you want to sleep."),
        "Irish et al. (2015)")
  }
  if (mean(recent$stress) >= rules$high_stress) {
    add("Make room for relaxation",
        sprintf("Your average stress was %.1f/10. A short relaxation routine before bed (breathing, writing down worries) can make it easier to fall asleep.",
                mean(recent$stress)),
        "Irish et al. (2015)")
  }
  if (mean(recent$sleep_quality) <= rules$low_quality) {
    add("Check the basics",
        sprintf("Your average sleep quality was %.1f/10. Avoid caffeine after lunch and alcohol before bed, and keep the bedroom dark, quiet and cool.",
                mean(recent$sleep_quality)),
        "Irish et al. (2015)")
  }
  if (negative_share > rules$max_negative_share) {
    add("Many unpleasant dreams",
        sprintf("%.0f%% of your recalled dreams had negative emotions. If nightmares keep coming back and bother you, imagery rehearsal therapy is an effective treatment; talk to a GP or psychologist.",
                100 * negative_share),
        "Krakow & Zadra (2006)")
  }

  if (length(advice) == 0) {
    return(tibble(title = "Your routine looks healthy",
                  message = "Nothing stood out in your recent nights. Keep it up!",
                  source = ""))
  }
  bind_rows(advice)
}

recommendation_references <- c(
  "Hirshkowitz, M., et al. (2015). National Sleep Foundation's sleep time duration recommendations: Methodology and results summary. Sleep Health, 1(1), 40-43.",
  "Irish, L. A., Kline, C. E., Gunn, H. E., Buysse, D. J., & Hall, M. H. (2015). The role of sleep hygiene in promoting public health: A review of empirical evidence. Sleep Medicine Reviews, 22, 23-36.",
  "Krakow, B., & Zadra, A. (2006). Clinical management of chronic nightmares: Imagery rehearsal therapy. Behavioral Sleep Medicine, 4(1), 45-70.",
  "Phillips, A. J. K., et al. (2017). Irregular sleep/wake patterns are associated with poorer academic performance and delayed circadian and sleep/wake timing. Scientific Reports, 7, 3216."
)
