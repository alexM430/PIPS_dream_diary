# simulate_journal.R
# Complete replacement for the pasted simulator.
# Requires the existing evening_hours_to_clock() and sleep_duration_hours()
# helpers from the project. Other package calls are namespaced.
# No function here overwrites a journal CSV or the app's existing RDS snapshot.
# All numeric defaults are ASSUMPTIONS unless a verified source is supplied.

negative_themes <- c("falling", "chased", "exams", "teeth", "lost", "death", "work")
positive_themes <- c("flying", "water", "romance", "family", "travel")
negative_emotions <- c("fear", "anger", "sadness", "shame", "confusion")
positive_emotions <- c("joy", "calm", "surprise")

# These pool names reproduce the old demonstration, NOT psychological norms.
# Confusion/surprise can be ambiguous; travel/water need not be pleasant.
# Legacy mode is kept by default so existing downstream code remains usable.
theme_templates <- list(
  falling = c("I was standing at the edge of a cliff with my {person} and then I fell.",
              "I kept falling down an endless staircase while holding a {object}."),
  chased = c("Someone was chasing me through {place} and I tried to hide behind a {object}.",
             "I was running away from my {person} but my legs would not move."),
  flying = c("I was flying high above {place}.",
             "I could float over the rooftops together with my {person}."),
  water = c("I was swimming in a dark ocean next to my {person}.",
            "The streets were flooded and the water kept rising around my {object}."),
  exams = c("I was back in school taking an exam I had not studied for.",
            "My {person} handed me a test in a classroom I did not recognise."),
  teeth = c("My teeth started crumbling while I talked to my {person}.",
            "I looked in a mirror and my teeth were loose."),
  lost = c("I was lost in {place} and could not find the way back.",
           "I wandered through a maze looking for my {object}."),
  death = c("I went to a funeral and my {person} was there.",
            "There was a ghost standing next to the {object}."),
  romance = c("I kissed my {person} at a wedding.",
              "I was on a date with my {person} in {place}."),
  family = c("I had dinner with my family in my childhood home.",
             "My {person} and I were cleaning the old house."),
  travel = c("I was at an airport and about to miss my plane.",
             "I was on a train going through {place} with a {object}."),
  work = c("I was in a meeting and my boss asked for the report.",
           "I was at the office trying to finish a deadline on my computer.")
)
emotion_words <- list(
  joy = c("happy", "excited", "delighted"),
  calm = c("calm", "peaceful", "relaxed"),
  surprise = c("surprised", "amazed", "shocked"),
  fear = c("scared", "terrified", "afraid"),
  anger = c("angry", "furious", "frustrated"),
  sadness = c("sad", "lonely", "hopeless"),
  shame = c("embarrassed", "ashamed", "guilty"),
  confusion = c("confused", "puzzled", "unsure")
)
dream_people <- c(mother = 5, friend = 4, boss = 3, sister = 2, partner = 2,
                  teacher = 1, stranger = 2, grandmother = 1)
dream_objects <- c(phone = 4, door = 3, car = 3, key = 2, dog = 2, bag = 2,
                   clock = 1, book = 1, snake = 1)
dream_places <- c("a dark forest", "a huge city", "a long hallway",
                  "a crowded market", "the beach", "an empty parking lot")

# Additional settings: use simulation_settings(sleep_sd = 0.6), for example.
simulation_settings <- function(...) {
  defaults <- list(
    stress_autocorrelation = 0.6, stress_innovation_sd = 1.3,
    weekday_stress_bump = 0.7, other_day_stress_bump = -0.8,
    bedtime_mean = 5.3, weekend_bedtime_bonus = 0.8,
    stress_on_bedtime = 0.1, bedtime_sd = 0.5,
    bedtime_lower = 2, bedtime_upper = 10,
    sleep_sd = 1.0, weekend_sleep_bonus = 0.6,
    late_bedtime_on_sleep = -0.4,
    short_night_prob = 0.08, short_night_reduction = 1.5,
    sleep_lower = 3.5, sleep_upper = 11,
    quality_mean = 6, sleep_on_quality = 0.8,
    stress_on_quality = -0.35, quality_sd = 1,
    rating_lower = 1, rating_upper = 10,
    rating_heap_prob = 0.10, stress_heap_value = 5, quality_heap_value = 6,
    negative_intercept = -0.2, recall_on_sleep = 0.20,
    missing_prob = 0, theme_tone_strength = 1,
    mixed_emotion_prob = 0, two_label_prob = 0.4
  )
  overrides <- list(...)
  if (length(overrides) && (is.null(names(overrides)) || any(!nzchar(names(overrides)))))
    stop("All settings must be named.")
  unknown <- setdiff(names(overrides), names(defaults))
  if (length(unknown)) stop("Unknown settings: ", paste(unknown, collapse = ", "))
  utils::modifyList(defaults, overrides)
}

.sim_validate_scalar <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value))
    stop(name, " must be one finite number.", call. = FALSE)
}
.sim_validate_settings <- function(cfg) {
  expected <- names(simulation_settings())
  if (!is.list(cfg) || !setequal(names(cfg), expected))
    stop("Build settings with simulation_settings().")
  for (name in expected) .sim_validate_scalar(cfg[[name]], name)
  for (name in c("short_night_prob", "rating_heap_prob", "missing_prob",
                 "theme_tone_strength", "mixed_emotion_prob", "two_label_prob"))
    if (cfg[[name]] < 0 || cfg[[name]] > 1) stop("Invalid probability: ", name)
  for (name in c("stress_innovation_sd", "bedtime_sd", "sleep_sd", "quality_sd"))
    if (cfg[[name]] <= 0) stop(name, " must be positive.")
  if (abs(cfg$stress_autocorrelation) >= 1) stop("Autocorrelation must be between -1 and 1.")
  if (cfg$short_night_reduction < 0) stop("Short-night reduction must be nonnegative.")
  if (cfg$bedtime_lower >= cfg$bedtime_upper || cfg$sleep_lower >= cfg$sleep_upper ||
      cfg$rating_lower >= cfg$rating_upper) stop("Invalid bounds.")
  if (cfg$sleep_lower <= 0 || cfg$sleep_upper >= 24) stop("Sleep bounds must be within 0 and 24 hours.")
  if (cfg$rating_lower != 1 || cfg$rating_upper != 10)
    stop("Keep ratings on 1-10 for compatibility with the app.")
  for (name in c("stress_heap_value", "quality_heap_value"))
    if (cfg[[name]] < 1 || cfg[[name]] > 10 || cfg[[name]] != round(cfg[[name]]))
      stop("Heaping values must be integers from 1 to 10.")
  invisible(cfg)
}
clamp_round <- function(x, lower, upper) pmin(pmax(round(x), lower), upper)
.sim_pack <- function(x) paste(unique(x), collapse = ";")
.sim_pick <- function(x, n = 1L, prob = NULL) x[sample.int(length(x), n, prob = prob)]
fill_template <- function(template) {
  template |>
    stringr::str_replace("\\{person\\}", .sim_pick(names(dream_people), prob = dream_people)) |>
    stringr::str_replace("\\{object\\}", .sim_pick(names(dream_objects), prob = dream_objects)) |>
    stringr::str_replace("\\{place\\}", .sim_pick(dream_places))
}

simulate_dream <- function(negative, theme_tone_strength = 1,
                           mixed_emotion_prob = 0, two_label_prob = 0.4) {
  if (length(negative) != 1 || is.na(negative)) stop("Choose one dream tone.")
  tone_pool <- if (negative) negative_themes else positive_themes
  # Lower strength allows themes from either pool, without changing true tone.
  theme_pool <- if (runif(1) < theme_tone_strength) tone_pool else names(theme_templates)
  n_theme <- 1L + as.integer(runif(1) < two_label_prob)
  themes <- .sim_pick(theme_pool, min(n_theme, length(theme_pool)))
  emotion_pool <- if (negative) negative_emotions else positive_emotions
  other_pool <- if (negative) positive_emotions else negative_emotions
  n_emotion <- 1L + as.integer(runif(1) < two_label_prob)
  emotions <- .sim_pick(emotion_pool, min(n_emotion, length(emotion_pool)))
  if (runif(1) < mixed_emotion_prob) emotions <- unique(c(emotions, .sim_pick(other_pool)))
  sentences <- vapply(themes, function(theme) fill_template(.sim_pick(theme_templates[[theme]])), "")
  feelings <- vapply(emotions, function(emotion)
    paste0("I felt ", .sim_pick(emotion_words[[emotion]]), "."), "")
  list(text = paste(c(sentences, feelings), collapse = " "),
       themes = .sim_pack(themes), emotions = .sim_pack(emotions))
}

# Existing positional arguments are preserved; new options follow them.
# n_nights is scheduled calendar nights, not necessarily returned diary rows.
# recall_prob is recall probability at sleep_mean, not the marginal rate.
# sleep_mean/stress_mean are reference levels, not guaranteed achieved means.
# return_details = TRUE returns journal, truth, parameters, provenance.
# Otherwise returns the same nine journal columns as before, with metadata attrs.
simulate_dream_journal <- function(n_nights = 180,
                                   start_date = as.Date("2026-01-01"), seed = 2026,
                                   stress_mean = 5, sleep_mean = 7.4, stress_on_sleep = -0.18,
                                   stress_on_negative = 0.45, sleep_on_negative = -0.5, recall_prob = 0.8,
                                   settings = simulation_settings(), return_details = FALSE) {
  for (name in c("n_nights", "seed", "stress_mean", "sleep_mean", "stress_on_sleep",
                 "stress_on_negative", "sleep_on_negative", "recall_prob"))
    .sim_validate_scalar(get(name), name)
  if (n_nights < 1 || n_nights != floor(n_nights)) stop("n_nights must be a positive integer.")
  if (seed < 0 || seed > .Machine$integer.max || seed != floor(seed)) stop("Invalid seed.")
  if (recall_prob < 0 || recall_prob > 1) stop("recall_prob must be between 0 and 1.")
  start_date <- as.Date(start_date)
  if (length(start_date) != 1 || is.na(start_date)) stop("Invalid start_date.")
  .sim_validate_settings(settings)
  cfg <- settings
  for (helper in c("evening_hours_to_clock", "sleep_duration_hours"))
    if (!exists(helper, mode = "function")) stop("Load the project's journal helpers first: ", helper)
  
  # Local reproducibility without changing the caller's RNG state permanently.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  dates <- start_date + seq_len(n_nights) - 1L
  weekday <- lubridate::wday(dates, week_start = 1)
  weekend <- weekday %in% c(5, 6)
  # Preserve the original Monday-Thursday / other-day convention.
  workday_bump <- ifelse(weekday <= 4, cfg$weekday_stress_bump, cfg$other_day_stress_bump)
  stress_raw <- numeric(n_nights)
  previous <- stress_mean
  for (i in seq_len(n_nights)) {
    previous <- stress_mean + cfg$stress_autocorrelation * (previous - stress_mean) +
      workday_bump[i] + rnorm(1, 0, cfg$stress_innovation_sd)
    stress_raw[i] <- previous
  }
  stress <- clamp_round(stress_raw, 1, 10)
  stress[runif(n_nights) < cfg$rating_heap_prob] <- cfg$stress_heap_value
  # The model uses recorded stress. Heaping is NOT latent measurement error here.
  bed <- pmin(pmax(cfg$bedtime_mean + cfg$weekend_bedtime_bonus * weekend +
                     cfg$stress_on_bedtime * (stress - stress_mean) +
                     rnorm(n_nights, 0, cfg$bedtime_sd), cfg$bedtime_lower), cfg$bedtime_upper)
  short <- runif(n_nights) < cfg$short_night_prob
  hours <- pmin(pmax(sleep_mean + stress_on_sleep * (stress - stress_mean) +
                       cfg$weekend_sleep_bonus * weekend +
                       cfg$late_bedtime_on_sleep * (bed - cfg$bedtime_mean) * (!weekend) -
                       cfg$short_night_reduction * short + rnorm(n_nights, 0, cfg$sleep_sd),
                     cfg$sleep_lower), cfg$sleep_upper)
  bedtime <- evening_hours_to_clock(bed)
  wake_time <- evening_hours_to_clock(bed + hours)
  hours <- round(sleep_duration_hours(bedtime, wake_time), 2)
  quality <- clamp_round(cfg$quality_mean + cfg$sleep_on_quality * (hours - sleep_mean) +
                           cfg$stress_on_quality * (stress - stress_mean) + rnorm(n_nights, 0, cfg$quality_sd), 1, 10)
  quality[runif(n_nights) < cfg$rating_heap_prob] <- cfg$quality_heap_value
  p_negative <- plogis(cfg$negative_intercept + stress_on_negative * (stress - stress_mean) +
                         sleep_on_negative * (hours - sleep_mean))
  negative <- runif(n_nights) < p_negative
  # Explicit endpoints avoid infinite-logit arithmetic.
  p_recall <- if (recall_prob %in% c(0, 1)) rep(recall_prob, n_nights) else
    plogis(qlogis(recall_prob) + cfg$recall_on_sleep * (hours - sleep_mean))
  recalled <- runif(n_nights) < p_recall
  logged <- runif(n_nights) >= cfg$missing_prob
  dreams <- lapply(seq_len(n_nights), function(i) {
    if (!recalled[i]) return(list(text = "", themes = "", emotions = ""))
    simulate_dream(negative[i], cfg$theme_tone_strength,
                   cfg$mixed_emotion_prob, cfg$two_label_prob)
  })
  full <- tibble::tibble(date = dates, bedtime = bedtime, wake_time = wake_time,
                         sleep_hours = hours, sleep_quality = as.integer(quality), stress = as.integer(stress),
                         dream_text = vapply(dreams, `[[`, "", "text"),
                         themes = vapply(dreams, `[[`, "", "themes"), emotions = vapply(dreams, `[[`, "", "emotions"))
  truth <- tibble::tibble(date = dates, stress_raw = stress_raw, stress = stress,
                          sleep_hours = hours, weekend_night = weekend, short_night = short,
                          negative = as.integer(negative), p_negative = p_negative,
                          recalled = recalled, p_recall = p_recall, logged = logged,
                          observed_negative = ifelse(logged & recalled, as.integer(negative), NA_integer_))
  parameters <- c(list(stress_mean = stress_mean, sleep_mean = sleep_mean,
                       stress_on_sleep = stress_on_sleep, stress_on_negative = stress_on_negative,
                       sleep_on_negative = sleep_on_negative, recall_prob = recall_prob), cfg)
  provenance <- list(synthetic = TRUE, seed = seed, start_date = start_date,
                     scheduled_nights = n_nights, parameters = parameters,
                     description = "Assumption-based simulated diary; no human observations or reviews.")
  journal <- full[logged, ]
  attr(journal, "simulation") <- provenance
  result <- list(journal = journal, truth = truth, parameters = parameters,
                 simulation = provenance)
  if (isTRUE(return_details)) result else journal
}

simulation_parameter_table <- function(sim, justifications = NULL) {
  p <- sim$parameters
  out <- data.frame(parameter = names(p), value = unlist(p, use.names = FALSE),
                    source_or_justification = "Assumed simulation value; not a literature estimate.",
                    stringsAsFactors = FALSE)
  if (!is.null(justifications)) {
    if (is.null(names(justifications))) stop("Supply named justification strings.")
    unknown <- setdiff(names(justifications), names(p))
    if (length(unknown)) stop("Unknown parameter names: ", paste(unknown, collapse = ", "))
    out$source_or_justification[match(names(justifications), out$parameter)] <-
      as.character(justifications)
  }
  out
}

simulation_diagnostics <- function(sim) {
  t <- sim$truth
  safe_mean <- function(x) if (length(x)) mean(x) else NA_real_
  data.frame(scheduled_nights = nrow(t), logged_nights = sum(t$logged),
             analysed_nights = sum(t$logged & t$recalled),
             achieved_sleep_mean = mean(t$sleep_hours), achieved_sleep_sd = sd(t$sleep_hours),
             recall_fraction_logged = safe_mean(t$recalled[t$logged]),
             missing_fraction = mean(!t$logged),
             negative_fraction_observed = safe_mean(t$negative[t$logged & t$recalled]),
             weekday_sleep_mean = safe_mean(t$sleep_hours[!t$weekend_night]),
             weekend_sleep_mean = safe_mean(t$sleep_hours[t$weekend_night]))
}

# Optional separate snapshot for app integration. The existing main snapshot
# is NEVER overwritten; caller must explicitly supply a new path.
# Review rows remain unanswered and entities unconfirmed.
save_simulated_snapshot <- function(sim, path) {
  if (file.exists(path)) stop("Output exists; choose a NEW snapshot path.")
  if (!exists("review_initial_state", mode = "function") ||
      !exists("review_write_state", mode = "function"))
    stop("Load the reviewed-state functions from the current project first.")
  s <- review_initial_state(sim$journal)
  s$simulation <- sim$simulation
  s$simulation$description <- paste(s$simulation$description,
                                    "Reviewed emotion stages are unanswered; entities are not human-confirmed.")
  # No VAD ratings are invented or copied here.
  review_write_state(s, path)
  invisible(path)
}

# Benchmark estimator: conditional logistic model on known observed tone.
# This is NOT a validation of a text classifier or the current Patterns tab.
# Alternative estimator(sim) must return columns:
# term ('stress'/'sleep_hours'), estimate, conf_low, conf_high, p_value.
# Estimates and interval endpoints must be on the LOG-ODDS scale.
fit_simulation_benchmark <- function(sim) {
  d <- sim$truth[sim$truth$logged & sim$truth$recalled, ]
  if (nrow(d) < 8 || length(unique(d$negative)) < 2) stop("Insufficient outcome variation.")
  fit <- suppressWarnings(stats::glm(negative ~ stress + sleep_hours,
                                     family = stats::binomial(), data = d))
  cc <- stats::coef(summary(fit))
  terms <- c("stress", "sleep_hours")
  if (!isTRUE(fit$converged) || isTRUE(fit$boundary) ||
      !all(terms %in% rownames(cc)) || any(!is.finite(cc)) ||
      any(abs(cc[, 1]) > 10) || any(cc[, 2] > 10))
    stop("Unstable fit; heuristic separation screen.")
  data.frame(term = terms, estimate = cc[terms, 1],
             conf_low = cc[terms, 1] - stats::qnorm(0.975) * cc[terms, 2],
             conf_high = cc[terms, 1] + stats::qnorm(0.975) * cc[terms, 2],
             p_value = cc[terms, 4], row.names = NULL)
}

run_simulation_recovery <- function(nights = c(30, 90, 180, 365, 730),
                                    repetitions = 500, seed = 10000, simulation_args = list(),
                                    estimator = fit_simulation_benchmark, alpha = 0.05) {
  if (!length(nights) || any(!is.finite(nights) | nights < 1 | nights != floor(nights)))
    stop("Invalid scheduled-night counts.")
  if (length(repetitions) != 1 || !is.finite(repetitions) ||
      repetitions < 2 || repetitions != floor(repetitions)) stop("Use at least two repetitions.")
  .sim_validate_scalar(seed, "seed")
  .sim_validate_scalar(alpha, "alpha")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be between 0 and 1.")
  if (!is.function(estimator)) stop("estimator must be a function.")
  if (length(simulation_args) && (is.null(names(simulation_args)) ||
                                  any(!nzchar(names(simulation_args))) || anyDuplicated(names(simulation_args))))
    stop("simulation_args must contain uniquely named arguments.")
  if (any(c("n_nights", "seed", "return_details") %in% names(simulation_args)))
    stop("Do not put n_nights, seed or return_details in simulation_args.")
  if (seed < 0 || seed != floor(seed) ||
      seed + length(nights) * repetitions > .Machine$integer.max) stop("Invalid seed range.")
  raw <- list()
  index <- 0L
  for (n in nights) for (r in seq_len(repetitions)) {
    args <- c(list(n_nights = n, seed = seed + index, return_details = TRUE), simulation_args)
    sim <- do.call(simulate_dream_journal, args)
    fitted <- tryCatch(estimator(sim), error = function(e) e)
    reason <- if (inherits(fitted, "error")) conditionMessage(fitted) else ""
    needed <- c("term", "estimate", "conf_low", "conf_high", "p_value")
    format_ok <- is.data.frame(fitted) && all(needed %in% names(fitted))
    for (term in c("stress", "sleep_hours")) {
      beta <- if (term == "stress") sim$parameters$stress_on_negative else sim$parameters$sleep_on_negative
      row <- if (format_ok) fitted[fitted$term == term, , drop = FALSE] else NULL
      usable <- !is.null(row) && nrow(row) == 1 &&
        all(vapply(row[c("estimate", "conf_low", "conf_high", "p_value")],
                   function(x) is.numeric(x) && length(x) == 1 && is.finite(x), logical(1)))
      if (usable) usable <- row$conf_low <= row$conf_high && row$p_value >= 0 && row$p_value <= 1
      raw[[length(raw) + 1L]] <- data.frame(scheduled_nights = n, replicate = r,
                                            term = term, true_beta = beta, usable = usable,
                                            analysed_nights = sum(sim$truth$logged & sim$truth$recalled),
                                            estimate = if (usable) row$estimate else NA_real_,
                                            covered = if (usable) row$conf_low <= beta && row$conf_high >= beta else NA,
                                            detected = if (usable) row$p_value < alpha else NA,
                                            failure_reason = if (usable) "" else if (nzchar(reason)) reason else "Invalid estimator output")
    }
    index <- index + 1L
  }
  raw <- dplyr::bind_rows(raw)
  summary <- raw |>
    dplyr::group_by(scheduled_nights, term, true_beta) |>
    dplyr::summarise(attempted = dplyr::n(), usable_fits = sum(usable),
                     failure_rate = mean(!usable), mean_analysed_nights = mean(analysed_nights),
                     mean_beta = if (any(usable)) mean(estimate[usable]) else NA_real_,
                     beta_bias = if (any(usable)) mean(estimate[usable] - true_beta[usable]) else NA_real_,
                     beta_rmse = if (any(usable)) sqrt(mean((estimate[usable] - true_beta[usable])^2)) else NA_real_,
                     coverage_usable = if (any(usable)) mean(covered[usable]) else NA_real_,
                     power_usable = if (any(usable)) mean(detected[usable]) else NA_real_,
                     detection_all = sum(detected, na.rm = TRUE) / dplyr::n(), .groups = "drop") |>
    dplyr::mutate(true_OR = exp(true_beta), OR_from_mean_beta = exp(mean_beta),
                  coverage_mcse = sqrt(coverage_usable * (1 - coverage_usable) / usable_fits),
                  detection_mcse = sqrt(detection_all * (1 - detection_all) / attempted))
  list(raw = raw, summary = summary, simulation_args = simulation_args,
       seed = seed, repetitions = repetitions,
       note = paste("Default estimator uses known observed tone and Wald 95% intervals.",
                    "Failed/unstable fits are separately counted; detection_all treats failure as no detection.",
                    "Coverage/power_usable condition on successful fits. A null effect gives type-I error, not power."))
}

plot_simulation_power <- function(study) {
  ggplot2::ggplot(study$summary, ggplot2::aes(scheduled_nights, detection_all, colour = term)) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = pmax(0, detection_all - 1.96 * detection_mcse),
      ymax = pmin(1, detection_all + 1.96 * detection_mcse)), width = 8) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::labs(x = "Scheduled calendar nights", y = "Detection probability", colour = "Effect",
                  subtitle = "Assumption-based scenarios; approximate Monte Carlo error bars") + ggplot2::theme_minimal()
}

# Smallest TESTED sample size; not a universal sample-size guarantee.
simulation_sample_size <- function(study, term = "sleep_hours", target = 0.8) {
  x <- study$summary[study$summary$term == term & study$summary$detection_all >= target, ]
  if (!nrow(x)) return(data.frame(term = term, target = target, scheduled_nights = NA_integer_,
                                  note = "No tested sample size met the target; extend the grid."))
  x <- x[which.min(x$scheduled_nights), ]
  data.frame(term = term, target = target, scheduled_nights = x$scheduled_nights,
             note = "Smallest tested size meeting point-estimate target; inspect Monte Carlo uncertainty.")
}

# Deterministic checks, not a substitute for statistical recovery.
check_simulation <- function() {
  a <- simulate_dream_journal(60, seed = 1, return_details = TRUE)
  b <- simulate_dream_journal(60, seed = 1, return_details = TRUE)
  stopifnot(identical(a, b), nrow(a$journal) == 60,
            all(a$journal$stress %in% 1:10), all(a$journal$sleep_quality %in% 1:10))
  for (prob in c(0, 1)) {
    s <- simulate_dream_journal(30, recall_prob = prob, return_details = TRUE)
    stopifnot(all(s$truth$p_recall == prob), all(s$truth$recalled == (prob == 1)))
  }
  s <- simulate_dream_journal(30, settings = simulation_settings(missing_prob = 1), return_details = TRUE)
  stopifnot(nrow(s$journal) == 0, all(is.na(s$truth$observed_negative)))
  # Verify metadata survives construction of the returned diary.
  stopifnot(isTRUE(attr(a$journal, "simulation")$synthetic))
  message("Basic simulation checks passed.")
  invisible(TRUE)
}

# ---------------- HOW TO RUN (uncomment in the console) ----------------
# source("src/setup.R")
# source("src/simulate_journal.R")
# check_simulation()
# cfg <- simulation_settings(missing_prob = 0.10)
# sim <- simulate_dream_journal(settings = cfg, return_details = TRUE)
# print(simulation_diagnostics(sim))
# print(simulation_parameter_table(sim))
# journal <- sim$journal
#
# Smoke test first; do not report ten-replicate results as final evidence.
# smoke <- run_simulation_recovery(repetitions = 10,
#   simulation_args = list(settings = cfg))
# print(smoke$summary)
# study <- run_simulation_recovery(repetitions = 500,
#   simulation_args = list(settings = cfg))
# print(study$summary)
# print(plot_simulation_power(study))
# print(simulation_sample_size(study))
#
# Null-effect sensitivity check (reports false-positive probability):
# null_study <- run_simulation_recovery(repetitions = 500,
#   simulation_args = list(settings = cfg, sleep_on_negative = 0))
#
# Optional richer/mixed CONTENT scenario, not compatible with interpreting
# "any negative label" as the known binary dream tone:
# mixed <- simulate_dream_journal(return_details = TRUE,
#   settings = simulation_settings(theme_tone_strength = 0.6, mixed_emotion_prob = 0.2))
#
# Optional NEW app snapshot; reviews unanswered, VAD empty:
# save_simulated_snapshot(sim, "data/research_demo_NEW.rds")
# run_dream_app(state_path = "data/research_demo_NEW.rds")
# NOTE: inspect/update the app's synthetic banner to match this provenance;
# it must not claim waking reviews or entity confirmations were simulated.
#
# ---------------- NOTES FOR main.qmd ----------------
# Use knitr::kable(simulation_parameter_table(sim)) for the parameter table.
# Add verified citations with named justifications, for example:
# justifications <- c(sleep_sd = "Assumed residual SD; assess 0.6, 1.0, 1.4 hours.")
# knitr::kable(simulation_parameter_table(sim, justifications))
# Every cast/prop weight and text template above is an illustrative assumption.
# No research claims or references are invented here.
#
# Model equations (write using double-dollar delimiters in main.qmd):
# logit P(negative_t = 1) = negative_intercept
#   + stress_on_negative * (recorded_stress_t - stress_mean)
#   + sleep_on_negative * (sleep_hours_t - sleep_mean).
# logit P(recall_t = 1) = logit(recall_prob)
#   + recall_on_sleep * (sleep_hours_t - sleep_mean).
# Diagram: stress -> sleep; stress -> negative tone; sleep -> negative tone;
# sleep -> recall; recall and diary completion -> outcome observed.
# Stress is a confounder of the stipulated sleep effect; sleep mediates part
# of the stipulated stress effect. These are simulation assumptions.
# Diary omission is independent (MCAR); recall depends only on sleep included
# in the fitted model. Other missingness mechanisms need separate scenarios.
# AR(1) predictors do not themselves add serial dependence to outcome errors
# conditional on those predictors. Actual diary dependence may be different.
# Emotion-token negativity norms cannot directly calibrate dream-level tone.
# Exponentiating mean beta is not the arithmetic mean of fitted odds ratios.
# Keep failure rates beside coverage estimates. Wald small-sample coverage
# and the separation screen are imperfect; penalised estimators need their
# own validation. Do not tune effect sizes to get a significant result.
# The benchmark checks a known generated outcome, NOT your analysis.R.
# To validate that file, supply estimator(sim) calling its real preprocessing
# and model, returning the documented five-column log-odds result table.
