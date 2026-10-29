library(tidyverse)
library(readxl)

version <- "0.1.0.pre"
release_path <- "/Volumes/bwh-sleepepi-nsrr-staging/20261021-reset/nsrr-prep/releases"
source_path <- "/Volumes/bwh-sleepepi-nsrr-staging/20261021-reset/nsrr-prep/source"

reset_df <- read.csv(file.path(source_path, "RESETLabSessions_DATA_NSRR 1-2026.xlsx - RESETLabSessions_DATA_NSRR 1-20.csv"), skip = 1)

df_clean <- reset_df |>
  rename(
    event_name = Event.Name,
    id_raw= Please.enter.the.participant.ID.number.your.experimenters.gave.you.
  ) |>
  mutate(
    # Extract participant ID
    participant_id = as.integer(str_extract(id_raw, "^\\d+")),
    # Extract visit number
    visit = as.integer(str_match(event_name, "Visit\\s*(\\d+)")[, 2]),
    # Extract wake/nap condition
    condition = case_when(
      str_detect(id_raw, regex("Wake", ignore_case = TRUE)) ~ "Wake",
      str_detect(id_raw, regex("Nap", ignore_case = TRUE)) ~ "Nap",
      TRUE ~ NA_character_
    ),
    event_time = str_remove(event_name, "^Visit\\s*\\d+,\\s*")
  ) |>
  select(participant_id, visit, condition, event, everything()) |>
  rename_with(~ stringr::str_replace_all(.x, "_{2,}", "_"))


daily_vars <- c(
  "time_in_bed", "time_to_sleep", "fall_asleep_time","num_wakenings", "waking_time", "tib_waking",
  "early_wake", "early_wake_length", "time_awake","sleep_quality","refreshed","times_nap","nap_length",
  "alcohol_num", "time_alcohol", "caffeine_drinks", "time_caffeine","sleep_meds","types_of_sleep_meds","additional_comments",
  "csd_hours_tst", "csd_minutes_tst","csd_hours", "csd_minutes",
  "headache_pcss", "nausea_pcss", "vomit_pcss", "balance_pcss", "dizzy_pcss", "fatigue_pcss",
  "light_pcss", "noise_pcss", "numb_pcss", "foggy_pcss", "slowed_pcss", "concentrate_pcss", 
  "remember_pcss", "drowsy_pcss", "sleepless_pcss", "sleepmore_pcss", "fallasleep_pcss",
  "irritability_pcss", "sadness_pcss", "nervousness_pcss", "moreemot_pcss", 
  "consensus_sleep_diary_complete")

# Identify columns corresponding to daily questionnaires
daily_cols <- names(df_clean)[
  str_detect(names(df_clean),
             paste0("^(", paste(daily_vars, collapse = "|"),
                    ")_TP[0-9]+$")
  )
]

length(daily_cols)
setdiff(daily_vars, str_remove(daily_cols, "_TP[0-9]+$"))
setdiff(str_remove(daily_cols, "_TP[0-9]+$"), daily_vars)


# Extract daily questionnaire columns from their current source row
daily_wide <- df_clean |>
  filter(
    visit == 1,
    event == "Before Nap/wake and daily surveys"
  ) |>
  select(participant_id, all_of(daily_cols)) |>
  mutate(
    across(matches("^num_wakenings_TP[0-9]+$"), as.character),
    across(matches("^sleep_quality_TP[0-9]+$"),as.character),
    across(matches("^nap_length_TP[0-9]+$"),as.character)
  )

# Reshape daily questionnaires into long format
daily_long <- daily_wide |>
  pivot_longer(
    cols = all_of(daily_cols),
    names_to = c(".value", "visit"),
    names_pattern = "^(.*)_(TP[0-9]+)$")
  # ) |>
  # mutate(event = "Daily Survey",
  #        consensus_sleep_diary_complete = ifelse(is.na(consensus_sleep_diary_complete), 0, consensus_sleep_diary_complete))


visit_order <- c("1", paste0("TP", 1:9), "2")
event_order <- c("Before Nap/wake and daily surveys", "Before Nap/wake", "After Nap/wake", "After Nap/Wake","After eye tracking")

# Combine with existing visit/event data
df_clean_long <- df_clean |>
  select(-all_of(daily_cols)) |>
  mutate(visit = as.character(visit)) |>
  bind_rows(daily_long) |>
  arrange(
    participant_id,
    match(visit, visit_order),
    match(event, event_order))

