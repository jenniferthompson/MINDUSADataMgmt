################################################################################
## Create dummy data set with IDs, REDCap event names, days since
##   enrollment/randomization for each event
## This will help with deciding which days to include in variables like DCFDs
################################################################################

library(tidyverse)
library(stringr)

## Source data management functions
source("data_functions.R")

## -- Import data dictionaries from REDCap -------------------------------------
## All tokens are stored in .Renviron
ih_dd <- get_datadict("MINDUSA_IH_TOKEN")
ih_events <- get_events("MINDUSA_IH_TOKEN") %>% mutate(event_num = 1:nrow(.))
## Add event_num to help with sorting events later
ih_mapping <- get_event_mapping("MINDUSA_IH_TOKEN")

## -- ptstatus_df = definitive source of those randomized w/ no exclusions -----
ptstatus_df <- readRDS("analysisdata/rds/ptstatus.rds")

## Vector of all consented patients never excluded
consented_pts <- ptstatus_df %>%
  filter(consented & !excluded_ever) %>%
  pull(id)

## Vector of all randomized patients never excluded
rand_pts <- ptstatus_df %>%
  filter(randomized & !excluded_ever) %>%
  pull(id)

## -- Import REDCap event names and daily dates for all consented patients -----
events_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "redcap_event_name", "abcde_icu", "daily_date", "assess_date"),
  ## Randomization, hospital discharge events don't have daily data collection
  events = setdiff(
    ih_events$unique_event_name,
    c("randomization_arm_1", "prior_to_hospital_arm_1")
  )
) %>%
  ## Keep all consented patients who were not excluded
  filter(id %in% consented_pts) %>%
  ## Delete any records without a date on either the Daily Data form (vast
  ## majority) or the PAD form (3 additional). REDCap exports all records once
  ## they're begun, even if they're blank. Per conversation with BP December
  ## 2017, Daily Data form can be taken as roughly a "source of truth" for
  ## whether real data was entered on this day; it should be filled out with at
  ## least a date every day the patient was in the hospital, in or out of the
  ## ICU, and was being followed for the study (ie, it is not filled out after
  ## IT day 14 if the patient has been off study drug at least 4 days already).
  ## We do want to keep any PAD data which was collected on a day the Daily Data
  ## form was not filled out.
  filter((!is.na(daily_date) & !(trimws(daily_date) == "")) |
           (!is.na(assess_date) & !(trimws(assess_date) == ""))) %>%
  ## Create a single "today's date" variable
  mutate(date_today = ifelse(!is.na(daily_date) & trimws(daily_date) != "",
                             daily_date,
                             assess_date))

## -- Import enrollment, randomization, death, discharge times -----------------
time_vars <- c("enroll_time", "randomization_time", "death_time", "hospdis_time")

dates_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "studywd_time", time_vars),
  events = "enrollment__day_0_arm_1"
) %>%
  select(-redcap_event_name) %>%
  ## Do not want patients who were ever excluded
  filter(id %in% consented_pts) %>%
  ## Make date/time variables
  mutate_at(time_vars, ymd_hm) %>%
  mutate_at(time_vars, funs("date" = date)) %>%
  mutate(studywd_date = as.Date(studywd_time, format = "%Y-%m-%d")) %>%
  rename_at(paste0(time_vars, "_date"), ~ str_replace(., "\\_time", ""))

## -- Combine patient times + events, determine event of randomization ---------
allpts_events <- left_join(dates_raw, events_raw, by = "id") %>%
  mutate(
    date_today = ymd(date_today),
    days_since_consent = days_diff(date_today, enroll_date),
    days_since_randomization = days_diff(date_today, randomization_date),
    in_icu = as.logical(abcde_icu),
    ## Indicators for whether patient was in pre-randomization, intervention,
    ## post-intervention periods
    ## NOTE: We use 13 because conversations surrounding RCT analyses said
    ##  outcomes like DCFDs should look at the 14 days *including* and following
    ##  the day of randomization.
    prerandom = days_since_randomization < 0,
    intervention = between(days_since_randomization, 0, 13),
    postint = days_since_randomization > 13
  )

## -- Data checks --------------------------------------------------------------
## All results are written to a text file, ptevents_checks.txt
sink("ptevents_checks.txt")

## No one should have:
## - >4 pre-randomization days (enrollment + 4 PR days)
## - >14 intervention days (days 0-13)
## - >5 post-IT days (days 14-18)
toomany_days <- allpts_events %>%
  group_by(id) %>%
  summarise(pr_days = sum_na(prerandom),
            int_days = sum_na(intervention),
            post_days = sum_na(postint)) %>%
  filter(pr_days > 5 | int_days > 14 | post_days > 5)

print_datachecks(
  "Patients with >5 pre-randomization, >14 intervention, and/or >4 post-intervention days:",
  toomany_days
)

sink()

## -- Create dummy data set for all *randomized* patients, with status on ------
## -- each day up to IT day 18 (last possible event in database) ---------------
## Prep: Select record from allpts_events from the *day* of randomization only
rand_day <- filter(
  allpts_events,
  id %in% rand_pts & days_since_randomization == 0
) %>%
  rename(randomization_event = "redcap_event_name") %>%
  select(id, randomization_event, days_since_consent)

## Create data frame with, for each randomized patient, one record per
## patient-day, day of randomization -> IT day 18 (final event in database)
randpts_events <- cross_df(
  list(
    id = rand_pts,
    redcap_event_name = setdiff(
      ih_events$unique_event_name,
      c("randomization_arm_1", "prior_to_hospital_arm_1")
    )
  )
) %>%
  left_join(
    select(allpts_events, id, redcap_event_name,
           in_icu, date_today, days_since_consent, days_since_randomization),
    by = c("id", "redcap_event_name")) %>%
  ## Remove pre-randomization days which weren't used for that patient
  filter(
    !(str_detect(redcap_event_name, "^prerandomization") &
        is.na(days_since_consent))
  ) %>%
  
  ## Re-add info from dates tracking so that it's present on every event
  ## (if we add it from allpts_events, it's blank for events not in REDCap)
  left_join(dates_raw, by = "id") %>%

  ## Keep only day of randomization + anything in interventional period
  filter(is.na(days_since_randomization) | days_since_randomization >= 0) %>%

  ## Redo some variables from allpts_events which are missing after patient
  ## leaves hospital: date_today; intervention; postint
  ## Create new variables:
  ## - study_day (will need this once we delete dates)
  ## - in_redcap (data actually entered)
  ## - study_status (patient's status on each day following randomization)
  ## - hospitalized (indicator for whether patient was still in the hospital)
  group_by(id) %>%
  mutate(
    study_day = 1:n() - 1,
    date_today = randomization_date + study_day,
    in_redcap = !is.na(days_since_randomization),
    intervention = study_day %in% 0:13,
    postint = study_day > 13,
    study_status = factor(
      case_when(
        intervention & in_redcap ~ 1,
        postint & in_redcap ~ 2,
        !is.na(hospdis_date) & date_today >= hospdis_date ~ 4,
        !is.na(death_date) & date_today >= death_date ~ 5,
        !is.na(studywd_date) & date_today >= studywd_date ~ 6,
        TRUE ~ 3
      ),
      levels = 1:6,
      labels = c("Intervention", "Post-intervention",
                 "Hospitalized; no longer being assessed",
                 "Discharged", "Deceased", "Withdrawn")
    ),
    ## Replace missing days since consent (after patient left hospital/followup);
    ##  "study_day" now equivalent to days since randomization
    days_since_consent = days_diff(date_today, enroll_date),
    ## Replace missing in_icu
    in_icu = ifelse(is.na(in_icu), FALSE, in_icu),
    ## Indicator for "still in hospital": has record in REDCap, or has not yet
    ## died/been discharged/withdrawn
    hospitalized =
      in_redcap | study_status == "Hospitalized; no longer being assessed"
  ) %>%
  ## Remove dates, select only variables we need
  select(id, redcap_event_name, study_day, days_since_consent, in_redcap,
         hospitalized, intervention, postint, in_icu, study_status) %>%
  ## Sort by patient, then event
  arrange(id, study_day)

## Note: One patient has 18 events here, because s/he was randomized at the very
##  last opportunity on IT day 1, after maxing out all PR days. Others have 19.

## -- Remove date variables from allpts_events ---------------------------------
allpts_events <-
  select(allpts_events, id, redcap_event_name, days_since_consent:postint)

## -- Save both final dataframes to analysisdata -------------------------------
saveRDS(allpts_events, file = "analysisdata/rds/allptevents.rds")
saveRDS(randpts_events, file = "analysisdata/rds/randptevents.rds")
write_csv(allpts_events, path = "analysisdata/csv/allptevents.csv")
write_csv(randpts_events, path = "analysisdata/csv/randptevents.csv")
