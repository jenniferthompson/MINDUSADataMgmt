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
ih_events <- get_events("MINDUSA_IH_TOKEN") %>% mutate(event_num = 1:nrow(.))
## Add event_num to help with sorting events later
ih_mapping <- get_event_mapping("MINDUSA_IH_TOKEN")

## -- ptstatus_df = definitive source of those randomized w/ no exclusions -----
ptstatus_df <- readRDS("analysisdata/rds/ptstatus.rds")

## Vector of all randomized patients never excluded
rand_pts <- ptstatus_df %>%
  filter(randomized & !excluded_ever) %>%
  pull(id)

## -- Import REDCap event names, whether patient in ICU (from ABCDEF form) -----
events_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "redcap_event_name", "abcde_icu"),
  ## Randomization, hospital discharge events don't have daily data collection
  events = setdiff(
    ih_events$unique_event_name,
    c("randomization_arm_1", "prior_to_hospital_arm_1")
  )
) %>%
  ## Keep all consented patients who were not excluded
  filter(id %in% subset(ptstatus_df, consented & !excluded_ever)$id)

## Last day without "interventional" in event name = day of randomization
last_pr_day <- events_raw %>%
  select(id, redcap_event_name) %>%
  left_join(select(ih_events, unique_event_name, event_num),
            by = c("redcap_event_name" = "unique_event_name")) %>%
  filter(id %in% rand_pts & !str_detect(redcap_event_name, "^interventional")) %>%
  group_by(id) %>%
  arrange(event_num) %>%
  mutate(randomization_day = 1:n()) %>%
  slice(n()) %>%
  ungroup() %>%
  rename(randomization_event = "redcap_event_name")

## Add randomization event onto daily data; calculate days to each event
## 0 = day of consent/randomization
allpts_events <- events_raw %>%
  left_join(last_pr_day %>% select(-event_num), by = "id") %>%
  group_by(id) %>%
  mutate(
    days_since_consent = 1:n() - 1,
    days_since_randomization = (days_since_consent - randomization_day) + 1,
    in_icu = as.logical(abcde_icu)
  ) %>%
  select(
    id, redcap_event_name, days_since_consent, days_since_randomization, in_icu
  )

## -- Create dummy data set for all *randomized* patients, with status on ------
## -- each day up to IT day 18 (last possible event in database) ---------------

## Download necessary dates/times from dates tracking form
time_vars <- c("enroll_time", "randomization_time", "death_time", "hospdis_time")

dates_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "studywd_time", time_vars),
  events = "enrollment__day_0_arm_1"
) %>%
  ## Only want randomized patients
  filter(id %in% rand_pts) %>%
  ## Make date/time variables
  mutate_at(time_vars, ymd_hm) %>%
  mutate_at(time_vars, funs("date" = date)) %>%
  mutate(studywd_date = as.Date(studywd_time, format = "%Y-%m-%d")) %>%
  rename_at(paste0(time_vars, "_date"), ~ str_replace(., "\\_time", ""))

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
  ## Keep only day of randomization + anything in interventional period
  ## Patients could be randomized on any of five REDCap events, so this is a
  ##  little tricky
  left_join(select(ih_events, unique_event_name, event_num),
            by = c("redcap_event_name" = "unique_event_name")) %>%
  left_join(last_pr_day %>% select(id, randomization_event, randomization_day),
            by = "id") %>%
  filter(redcap_event_name == randomization_event |
           str_detect(redcap_event_name, "^interventional")) %>%
  select(id, redcap_event_name) %>%
  
  ## Add info from dates tracking, days since consent/randomization, whether
  ##   patient in ICU
  left_join(dates_raw %>% select(-redcap_event_name), by = "id") %>%
  left_join(allpts_events, by = c("id", "redcap_event_name")) %>%
  
  ## Determine *date* for each record, patient's overall status in the study on
  ##  each event, other indicators of patient status
  group_by(id) %>%
  mutate(
    study_day = 1:n() - 1,
    date_today = randomization_date + study_day,
    in_redcap = !is.na(days_since_randomization),
    intervention = study_day %in% 0:13,
    postintervention = study_day > 13,
    study_status = factor(
      case_when(
        intervention & in_redcap ~ 1,
        postintervention & in_redcap ~ 2,
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
    ## Indicator for "still in hospital": has record in REDCap, or has not yet
    ## died/been discharged/withdrawn
    hospitalized =
      in_redcap | study_status == "Hospitalized; no longer being assessed"
  ) %>%
  ## Remove dates, select only variables we need
  select(id, redcap_event_name, study_day, days_since_consent, in_redcap,
         hospitalized, intervention, postintervention, in_icu, study_status)

## -- Save both final dataframes to analysisdata -------------------------------
saveRDS(allpts_events, file = "analysisdata/rds/allptevents.rds")
saveRDS(randpts_events, file = "analysisdata/rds/randptevents.rds")
write_csv(allpts_events, path = "analysisdata/csv/allptevents.csv")
write_csv(randpts_events, path = "analysisdata/csv/randptevents.csv")
