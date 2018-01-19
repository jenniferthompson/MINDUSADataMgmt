################################################################################
## Data management for Daily Safety Assessment form
## This form is filled out once daily through Intervention Day 14, and possibly
##   through Intervention Day 18 if needed for four days post-study drug
################################################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(assertr)

## Source data management functions
source("data_functions.R")

## Read in datasets with all study events for randomized, consented patients;
## will need them to calculate X within various time periods
allpts_events <- readRDS("analysisdata/rds/allptevents.rds")
randpts_events <- readRDS("analysisdata/rds/randptevents.rds")

## -- Import data dictionaries from REDCap -------------------------------------
## All tokens are stored in .Renviron
ih_dd <- get_datadict("MINDUSA_IH_TOKEN")
ih_events <- get_events("MINDUSA_IH_TOKEN") %>% mutate(event_num = 1:nrow(.))
## Add event_num to help with sorting events later
ih_mapping <- get_event_mapping("MINDUSA_IH_TOKEN")

## -- Bring in ptstatus_df; we'll use some variables from there ----------------
ptstatus_df <- readRDS("analysisdata/rds/ptstatus.rds")

## Vector of all randomized patients never excluded
rand_pts <- ptstatus_df %>%
  filter(randomized & !excluded_ever) %>%
  pull(id)

## -- Download daily safety form, plus a few other variables for Torsades ------
## Torsades can be recorded in several places:
## - Daily Data Collection form, as a type of arrhythmia
## - 12-Lead ECG Report form, as an ECG interpretation
## - Study Drug Admin form, as a reason study drug was permanently discontinued
## - Event Reporting form, as a type of AE
## Options 1-3 are downloaded alongside additional safety data; they are
## recorded on a daily basis. The AE form is on the Enrollment event and thus
## AEs are downloaded separately.
torsades_ecg_vars <- str_subset(ih_dd$field_name, "ecg\\_interpretation")
torsades_permdc_vars <- str_subset(ih_dd$field_name, "permanent\\_stop\\_why")

safety_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "redcap_event_name", "arrhythmia_daily",
             torsades_ecg_vars, torsades_permdc_vars),
  forms = c("daily_safety_assessment_form"),
  events = setdiff(
    ih_events$unique_event_name,
    c("randomization_arm_1", "prior_to_hospital_arm_1")
  )
) %>%
  ## Remove unneeded variables
  select(
    -daily_safety_assessment_form_complete,
    ## ECG interpretation and arrhythmias are checkboxes; for Torsades, we're
    ## only interested in one option each (arrhythmia #4, ECG #5)
    -matches("^arrhythmia\\_daily\\_[1-3, 5, 9]+$"),
    -matches("^ecg\\_interpretation\\_[1-5]\\_[0-4, 6]$")
  ) %>%
  ## Restrict to "real" events, as determined in ptevents.R
  right_join(allpts_events, by = c("id", "redcap_event_name"))

ae_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  ## Also export date/times of enrollment and randomization, to determine what
  ## study day the event occurred on
  fields = c("id", "enroll_time", "randomization_time"),
  forms = c("event_reporting_form"),
  events = "enrollment__day_0_arm_1"
) %>%
  mutate_at(vars("enroll_time", "randomization_time"), ymd_hm) %>%
  mutate_at(vars(starts_with("ae_date")), ymd)

## -- Extrapyramidal symptoms (EPS) assessment ---------------------------------
## List of variables involved in EPS scoring
eps_vars <- c("elbow_rigidity", "wrist_rigidity", "glabella_tap",
              "resting_tremor", "salivation")

eps_df <- safety_raw %>%
  dplyr::select(id, redcap_event_name, intervention, one_of(eps_vars)) %>%
  ## All EPS variables have numeric values which reflect actual score, except
  ## that 5 = "not done". Change these to NA.
  mutate_at(vars(one_of(eps_vars)), funs(ifelse(. %in% c(5), NA, .))) %>%
  ## In addition, glabellar tap has a value of 99 = UTA. Per PIs (Jan 2018),
  ## this should be considered equivalent to a score of 0.
  mutate_at(vars(one_of(eps_vars)), funs(ifelse(. %in% c(99), 0, .))) %>%
  ## How many EPS symptoms are missing each day?
  mutate(eps_symptoms_avail = rowSums(!is.na(.[, eps_vars]))) %>%
  ## If >=1 EPS symptom is present, EPS score = mean of all symptom scores.
  ## If all symptoms are missing/not done, score is NA.
  mutate(
    eps_score = ifelse(eps_symptoms_avail == 0, NA,
                       rowMeans(.[, eps_vars], na.rm = TRUE))
  ) %>%
  ## Indicators for whether each symptom is >=3
  ##  Per our protocol, if 3 or more symptoms had scores >=3, we considered it
  ##  an official diagnosis of EPS and study drug should have been stopped.
  mutate_at(
    vars(one_of(eps_vars)),
    funs(eps = . >= 3)
  ) %>%
  rename_at(
    vars(ends_with("_eps")),
    funs(ifelse(. == "resting_tremor_eps", "eps_tremor",
                paste0("eps_", gsub("\\_.+", "", .))))
  ) %>%
  mutate(
    eps_symptoms =
      rowSums(.[, paste0("eps_", c("elbow", "wrist", "glabella", "tremor",
                                   "salivation"))],
              na.rm = TRUE),
    eps_yn = eps_symptoms >= 3
  )

## Summarize EPS for each patient during entire hospitalization, intervention pd
## - Mean, min, max score
## - Days with each symptom >=3
## - Days with official EPS diagnosis
eps_summary_ih <- eps_df %>%
  group_by(id) %>%
  summarise(
    days_eps_elbow_ih      = sum_na(eps_elbow),
    days_eps_wrist_ih      = sum_na(eps_wrist),
    days_eps_glabella_ih   = sum_na(eps_glabella),
    days_eps_tremor_ih     = sum_na(eps_tremor),
    days_eps_salivation_ih = sum_na(eps_salivation),
    days_eps_ih            = sum_na(eps_yn),
    ever_eps_ih            = sum_na(eps_yn) > 0,
    eps_score_min_ih       = min_na(eps_score),
    eps_score_max_ih       = max_na(eps_score),
    eps_score_mean_ih      = mean_na(eps_score),
    eps_score_min_exp_ih   = min_na(eps_score[eps_yn]),
    eps_score_max_exp_ih   = max_na(eps_score[eps_yn]),
    eps_score_mean_exp_ih  = mean_na(eps_score[eps_yn])
  )

eps_summary_int <- eps_df %>%
  filter(intervention) %>%
  group_by(id) %>%
  summarise(
    days_eps_elbow_int      = sum_na(eps_elbow),
    days_eps_wrist_int      = sum_na(eps_wrist),
    days_eps_glabella_int   = sum_na(eps_glabella),
    days_eps_tremor_int     = sum_na(eps_tremor),
    days_eps_salivation_int = sum_na(eps_salivation),
    days_eps_int            = sum_na(eps_yn),
    ever_eps_int            = sum_na(eps_yn) > 0,
    eps_score_min_int       = min_na(eps_score),
    eps_score_max_int       = max_na(eps_score),
    eps_score_mean_int      = mean_na(eps_score),
    eps_score_min_exp_int   = min_na(eps_score[eps_yn]),
    eps_score_max_exp_int   = max_na(eps_score[eps_yn]),
    eps_score_mean_exp_int  = mean_na(eps_score[eps_yn])
  )

## -- Torsades de pointes ------------------------------------------------------
## Eventual goal: Have an indicator for whether patient experienced Torsades on
## a given day, and an indicator for whether patient ever experienced Torsades
## at all.

## Adverse event data
## Reshape to one row per event, then restrict to events the VCC determined were
## in fact Torsades. Include study phase.
torsades_events <- ae_raw %>%
  dplyr::select(
    id, enroll_time, randomization_time,
    matches("^ae\\_date\\_[0-9]$"),
    starts_with("ae_portion"),
    starts_with("ae_vcc_final"),
    starts_with("ae_serious"),
    starts_with("ae_description")
  ) %>%
  gather(key = ae_var_time, value = ae_value, ae_date_1:ae_description_3) %>%
  separate(ae_var_time, into = c("ae_var", "ae_num"), sep = "\\_(?=[0-9]$)") %>%
  spread(key = ae_var, value = ae_value) %>%
  filter(ae_vcc_final == 3) %>%
  mutate_at(
    vars(ae_num, ae_date, ae_portion, ae_vcc_final),
    funs(as.numeric)
  ) %>%
  mutate(
    ## Determine days after consent, randomization to merge with daily data
    ae_date = as.Date(ae_date, origin = "1970-1-1"),
    days_since_consent = days_diff(ae_date, date(enroll_time)),
    days_since_randomization = days_diff(ae_date, date(randomization_time)),
    ## Remove PHI from descriptions of AE
    ae_description = str_replace_all(
      ae_description,
      c("[0-9]+/[0-9]+/[0-9][0-9]" = "xx/xx/xx", ## dates
        "[0-9][0-9]:*[0-9][0-9]"   = "xx:xx",    ## times
        " \\(.+\\)"                = "..."       ## v specific info within ()
      )
    ),
    ## Indicator for when we merge with daily data
    torsades_ae = TRUE
  )
  
## Daily data: Based on arrhythmia, ECG, study drug, AE info, create a single
## indicator for whether patient had Torsades on a given day

torsades_ecg_vars <- paste0(torsades_ecg_vars, "_5")

torsades_df <- safety_raw %>%
  left_join(
    torsades_events,
    by = c("id", "days_since_consent", "days_since_randomization")
  ) %>%
  ## Update indicator for Torsades AE
  mutate(torsades_ae = ifelse(is.na(torsades_ae), FALSE, torsades_ae)) %>%
  ## Indicator for whether patient had Torsades today via any of the following:
  ## - Arrhythmia (arrhythymia_daily_4 = 1)
  ## - ECG interpretation (ecg_interpretation_x_5 = 1)
  ## - Reason study drug discontinued (permanent_stop_why_x = 5)
  mutate(
    torsades_arr    = arrhythmia_daily_4 == 1,
    torsades_ecg    = rowSums(.[, torsades_ecg_vars], na.rm = TRUE) > 0,
    torsades_permdc = rowSums(.[, torsades_permdc_vars] == 5, na.rm = TRUE) > 0
  ) %>%
  mutate(
    torsades_any =
      rowSums(.[, str_subset(names(.), "^torsades\\_")], na.rm = TRUE) > 0
  )

## Summarize Torsades by patient, phase (in hospital; during intervention)
torsades_summary_ih <- torsades_df %>%
  group_by(id) %>%
  summarise_at(
    vars(starts_with("torsades_")),
    funs(days = sum_na(.))
  ) %>%
  mutate_at(vars(-id), funs(ever = . > 0)) %>%
  ungroup() %>%
  rename_at(
    vars(ends_with("_days")),
    funs(paste0("days_", gsub("\\_days$", "", .), "_ih"))
  ) %>%
  rename_at(
    vars(ends_with("_ever")),
    funs(paste0("ever_", gsub("\\_days\\_ever$", "", .), "_ih"))
  )
  
torsades_summary_int <- torsades_df %>%
  filter(intervention) %>%
  group_by(id) %>%
  summarise_at(
    vars(starts_with("torsades_")),
    funs(days = sum_na(.))
  ) %>%
  mutate_at(vars(-id), funs(ever = . > 0)) %>%
  ungroup() %>%
  rename_at(
    vars(ends_with("_days")),
    funs(paste0("days_", gsub("\\_days$", "", .), "_int"))
  ) %>%
  rename_at(
    vars(ends_with("_ever")),
    funs(paste0("ever_", gsub("\\_days\\_ever$", "", .), "_int"))
  )

## Create dataset with pertinent info for AE, redacting PHI; this will be
## printed in final report
torsades_events <- torsades_events %>%
  dplyr::select(id, ae_portion, ae_serious, ae_description) %>%
  mutate(
    ae_portion = factor(
      ae_portion,
      levels = get_levels_ih("ae_portion_1"),
      labels = names(get_levels_ih("ae_portion_1"))
    ),
    ae_serious = factor(
      ae_serious,
      levels = get_levels_ih("ae_serious_1"),
      labels = names(get_levels_ih("ae_serious_1"))
    )
  )

## -- Combine and save final datasets ------------------------------------------
## Info on Torsades AEs
saveRDS(torsades_events, file = "analysisdata/rds/torsadesaes.rds")
write_csv(torsades_events, path = "analysisdata/csv/torsadesaes.csv")

## Daily data
safety_df <- reduce(
  list(
    allpts_events %>% dplyr::select(id, redcap_event_name),
    eps_df %>% dplyr::select(id, redcap_event_name, eps_score:eps_yn),
    torsades_df %>% dplyr::select(id, redcap_event_name, starts_with("torsades_"))
  ),
  left_join, by = c("id", "redcap_event_name")
)

saveRDS(safety_df, file = "analysisdata/rds/safetydaily.rds")
write_csv(safety_df, path = "analysisdata/csv/safetydaily.csv")

## Summary variables (NOTE: waiting on final EPS info from PIs)
safety_summary <- reduce(
  list(
    data.frame(id = unique(allpts_events$id)),
    eps_summary_ih,
    eps_summary_int,
    torsades_summary_ih,
    torsades_summary_int
  ),
  left_join, by = "id"
)

saveRDS(safety_summary, file = "analysisdata/rds/safetysummary.rds")
write_csv(safety_summary, path = "analysisdata/csv/safetysummary.csv")
