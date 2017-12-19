################################################################################
## Data management for dates tracking form
################################################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(assertr)

## Source data management functions
source("data_functions.R")

## -- NAMING CONVENTIONS -------------------------------------------------------
## "_exp" = "among those exposed" (eg, time on vent among patients ever on MV)
## "_all" = "all patients" (eg, patients never on MV get 0 for this version)

## -- Import data dictionaries from REDCap -------------------------------------
## All tokens are stored in .Renviron
ih_dd <- get_datadict("MINDUSA_IH_TOKEN")
ih_events <- get_events("MINDUSA_IH_TOKEN") %>% mutate(event_num = 1:nrow(.))
## Add event_num to help with sorting events later
ih_mapping <- get_event_mapping("MINDUSA_IH_TOKEN")

get_levels_ih <-
  function(varname){ get_factor_levels(ddict = ih_dd, varname = varname) }

## -- Bring in ptstatus_df; we'll use some variables from there ----------------
ptstatus_df <- readRDS("analysisdata/rds/ptstatus.rds")

## Vector of all randomized patients never excluded
rand_pts <- ptstatus_df %>%
  filter(randomized & !excluded_ever) %>%
  pull(id)

## -- Download variables from dates tracking form ------------------------------
dates_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  forms = c("dates_tracking_form"),
  events = "enrollment__day_0_arm_1"
) %>%
  ## Remove test patients, restrict to only randomized patients
  filter(!str_detect(toupper(id), "TEST") & id %in% rand_pts) %>%
  ## Remove unneeded variables, REDCap calculated fields
  ##  (they don't handle missings as we want)
  select(-redcap_event_name, -dates_tracking_form_complete, -starts_with("ntf"),
         -starts_with("ae"))

## Get names of all date, date/time variables for easy conversion
date_vars <- ih_dd %>%
  filter(text_validation_type_or_show_slider_number == "date_mdy") %>%
  pull(field_name)
date_vars <- date_vars[date_vars %in% names(dates_raw)]

time_vars <- ih_dd %>%
  filter(text_validation_type_or_show_slider_number == "datetime_mdy") %>%
  pull(field_name)
time_vars <- time_vars[time_vars %in% names(dates_raw)]

## -- Convert all date, time variables -----------------------------------------
dates_df <- dates_raw %>%
  mutate_at(vars(one_of(date_vars)), ymd) %>%
  mutate_at(vars(one_of(time_vars)), ymd_hm)

## -- Find "last in-hospital" date/time: ---------------------------------------
## If hospital discharge available, use that
## Otherwise, if death available, use that
## Last resort: study withdrawal
dates_df <- dates_df %>%
  ## Add time to study withdrawal; assign last time available that date
  mutate(
    studywd_time = ymd_hm(sprintf("%s 23:59", as.character(studywd_time)))
  ) %>%
  
  ## If hospital discharge date is available, use that; otherwise if death date
  ##  is available, use that; otherwise, use study withdrawal as last resort
  mutate(
    last_inhosp_time = case_when(
      !is.na(hospdis_time) ~ hospdis_time,
      !is.na(death_time)   ~ death_time,
      TRUE                 ~ studywd_time
    ),
    last_inhosp_date = date(last_inhosp_time)
  )
## Parse warning is for missing withdrawal times

## -- Ventilation. Here. We. Go. -----------------------------------------------
## For our purposes, for now, all types of MV (invasive + noninvasive) will be
## considered the same.

## -- Create dataset of all instances of MV after/including randomization ------
## One record per instance; final dataset includes:
## - Dates/times of randomization, death, last in-hospital contact (discharge, death, or withdrawal)
## - Instance (eg, "invasive_1"); mv_instance
## - Actual start/stop time; mv_start, mv_stop
## - "Final" stop time - meaning, if patient was not officially discontinued,
##   impute hospital discharge, death, or w/d date as available; mv_stop_final
## - Number of days between randomization and initiation; mv_days_postrand
## - Whether instance overlapped randomization (mv_atrand) and/or began within
##   24h after randomization (mv_rand24h)
## - Total length of instance; mv_length
## - Time patient's *next* instance of MV began (or death, if no next instance
##   available); next_mv_start
## - How long between discontinuation and next MV/death; time_off_mv
## - Indicator for whether discontinuation is "successful" - definition: off MV
##   for at least two days prior to death or reinitiation; mv_dc_success
mv_dates <- dates_df %>%
  select(id,
         matches("^int\\_[1-6]\\_time$"),
         matches("^ext\\_[1-6]\\_time$"),
         matches("^noninv\\_[1-6]\\_time$"),
         matches("^noninv\\_[1-6]\\_dc\\_time$")) %>%
  
  ## Rename variables for easier gathering/separating/etc
  rename_at(
    vars(matches("^int\\_[1-6]\\_time$")),
    funs(str_replace(str_replace(., "^int", "invasive_start"), "\\_time$", ""))
  ) %>%
  rename_at(
    vars(matches("^ext\\_[1-6]\\_time$")),
    funs(str_replace(str_replace(., "^ext", "invasive_stop"), "\\_time$", ""))
  ) %>%
  rename_at(
    vars(matches("^noninv\\_[1-6]\\_time$")),
    funs(str_replace(str_replace(., "^noninv", "nippv_start"), "\\_time$", ""))
  ) %>%
  rename_at(
    vars(matches("^noninv\\_[1-6]\\_dc\\_time$")),
    funs(str_replace(str_replace(., "^noninv", "nippv_stop"), "\\_dc\\_time$", ""))
  ) %>%
  
  ## Reshape: One row per instance (eg, invasive #1), with one column each for
  ##   start/stop times
  gather(key = keyvar, value = mv_time, -id) %>%
  separate(keyvar, into = c("mv_type", "mv_which", "mv_instance"), sep = "\\_") %>%
  unite(mv_instance, c(mv_type, mv_instance)) %>%
  spread(key = mv_which, value = mv_time) %>%
  rename_at(vars(start, stop), funs(paste0("mv_", .))) %>%
  mutate_at(
    vars(mv_start, mv_stop),
    as.POSIXct, origin = "1970-1-1 00:00", tz = "UTC"
  ) %>%
  
  ## Add enrollment, randomization, death, last in-hospital times
  left_join(
    select(dates_df, id, randomization_time, death_time, last_inhosp_time),
    by = "id"
  ) %>%
  
  ## If there is no stop time for MV, assign last in-hospital time
  ## Ex: DEN-013, invasive #1
  mutate(
    mv_stop_final = case_when(
      is.na(mv_start) ~ as.POSIXct(NA),
      !is.na(mv_stop) ~ mv_stop,
      TRUE            ~ last_inhosp_time
    )
  ) %>%
  select(-last_inhosp_time) %>%

  ## Restrict to only MV instances which overlap or come after randomization
  filter(
    !is.na(mv_start) &
      (mv_start >= randomization_time |
         (mv_start < randomization_time & mv_stop_final >= randomization_time))
  ) %>%

  mutate(
    ## How long after (or before) randomization was initiation?
    ## Note: PEN-021 was in the hospital a very long time and has multiple
    ##  intubations long after randomization. Explains extremely large values.
    mv_days_postrand =
      as.numeric(difftime(mv_start, randomization_time, units = "days")),
    
    ## Create indicators for whether this instance included time of
    ##  randomization, or began within 24 hours of randomization
    mv_atrand =
      randomization_time >= mv_start & randomization_time < mv_stop_final,
    mv_rand24h = mv_atrand | between(mv_days_postrand, 0, 1),
    
    ## Calculate length of each ventilation instance; if patient was on MV at
    ## the time of randomization or initiation was within 24 hours of
    ## randomization, that instance begins at randomization (time 0)
    mv_length = ifelse(mv_rand24h,
                       days_diff(mv_stop_final, randomization_time),
                       days_diff(mv_stop_final, mv_start))
  ) %>%
  ## Calculate time between end of instance and next instance or death
  arrange(id, mv_start) %>%
  group_by(id) %>%
  mutate(next_mv_start = lead(mv_start)) %>%
  ungroup() %>%
  mutate(
    ## If patient was not initiated again, but did die,
    ##  use death date as "next start time"
    next_mv_start = as.POSIXct(
      ifelse(is.na(next_mv_start) & !is.na(death_time),
             death_time, next_mv_start),
      origin = "1970-1-1 00:00", tz = "UTC"
    ),
    
    ## Calculate time between discontinuation of MV and reinitiation/death;
    ## if there is no next initiation recorded, use Inf
    time_off_mv = ifelse(is.na(next_mv_start),
                         Inf,
                         days_diff(next_mv_start, mv_stop_final)),
    
    ## Indicator for whether discontinuation was "successful"
    ##  (>48 hours between discontinuation and either reinitiation or death)
    mv_dc_success = time_off_mv > 2
  )

## -- Summarize each patient's MV experience -----------------------------------
## Need to end up with:
## - Total time on MV (either time)
## - Time of "liberation from MV" (first successful discont.), two versions:
##   - Only for patients who ever *were* successfully liberated
##   - TTE outcome time for all pts (substituting last in-hosp time if needed)

## Calculate total time on any form of MV during entire hospitalization
mv_los <- mv_dates %>%
  group_by(id) %>%
  summarise(days_mv_exp = sum_na(mv_length), ## _exp = among exposed
            on_mv_atrand = sum_na(mv_atrand) > 0,
            on_mv_rand24 = sum_na(mv_rand24h) > 0,
            ever_mvlib = sum_na(mv_dc_success) > 0)

## Find first successful extubation for each patient, calculate time to that
mv_firstsucc <- mv_dates %>%
  filter(mv_dc_success) %>%
  group_by(id) %>%
  summarise(first_succ_mvdc = min_na(mv_stop_final)) %>%
  ungroup()

## Combine patient MV info
mv_summary <- reduce(
  list(
    subset(ptstatus_df, randomized, select = c(id)),
    subset(dates_df, select = c(id, randomization_time, last_inhosp_time)),
    mv_los,
    mv_firstsucc
  ),
  left_join, by = "id"
) %>%
  ## Create variable versions for *all* randomized patients
  mutate(
    ## Indicator for whether patient was ever on MV during hospitalization
    ever_mv = !is.na(days_mv_exp),
    ## Total time on vent for all patients (assign 0 if never on MV)
    days_mv_all = ifelse(is.na(days_mv_exp), 0, days_mv_exp),
    ## Days to MV liberation from randomization; should be NA if patients not
    ## on MV at/within 24h after randomization
    daysto_mvlib_exp = ifelse(is.na(on_mv_atrand) | !on_mv_atrand, NA,
                              days_diff(first_succ_mvdc, randomization_time)),
    daysto_mvlib_all = ifelse(is.na(on_mv_atrand) | !on_mv_atrand, NA,
                       ifelse(!is.na(daysto_mvlib_exp), daysto_mvlib_exp,
                              days_diff(last_inhosp_time, randomization_time)))
  ) %>%
  select(-randomization_time, -last_inhosp_time)

# ## -- Data checks: randomization qualification form vs dates tracking ----------
# ## How many patients were on either form of MV at randomization?
# mv_random <- mv_dates %>%
#   filter(mv_start <= randomization_time & mv_stop_final > randomization_time) %>%
#   add_count(id) ## UNC-044 has overlapping invasive/NIPPV times; sent to BP/EH
# 
# ## Cross-check this with checkbox of organ failures at randomization
# mv_random <- mv_random %>%
#   left_join(select(ptstatus_df, id, rand_mv, rand_nippv), by = "id")
# 
# ## Calculate time between randomization and first initiation of MV
# first_mv_random <- mv_dates %>%
#   arrange(id, mv_start) %>%
#   group_by(id) %>%
#   slice(1) %>%
#   mutate(
#     days_mv_rand =
#       as.numeric(difftime(mv_start, randomization_time, units = "days"))
#   ) %>%
#   left_join(select(ptstatus_df, id, rand_mv, rand_nippv))
# 
# ## How many patients on MV at randomization did not have either MV-related
# ##   organ failure marked on RQ form?
# 
# ggplot(data = mv_dates, aes(x = mv_days_postrand)) +
#   geom_histogram(binwidth = 1) +
#   geom_vline(xintercept = 0, color = "red") +
#   labs(title = "Days between Randomization and Initiation of MV,\nAll Patients Who Had MV")
# 
# ggplot(data = subset(first_mv_random, mv_start > randomization_time),
#        aes(x = days_mv_rand)) +
#   geom_histogram(binwidth = 1) +
#   labs(title = "Days between Randomization and First Initiation of MV,\nPatients Who Started MV after Randomization")
# 
# subset(first_mv_random, mv_start <= randomization_time & !(rand_mv | rand_nippv)) %>% 
#   write_csv(path = "rand_onmv_noorgfailure.csv")
