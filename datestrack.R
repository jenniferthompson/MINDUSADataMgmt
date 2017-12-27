################################################################################
## Data management for dates tracking form
################################################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(assertr)

## Source data management functions
source("data_functions.R")

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

## -- Download variables from dates tracking form ------------------------------
dates_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  forms = c("dates_tracking_form"),
  events = "enrollment__day_0_arm_1"
) %>%
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
  mutate_at(vars(one_of(time_vars)), ymd_hm) %>%
  ## Rename hospital admission time for consistency with ICU admissions
  rename(hospadm_time = "hosp_admin_time")

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

## -- Calculate overall date/time variables (eg, hospital LOS) -----------------
## Create factors out of categorical variables
## Calculate days between hospital, ICU admission and enrollment, randomization;
##  hospital LOS (days between randomization and first of discharge,
##  death, or withdrawal); time to events (all beginning at randomization):
## Time to hospital discharge among patients who were discharged
## Time to withdrawal among patients who withdrew, ever and in-hospital
## Time to death among patients who died, ever and in-hospital
## Time to DNR among those who were made DNR/DNI
## Time to stroke among those who had one
## Time to trach among those trached
dates_df <- dates_df %>%
  mutate(
    days_hospadm_enroll = days_diff(enroll_time, hospadm_time),
    days_hospadm_rand   = days_diff(randomization_time, hospadm_time),
    days_icuadm_enroll  = days_diff(enroll_time, icuadm_1_time),
    days_icuadm_rand    = days_diff(randomization_time, icuadm_1_time),
    hosp_los            = days_diff(last_inhosp_time, randomization_time),
    daysto_hospdis      = days_diff(hospdis_time, randomization_time),
    daysto_wd           = days_diff(date(studywd_time), date(randomization_time)),
    daysto_wd_ih        = ifelse(is.na(hospdis_time), daysto_wd, NA),
    daysto_death        = days_diff(death_time, randomization_time),
    daysto_death_ih     = ifelse(is.na(hospdis_time), NA, daysto_death),
    daysto_dnr          = days_diff(dnr_time, randomization_time),
    daysto_stroke       = days_diff(stroke_time, randomization_time),
    daysto_trach        = days_diff(trach_time, randomization_time)
  ) %>%
  ## Create factors
  mutate(
    ## Died *in hospital* if a) died and b) was not discharged
    death_ih = factor(
      as.numeric(death == 1 & hospdis == 0),
      levels = 0:1, labels = c("No", "Yes")
    ),
    ## Withdrew *in hospital* if a) withdrew and b) was not discharged, or if
    ##   withdrawal date on/before hospital discharge date
    studywd_ih = factor(
      as.numeric(
        studywd == 1 &
          (hospdis == 0 | date(studywd_time) <= date(hospdis_time))
      ),
      levels = 0:1, labels = c("No", "Yes")
    ),
    death          = make_factor_ih(., "death"),
    death_wdtrt    = make_factor_ih(., "death_wdtrt"),
    studywd        = make_factor_ih(., "studywd"),
    studywd_person = make_factor_ih(., "studywd_person"),
    studywd_how    = make_factor_ih(., "studywd_how"),
    hospdis        = make_factor_ih(., "hospdis"),
    hospdis_loc    = make_factor_ih(., "hospdis_loc"),
    hospdis_vent   = make_factor_ih(., "hospdis_vent"),
    dnr            = make_factor_ih(., "dnr"),
    sepsis         = make_factor_ih(., "sepsis"),
    stroke         = make_factor_ih(., "stroke"),
    neurosx        = make_factor_ih(., "neurosx"),
    trach          = make_factor_ih(., "trach"),
    liver_tx       = make_factor_ih(., "liver_tx"),
    ## Total instances of either type of MV
    mv_num = int_num + noninv_num
  ) %>%
  ## Rename study withdrawal levels, coenrollment studies
  rename(
    studywd_writing_further  = "studywd_writing_1",
    studywd_writing_phi      = "studywd_writing_2",
    studywd_writing_data     = "studywd_writing_3",
    studywd_writing_blood    = "studywd_writing_4",
    studywd_writing_otherreq = "studywd_writing_5", ## _other already taken
    coenroll_sails      = "coenroll_0",
    coenroll_aki        = "coenroll_1",
    coenroll_citrulline = "coenroll_2",
    coenroll_tylenol    = "coenroll_3",
    coenroll_balance    = "coenroll_4"
  )

## CSV of times that look weird
subset(dates_df,
       days_hospadm_enroll < 0 | days_hospadm_enroll > 100 |
         days_hospadm_rand < 0 | days_hospadm_rand > 100 |
         days_icuadm_enroll < 0 | days_icuadm_enroll > 100 |
         days_icuadm_rand < 0 | days_icuadm_rand > 100,
       select = c(id, hospadm_time, icuadm_1_time, enroll_time,
                  randomization_time, days_hospadm_enroll, days_hospadm_rand,
                  days_icuadm_enroll, days_icuadm_rand)) %>%
  write_csv(path = "admissiondate_errors.csv")

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
## - Total time on MV (either type); per TG December 2017, this should *include*
##     times patient was off the vent <48h before death or reinitiation
## - Time of "liberation from MV" (first successful discont.), two versions:
##   - Only for patients who ever *were* successfully liberated
##   - TTE outcome time for all pts (substituting last in-hosp time if needed)

## Calculate total time on any form of MV during entire hospitalization
mv_los <- mv_dates %>%
  mutate(
    ## New LOS variable: if discontinuation was unsuccessful, add actual time on
    ## MV + time off MV
    mv_length_succ = ifelse(!mv_dc_success, mv_length + time_off_mv, mv_length)
  ) %>%
  group_by(id) %>%
  summarise(days_mv_exp = sum_na(mv_length_succ), ## _exp = among exposed
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

## -- ICU length of stay -------------------------------------------------------
## Total ICU LOS = sum of all individual ICU admissions. If patient has no ICU
## discharge time, substitute last in-hospital time. For first ICU admission,
## start the "clock" at randomization, not ICU admission.
icu_dates <- dates_df %>%
  select(id,
         randomization_time,
         last_inhosp_time,
         matches("^icuadm\\_[1-6]\\_time$"),
         matches("^icudis\\_[1-6]\\_time$")) %>%
  
  ## Reshape: One row per instance (eg, ICU stay #1), with one column each for
  ##   admission/discharge times
  gather(key = keyvar, value = icu_time, icuadm_1_time:icudis_6_time) %>%
  mutate(keyvar = gsub("^icu|\\_time$", "", keyvar)) %>%
  separate(keyvar, into = c("icu_date", "icu_stay"), sep = "\\_") %>%
  spread(key = icu_date, value = icu_time) %>%
  rename_at(vars(adm, dis), funs(paste0("icu_", .))) %>%
  
  ## If no ICU discharge date entered, substitute last in-hospital time
  mutate(
    icu_dis_final = ifelse(!is.na(icu_dis), icu_dis, last_inhosp_time)
  ) %>%
  mutate_at(
    vars(icu_adm, icu_dis, icu_dis_final),
    as.POSIXct, origin = "1970-1-1 00:00", tz = "UTC"
  ) %>%
  
  ## Restrict to only ICU admissions which come after or overlap randomization
  filter(
    !is.na(icu_adm) &
      (icu_adm >= randomization_time |
         (icu_adm < randomization_time & icu_dis_final >= randomization_time))
  ) %>%
  
  ## Calculate length of each ICU admission
  mutate(
    icu_los = ifelse(randomization_time > icu_adm,
                     days_diff(icu_dis_final, randomization_time),
                     days_diff(icu_dis_final, icu_adm))
  )
  
## CSV of negative ICU LOSes
subset(icu_dates, icu_los < 0) %>%
  write_csv(path = "icudate_errors.csv")

## Summarize ICU LOS for each patient
icu_summary <- icu_dates %>%
  group_by(id) %>%
  summarise_at("icu_los", sum_na) %>%
  ungroup()

## -- Create final summary dataset ---------------------------------------------
## For now, leaves out all dates as potential identifiers
datestrack_df <- reduce(
  list(
    subset(ptstatus_df, randomized, select = c(id)),
    subset(
      dates_df,
      select = c(
        id,
        coenroll_sails:coenroll_balance,
        days_hospadm_enroll:days_icuadm_rand,
        dnr, daysto_dnr, sepsis, stroke, daysto_stroke, neurosx,
        trach, daysto_trach, liver_tx,
        int_num, noninv_num, mv_num, icu_readmit_number,
        death, death_ih, death_wdtrt, death_summary, daysto_death, daysto_death_ih,
        studywd, studywd_ih, studywd_person:studywd_writing_other,
        daysto_wd, daysto_wd_ih,
        hospdis, hospdis_loc, hospdis_loc_other, hospdis_vent,
        hosp_los, daysto_hospdis
      )
    ),
    mv_summary,
    icu_summary
  ),
  left_join,
  by = "id"
) %>%
  ## Reorder variables: enrollment/randomization; MV; ICU/hospital LOS;
  ## discharge, withdrawal, death info
  select(id, coenroll_sails:mv_num, ever_mv, days_mv_all, days_mv_exp,
         on_mv_atrand, on_mv_rand24, ever_mvlib, daysto_mvlib_exp,
         daysto_mvlib_all, icu_readmit_number, icu_los, hosp_los,
         hospdis, daysto_hospdis, hospdis_loc:hospdis_vent,
         studywd, daysto_wd, studywd_ih, daysto_wd_ih,
         studywd_person:studywd_writing_other,
         death, death_wdtrt, daysto_death, death_ih, daysto_death_ih)

saveRDS(datestrack_df, file = "analysisdata/rds/datestrack.rds")
write_csv(datestrack_df, path = "analysisdata/csv/datestrack.csv")
