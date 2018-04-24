################################################################################
## Data management for dates tracking form
################################################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(assertr)

## Source data management functions
source("data_functions.R")

## When to censor time-to-event outcomes for each time frame
censor_14 <- 14.01
censor_30 <- 30.01
censor_90 <- 90.01

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
  rename(hospadm_time = "hosp_admin_time") %>%
  ## Randomized patients who have both death and hospdis marked No all withdrew
  ## in the hospital; change these indicators both to NA - by inspection of the
  ## database, it appears that study staff were asked not to access PHI,
  ## formally or informally
  mutate(
    hospdis_noinfo = id %in% rand_pts & death == 0 & hospdis == 0,
    death = ifelse(hospdis_noinfo, NA, death),
    hospdis = ifelse(hospdis_noinfo, NA, hospdis)
  )

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
  ) %>%
  mutate(
    days_hospadm_enroll = days_diff(enroll_time, hospadm_time),
    days_hospadm_rand   = days_diff(randomization_time, hospadm_time),
    days_icuadm_enroll  = days_diff(enroll_time, icuadm_1_time),
    days_icuadm_rand    = days_diff(randomization_time, icuadm_1_time),
    hosp_los            = days_diff(last_inhosp_time, randomization_time),
    daysto_hospdis      = days_diff(hospdis_time, randomization_time),
    daysto_wd           = days_diff(date(studywd_time), date(randomization_time)),
    daysto_death        = days_diff(death_time, randomization_time),
    ## Days to in-hospital death, withdrawal:
    ## If patient didn't die/withdraw, NA
    ## If patient died/withdrew, but time is after hospital discharge, NA
    ## Otherwise, days to overall death/withdrawal
    daysto_death_ih     = case_when(
      is.na(daysto_death)                              ~ as.numeric(NA),
      !is.na(hospdis_time) & hospdis_time < death_time ~ as.numeric(NA),
      TRUE                                             ~ daysto_death
    ),
    daysto_wd_ih     = case_when(
      is.na(daysto_wd)                                   ~ as.numeric(NA),
      !is.na(hospdis_time) &
        date(hospdis_time) < date(studywd_time) ~ as.numeric(NA),
      TRUE                                               ~ daysto_wd
    ),
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
        studywd == 1 & (
          hospdis_noinfo | ## pts who withdrew PHI access
            hospdis == 0 |  ## pts who were not discharged
            date(studywd_time) <= date(hospdis_time) ## discharged after w/d
        )
      ),
      levels = 0:1, labels = c("No", "Yes")
    ),
    ## Was hospital discharge "successful" (followed by >=48h alive)?
    hospdis_succ = factor(
      case_when(
        is.na(hospdis) | (hospdis == 1 & is.na(hospdis_time)) ~ as.numeric(NA),
        is.na(hospdis_time) ~ 0,
        is.na(death_time) ~ 1,
        as.numeric(difftime(death_time, hospdis_time, units = "hours")) >= 0 &
          as.numeric(difftime(death_time, hospdis_time, units = "hours")) <= 48
        ~ 0,
        TRUE ~ 1
      ),
      levels = 0:1, labels = c("No", "Yes")
    ),
    ## Time to successful discharge among exposed
    daysto_hospdis_succ = case_when(
      !is.na(hospdis_succ) & hospdis_succ == "Yes" ~ hosp_los,
      TRUE                                         ~ as.numeric(NA)
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
  write_csv(path = "datachecks/admissiondate_errors.csv")

## -- Ventilation. Here. We. Go. -----------------------------------------------
## For our purposes, for now, all types of MV (invasive + noninvasive) will be
## considered the same.

## -- Create dataset of all instances of MV after/including randomization ------
## One record per instance; final dataset includes:
## - Dates/times of randomization, death, last in-hospital contact
##     (discharge, death, or withdrawal)
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
    dplyr::select(
      dates_df,
      id, randomization_time, hospdis_noinfo, death_time, last_inhosp_time
    ),
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
    ##  (MV actually discontinued, and >48 hours between discontinuation and
    ##   either reinitiation or death)
    mv_dc_success = ifelse(
      ## If no discharge or death info is available, we don't know
      hospdis_noinfo, NA,
      !is.na(mv_stop) & time_off_mv > 2
    )
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
  ungroup() %>%
  mutate(
    first_succ_mvdc =
      as.POSIXct(first_succ_mvdc, origin = "1970-1-1 00:00", tz = "UTC")
  )

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
    ## Indicators for whether patient was on MV at, within 24h of randomization
    on_mv_atrand = ifelse(is.na(on_mv_atrand), FALSE, on_mv_atrand),
    on_mv_rand24 = ifelse(is.na(on_mv_rand24), FALSE, on_mv_rand24),
    ## Ind: ever liberated from MV *among pts on it w/in 24h of randomization*
    ever_mvlib_rand24 = ifelse(!on_mv_rand24, NA, ever_mvlib),
    ## Total time on vent for all patients (assign 0 if never on MV)
    days_mv_all = ifelse(is.na(days_mv_exp), 0, days_mv_exp),
    ## Days to MV liberation from randomization; should be NA if patients not
    ## on MV at/within 24h after randomization
    daysto_mvlib_exp = ifelse(is.na(on_mv_rand24) | !on_mv_rand24, NA,
                              days_diff(first_succ_mvdc, randomization_time)),
    daysto_mvlib_all = ifelse(is.na(on_mv_rand24) | !on_mv_rand24, NA,
                       ifelse(!is.na(daysto_mvlib_exp), daysto_mvlib_exp,
                              days_diff(last_inhosp_time, randomization_time)))
  ) %>%
  select(-randomization_time, -last_inhosp_time)

## -- Data checks: randomization qualification form vs dates tracking ----------
## How many patients were on either form of MV at randomization?
mv_random <- mv_dates %>%
  filter(mv_start <= randomization_time & mv_stop_final > randomization_time) %>%
  add_count(id) ## UNC-044 has overlapping invasive/NIPPV times; sent to BP/EH

## Cross-check this with checkbox of organ failures at randomization
mv_random <- mv_random %>%
  left_join(select(ptstatus_df, id, rand_mv, rand_nippv), by = "id")

## Calculate time between randomization and first initiation of MV
first_mv_random <- mv_dates %>%
  arrange(id, mv_start) %>%
  group_by(id) %>%
  slice(1) %>%
  mutate(
    days_mv_rand =
      as.numeric(difftime(mv_start, randomization_time, units = "days"))
  ) %>%
  left_join(select(ptstatus_df, id, rand_mv, rand_nippv))

## How many patients on MV at randomization did not have either MV-related
##   organ failure marked on RQ form?

ggplot(data = mv_dates, aes(x = mv_days_postrand)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Days between Randomization and Initiation of MV,\nAll Patients Who Had MV")

ggplot(data = subset(first_mv_random, mv_start > randomization_time),
       aes(x = days_mv_rand)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Days between Randomization and First Initiation of MV,\nPatients Who Started MV after Randomization")

subset(first_mv_random, mv_start <= randomization_time & !(rand_mv | rand_nippv)) %>%
  write_csv(path = "datachecks/rand_onmv_noorgfailure.csv")
## Note: MOC-034 shows up in this check but is confirmed OK Feb 2018; this was a
## combimation of unusual circumstances + study definition of extubation (48h).
## See event reporting form for details.

## -- ICU length of stay -------------------------------------------------------
## Total ICU LOS = sum of all individual ICU admissions. If patient has no ICU
## discharge time, substitute last in-hospital time. For first ICU admission,
## start the "clock" at randomization, not ICU admission.
icu_dates <- dates_df %>%
  select(id,
         hospdis_noinfo,
         randomization_time,
         last_inhosp_time,
         death_time,
         studywd_time,
         matches("^icuadm\\_[1-6]\\_time$"),
         matches("^icudis\\_[1-6]\\_time$")) %>%
  
  ## Reshape: One row per instance (eg, ICU stay #1), with one column each for
  ##   admission/discharge times
  gather(key = keyvar, value = icu_time, icuadm_1_time:icudis_6_time) %>%
  mutate(keyvar = gsub("^icu|\\_time$", "", keyvar)) %>%
  separate(keyvar, into = c("icu_date", "icu_stay"), sep = "\\_") %>%
  spread(key = icu_date, value = icu_time) %>%
  rename_at(vars(adm, dis), funs(paste0("icu_", .))) %>%
  
  ## If no ICU discharge date entered (eg, patient died before discharge),
  ##  substitute last in-hospital time
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
  
  ## Calculate length of each ICU admission, determine whether it was
  ##  "successful" - followed by at least 48 hours alive
  mutate(
    icu_los = ifelse(randomization_time > icu_adm,
                     days_diff(icu_dis_final, randomization_time),
                     days_diff(icu_dis_final, icu_adm)),
    icudis_succ = ifelse(
      ## If patient has no discharge or death info, we don't know if it was
      ##  successful
      hospdis_noinfo, NA,
      !((!is.na(death_time) & days_diff(death_time, icu_dis_final) <= 2) |
          (!is.na(studywd_time) & is.na(icu_dis)))
    )
  ) %>%
  ## Add date/time of *next* ICU admission, if applicable (NA otherwise)
  arrange(id, icu_adm) %>%
  group_by(id) %>%
  mutate(
    icu_adm_next = lead(icu_adm),
    time_on_floor = case_when(
      !is.na(icu_adm_next) ~ as.numeric(
        difftime(icu_adm_next, icu_dis, units = "days")
      ),
      TRUE ~ as.numeric(NA)
    )
  ) %>%
  ungroup()

## CSV of negative ICU LOSes
subset(icu_dates, icu_los < 0) %>%
  write_csv(path = "datachecks/icudate_errors.csv")

## Summarize ICU LOS for each patient
icu_summary <- icu_dates %>%
  group_by(id) %>%
  summarise_at("icu_los", sum_na) %>%
  ungroup()

## Get date of *final* ICU discharge for each patient who was discharged
last_icu_dc <- icu_dates %>%
  arrange(id, icu_adm) %>%
  group_by(id) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(
    daysto_icudis_all = days_diff(icu_dis_final, randomization_time),
    daysto_icudis_exp = days_diff(icu_dis, randomization_time),
    icudis_succ = factor(
      as.numeric(icudis_succ), levels = 1:0, labels = c("Yes", "No")
    )
  )

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
        death, death_wdtrt, death_summary, daysto_death,
        death_ih, daysto_death_ih,
        studywd, studywd_ih, hospdis_noinfo, studywd_person:studywd_writing_other,
        daysto_wd, daysto_wd_ih,
        hospdis, hospdis_succ, hospdis_loc, hospdis_loc_other, hospdis_vent,
        hosp_los, daysto_hospdis, daysto_hospdis_succ
      )
    ),
    mv_summary,
    icu_summary,
    last_icu_dc %>%
      dplyr::select(id, icudis_succ, daysto_icudis_all, daysto_icudis_exp)
  ),
  left_join,
  by = "id"
) %>%
  ## Create time-to-event variables, cut off or censored at a given time point
  ## In-hospital outcomes all cut off at 30 days
  ## "Successful" ICU or hospital discharge is followed by 48 hours alive.
  mutate(
    ## Thanks to A+ followup team, we can be pretty certain that if a patient
    ## died, the date will be entered, provided we had permission to access PHI.
      ## One known exception (VAN-278): family member could not remember exact
      ## date of death & team couldn't find obituary. At end of study, we'll
      ## find it using SSDI. We know that it was >30 days after randomization,
      ## and account for that here.
    ## is.na(death) included for patients who withdrew in the hospital with no
    ## PHI access permitted; these should be censored at time of withdrawal.
    tte_death_30 = case_when(
      ## Special case as noted above:
      ##  VAN-278 known to have died > day 30, no death date currently available
      !is.na(death) & death == "Yes" & is.na(daysto_death) ~ censor_30,
      is.na(hosp_los)                                      ~ as.numeric(NA),
      (is.na(death) | death == "Yes") &
        is.na(daysto_death) & hosp_los <= 30               ~ hosp_los,
      (is.na(death) | death == "Yes") &
        !is.na(daysto_death) & daysto_death <= 30          ~ daysto_death,
      TRUE                                                 ~ censor_30
    ),
    event_death_30 = case_when(
      death == "Yes" & daysto_death <= 30 ~ TRUE,
      TRUE                                ~ FALSE
    ),
    tte_death_90 = case_when(
      ## Special case as noted above:
      ##  VAN-278 known to have died > day 30, no death date currently available
      !is.na(death) & death == "Yes" & is.na(daysto_death) ~ censor_90,
      is.na(hosp_los)                                      ~ as.numeric(NA),
      (is.na(death) | death == "Yes") &
        is.na(daysto_death) & hosp_los <= 90               ~ hosp_los,
      (is.na(death) | death == "Yes") &
        !is.na(daysto_death) & daysto_death <= 90          ~ daysto_death,
      TRUE                                                 ~ censor_90
    ),
    event_death_90 = case_when(
      death == "Yes" & daysto_death <= 90 ~ TRUE,
      TRUE                                ~ FALSE
    ),
    tte_mvlib_30 = case_when(
      !on_mv_rand24 | is.na(daysto_mvlib_all) ~ as.numeric(NA),
      daysto_mvlib_all <= 30                  ~ daysto_mvlib_all,
      TRUE                                    ~ censor_30
    ),
    event_mvlib_30 = case_when(
      !on_mv_rand24                       ~ as.logical(NA),
      ever_mvlib & daysto_mvlib_all <= 30 ~ TRUE,
      TRUE                                ~ FALSE
    ),
    ftype_mvlib_30 = factor(
      case_when(
        !on_mv_rand24                       ~ as.numeric(NA),
        event_mvlib_30                      ~ 1,
        death == "Yes" & daysto_death <= 30 ~ 2,
        TRUE                                ~ 0
      ),
      levels = 0:2, labels = c("Censored", "MV Liberation", "Death")
    ),
    tte_icudis_90 = case_when(
      is.na(daysto_icudis_all)                 ~ as.numeric(NA),
      !is.na(daysto_death) &
        (daysto_death - daysto_icudis_all < 2) ~ daysto_death,
      daysto_icudis_all <= 90                  ~ daysto_icudis_all,
      TRUE                                     ~ censor_90
    ),
    event_icudis_90 = case_when(
      !is.na(icudis_succ) &
        icudis_succ == "Yes" & daysto_icudis_all <= 90 ~ TRUE,
      TRUE                                             ~ FALSE
    ),
    ftype_icudis_90 = factor(
      case_when(
        event_icudis_90 ~ 1,
        event_death_90  ~ 2,
        TRUE            ~ 0
      ),
      levels = 0:2, labels = c("Censored", "ICU Discharge", "Death")
    ),
    tte_hospdis_90 = case_when(
      is.na(hosp_los)                                      ~ as.numeric(NA),
      daysto_hospdis_succ <= 90                            ~ daysto_hospdis_succ,
      !is.na(daysto_death) & (daysto_death - hosp_los < 2) ~ daysto_death,
      hosp_los <= 90                                       ~ hosp_los,
      TRUE                                                 ~ censor_90
    ),
    event_hospdis_90 = case_when(
      hospdis_succ == "Yes" & daysto_hospdis_succ <= 90 ~ TRUE,
      TRUE                                              ~ FALSE
    ),
    ftype_hospdis_90 = factor(
      case_when(
        event_hospdis_90 ~ 1,
        event_death_90   ~ 2,
        TRUE             ~ 0
      ),
      levels = 0:2, labels = c("Censored", "Hospital Discharge", "Death")
    )
  ) %>%
  ## Reorder variables: enrollment/randomization; MV; ICU/hospital LOS;
  ## discharge, withdrawal, death info
  select(id, coenroll_sails:mv_num, ever_mv, days_mv_all, days_mv_exp,
         on_mv_atrand, on_mv_rand24, ever_mvlib, ever_mvlib_rand24,
         daysto_mvlib_exp, daysto_mvlib_all,
         icu_readmit_number, icu_los, icudis_succ, daysto_icudis_all,
         daysto_icudis_exp,
         hosp_los, hospdis, daysto_hospdis, hospdis_succ, daysto_hospdis_succ,
         hospdis_loc:hospdis_vent,
         studywd, daysto_wd, studywd_ih, hospdis_noinfo, daysto_wd_ih,
         studywd_person:studywd_writing_other,
         death, death_wdtrt, daysto_death, death_ih, daysto_death_ih,
         matches("^tte|event|ftype"))

saveRDS(datestrack_df, file = "analysisdata/rds/datestrack.rds")
write_csv(datestrack_df, path = "analysisdata/csv/datestrack.csv")
