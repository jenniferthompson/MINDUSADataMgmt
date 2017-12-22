################################################################################
## Data management for PAD form (Pain, Agitation, Delirium)
## This form is filled out:
## - Twice daily between consent and randomization
## - Twice daily between randomization and Intervention Day 14 while in ICU
## - Once daily outside ICU and/or on Intervention Days 15-18, if patient still
##   hospitalized
################################################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(assertr)

## Source data management functions
source("data_functions.R")

## Read in dataset with all study events for randomized patients; will need this
## to calculate X within first 14 days inc/after randomization
randpts_events <- readRDS("analysisdata/rds/randptevents.rds")

## -- NAMING CONVENTIONS -------------------------------------------------------
## "_exp" = "among those exposed" (eg, time on vent among patients ever on MV)
## "_all" = "all patients" (eg, patients never on MV get 0 for this version)
## "_ih" = "in hospital" (vs ever; eg, death at any point vs in-hospital death)

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
pad_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "redcap_event_name", "abcde_icu"),
  forms = c("daily_pain_agitation_delirium_pad_assessment"),
  events = setdiff(
    ih_events$unique_event_name,
    c("randomization_arm_1", "prior_to_hospital_arm_1")
  )
) %>%
  ## Remove unneeded variables
  select(-daily_pain_agitation_delirium_pad_assessment_complete)

## -- Data management with raw assessments -------------------------------------
## All "reasons not done" use the same levels; set these ahead of time
## Match order of "xxxxx_incomp_rsn_0" variables
rsns_notdone <-
  c("Patient off floor", "Patient in procedure", "Family refused",
    "Patient refused", "Other",
    "Died, discharged, DQ or w/d prior to assessment")

## Create one long data frame with a record for every *assessment* the patient
## "should" have, plus any "extras"
## eg, on the day patient was discharged from ICU to floor, they may have 1 or 2;
## don't include an "extra" if only one was done, but keep both if two were done
pad_long <- pad_raw %>%
  filter(!is.na(assess_date)) %>% ## These records have no real data
  gather(key = padvar, value = padvalue, assess_time_1:cam_incomp_other_2) %>%
  separate(padvar, into = c("padvar", "assess_which"), sep = "\\_(?=[0-9]$)") %>%
  spread(key = padvar, value = padvalue) %>%
  rename(assess_today = "assess_number") %>%
  ## If no assessments done at all that day, impute reason entered for entire
  ##  day for each assessment (all-day reasons have same factor levels as
  ##  individual assessments, plus 5 = died/discharged/DQ/wd before assessment)
  mutate(
    ## How many assessments "should" the patient have this day?
    ## If in the ICU, and not IT days 15-18, two;
    ##   if not in the ICU, or IT days 15-18, or ABCDEF form not filled out, one
    ## We don't want to keep blank assessments which "shouldn't" be there (eg,
    ## assessment 2 when patient is out of the ICU)
    ## assume days w/o ABCDE form [abcde_icu NA] are out of ICU/after IT period
    it_period = !(redcap_event_name %in%
      subset(ih_events, day_offset > -11)$unique_event_name),
    assess_should = case_when(
      ## OK to only have one assessment on day of enrollment
      redcap_event_name == "enrollment__day_0_arm_1" ~ 1,
      it_period & !is.na(abcde_icu) & abcde_icu == 1 ~ 2,
      TRUE ~ 1
    ),

    cpot_incomp_rsn =
      ifelse(assess_today == 0, cpot_incomp_rsn_0, cpot_incomp_rsn),
    cpot_incomp_other =
      ifelse(assess_today == 0, cpot_incomp_other_0, cpot_incomp_other),
    rass_incomp_rsn =
      ifelse(assess_today == 0, rass_incomp_rsn_0, rass_incomp_rsn),
    rass_incomp_other =
      ifelse(assess_today == 0, rass_incomp_other_0, rass_incomp_other),
    cam_incomp_rsn =
      ifelse(assess_today == 0, cam_incomp_rsn_0, cam_incomp_rsn),
    cam_incomp_other =
      ifelse(assess_today == 0, cam_incomp_other_0, cam_incomp_other),
    
    ## Create dates, times, factors
    ## Values of 99 = Not Done; make these missing in factor versions
    assess_time = ymd_hm(paste(assess_date, assess_time)),
    assess_date = ymd(assess_date),
    
    assess_who = factor(
      assess_who,
      levels = get_levels_ih("assess_who_1"),
      labels = names(get_levels_ih("assess_who_1"))
    ),
    
    cpot = ifelse(cpot == 99, NA, cpot),
    cpot_incomp_rsn = factor(
      cpot_incomp_rsn, levels = 0:5, labels = rsns_notdone
    ),
    
    ## Want to use numeric values of RASS; get these direct from database
    rass = as.numeric(as.character(factor(
      ifelse(rass == 99, NA, rass),
      levels = head(get_levels_ih("rass_1"), -1),
      labels = names(head(get_levels_ih("rass_1"), -1))
    ))),
    rass_incomp_rsn = factor(
      rass_incomp_rsn, levels = 0:5, labels = rsns_notdone
    ),
    
    cam = factor(
      ifelse(cam == 99, NA, cam),
      levels = 0:2, labels = c("Negative", "Positive", "Unable to Assess")
    ),
    cam_f1 = factor(cam_f1, levels = 0:1, labels = c("Absent", "Present")),
    cam_f2 = factor(cam_f2, levels = 0:1, labels = c("Absent", "Present")),
    cam_f3 = factor(cam_f3, levels = 0:1, labels = c("Absent", "Present")),
    cam_f4 = factor(cam_f4, levels = 0:1, labels = c("Absent", "Present")),
    
    cam_incomp_rsn = factor(cam_incomp_rsn, levels = 0:5, labels = rsns_notdone)
  ) %>%
  ## Only keep as many assessments as "should" have been done (df currently has
  ## two for each day), or any "extras"
  filter(assess_which <= assess_should | !is.na(assess_time)) %>%
  ## Add on date of randomization and calculate days before/after randomization
  select(id, redcap_event_name, assess_date, assess_today, assess_should,
         assess_which:assess_who, cpot, cpot_incomp_rsn, cpot_incomp_other,
         rass, rass_incomp_rsn, rass_incomp_other, cam:cam_f2, cam_incomp_rsn,
         cam_incomp_other)

## -- Determine presence of delirium and/or coma on every *day* ----------------
## Coma: Present if any assessment has RASS -4 or -5
## Delirium: Present if any assessment has RASS >= -3 and CAM+
## Daily mental status:
## - delirious if any delirium present
## - otherwise, comatose if any coma present
## - otherwise, normal if at least CAM/RASS done
pad_long <- pad_long %>%
  mutate(
    ## Indicators for whether CAM and/or RASS available (if RASS is -4/-5, can
    ## still determine status even if CAM missing)
    ## If RASS > -4 but CAM is UTA, consider CAM missing; this shouldn't happen
    has_rass = !is.na(rass),
    has_cam = !is.na(cam),
    has_camrass = !is.na(cam) & !is.na(rass) & !(rass > -4 & cam == "Unable to Assess"),
    
    ## Indicators for delirium, coma
    comatose = ifelse(!has_rass, NA, has_rass & rass < -3),
    delirious = ifelse(!has_camrass, NA, rass > -4 & cam == "Positive"),
    ## Currently using most conservative approach: if we can't definitively
    ## say patient is/is not comatose and delirious, brain dysfunction is NA
    braindys =
      ifelse(is.na(comatose) | is.na(delirious), NA,
             (!is.na(comatose) & comatose) | (!is.na(delirious) & delirious))
  )

## Write conflicting CAM/RASSes to CSV for data cleaning
subset(pad_long,
       rass > -4 & cam == "Unable to Assess",
       select = c(id, redcap_event_name, rass, cam)) %>%
  left_join(ih_events %>% select(event_name, unique_event_name),
            by = c("redcap_event_name" = "unique_event_name")) %>%
  write_csv(path = "conflicting_camrass.csv")

## Summarise coma, delirium, brain dysfunction; assign mental status each *day*
pad_daily <- pad_long %>%
  group_by(id, redcap_event_name) %>%
  summarise(
    n_coma = sum(!is.na(comatose)),
    n_del = sum(!is.na(delirious)),
    n_dys = sum(!is.na(braindys)),
    has_rass = sum_na(has_rass) > 0,
    has_cam = sum_na(has_cam) > 0,
    has_camrass = sum_na(has_camrass) > 0,
    comatose = ifelse(n_coma == 0, NA, sum_na(comatose) > 0),
    delirious = ifelse(n_del == 0, NA, sum_na(delirious) > 0),
    braindys = ifelse(n_dys == 0, NA, sum_na(braindys) > 0)
  ) %>%
  ungroup() %>%
  select(-matches("^n\\_")) %>%
  mutate(
    mental_status = factor(
      ## Currently using most conservative approach: if we can't definitively
      ## say that patient is/is not comatose and delirious, mental status is NA
      ifelse(is.na(comatose) | is.na(delirious), NA,
      ifelse(!is.na(delirious) & delirious, 2,
      ifelse(!is.na(comatose) & comatose, 3, 1))),
      levels = 1:3, labels = c("Normal", "Delirious", "Comatose")
    )
  )

## -- Mental status variables during 14 days including+after randomization -----
## Currently assumes no imputation; any hospital day that is missing or patient
##   is withdrawn is essentially assumed to be normal

## Create dataset of only intervention period and determine whether patient was
## alive and free of delirium and coma
pad_int <- pad_daily %>%
  right_join(randpts_events, by = c("id", "redcap_event_name")) %>%
  filter(study_day %in% 0:13) %>%
  mutate(
    dcfree = case_when(
      study_status == "Deceased"    ~ FALSE,
      !is.na(comatose) & comatose   ~ FALSE,
      !is.na(delirious) & delirious ~ FALSE,
      study_status == "Discharged"  ~ TRUE,
      !is.na(braindys) & !braindys  ~ TRUE,
      TRUE                          ~ as.logical(NA)
    )
  )

## Summarize mental status variables for each patient
pad_summary <- pad_int %>%
  group_by(id) %>%
  summarise(
    ## Because RASS was sometimes available when CAM was not, we can determine
    ##  coma status more frequently than we can determine delirium/overall
    n_coma_avail = sum(!is.na(comatose)),
    n_mental_avail = sum(!is.na(mental_status)),
    n_dcfree_avail = sum(!is.na(dcfree)),
    n_missing_dcfree = sum(is.na(dcfree)),
    
    ## Among all patients
    coma_int_all = ifelse(n_coma_avail == 0, NA, sum_na(comatose)),
    del_int_all = ifelse(n_mental_avail == 0, NA, sum_na(delirious)),
    delcoma_int_all = ifelse(n_mental_avail == 0, NA, sum_na(braindys)),
    
    ## Among patients who ever experienced coma/delirium
    ## Delirium and delirium/coma *should* be the same; as of Dec 2017, we know
    ##  that one patient has a difference in delirium (0 delirium days)
    coma_int_exp =
      ifelse(is.na(coma_int_all) | coma_int_all == 0, NA, coma_int_all),
    del_int_exp =
      ifelse(is.na(del_int_all) | del_int_all == 0, NA, del_int_all),
    delcoma_int_exp =
      ifelse(is.na(delcoma_int_all) | delcoma_int_all == 0, NA, delcoma_int_all),
    
    ## Current calculation: Assume all days with missing info are NORMAL
    dcfd_int_all = ifelse(
      n_dcfree_avail == 0, NA,
      sum(is.na(dcfree)) + sum_na(dcfree)
    )
  ) %>%
  select(id, n_coma_avail, coma_int_all, coma_int_exp, n_mental_avail,
         del_int_all, del_int_exp, delcoma_int_all, delcoma_int_exp,
         n_dcfree_avail, dcfd_int_all)

## -- Write datasets to analysisdata -------------------------------------------
## 1. All assessments
saveRDS(pad_long %>% select(-assess_date, assess_time),
        file = "analysisdata/rds/padasmts.rds")
write_csv(pad_long %>% select(-assess_date, assess_time),
          path = "analysisdata/csv/padasmts.csv")

## 2. Daily records
saveRDS(pad_daily, file = "analysisdata/rds/paddays.rds")
write_csv(pad_daily, path = "analysisdata/csv/paddays.csv")

## 3. Summary variables
saveRDS(pad_summary, file = "analysisdata/rds/padsummary.rds")
write_csv(pad_summary, path = "analysisdata/csv/padsummary.csv")

## -- Exploration of missingness -----------------------------------------------
## Show how many patients had how many days with DCFD info available
dcfd_avail <- pad_summary %>%
  ## Did patients ever withdraw from the study?
  left_join(ptstatus_df %>% select(id, wd_inhosp)) %>%
  mutate(
    all_dcfd_avail = factor(
      as.numeric(n_dcfree_avail == 14),
      levels = 0:1, labels = c("Missing >=1 day", "All days available")
    ),
    wd_inhosp = factor(
      as.numeric(wd_inhosp),
      levels = 0:1,
      labels = c("Remained in study", "Officially withdrew in hospital")
    )
  )

ggplot(data = dcfd_avail, aes(x = n_dcfree_avail)) +
  facet_wrap(~ wd_inhosp) +
  geom_histogram(aes(fill = all_dcfd_avail), binwidth = 1) +
  scale_fill_viridis_d(name = "Missing anything?", end = 0.75) +
  scale_x_continuous(
    breaks = seq.int(0, 14, 2),
    name = "Number of days with sufficient info to determine mental status"
  ) +
  scale_y_continuous(name = "Count") +
  labs(
    title = "Number of days with info available for mental status"
  )

ms_missing <-
  subset(
    pad_int,
    study_status %in% c("Intervention", "Post-intervention",
                        "Hospitalized; no longer being assessed") &
      is.na(mental_status)
  )

