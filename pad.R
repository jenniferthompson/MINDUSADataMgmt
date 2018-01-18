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
library(mice) ## for imputing missing mental status

## Source data management functions
source("data_functions.R")

## Read in dataset with all study events for randomized patients; will need this
## to calculate X within first 14 days inc/after randomization
randpts_events <- readRDS("analysisdata/rds/randptevents.rds")

## Read in daily, baseline data to help with imputation
demog_df <- readRDS("analysisdata/rds/admission.rds")
daily_df <- readRDS("analysisdata/rds/dailydata.rds")

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

## -- Download variables from daily PAD form -----------------------------------
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

## -- Download time of CAM+ that qualified patient for randomization -----------
## Also get date/time of randomization
randdate_df <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  fields = c("id", "randomization_time"),
  events = "enrollment__day_0_arm_1"
) %>%
  filter(id %in% rand_pts) %>%
  select(-redcap_event_name) %>%
  mutate(randomization_time = ymd_hm(randomization_time))

randcam_df <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "first_cam_time"),
  events = "randomization_arm_1"
) %>%
  filter(id %in% rand_pts) %>%
  mutate(first_cam_time = ymd_hm(first_cam_time)) %>%
  select(-redcap_event_name)

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
## CAM must be present to qualify for delirium or normal status.
## Coma can be based on RASS or CAM alone.
##
## Coma: Present if RASS -4 or -5, or if RASS is missing and CAM is UTA
## Delirium: Present if RASS either missing or >= -3, and CAM+
## Normal: Present if RASS either missing or >= -3, and CAM-
## Missing: Anything else
##
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
    has_camrass =
      !is.na(cam) & !is.na(rass) & !(rass > -4 & cam == "Unable to Assess"),
    ## Indicator for whether CAM, RASS conflict: RASS > -4 and CAM is UTA, or vv
    camrass_conflict = case_when(
      has_rass & rass > -4 & has_cam & cam == "Unable to Assess"          ~ TRUE,
      has_rass & rass < -3 & has_cam & cam %in% c("Positive", "Negative") ~ TRUE,
      TRUE                                                                ~ FALSE
    ),

    ## Indicators for delirium, coma, normal status
    comatose = case_when(
      camrass_conflict | (!has_rass & !has_cam) ~ as.logical(NA),
      has_rass & rass < -3                      ~ TRUE,
      has_cam & cam == "Unable to Assess"       ~ TRUE,
      TRUE                                      ~ FALSE
    ),
    delirious = case_when(
      camrass_conflict            ~ as.logical(NA),
      has_rass & rass < -3        ~ FALSE,
      !has_cam                    ~ as.logical(NA),
      has_cam & cam == "Positive" ~ TRUE,
      TRUE                        ~ FALSE
    ),
    normal = case_when(
      camrass_conflict | !has_cam ~ as.logical(NA),
      has_cam & cam == "Negative" ~ TRUE,
      TRUE                        ~ FALSE
    ),
    ## Currently using most conservative approach: if we can't definitively
    ## say patient is/is not comatose and delirious, brain dysfunction is NA
    braindys = case_when(
      camrass_conflict              ~ as.logical(NA),
      !is.na(comatose) & comatose   ~ TRUE,
      !is.na(delirious) & delirious ~ TRUE,
      !is.na(normal) & normal       ~ FALSE,
      TRUE                          ~ as.logical(NA)
    )
  )

## Write conflicting CAM/RASSes to CSV for data cleaning
subset(pad_long,
       rass > -4 & cam == "Unable to Assess",
       select = c(id, redcap_event_name, rass, cam)) %>%
  left_join(ih_events %>% select(event_name, unique_event_name),
            by = c("redcap_event_name" = "unique_event_name")) %>%
  write_csv(path = "datachecks/conflicting_camrass.csv")

## Summarise coma, delirium, brain dysfunction; assign mental status each *day*
pad_daily <- pad_long %>%
  ## Need to add on in-hospital days during in-hospital followup (day 0 up to
  ## potentially day 18) where patient had no assessment data (due to withdrawal
  ## or other reasons). Not in current dataset because no assessment was done,
  ## but we want to impute mental status for these days to avoid having to
  ## assume they are normal.
  full_join(
    randpts_events %>%
      filter(
        study_status %in% c("Intervention", "Post-intervention", "Withdrawn")
      ) %>%
      dplyr::select(id, redcap_event_name),
    by = c("id", "redcap_event_name")
  ) %>%
  group_by(id, redcap_event_name) %>%
  summarise(
    n_coma = sum(!is.na(comatose)),
    n_del = sum(!is.na(delirious)),
    n_normal = sum(!is.na(normal)),
    n_dys = sum(!is.na(braindys)),
    has_rass = sum_na(has_rass) > 0,
    has_cam = sum_na(has_cam) > 0,
    has_camrass = sum_na(has_camrass) > 0,
    ## _raw because we will also have imputed versions of these
    comatose_raw = ifelse(n_coma == 0, NA, sum_na(comatose) > 0),
    delirious_raw = ifelse(n_del == 0, NA, sum_na(delirious) > 0),
    normal_raw = ifelse(n_normal == 0, NA, sum_na(normal) > 0),
    braindys_raw = ifelse(n_dys == 0, NA, sum_na(braindys) > 0)
  ) %>%
  ungroup() %>%
  select(-matches("^n\\_")) %>%
  mutate(
    mental_status_raw = factor(
      ## Currently using most conservative approach: if we can't definitively
      ## say that patient is/is not comatose and delirious, mental status is NA
      ifelse(is.na(comatose_raw) | is.na(delirious_raw), NA,
      ifelse(!is.na(delirious_raw) & delirious_raw, 2,
      ifelse(!is.na(comatose_raw) & comatose_raw, 3, 1))),
      levels = 1:3, labels = c("Normal", "Delirious", "Comatose")
    )
  )

# ## -- Explore missingness to help inform imputation decisions ------------------
# ## All in-hospital days
# allpts_events <- readRDS("analysisdata/rds/allptevents.rds")
# pad_missing_ih <- pad_daily %>%
#   right_join(
#     dplyr::select(
#       allpts_events,
#       id, redcap_event_name, days_since_consent, intervention
#     ),
#     by = c("id", "redcap_event_name")
#   ) %>%
#   dplyr::select(id, days_since_consent, mental_status_raw) %>%
#   mutate(has_data_ih = factor(
#     as.numeric(!is.na(mental_status_raw)),
#     levels = 0:1, labels = c("Missing", "Present")
#   ))
# 
# pad_daysmiss_ih <- pad_missing_ih %>%
#   group_by(id) %>%
#   summarise(nmiss = sum(has_data_ih == "Missing")) %>%
#   ungroup() %>%
#   arrange(desc(nmiss), id) %>%
#   mutate(new_id = 1:n())
# 
# pad_missing_ih <- left_join(pad_missing_ih, pad_daysmiss_ih, by = "id")
# 
# ggplot(data = pad_missing_ih %>% filter(nmiss > 0),
#        aes(x = new_id, y = days_since_consent)) +
#   geom_hline(yintercept = seq(0, 20, 1), colour = "gray50", size = 0.5) +
#   geom_raster(aes(fill = has_data_ih)) +
#   scale_fill_manual(values = c("darkred", "black")) +
#   theme_dark() +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.x = element_blank(),
#         legend.position = "bottom",
#         legend.direction = "horizontal")
# 
# ## Days during the intervention period
# pad_missing_int <- pad_daily %>%
#   right_join(
#     dplyr::select(
#       randpts_events,
#       id, redcap_event_name, study_day, study_status, intervention
#     ),
#     by = c("id", "redcap_event_name")
#   ) %>%
#   filter(intervention) %>%
#   dplyr::select(id, study_day, study_status, mental_status_raw) %>%
#   mutate(has_data_int = factor(
#     ifelse(
#       study_status %in%
#         c("Hospitalized; no longer being assessed", "Discharged", "Deceased"),
#       3,
#       as.numeric(!is.na(mental_status_raw)) + 1
#     ),
#     levels = 1:3,
#     labels = c("Missing", "Present",
#                "No assessment needed\n(Discharged, deceased)")
#   ))
# 
# ## How many days is each patient missing? Will order plot by this
# pad_daysmiss_int <- pad_missing_int %>%
#   group_by(id) %>%
#   summarise(nmiss = sum(has_data_int == "Missing")) %>%
#   ungroup() %>%
#   arrange(desc(nmiss), id) %>%
#   mutate(new_id = 1:n())
# 
# pad_missing_int <- left_join(pad_missing_int, pad_daysmiss_int, by = "id")
# 
# pad_missplot_int <-
#   ggplot(data = pad_missing_int %>% filter(nmiss > 0),
#          aes(x = new_id, y = study_day)) +
#   geom_raster(aes(fill = has_data_int)) +
#   # geom_hline(yintercept = seq(0, 13, 1), colour = "gray50", size = 0.5) +
#   scale_y_continuous(breaks = seq(0, 13, 2)) +
#   scale_fill_manual(values = c("darkred", "black", "gray20")) +
#   theme_dark() +
#   theme(plot.background = element_rect(fill = "gray50"),
#         # axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.x = element_blank(),
#         legend.position = "bottom",
#         legend.direction = "horizontal",
#         legend.background = element_blank())
# 
# pdf(file = "pad_missplot_int.pdf", width = 11, height = 4)
# pad_missplot_int
# dev.off()
# 
# ## Dates tracking info for patients with >2 days missing - did they withdraw?
# datestrack_df <- readRDS("analysisdata/rds/datestrack.rds")
# dt_missing <- datestrack_df %>%
#   filter(id %in% subset(pad_daysmiss_int, nmiss > 2)$id)

## -- Decision: Use single imputation to impute missing mental status ----------

## Rationale: We have relatively low rates of missingness, thanks to the hard
##  work of our research coordinators (including days after withdrawal, <4% of
##  all in-hospital days; 4.9% hospital days during intervention period).
## Imputation lets us incorporate statuses on the day before/after as well as
##  other covariates, like severity of illness, into filling in missing values.
##  We use single imputation (vs multiple) because we'll need to summarize these
##  daily variables into variables like delirium duration and DCFDs. This choice
##  is preferable to LOCF both because mental status can change quite quickly,
##  meaning that relying on LOCF for >1 day can be risky, and because a few
##  consented patients have no mental status available on the day of consent,
##  meaning there is nothing to carry forward.

## Create dataset with variables for imputation
pad_forimputation <- pad_daily %>%
  ## Daily data: Drugs/drug classes, raw variables used for SOFA
  ## (Use total converted benzos, opioids, antipsychotics, not individual drugs)
  left_join(
    dplyr::select(
      daily_df,
      -nms_daily,
      -matches("^daily\\_midaz|diaz|loraz|fent|hydromorph|morph|halop|quet|ari|olanz|risp|zipras$"),
      -matches("sofa\\_.*[imp|raw]$")
    ),
    by = c("id", "redcap_event_name")
  ) %>%
  ## Baseline data: age, gender, insurance, bmi, home antipsychotics,
  ##  comorbidities, frailty, APACHE APS, education, language
  left_join(
    dplyr::select(
      demog_df,
      id, age_consent, gender, insurance, bmi, home_antipsyc, charlson_total,
      frailty, apache_aps_adm, education, english_level
    ),
    by = "id"
  ) %>%
  ## Add variables for mental status the day before, after
  group_by(id) %>%
  mutate(mental_status_before = lag(mental_status_raw, n = 1),
         mental_status_after = lead(mental_status_raw, n = 1),
         comatose_before = lag(comatose_raw, n = 1),
         comatose_after = lead(comatose_raw, n = 1)) %>%
  ungroup() %>%
  dplyr::select(-id, -redcap_event_name, -has_rass, -has_cam, -has_camrass)

## mice options:
## - For imputing mental status, use default method of polytomous regression
## - Impute variables in increasing order of missingness
## - Set seed for reproducibility
## - Need only one final dataset
daily_mice <- mice(
  data = pad_forimputation,
  m = 1,
  visitSequence = "monotone",
  seed = 8675309
)

## Extract filled-in mental status variable; append to pad_daily as new variable
pad_daily$mental_status_imp <- complete(daily_mice)$mental_status_raw

## Create imputed delirium, coma, normal, braindys versions
pad_daily$comatose_imp <- as.logical(complete(daily_mice)$comatose_raw)
pad_daily <- pad_daily %>%
  mutate(
    ## Coma: Patient could have mental_status of delirium but also be comatose;
    ##   due to imputation, comatose_imp could also be F while mental_status_imp
    ##   is comatose. Make sure these variables align, treating
    ##   mental_status_imp as source of truth.
    comatose_imp = mental_status_imp == "Comatose" |
      (comatose_imp & !(mental_status_imp == "Normal")),
    ## Delirium, normal are more straightforward - must match mental_status_imp
    delirious_imp = mental_status_imp == "Delirious",
    normal_imp = mental_status_imp == "Normal",
    braindys_imp = mental_status_imp %in% c("Comatose", "Delirious")
  )

## -- Figure out RASS level at the time of randomization -----------------------
## Tried matching by exact time of PAD assessment, but >100 patients had no RASS
## available by that means. New tactic: First available assessment within 2hr
## before/after randomization.

## Get all PAD assessments within X hours before/after time of randomization
within_hrs_rand <- 5

rand_asmts <- pad_long %>%
  left_join(randdate_df, by = "id") %>%
  ## Calculate hours between randomization and PAD assessment
  mutate(hrs_btwn_random = abs(
    as.numeric(difftime(randomization_time, assess_time, units = "hours"))
  )) %>%
  ## We want non-missing, non-comatose RASSes within 5 hours of randomization
  filter(!is.na(rass) & rass >= -3 & hrs_btwn_random) %>%
  ## Take non-missing RASS closest to randomization time per pt (before/after)
  arrange(id, hrs_btwn_random) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  rename(rass_rand = "rass") %>%
  select(id, rass_rand)

# ## Write CSV of patients whose "first CAM+" is not on the date of randomization
# ## Note: doesn't work with current code, sorry
# write_csv(
#   subset(
#     randcam_df,
#     first_cam_date != randomization_date,
#     select = c(id, first_cam_time, randomization_time)
#   ),
#   path = "datachecks/campos_notat_randomization.csv"
# )

## -- Mental status variables during 14 days including+after randomization -----
## Currently assumes no imputation; any hospital day that is missing or patient
##   is withdrawn is essentially assumed to be normal

## Create dataset of only intervention period and determine whether patient was
## alive and free of delirium and coma, per imputed mental statuses
pad_int <- pad_daily %>%
  right_join(randpts_events, by = c("id", "redcap_event_name")) %>%
  filter(study_day %in% 0:13) %>%
  mutate(
    dcfree = case_when(
      study_status == "Deceased"                        ~ FALSE,
      study_status == "Discharged"                      ~ TRUE,
      is.na(mental_status_imp)                          ~ as.logical(NA),
      mental_status_imp %in% c("Comatose", "Delirious") ~ FALSE,
      TRUE                                              ~ TRUE
    )
  )

## Summarize mental status variables for each patient
## NOTE: All mental status summary variables use **imputed** mental status
pad_summary_int <- pad_int %>%
  group_by(id) %>%
  summarise(
    ## Because RASS was sometimes available when CAM was not, we can determine
    ##  coma status more frequently than we can determine delirium/overall
    n_coma_avail_int = sum(!is.na(comatose_imp)),
    n_mental_avail_int = sum(!is.na(mental_status_imp)),
    n_dcfree_avail_int = sum(!is.na(dcfree)),
    n_dcfree_unavail_int = sum(is.na(dcfree)),
    
    ## Among all patients
    coma_int_all = ifelse(n_coma_avail_int == 0, NA, sum_na(comatose_imp)),
    ever_coma_int = ifelse(is.na(coma_int_all), NA, coma_int_all > 0),
    del_int_all = ifelse(n_mental_avail_int == 0, NA, sum_na(delirious_imp)),
    ever_del_int = ifelse(is.na(del_int_all), NA, del_int_all > 0),
    delcoma_int_all = ifelse(n_mental_avail_int == 0, NA, sum_na(braindys_imp)),
    ever_delcoma_int = ifelse(is.na(delcoma_int_all), NA, delcoma_int_all > 0),
    
    ## Among patients who ever experienced coma/delirium
    coma_int_exp =
      ifelse(is.na(coma_int_all) | coma_int_all == 0, NA, coma_int_all),
    del_int_exp =
      ifelse(is.na(del_int_all) | del_int_all == 0, NA, del_int_all),
    delcoma_int_exp =
      ifelse(is.na(delcoma_int_all) | delcoma_int_all == 0, NA, delcoma_int_all),
    
    ## Everyone has a value for DCFDs; no need to do _all, _exp versions
    dcfd_int = ifelse(n_dcfree_avail_int == 0, NA, sum_na(dcfree))
  ) %>%
  select(id, n_coma_avail_int, ever_coma_int, coma_int_all, coma_int_exp,
         n_mental_avail_int, ever_del_int, del_int_all, del_int_exp,
         ever_delcoma_int, delcoma_int_all, delcoma_int_exp,
         n_dcfree_avail_int, dcfd_int)

## -- Mental status variables during entire hospitalization --------------------
## Does *not* calculate DCFDs, since we need a stable denominator for that
## NOTE: All mental status summary variables use **imputed** mental status

## Summarize mental status variables for each patient
pad_summary_ih <- pad_daily %>%
  group_by(id) %>%
  summarise(
    ## Because RASS was sometimes available when CAM was not, we can determine
    ##  coma status more frequently than we can determine delirium/overall
    n_coma_avail_ih = sum(!is.na(comatose_imp)),
    n_mental_avail_ih = sum(!is.na(mental_status_imp)),

    ## Among all patients
    coma_ih_all = ifelse(n_coma_avail_ih == 0, NA, sum_na(comatose_imp)),
    ever_coma_ih = ifelse(is.na(coma_ih_all), NA, coma_ih_all > 0),
    del_ih_all = ifelse(n_mental_avail_ih == 0, NA, sum_na(delirious_imp)),
    ever_del_ih = ifelse(is.na(del_ih_all), NA, del_ih_all > 0),
    delcoma_ih_all = ifelse(n_mental_avail_ih == 0, NA, sum_na(braindys_imp)),
    ever_delcoma_ih = ifelse(is.na(delcoma_ih_all), NA, delcoma_ih_all > 0),
    
    ## Among patients who ever experienced coma/delirium
    ## Delirium and delirium/coma *should* be the same; as of Dec 2017, we know
    ##  that one patient has a difference in delirium (0 delirium days)
    coma_ih_exp =
      ifelse(is.na(coma_ih_all) | coma_ih_all == 0, NA, coma_ih_all),
    del_ih_exp =
      ifelse(is.na(del_ih_all) | del_ih_all == 0, NA, del_ih_all),
    delcoma_ih_exp =
      ifelse(is.na(delcoma_ih_all) | delcoma_ih_all == 0, NA, delcoma_ih_all)
  ) %>%
  select(id, n_coma_avail_ih, ever_coma_ih, coma_ih_all, coma_ih_exp,
         n_mental_avail_ih, ever_del_ih, del_ih_all, del_ih_exp,
         ever_delcoma_ih, delcoma_ih_all, delcoma_ih_exp)

## -- Write datasets to analysisdata -------------------------------------------
## 1. All assessments
saveRDS(pad_long %>% select(-assess_date, assess_time),
        file = "analysisdata/rds/padasmts.rds")
write_csv(pad_long %>% select(-assess_date, assess_time),
          path = "analysisdata/csv/padasmts.csv")

## 2. Daily records
saveRDS(pad_daily, file = "analysisdata/rds/paddaily.rds")
write_csv(pad_daily, path = "analysisdata/csv/paddaily.csv")

## 3. Summary variables
pad_summary <- reduce(
  list(data.frame(id = rand_pts), rand_asmts, pad_summary_int, pad_summary_ih),
  left_join, by = "id"
)

saveRDS(pad_summary, file = "analysisdata/rds/padsummary.rds")
write_csv(pad_summary, path = "analysisdata/csv/padsummary.csv")

## -- Exploration of missingness -----------------------------------------------
## Show how many patients had how many days with DCFD info available
dcfd_avail <- pad_summary %>%
  ## Did patients ever withdraw from the study?
  left_join(ptstatus_df %>% select(id, wd_inhosp)) %>%
  mutate(
    all_dcfd_avail = factor(
      as.numeric(n_dcfree_avail_int == 14),
      levels = 0:1, labels = c("Missing >=1 day", "All days available")
    ),
    wd_inhosp = factor(
      as.numeric(wd_inhosp),
      levels = 0:1,
      labels = c("Remained in study", "Officially withdrew in hospital")
    )
  )

ggplot(data = dcfd_avail, aes(x = n_dcfree_avail_int)) +
  facet_wrap(~ wd_inhosp) +
  geom_histogram(aes(fill = all_dcfd_avail), binwidth = 1) +
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
      is.na(mental_status_raw)
  )

