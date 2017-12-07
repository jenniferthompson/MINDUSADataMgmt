################################################################################
## Data management for demographic/baseline information
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

get_levels_ih <-
  function(varname){ get_factor_levels(ddict = ih_dd, varname = varname) }

## -- Download variables from prehospital, enrollment data collection forms ----

## From enrollment qualification form: All variables
## From prehospital form: Only IQCODE (patient should be excluded if >= 4.5)
iqcode_vars <- sprintf("iqcode_%s_ph", 1:16)

demog_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  forms = c("enrollment_data_collection_form", "prehospital_form"),
  fields = c("id", "enroll_time"),
  events = "enrollment__day_0_arm_1"
) %>%
  ## Remove test patients
  filter(!str_detect(toupper(id), "TEST")) %>%
  ## Remove unneeded variables, REDCap calculated fields
  ##  (they don't handle missings as we want)
  select(-redcap_event_name, -prehospital_form_complete,
         -enrollment_data_collection_form_complete, -iqcode_score)
  
## -- Prehospital form ---------------------------------------------------------
## Helper function to change numeric codes > a certain level to NA
blank_to_na <- function(x, lim){ ifelse(x > lim, NA, x) }

## Character vectors of variables for each instrument
iqcode_questions <- str_subset(names(demog_raw), "^iqcode\\_[0-9]+\\_ph$")
adl_questions <- str_subset(names(demog_raw), "^adl\\_[0-9]+\\_ph$")
faq_questions <- str_subset(names(demog_raw), "^faq\\_[0-9]+\\_ph$")
audit_questions <- str_subset(names(demog_raw), "^audit\\_[0-9]+\\_ph$")

## -- Prehospital form ---------------------------------------------------------
ph_form <- demog_raw %>%

  ## Change codes for "don't know", "left blank" to NA
  ## IQCODE = 6; ADL = 3; FAQ = 4; AUDIT = 5/6
  mutate_at(vars(one_of(iqcode_questions)), funs(blank_to_na(., 5))) %>%
  mutate_at(vars(one_of(adl_questions)), funs(blank_to_na(., 2))) %>%
  mutate_at(vars(one_of(faq_questions)), funs(blank_to_na(., 3))) %>%
  mutate_at(vars(one_of(audit_questions)), funs(blank_to_na(., 4))) %>%
  
  ## If AUDIT Q1 is 0 ("never" drinks), Q2 can't be answered; assign 0 for Q2
  mutate(
    audit_2_ph = ifelse(!is.na(audit_1_ph) & audit_1_ph == 0, 0, audit_2_ph)
  ) %>%
  
  ## How many components of each questionnaire are present?
  mutate(
    iqcode_present = rowSums(!is.na(.[, iqcode_questions])),
    adl_present = rowSums(!is.na(.[, adl_questions])),
    faq_present = rowSums(!is.na(.[, faq_questions])),
    audit_present = rowSums(!is.na(.[, audit_questions]))
  ) %>%
  
  ## For IQCODE, ADL, FAQ:
  ## - If no questions answered, total = NA
  ## - If data are partially available, impute patient mean for missing questions
  ##   (there is very little missing data for IQCODE/ADL; a bit more for FAQ,
  ##   with ~3% consented patients having partially missing data on >=1 of these)
  mutate(
    ## IQCODE: Mean of all non-missing questions
    iqcode_total_ph =
      ifelse(
        iqcode_present == 0, NA, rowMeans(.[, iqcode_questions], na.rm = TRUE)
      ),
    ## ADL, FAQ: Sum of all non-missing questions, imputing patient mean for
    ## missing questions
    adl_total_ph =
      ifelse(
        adl_present == 0, NA, rowMeans(.[, adl_questions], na.rm = TRUE) * 6
      ),
    faq_total_ph =
      ifelse(
        faq_present == 0, NA, rowMeans(.[, faq_questions], na.rm = TRUE) * 10
      ),
    
    ## AUDIT: Sum only available and answered questions (not sure about this;
    ## PIs and follow-up team are discussing how best to handle missingness due
    ## to questionnaire being administered to surrogate rather than patient)
    audit_total_ph_complete =
      ifelse(audit_present == 0, NA, rowSums(.[, audit_questions])),
    audit_total_ph_partial =
      ifelse(audit_present == 0, NA, rowSums(.[, audit_questions], na.rm = TRUE))
  ) %>%
  
  ## Add factor levels
  mutate(
    education = factor(
      education,
      levels = get_levels_ih("education"),
      # labels = names(get_levels_ih("education")) I can't handle mixed caps
      labels = c("Less than high school", "High school diploma/GED",
                 "Some college, no degree", "Associate degree",
                 "Bachelor's degree", "Master's degree",
                 "Doctorate (MD, PhD, JD, others)")
    ),
    english_level = factor(
      ifelse(is.na(language), NA,
      ifelse(language %in% 1:2, 1,
      ifelse(language == 3 & english_how_well %in% 0:1, 2,
      ifelse(language == 3 &
               (is.na(english_how_well) | english_how_well == 2), 3, 4)))),
      levels = 1:4,
      labels = c("Yes/primary",
                 "Secondary, well/very well",
                 "Secondary, not well or fluency unknown",
                 "No; Spanish only")
    )
  ) %>%
  
  ## Select only needed variables
  select(id, education, english_level, iqcode_total_ph, adl_total_ph,
         faq_total_ph, audit_total_ph_complete, audit_total_ph_partial)

## -- Demographic/baseline info ------------------------------------------------
baseline <- demog_raw %>%
  ## Dates/times
  mutate(
    dob = as.Date(dob, format = "%Y-%m-%d"),
    enroll_time = ymd_hm(enroll_time),
    enroll_date = date(enroll_time)
  ) %>%
  
  ## Factors direct from database
  mutate(
    gender = factor(
      gender,
      levels = get_levels_ih("gender"),
      labels = names(get_levels_ih("gender"))
    ),
    insurance = factor(
      insurance,
      levels = get_levels_ih("insurance"),
      labels = names(get_levels_ih("insurance"))
    ),
    frailty_f = factor(
      frailty,
      levels = get_levels_ih("frailty"),
      labels = names(get_levels_ih("frailty"))
    ),
    ## Also want numeric version of frailty
    frailty = frailty + 1
  ) %>%
  
  ## Create new variables
  mutate(
    age_consent = as.numeric(difftime(enroll_date, dob, units = "days")) / 365.25,
    ## Race: combine categories using NIH docs at
    ##   https://grants.nih.gov/grants/guide/notice-files/NOT-OD-15-089.html,
    ##   plus category for multiple/"other" races
    num_races = rowSums(.[, str_subset(names(.), "^race\\_[0-9]+$")], na.rm = TRUE),
    race_cat = factor(
      ifelse(num_races > 1 | race_14, 6,
      ifelse(num_races == 0, NA,
      ifelse(race_7, 1,
      ifelse(race_3 | race_4 | race_5 | race_8 | race_9 | race_11 | race_13, 2,
      ifelse(race_2, 3,
      ifelse(race_6 | race_10 | race_15 | race_12, 4,
      ifelse(race_1, 5, NA))))))),
      levels = 1:6,
      labels = c("American Indian or Alaska Native",
                 "Asian",
                 "Black or African American",
                 "Native Hawaiian or Other Pacific Islander",
                 "White",
                 "Multiple or other race(s)")
    ),
    ## Ethnicity: Per NIH link above, combine all Hispanic/Latino categories
    ethnicity = factor(
      ifelse(is.na(hispanic), NA,
      ifelse(hispanic > 0, 1, 2)),
      levels = 1:2, labels = c("Hispanic or Latino", "Not Hispanic or Latino")
    ),
    ## Home antipsychotics: any vs none
    num_home_antipsyc =
      rowSums(.[, str_subset(names(.), "^home\\_meds\\_antipsych\\_[0-9]+$")]),
    home_antipsyc = factor(
      ifelse(home_meds_antipsych_0 & num_home_antipsyc == 1, 1,
      ifelse(num_home_antipsyc > 0, 2, NA)),
      levels = 1:2, labels = c("None", "At least one")
    ),
    ## Primary admission diagnosis (TBD pending PI input); one suggestion:
    # Sepsis, septic shock, ARDS d/t infection (1, 2)
    # ARDS without infection, airway issues (3, 4)
    # COPD/asthma (5)
    # Other pulmonary (6)
    # CHF/cardiomyopathy, acute MI/cardiogenic shock, arrhythmia (7, 8, 9)
    # GI bleed, hemorrhagic shock (10, 12)
    # Renal failure, cirrhosis/hepatic failure, metabolic/endocrine/electrolyte (11, 13, 15)
    # Other infectious disease (14)
    # Malignancy (16)
    # Seizures/status eplepticus, neurological disease (17, 18)
    # Non-transplant surgery (19, 20, 22 - 27)
    # Transplant (21)
    # Any other (99)
    
    ## -- Charlson score -------------------------------------------------------
    ## 1. How many actual conditions in each category did patient have?
    ##    (Does not include "None")
    charlson_cond_1 =
      rowSums(.[, str_subset(names(.), "^charlson\\_cat\\_1\\_[^0$]")]),
    charlson_cond_2 =
      rowSums(.[, str_subset(names(.), "^charlson\\_cat\\_2\\_[^0$]")]),
    charlson_cond_3 = charlson_cat_3_1, ## only one 3-point condition
    charlson_cond_6 =
      rowSums(.[, str_subset(names(.), "^charlson\\_cat\\_4\\_[^0$]")]),
      ## Category 4 is worth 6 points
    
    ## 2. For each score level, if a) nothing is checked (including None), or
    ##   b) None + something else is checked, value is NA; otherwise, value =
    ##   number of conditions * number of points
    charlson_1 = ifelse(
      (charlson_cond_1 == 0 & !charlson_cat_1_0) |
        (charlson_cond_1 > 1 & charlson_cat_1_0),
      NA,
      charlson_cond_1 * 1
    ),
    charlson_2 = ifelse(
      (charlson_cond_2 == 0 & !charlson_cat_2_0) |
        (charlson_cond_2 > 1 & charlson_cat_2_0),
      NA,
      charlson_cond_2 * 2
    ),
    charlson_3 = ifelse(
      (charlson_cond_3 == 0 & !charlson_cat_3_0) |
        (charlson_cond_3 > 1 & charlson_cat_3_0),
      NA,
      charlson_cond_3 * 3
    ),
    charlson_6 = ifelse(
      (charlson_cond_6 == 0 & !charlson_cat_4_0) |
        (charlson_cond_6 > 1 & charlson_cat_4_0),
      NA,
      charlson_cond_6 * 6
    ),
    
    ## 3. Total Charlson = sum of all point levels
    charlson_total = charlson_1 + charlson_2 + charlson_3 + charlson_6
  )
