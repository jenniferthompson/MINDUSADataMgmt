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
  events = "enrollment__day_0_arm_1"
) %>%
  ## Remove test patients
  filter(!str_detect(toupper(id), "TEST"))
  
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
  ## Remove REDCap calculated field; it doesn't handle missings like I want
  select(-iqcode_score,
         -prehospital_form_complete,
         -enrollment_data_collection_form_complete) %>%
  
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

