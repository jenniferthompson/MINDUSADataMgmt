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
    frailty = frailty + 1,
    uo_enr_f = factor(
      ifelse(!is.na(uo_enr) & uo_enr > 0, uo_enr, NA),
      levels = 1:3,
      labels = c("0-200", "201-500", "500+")
    )
  )
  
## -- Straightforward new variables --------------------------------------------
baseline <- baseline %>%
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
    )
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
  )

## -- More complicated variables -----------------------------------------------
baseline <- baseline %>%
  ## -- Charlson score ---------------------------------------------------------
  mutate(
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

## -- APACHE II score --------------------------------------------------------
## Reference: Knaus et al, CCM 1985 Oct; 13(10):818-29
## https://www.ncbi.nlm.nih.gov/pubmed/3928249
baseline <- baseline %>%
  mutate(
    ## Temperature
    temp_ap = case_when(
      is.na(temp_enr_high)  | is.na(temp_enr_low) ~ as.numeric(NA),
      temp_enr_high >= 41   | temp_enr_low < 30   ~ 4,
      temp_enr_high >= 39   | temp_enr_low < 32   ~ 3,
                              temp_enr_low < 34   ~ 2,
      temp_enr_high >= 38.5 | temp_enr_low < 36   ~ 1,
      TRUE                                        ~ 0
    ),
    
    ## Mean arterial pressure
    map_ap = case_when(
      is.na(map_enr_high) | is.na(map_enr_low) ~ as.numeric(NA),
      map_enr_high >= 160 | map_enr_low <= 49  ~ 4,
      map_enr_high >= 130                      ~ 3,
      map_enr_high >= 110 | map_enr_low <= 70  ~ 2,
      TRUE                                     ~ 0
    ),
    
    ## Heart rate
    hr_ap = case_when(
      is.na(hr_enr_high) | is.na(hr_enr_low) ~ as.numeric(NA),
      hr_enr_high >= 180 | hr_enr_low <= 39  ~ 4,
      hr_enr_high >= 140 | hr_enr_low <= 54  ~ 3,
      hr_enr_high >= 110 | hr_enr_low <= 69  ~ 2,
      TRUE ~ 0
    ),
    
    ## Respiratory rate
    rr_ap = case_when(
      is.na(rr_enr_high) | is.na(rr_enr_low) ~ as.numeric(NA),
      rr_enr_high >= 50  | rr_enr_low <= 5   ~ 4,
      rr_enr_high >= 35                      ~ 3,
                           rr_enr_low <= 9   ~ 2,
      rr_enr_high >= 25  | rr_enr_low <= 11  ~ 1,
      TRUE                                   ~ 0
    ),
    
    ## Oxygenation: the fun part!
    ## If oxygenation info available (FiO2, PCO2, PaO2), calculate A-a gradient:
    ## ((713 * FiO2) - (PCO2 / 0.8)) - PaO2; formula confirmed by EWE Dec 2017
    aa_ap = ifelse(
      is.na(fio2_enr) | is.na(pco2_enr) | is.na(pao2_enr), NA,
      ((713 * fio2_enr) - (pco2_enr / 0.8)) - pao2_enr
    ),
    
    ## If FiO2 >= 0.5, assign points using A-a gradient;
    ##   if < 0.5, assign points using PaO2
    oxy_ap = case_when(
      is.na(fio2_enr)                  ~ as.numeric(NA),
      fio2_enr >= 0.5 & aa_ap >= 500   ~ 4,
      fio2_enr >= 0.5 & aa_ap >= 350   ~ 3,
      fio2_enr >= 0.5 & aa_ap >= 200   ~ 2,
      fio2_enr >= 0.5 & aa_ap < 200    ~ 0,
      fio2_enr < 0.5  & pao2_enr < 55  ~ 4,
      fio2_enr < 0.5  & pao2_enr <= 60 ~ 3,
      fio2_enr < 0.5  & pao2_enr <= 70 ~ 1,
      TRUE                             ~ 0
    ),
    
    ## Acid (pH, or alternatively CO2)
    acid_ap = case_when(
      ## If pH is available, use that
      !is.na(ph_enr) & (ph_enr >= 7.7 | ph_enr < 7.15) ~ 4,
      !is.na(ph_enr) & (ph_enr >= 7.6 | ph_enr < 7.25) ~ 3,
      !is.na(ph_enr) &                  ph_enr < 7.33  ~ 2,
      !is.na(ph_enr) &  ph_enr >= 7.5                  ~ 1,
      !is.na(ph_enr)                                   ~ 0,
      ## If neither pH nor CO2 (HCO3) is available, score is missing
      is.na(co2_enr_high) | is.na(co2_enr_low) ~ as.numeric(NA),
      ## If CO2 (HCO3) is available, use that
      co2_enr_high >= 52 | co2_enr_low < 15 ~ 4,
      co2_enr_high >= 41 | co2_enr_low < 18 ~ 3,
                           co2_enr_low < 22 ~ 2,
      co2_enr_high >= 32                    ~ 1,
      TRUE                                  ~ 0
    ),
    
    ## Sodium
    na_ap = case_when(
      is.na(na_enr_high) | is.na(na_enr_low) ~ as.numeric(NA),
      na_enr_high >= 180 | na_enr_low <= 110 ~ 4,
      na_enr_high >= 160 | na_enr_low <= 119 ~ 3,
      na_enr_high >= 155 | na_enr_low <= 129 ~ 2,
      na_enr_high >= 150                     ~ 1,
      TRUE                                   ~ 0
    ),
    
    ## Potassium
    k_ap = case_when(
      is.na(k_enr_high) | is.na(k_enr_low) ~ as.numeric(NA),
      k_enr_high >= 7   | k_enr_low < 2.5  ~ 4,
      k_enr_high >= 6                      ~ 3,
                          k_enr_low < 3    ~ 2,
      k_enr_high >= 5.5 | k_enr_low < 3.5  ~ 1,
      TRUE                                 ~ 0
    ),
    
    ## Creatinine
    cr_ap = case_when(
      is.na(cr_enr_high) | is.na(cr_enr_low) ~ as.numeric(NA),
      cr_enr_high >= 3.5                     ~ 4,
      cr_enr_high >= 2                       ~ 3,
      cr_enr_high >= 1.5 | cr_enr_low < 0.6  ~ 2,
      TRUE                                   ~ 0
    ),
    
    ## Hematocrit/PCV
    pcv_ap = case_when(
      is.na(pcv_enr_high) | is.na(pcv_enr_low) ~ as.numeric(NA),
      pcv_enr_high >= 60 | pcv_enr_low < 20    ~ 4,
      pcv_enr_high >= 50 | pcv_enr_low < 30    ~ 2,
      pcv_enr_high >= 46                       ~ 1,
      TRUE                                     ~ 0
    ),
    
    ## White blood count
    wbc_ap = case_when(
      is.na(wbc_enr_high) | is.na(wbc_enr_low) ~ as.numeric(NA),
      wbc_enr_high >= 40 | wbc_enr_low < 1     ~ 4,
      wbc_enr_high >= 20 | wbc_enr_low < 3     ~ 2,
      wbc_enr_high >= 15                       ~ 1,
      TRUE                                     ~ 0
    ),
    
    ## Glasgow Coma Score
    gcs_ap = ifelse(is.na(gcs_enr) | gcs_enr == 99, NA, 15 - gcs_enr),
    
    ## Age
    age_ap = case_when(
      is.na(age_consent) ~ as.numeric(NA),
      age_consent < 45   ~ 0,
      age_consent < 55   ~ 2,
      age_consent < 65   ~ 3,
      age_consent < 75   ~ 5,
      TRUE               ~ 6
    ),
    
    ## Chronic disease
    ## 1. How many actual conditions did patient have?
    chronic_conditions =
      rowSums(.[, str_subset(names(.), "^chronic\\_dis\\_[1-5]$")]),
    ## 2. If patient had >1 condition *and* "none" marked, *or* neither "none"
    ##    nor any conditions marked, # conditions = NA
    chronic_conditions = ifelse(
      (chronic_conditions > 0 & chronic_dis_0) |
        (chronic_conditions == 0 & chronic_dis_0 == 0), NA,
      chronic_conditions
    ),
    ## 3. If patient had no chronic conditions, 0 points; otherwise, if they
    ##    had elective surgery, 2 points; otherwise, 5 points
    chrondis_ap = case_when(
      is.na(chronic_conditions) |
        (chronic_conditions > 0 & is.na(apache_chronic_points)) ~ as.numeric(NA),
      chronic_conditions == 0    ~ 0,
      apache_chronic_points == 2 ~ 2,
      TRUE                       ~ 5
    )
  )

## Sum components to get versions of APACHE II
## Vector of APS component names
vars_apache_aps <- paste0(
  c("temp", "map", "hr", "rr", "oxy", "acid", "na", "k", "cr", "pcv",
    "wbc", "gcs"),
  "_ap"
)
## Vector of overall APACHE component names
vars_apache_all <- c(vars_apache_aps, "age_ap", "chrondis_ap")

## How many components are available?
baseline$apache_vars_avail <- rowSums(!is.na(baseline[, vars_apache_all]))
baseline$apache_total_miss0 <- ifelse(
  baseline$apache_vars_avail == 0, NA,
  rowSums(baseline[, vars_apache_all], na.rm = TRUE)
)
baseline$apache_total_missNA <- ifelse(
  baseline$apache_vars_avail == 0, NA,
  rowSums(baseline[, vars_apache_all], na.rm = FALSE)
)

# ## Plot two different versions
# ggplot(data = baseline) +
#   geom_histogram(aes(x = apache_total_miss0), fill = "navy", binwidth = 1, alpha = 0.4) +
#   geom_histogram(aes(x = apache_total_missNA), fill = "darkgreen", binwidth = 1, alpha = 0.4)

## -- SOFA score ---------------------------------------------------------------
## References:
##   Original SOFA paper, Vincent et al (https://doi.org/10.1007/BF01709751)
##   S/F as substitute for unavailable P/F:
##     Pandharipande et al https://www.ncbi.nlm.nih.gov/pubmed/19242333
baseline <- baseline %>%
  mutate(
    ## Respiratory component: Use P/F if available; otherwise use S/F
    resp_sofa = case_when(
      is.na(pfratio_worst_enr) & is.na(sf_enr) ~ as.numeric(NA),
      pfratio_worst_enr <= 100  ~ 4,
      pfratio_worst_enr <= 200  ~ 3,
      pfratio_worst_enr <= 300  ~ 2,
      pfratio_worst_enr <= 400  ~ 1,
      !is.na(pfratio_worst_enr) ~ 0,
      sf_enr <= 89              ~ 4,
      sf_enr <= 214             ~ 3,
      sf_enr <= 357             ~ 2,
      sf_enr <= 512             ~ 1,
      TRUE                      ~ 0
    ),
    
    ## Coagulation
    coag_sofa = case_when(
      is.na(plt_enr) ~ as.numeric(NA),
      plt_enr <= 20  ~ 4,
      plt_enr <= 50  ~ 3,
      plt_enr <= 100 ~ 2,
      plt_enr <= 150 ~ 1,
      TRUE           ~ 0
    ),
    
    ## Liver
    liver_sofa = case_when(
      is.na(bilirubin_enr) ~ 0,
        ## assume no bilirubin = no clinical indication of liver dysfunction
        ## confirmed by PIs on...
      bilirubin_enr >= 12  ~ 4,
      bilirubin_enr >= 6   ~ 3,
      bilirubin_enr >= 2   ~ 2,
      bilirubin_enr >= 1.2 ~ 1,
      TRUE                 ~ 0
    ),
    
    ## Central nervous system
    cns_sofa = case_when(
      is.na(gcs_enr) | gcs_enr == 99 ~ as.numeric(NA),
      gcs_enr < 6  ~ 4,
      gcs_enr < 10 ~ 3,
      gcs_enr < 13 ~ 2,
      gcs_enr < 15 ~ 1,
      TRUE         ~ 0
    ),
    
    ## Renal: Look at highest creatinine *and* urine output
    renal_sofa = case_when(
      is.na(cr_enr_high) | is.na(uo_enr_f)       ~ as.numeric(NA),
      cr_enr_high >= 5   | uo_enr_f == "0-200"   ~ 4,
      cr_enr_high >= 3.5 | uo_enr_f == "201-500" ~ 3,
      cr_enr_high >= 2                           ~ 2,
      cr_enr_high >= 1.2                         ~ 1,
      TRUE                                       ~ 0
    )
  ) %>%
  ## Cardiovascular: already calculated; rename for consistency, create factor
  ##  version for Table 1
  rename(cv_sofa = "sofa_cv_enr") %>%
  mutate(
    cv_sofa_f = factor(
      cv_sofa,
      levels = get_levels_ih("sofa_cv_enr"),
      labels = get_levels_ih("sofa_cv_enr")
    )
  )

## Sum components to get versions of SOFA
## Vector of modified SOFA component names (leaves out CNS; GCS tricky w/ MV pts)
vars_sofa_mod <- paste0(c("resp", "coag", "liver", "renal", "cv"), "_sofa")
## Vector of overall SOFA component names
vars_sofa_all <- c(vars_sofa_mod, "cns_sofa")

## How many components are available?
baseline$sofa_vars_avail <- rowSums(!is.na(baseline[, vars_sofa_all]))
baseline$sofa_total_miss0 <- ifelse(
  baseline$sofa_vars_avail == 0, NA,
  rowSums(baseline[, vars_sofa_all], na.rm = TRUE)
)
baseline$sofa_total_missNA <- ifelse(
  baseline$sofa_vars_avail == 0, NA,
  rowSums(baseline[, vars_sofa_all], na.rm = FALSE)
)

# ## Plot two different versions
# ggplot(data = baseline) +
#   geom_histogram(aes(x = sofa_total_miss0), fill = "navy", binwidth = 1, alpha = 0.4) +
#   geom_histogram(aes(x = sofa_total_missNA), fill = "darkgreen", binwidth = 1, alpha = 0.4)

