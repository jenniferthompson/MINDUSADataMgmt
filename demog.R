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

## -- Download variables from necessary forms ----------------------------------
## Enrollment qualification, prehospital, randomization qualification

demog_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  forms = c("enrollment_data_collection_form", "prehospital_form"),
  fields = c("id", "enroll_time", "icuadm_1_time", "organ_failures_present"),
  events = "enrollment__day_0_arm_1"
) %>%
  ## Remove unneeded variables, REDCap calculated fields
  ##  (they don't handle missings as we want)
  select(-redcap_event_name, -prehospital_form_complete,
         -enrollment_data_collection_form_complete, -iqcode_score)

## -- Download RASS, lab values for SOI scores from daily forms ----------------
## For APACHE and SOFA components which are missing data at enrollment, we'll
##  carry values backwards a max of three days
soi_components <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "daily_date",
             ## SOFA components
             "rass_1", "rass_2", "rass_daily_low", "gcs_daily", ## CNS
             "o2sat_daily", "sf_daily", "pfratio_daily", ## Respiratory
             "bili_daily", "cr_daily", "uo_daily",       ## Liver, renal
             "cv_sofa_daily", "plt_daily",               ## CV, coagulation
             ## Additional APACHE components
             "temp_daily_low", "temp_daily_high",        ## Temperature
             "map_daily_low", "map_daily_high",          ## MAP
             "hr_daily_low", "hr_daily_high",            ## Heart rate
             "rr_daily_low", "rr_daily_high",            ## Respiratory rate
             "pcv_daily_low", "pcv_daily_high",          ## Hematocrit
             "wbc_daily_low", "wbc_daily_high",          ## WBC
             "co2_daily_low", "co2_daily_high",          ## HCO3/CO2
             "k_daily_low", "k_daily_high",              ## Potassium
             "na_daily_low", "na_daily_high"),           ## Sodium
  events = setdiff(
    ih_events$unique_event_name,
    c("randomization_arm_1", "prior_to_hospital_arm_1")
  )
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

## Everyone should have an IQCODE
write_csv(
  ph_form %>%
    filter(is.na(iqcode_total_ph)) %>%
    dplyr::select(id, iqcode_total_ph),
  path = "datachecks/no_iqcode.csv"
)

## -- Demographic/baseline info ------------------------------------------------
baseline <- demog_raw %>%
  ## Dates/times
  mutate(
    dob = as.Date(dob, format = "%Y-%m-%d"),
    enroll_time = ymd_hm(enroll_time),
    enroll_date = date(enroll_time),
    icuadm_time_1 = ymd_hm(icuadm_1_time),
    icuadm_date_1 = date(icuadm_1_time)
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
    ),
    icu_rsn = factor(
      icu_rsn,
      levels = get_levels_ih("icu_rsn"),
      labels = names(get_levels_ih("icu_rsn"))
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
    ## BMI (weight (kg) / [height in meters]^2)
    bmi = ifelse(is.na(weight) | is.na(height), NA,
                 weight / ((height / 100) ^ 2)),
    
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

## -- SOI score prep -----------------------------------------------------------
## Put all labs, RASS values from daily data collection in a data frame; find
## closest one to ICU admission within three days; use that for SOI scores
## Decided December 2017, Monday meeting discussion

soi_daily <- soi_components %>%
  ## Make urine output a factor for consistency
  mutate(
    uo_daily_f = factor(
      uo_daily,
      levels = get_levels_ih("uo_daily"),
      labels = names(get_levels_ih("uo_daily"))
    ),
    daily_date = as_date(daily_date)
  ) %>%
  select(id, redcap_event_name, daily_date,
         ## SOFA variables, in order
         pfratio_daily, sf_daily, plt_daily, bili_daily, cv_sofa_daily,
         gcs_daily, cr_daily, uo_daily,
         ## Additional APACHE variables
         temp_daily_low, temp_daily_high, map_daily_low, map_daily_high,
         hr_daily_low, hr_daily_high, rr_daily_low, rr_daily_high, o2sat_daily,
         na_daily_low, na_daily_high, k_daily_low, k_daily_high, pcv_daily_low,
         pcv_daily_high, wbc_daily_low, wbc_daily_high, co2_daily_low,
         co2_daily_high)

names(soi_daily) <- c("id", "event_name", "study_date", "pf_worst", "sf_worst",
                      "plt_low", "bili_high", "sofa_cv", "gcs", "cr_high",
                      "uo", "temp_low", "temp_high", "map_low", "map_high",
                      "hr_low", "hr_high", "rr_low", "rr_high", "o2sat_low",
                      "na_low", "na_high", "k_low", "k_high", "pcv_low",
                      "pcv_high", "wbc_low", "wbc_high", "co2_low", "co2_high")

## Restrict to events within three days of ICU admission, find earliest
## non-missing value for each quantity
soi_daily <- soi_daily %>%
  ## Restrict to events within three days of ICU admission
  ##  ("three days" = admission + three following days)
  left_join(select(baseline, id, icuadm_date_1)) %>%
  mutate(
    ## Change GCS = 99 to N/A - don't want these values to "count"
    gcs = ifelse(gcs == 99, NA, gcs),
    days_after_admission =
      as.numeric(difftime(study_date, icuadm_date_1, units = "days"))
  ) %>%
  filter(days_after_admission <= 3) %>%
  ## Take earliest non-missing value for each value per patient
  arrange(id, days_after_admission) %>%
  group_by(id) %>%
  summarise_at(vars(-study_date, -event_name, -icuadm_date_1),
               funs(first_notna)) %>%
  ungroup() %>%
  rename_at(vars(-id), funs(paste0(., "_postadm"))) %>%
  ## Factor version of urine output
  mutate(
    uo_postadm_f = factor(
      uo_postadm,
      levels = get_levels_ih("uo_enr"),
      labels = names(get_levels_ih("uo_enr"))
    )
  )

## -- SOFA score at ICU admission ----------------------------------------------
## References:
##   Original SOFA paper, Vincent et al (https://doi.org/10.1007/BF01709751)
##   S/F as substitute for unavailable P/F:
##     Pandharipande et al https://www.ncbi.nlm.nih.gov/pubmed/19242333
##   RASS as substitute for unavailable GCS:
##     Vasilevskis et al https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4748963/
##     Method C has best predictive validity
baseline <- baseline %>%
  left_join(soi_daily, by = "id") %>%
  ## RASS at ICU admission (to use in place of missing GCS)
  mutate(
    rass_low_enr = ifelse(
      scale_enr == 2 & scale_sedated %in% -5:4, scale_sedated, NA
    )
  ) %>%
  mutate(
    ## Respiratory component: Use P/F if available; otherwise use S/F
    resp_sofa_adm = case_when(
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
    resp_sofa = case_when(
      !is.na(resp_sofa_adm)                             ~ resp_sofa_adm,
      is.na(pf_worst_postadm) & is.na(sf_worst_postadm) ~ as.numeric(NA),
      pf_worst_postadm <= 100                           ~ 4,
      pf_worst_postadm <= 200                           ~ 3,
      pf_worst_postadm <= 300                           ~ 2,
      pf_worst_postadm <= 400                           ~ 1,
      !is.na(pf_worst_postadm)                          ~ 0,
      sf_worst_postadm <= 89                            ~ 4,
      sf_worst_postadm <= 214                           ~ 3,
      sf_worst_postadm <= 357                           ~ 2,
      sf_worst_postadm <= 512                           ~ 1,
      TRUE                                              ~ 0
    ),
    
    ## Coagulation
    coag_sofa_adm = case_when(
      is.na(plt_enr) ~ as.numeric(NA),
      plt_enr <= 20  ~ 4,
      plt_enr <= 50  ~ 3,
      plt_enr <= 100 ~ 2,
      plt_enr <= 150 ~ 1,
      TRUE           ~ 0
    ),
    coag_sofa = case_when(
      !is.na(coag_sofa_adm)  ~ coag_sofa_adm,
      is.na(plt_low_postadm) ~ as.numeric(NA),
      plt_low_postadm <= 20  ~ 4,
      plt_low_postadm <= 50  ~ 3,
      plt_low_postadm <= 100 ~ 2,
      plt_low_postadm <= 150 ~ 1,
      TRUE                   ~ 0
    ),
    
    ## Liver
    liver_sofa_adm = case_when(
      is.na(bilirubin_enr) ~ 0,
      bilirubin_enr >= 12  ~ 4,
      bilirubin_enr >= 6   ~ 3,
      bilirubin_enr >= 2   ~ 2,
      bilirubin_enr >= 1.2 ~ 1,
      TRUE                 ~ 0
    ),
    liver_sofa = case_when(
      ## If liver score was not imputed at consent, use that; otherwise,
      ## base on bilirubin within the next 2 days; if none of those available,
      ## *then* assume normal (no clinical indication of liver dysfunction)
      !is.na(liver_sofa_adm) & !is.na(bilirubin_enr) ~ liver_sofa_adm,
      is.na(bili_high_postadm)                       ~ 0,
      bili_high_postadm >= 12                        ~ 4,
      bili_high_postadm >= 6                         ~ 3,
      bili_high_postadm >= 2                         ~ 2,
      bili_high_postadm >= 1.2                       ~ 1,
      TRUE                                           ~ 0
    ),
    
    ## Central nervous system: Use GCS if available; otherwise, use RASS
    cns_sofa_adm = case_when(
      (is.na(gcs_enr) | gcs_enr == 99) & is.na(rass_low_enr) ~ as.numeric(NA),
      !is.na(gcs_enr) & gcs_enr < 6  ~ 4,
      !is.na(gcs_enr) & gcs_enr < 10 ~ 3,
      !is.na(gcs_enr) & gcs_enr < 13 ~ 2,
      !is.na(gcs_enr) & gcs_enr < 15 ~ 1,
      rass_low_enr <= -4             ~ 4,
      rass_low_enr == -3             ~ 3,
      rass_low_enr == -2             ~ 2,
      rass_low_enr == -1             ~ 1,
      TRUE         ~ 0
    ),
    cns_sofa = case_when(
      !is.na(cns_sofa_adm)                     ~ cns_sofa_adm,
      (is.na(gcs_postadm) | gcs_postadm == 99) ~ as.numeric(NA),
      gcs_postadm < 6                          ~ 4,
      gcs_postadm < 10                         ~ 3,
      gcs_postadm < 13                         ~ 2,
      gcs_postadm < 15                         ~ 1,
      TRUE                                     ~ 0
    ),
    
    ## Renal: Look at highest creatinine *and* urine output
    renal_sofa_adm = case_when(
      is.na(cr_enr_high) ~ as.numeric(NA),
      cr_enr_high >= 5   | (!is.na(uo_enr_f) & uo_enr_f == "0-200")   ~ 4,
      cr_enr_high >= 3.5 | (!is.na(uo_enr_f) & uo_enr_f == "201-500") ~ 3,
      cr_enr_high >= 2                                                ~ 2,
      cr_enr_high >= 1.2                                              ~ 1,
      TRUE                                                            ~ 0
    ),
    renal_sofa = case_when(
      !is.na(renal_sofa_adm)                               ~ renal_sofa_adm,
      is.na(cr_high_postadm)                               ~ as.numeric(NA),
      cr_high_postadm >= 5   |
        (!is.na(uo_postadm_f) & uo_postadm_f == "0-200")   ~ 4,
      cr_high_postadm >= 3.5 |
        (!is.na(uo_postadm_f) & uo_postadm_f == "201-500") ~ 3,
      cr_high_postadm >= 2                                 ~ 2,
      cr_high_postadm >= 1.2                               ~ 1,
      TRUE                                                 ~ 0
    ),
    
    ## Cardiovascular: If not available at consent, impute later value
    cv_sofa_adm = sofa_cv_enr,
    cv_sofa = ifelse(!is.na(cv_sofa_adm), cv_sofa_adm, sofa_cv_postadm),
    
    ## Create factor version of CV SOFA for Table 1 (consent only)
    cv_sofa_adm_f = factor(
      cv_sofa_adm,
      levels = get_levels_ih("sofa_cv_enr"),
      labels = names(get_levels_ih("sofa_cv_enr"))
    )
  )

## Sum components to get versions of SOFA
## Vectors of SOFA variable prefixes
sofa_mod_prefixes <- c("resp", "coag", "liver", "renal", "cv")
sofa_all_prefixes <- c(sofa_mod_prefixes, "cns")

## Vectors of SOFA variable names
sofa_mod_vars_adm <- paste0(sofa_mod_prefixes, "_sofa_adm")
sofa_mod_vars <- paste0(sofa_mod_prefixes, "_sofa")
sofa_all_vars_adm <- paste0(sofa_all_prefixes, "_sofa_adm")
sofa_all_vars <- paste0(sofa_all_prefixes, "_sofa")

## How many components are available?
baseline$sofa_vars <- rowSums(!is.na(baseline[, sofa_all_vars]))
baseline$sofa_vars_adm <- rowSums(!is.na(baseline[, sofa_all_vars_adm]))
baseline$sofa_mod_vars <- rowSums(!is.na(baseline[, sofa_mod_vars]))
baseline$sofa_mod_vars_adm <- rowSums(!is.na(baseline[, sofa_mod_vars_adm]))

## Calculate totals; using na.rm = TRUE means that components still missing
## after looking ahead will be considered normal

## Overall SOFA, using day of admission + days and day of admission only
baseline$sofa_adm <- ifelse(
  baseline$sofa_vars == 0, NA,
  rowSums(baseline[, sofa_all_vars], na.rm = TRUE)
)
baseline$sofa_adm_only <- ifelse(
  baseline$sofa_vars_adm == 0, NA,
  rowSums(baseline[, sofa_all_vars_adm])
)

## Modified SOFA (no CNS component)
baseline$sofa_mod_adm <- ifelse(
  baseline$sofa_mod_vars == 0, NA,
  rowSums(baseline[, sofa_mod_vars], na.rm = TRUE)
)
baseline$sofa_mod_adm_only <- ifelse(
  baseline$sofa_mod_vars_adm == 0, NA,
  rowSums(baseline[, sofa_mod_vars_adm])
)

# ## -- Explore patients who had vs didn't have FiO2 at enrollment ---------------
# check_fio2 <- baseline %>%
#   mutate(has_fio2 = factor(
#     as.numeric(!is.na(fio2_enr)),
#     levels = 0:1,
#     labels = c("No FiO2 available", "Has FiO2")
#   ))
#
# ggplot(data = check_fio2, aes(x = o2sat_enr)) +
#   facet_wrap(~ has_fio2) +
#   geom_histogram() +
#   labs(title = "O2 sats at admission by availability of FiO2")
#
# ggplot(data = check_fio2, aes(x = sf_enr)) +
#   facet_wrap(~ has_fio2) +
#   geom_histogram() +
#   labs(title = "Worst S/F ratio at admission by availability of FiO2")
#
# ggplot(data = check_fio2, aes(x = pfratio_worst_enr)) +
#   facet_wrap(~ has_fio2) +
#   geom_histogram() +
#   labs(title = "Worst P/F ratio at admission by availability of FiO2")
#
# with(demog_raw, table(organ_failures_present_1, is.na(fio2_enr), useNA = "ifany"))

## -- APACHE II score ----------------------------------------------------------
## Reference: Knaus et al, CCM 1985 Oct; 13(10):818-29
## https://www.ncbi.nlm.nih.gov/pubmed/3928249

## Prep: data.frame of O2 sat -> PaO2 conversion values from EPIC II study
## https://www.intensive.org/epic2/Documents/Estimation%20of%20PO2%20and%20FiO2.pdf
## MANY THANKS, MILES McBAIN, FOR DATAPASTA
o2sat_pao2 <- tibble::tribble(
  ~so2_pao2,
  "80 44",
  "81 45",
  "82 46",
  "83 47",
  "84 49",
  "85 50",
  "86 52",
  "87 53",
  "88 55",
  "89 57",
  "90 60",
  "91 62",
  "92 65",
  "93 69",
  "94 73",
  "95 79",
  "96 86",
  "97 96",
  "98 112",
  "99 145"
) %>%
  separate(so2_pao2, into = c("o2sat_int", "pao2_est"), sep = " ") %>%
  mutate_all(as.numeric)

baseline <- baseline %>%
  ## Add estimated PaO2 onto dataset to use in oxygenation calculation
  ## Admission
  mutate(o2sat_int = floor(o2sat_enr),
         o2sat_low_postadm_int = floor(o2sat_low_postadm)) %>%
  left_join(o2sat_pao2, by = c("o2sat_int")) %>%
  ## Post-admission
  rename(pao2_est_adm = "pao2_est") %>%
  left_join(o2sat_pao2, by = c("o2sat_low_postadm_int" = "o2sat_int")) %>%
  rename(pao2_est_postadm = "pao2_est") %>%
  
  ## For O2 sats not included in EPIC II table:
  ## - If <80, assign lowest PaO2 in table
  ## - If 100, assign highest PaO2 in table
  mutate(
    pao2_est_adm = case_when(
      !is.na(pao2_est_adm) ~ pao2_est_adm,
      is.na(o2sat_int)     ~ as.numeric(NA),
      o2sat_int < 80       ~ 44,
      TRUE                 ~ 145
    ),
    pao2_est_postadm = case_when(
      !is.na(pao2_est_postadm)     ~ pao2_est_postadm,
      is.na(o2sat_low_postadm_int) ~ as.numeric(NA),
      o2sat_low_postadm_int < 80   ~ 44,
      TRUE                         ~ 145
    )
  ) %>%

  ## Calculate APACHE component scores
  mutate(
    ## Temperature
    temp_ap_adm = case_when(
      is.na(temp_enr_high)  | is.na(temp_enr_low) ~ as.numeric(NA),
      temp_enr_high >= 41   | temp_enr_low < 30   ~ 4,
      temp_enr_high >= 39   | temp_enr_low < 32   ~ 3,
                              temp_enr_low < 34   ~ 2,
      temp_enr_high >= 38.5 | temp_enr_low < 36   ~ 1,
      TRUE                                        ~ 0
    ),
    temp_ap = case_when(
      !is.na(temp_ap_adm) ~ temp_ap_adm,
      is.na(temp_high_postadm)  | is.na(temp_low_postadm) ~ as.numeric(NA),
      temp_high_postadm >= 41   | temp_low_postadm < 30   ~ 4,
      temp_high_postadm >= 39   | temp_low_postadm < 32   ~ 3,
                                  temp_low_postadm < 34   ~ 2,
      temp_high_postadm >= 38.5 | temp_low_postadm < 36   ~ 1,
      TRUE                                                ~ 0
    ),

    ## Mean arterial pressure
    map_ap_adm = case_when(
      is.na(map_enr_high) | is.na(map_enr_low) ~ as.numeric(NA),
      map_enr_high >= 160 | map_enr_low <= 49  ~ 4,
      map_enr_high >= 130                      ~ 3,
      map_enr_high >= 110 | map_enr_low <= 70  ~ 2,
      TRUE                                     ~ 0
    ),
    map_ap = case_when(
      !is.na(map_ap_adm)                               ~ map_ap_adm,
      is.na(map_high_postadm) | is.na(map_low_postadm) ~ as.numeric(NA),
      map_high_postadm >= 160 | map_low_postadm <= 49  ~ 4,
      map_high_postadm >= 130                          ~ 3,
      map_high_postadm >= 110 | map_low_postadm <= 70  ~ 2,
      TRUE                                             ~ 0
    ),

    ## Heart rate
    hr_ap_adm = case_when(
      is.na(hr_enr_high) | is.na(hr_enr_low) ~ as.numeric(NA),
      hr_enr_high >= 180 | hr_enr_low <= 39  ~ 4,
      hr_enr_high >= 140 | hr_enr_low <= 54  ~ 3,
      hr_enr_high >= 110 | hr_enr_low <= 69  ~ 2,
      TRUE                                   ~ 0
    ),
    hr_ap = case_when(
      !is.na(hr_ap_adm)                              ~ hr_ap_adm,
      is.na(hr_high_postadm) | is.na(hr_low_postadm) ~ as.numeric(NA),
      hr_high_postadm >= 180 | hr_low_postadm <= 39  ~ 4,
      hr_high_postadm >= 140 | hr_low_postadm <= 54  ~ 3,
      hr_high_postadm >= 110 | hr_low_postadm <= 69  ~ 2,
      TRUE                                           ~ 0
    ),

    ## Respiratory rate
    rr_ap_adm = case_when(
      is.na(rr_enr_high) | is.na(rr_enr_low) ~ as.numeric(NA),
      rr_enr_high >= 50  | rr_enr_low <= 5   ~ 4,
      rr_enr_high >= 35                      ~ 3,
                           rr_enr_low <= 9   ~ 2,
      rr_enr_high >= 25  | rr_enr_low <= 11  ~ 1,
      TRUE                                   ~ 0
    ),
    rr_ap = case_when(
      !is.na(rr_ap_adm)                              ~ rr_ap_adm,
      is.na(rr_high_postadm) | is.na(rr_low_postadm) ~ as.numeric(NA),
      rr_high_postadm >= 50  | rr_low_postadm <= 5   ~ 4,
      rr_high_postadm >= 35                          ~ 3,
                               rr_low_postadm <= 9   ~ 2,
      rr_high_postadm >= 25  | rr_low_postadm <= 11  ~ 1,
      TRUE                                           ~ 0
    ),

    ## Oxygenation: the fun part!
    ## If ABG info available (FiO2, PCO2, PaO2), calculate A-a gradient:
    ## ((713 * FiO2) - (PCO2 / 0.8)) - PaO2; formula confirmed by EWE Dec 2017
    ## If no ABG available,
    ## 1. Estimate PaO2 based on EPIC II (see reference above)
    ## 2. Assign points based on FiO2 < 0.5 for all pts - can't estimate PCO2
    aa_ap = ifelse(
      is.na(fio2_enr) | is.na(pco2_enr) | is.na(pao2_enr), NA,
      ((713 * fio2_enr) - (pco2_enr / 0.8)) - pao2_enr
    ),

    ## If FiO2 >= 0.5, assign points using A-a gradient;
    ##   if < 0.5, assign points using PaO2
    ## If FiO2 not available, assign points using PaO2 estimated from O2 sats
    oxy_ap_adm = case_when(
      is.na(fio2_enr) & is.na(pao2_est_adm) ~ as.numeric(NA),
      ## Preferred: ABG available
      !is.na(fio2_enr) & fio2_enr >= 0.5 & !is.na(aa_ap) & aa_ap >= 500     ~ 4,
      !is.na(fio2_enr) & fio2_enr >= 0.5 & !is.na(aa_ap) & aa_ap >= 350     ~ 3,
      !is.na(fio2_enr) & fio2_enr >= 0.5 & !is.na(aa_ap) & aa_ap >= 200     ~ 2,
      !is.na(fio2_enr) & fio2_enr >= 0.5 & !is.na(aa_ap) & aa_ap < 200      ~ 0,
      !is.na(fio2_enr) & fio2_enr < 0.5 & !is.na(pao2_enr) & pao2_enr < 55  ~ 4,
      !is.na(fio2_enr) & fio2_enr < 0.5 & !is.na(pao2_enr) & pao2_enr <= 60 ~ 3,
      !is.na(fio2_enr) & fio2_enr < 0.5 & !is.na(pao2_enr) & pao2_enr <= 70 ~ 1,
      ## If necessary: Use estimated PaO2 from O2 sats
      !is.na(pao2_est_adm) & pao2_est_adm < 55                              ~ 4,
      !is.na(pao2_est_adm) & pao2_est_adm <= 60                             ~ 3,
      !is.na(pao2_est_adm) & pao2_est_adm <= 70                             ~ 1,
      TRUE                                                                  ~ 0
    ),
    oxy_ap = case_when(
      !is.na(oxy_ap_adm)      ~ oxy_ap_adm,
      is.na(pao2_est_postadm) ~ as.numeric(NA),
      pao2_est_postadm < 55   ~ 4,
      pao2_est_postadm <= 60  ~ 3,
      pao2_est_postadm <= 70  ~ 1,
      TRUE                    ~ 0
    ),
    
    ## Acid (pH)
    acid_ap_adm = case_when(
      is.na(ph_enr)                 ~ as.numeric(NA),
      ph_enr >= 7.7 | ph_enr < 7.15 ~ 4,
      ph_enr >= 7.6 | ph_enr < 7.25 ~ 3,
                      ph_enr < 7.33 ~ 2,
      ph_enr >= 7.5                 ~ 1,
      TRUE                          ~ 0
    ),
    acid_ap = acid_ap_adm, ## pH never collected after consent
    
    ## CO2/HCO3: Use these points in place of acid, if no ABG
    co2_ap_adm = case_when(
      ## If neither pH nor CO2 (HCO3) is available, score is missing
      is.na(co2_enr_high) | is.na(co2_enr_low) ~ as.numeric(NA),
      ## If CO2 (HCO3) is available, use that
      co2_enr_high >= 52 | co2_enr_low < 15 ~ 4,
      co2_enr_high >= 41 | co2_enr_low < 18 ~ 3,
                           co2_enr_low < 22 ~ 2,
      co2_enr_high >= 32                    ~ 1,
      TRUE                                  ~ 0
    ),
    co2_ap = case_when(
      !is.na(co2_ap_adm)                               ~ co2_ap_adm,
      ## If CO2 (HCO3) is never available within 2 days of consent, score missing
      is.na(co2_high_postadm) | is.na(co2_low_postadm) ~ as.numeric(NA),
      co2_high_postadm >= 52 | co2_low_postadm < 15    ~ 4,
      co2_high_postadm >= 41 | co2_low_postadm < 18    ~ 3,
                               co2_low_postadm < 22    ~ 2,
      co2_high_postadm >= 32                           ~ 1,
      TRUE                                             ~ 0
    ),
    
    ## If acid points are missing, use CO2 instead
    acid_ap_adm = ifelse(is.na(acid_ap_adm), co2_ap_adm, acid_ap_adm),
    acid_ap = ifelse(is.na(acid_ap), co2_ap, acid_ap),
    
    ## Sodium
    na_ap_adm = case_when(
      is.na(na_enr_high) | is.na(na_enr_low) ~ as.numeric(NA),
      na_enr_high >= 180 | na_enr_low <= 110 ~ 4,
      na_enr_high >= 160 | na_enr_low <= 119 ~ 3,
      na_enr_high >= 155 | na_enr_low <= 129 ~ 2,
      na_enr_high >= 150                     ~ 1,
      TRUE                                   ~ 0
    ),
    na_ap = case_when(
      !is.na(na_ap_adm)                              ~ na_ap_adm,
      is.na(na_high_postadm) | is.na(na_low_postadm) ~ as.numeric(NA),
      na_high_postadm >= 180 | na_low_postadm <= 110 ~ 4,
      na_high_postadm >= 160 | na_low_postadm <= 119 ~ 3,
      na_high_postadm >= 155 | na_low_postadm <= 129 ~ 2,
      na_high_postadm >= 150                         ~ 1,
      TRUE                                           ~ 0
    ),

    ## Potassium
    k_ap_adm = case_when(
      is.na(k_enr_high) | is.na(k_enr_low) ~ as.numeric(NA),
      k_enr_high >= 7   | k_enr_low < 2.5  ~ 4,
      k_enr_high >= 6                      ~ 3,
                          k_enr_low < 3    ~ 2,
      k_enr_high >= 5.5 | k_enr_low < 3.5  ~ 1,
      TRUE                                 ~ 0
    ),
    k_ap = case_when(
      !is.na(k_ap_adm)                             ~ k_ap_adm,
      is.na(k_high_postadm) | is.na(k_low_postadm) ~ as.numeric(NA),
      k_high_postadm >= 7   | k_low_postadm < 2.5  ~ 4,
      k_high_postadm >= 6                          ~ 3,
                              k_low_postadm < 3    ~ 2,
      k_high_postadm >= 5.5 | k_low_postadm < 3.5  ~ 1,
      TRUE                                         ~ 0
    ),

    ## Creatinine (only *highest* collected after consent)
    ## If patient has acute renal failure, double creatinine score
    arf_cr = arf_enr + 1,
    cr_ap_adm = case_when(
      is.na(cr_enr_high) | is.na(cr_enr_low) ~ as.numeric(NA),
      cr_enr_high >= 3.5                     ~ 4 * arf_cr,
      cr_enr_high >= 2                       ~ 3 * arf_cr,
      cr_enr_high >= 1.5 | cr_enr_low < 0.6  ~ 2 * arf_cr,
      TRUE                                   ~ 0
    ),
    cr_ap = case_when(
      !is.na(cr_ap_adm)      ~ cr_ap_adm,
      is.na(cr_high_postadm) ~ as.numeric(NA),
      cr_high_postadm >= 3.5 ~ 4 * arf_cr,
      cr_high_postadm >= 2   ~ 3 * arf_cr,
      cr_high_postadm >= 1.5 ~ 2 * arf_cr,
      TRUE                   ~ 0
    ),

    ## Hematocrit/PCV
    pcv_ap_adm = case_when(
      is.na(pcv_enr_high) | is.na(pcv_enr_low) ~ as.numeric(NA),
      pcv_enr_high >= 60 | pcv_enr_low < 20    ~ 4,
      pcv_enr_high >= 50 | pcv_enr_low < 30    ~ 2,
      pcv_enr_high >= 46                       ~ 1,
      TRUE                                     ~ 0
    ),
    pcv_ap = case_when(
      !is.na(pcv_ap_adm)                               ~ pcv_ap_adm,
      is.na(pcv_high_postadm) | is.na(pcv_low_postadm) ~ as.numeric(NA),
      pcv_high_postadm >= 60 | pcv_low_postadm < 20    ~ 4,
      pcv_high_postadm >= 50 | pcv_low_postadm < 30    ~ 2,
      pcv_high_postadm >= 46                           ~ 1,
      TRUE                                             ~ 0
    ),

    ## White blood count
    wbc_ap_adm = case_when(
      is.na(wbc_enr_high) | is.na(wbc_enr_low) ~ as.numeric(NA),
      wbc_enr_high >= 40 | wbc_enr_low < 1     ~ 4,
      wbc_enr_high >= 20 | wbc_enr_low < 3     ~ 2,
      wbc_enr_high >= 15                       ~ 1,
      TRUE                                     ~ 0
    ),
    wbc_ap = case_when(
      !is.na(wbc_ap_adm)                               ~ wbc_ap_adm,
      is.na(wbc_high_postadm) | is.na(wbc_low_postadm) ~ as.numeric(NA),
      wbc_high_postadm >= 40 | wbc_low_postadm < 1     ~ 4,
      wbc_high_postadm >= 20 | wbc_low_postadm < 3     ~ 2,
      wbc_high_postadm >= 15                           ~ 1,
      TRUE                                             ~ 0
    ),

    ## Glasgow Coma Score
    ## Use same conversion for APACHE that Vasilevskis et al use for SOFA,
    ##  per TG December 2017
    gcs_ap_adm = case_when(
      (is.na(gcs_enr) | gcs_enr == 99) & is.na(rass_low_enr) ~ as.numeric(NA),
      !is.na(gcs_enr) & gcs_enr != 99                        ~ 15 - gcs_enr,
      rass_low_enr <= -4                                     ~ 4,
      rass_low_enr == -3                                     ~ 3,
      rass_low_enr == -2                                     ~ 2,
      rass_low_enr == -1                                     ~ 1,
      TRUE                                                   ~ 0
    ),
    gcs_ap = ifelse(!is.na(gcs_ap_adm), gcs_ap_adm,
             ifelse(is.na(gcs_postadm), NA, 15 - gcs_postadm)),

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
## Vectors of APS component names
apache_aps_prefixes <- c("temp", "map", "hr", "rr", "oxy", "acid", "na", "k",
                         "cr", "pcv", "wbc", "gcs")
vars_apache_aps_adm <- paste0(apache_aps_prefixes, "_ap_adm")
vars_apache_aps_ever <- paste0(apache_aps_prefixes, "_ap")

## Vector of overall APACHE component names
vars_apache_all_adm <- c(vars_apache_aps_adm, "age_ap", "chrondis_ap")
vars_apache_all_ever <- c(vars_apache_aps_ever, "age_ap", "chrondis_ap")

## How many components are available?
baseline$apache_vars_avail_adm <- rowSums(!is.na(baseline[, vars_apache_all_adm]))
baseline$apache_vars_avail_ever <- rowSums(!is.na(baseline[, vars_apache_all_ever]))

## Calculate totals; using na.rm = TRUE means that components still missing
## after looking three days ahead will be considered normal

## Total APACHE II, using both admission + days and day of admission only
baseline$apache_adm <- ifelse(
  baseline$apache_vars_avail_ever == 0, NA,
  rowSums(baseline[, vars_apache_all_ever], na.rm = TRUE)
)
baseline$apache_adm_only <- ifelse(
  baseline$apache_vars_avail_adm == 0, NA,
  rowSums(baseline[, vars_apache_all_adm], na.rm = TRUE)
)

## APACHE II Acute Physiology Score only
baseline$apache_aps_adm <- ifelse(
  baseline$apache_vars_avail_ever == 0, NA,
  rowSums(baseline[, vars_apache_aps_ever], na.rm = TRUE)
)
baseline$apache_aps_adm_only <- ifelse(
  baseline$apache_vars_avail_adm == 0, NA,
  rowSums(baseline[, vars_apache_aps_adm], na.rm = TRUE)
)

## -- Antipsychotics between ICU admission and consent -------------------------
## Includes >=1 of: aripiprazole, droperidol, haloperidol, olanzapine,
##   olanz/fluox, quetiapine, risperidone, ziprasidone
## Just want yes/no (unless reviewers ask for doses)
## These are stored as dropdowns in `med_x_enr`; up to 8 meds possible

antipsyc_meds <- c(
  "Aripiprazole", "Droperidol", "Haloperidol", "Olanzapine",
  "Olanzapine/Fluoxetine", "Quetiapine", "Risperidone", "Ziprasidone"
)

meds_admconsent <- baseline %>%
  dplyr::select(id, matches("^med\\_[0-9]\\_enr$")) %>%
  ## Convert numeric codes to medication names
  mutate_at(
    vars(matches("^med\\_[0-9]\\_enr$")),
    ~ factor(
      .,
      levels = get_levels_ih("med_1_enr"),
      labels = names(get_levels_ih("med_1_enr"))
    )
  ) %>%
  gather(key = med_num, value = med_name, med_1_enr:med_8_enr) %>%
  group_by(id) %>%
  summarise(antipsyc_adm = sum_na(med_name %in% antipsyc_meds) > 0)

## -- Combine prehospital, baseline datasets and save to analysisdata ----------
adm_df <- reduce(
  list(
    dplyr::select(
      baseline,
      id, age_consent, gender, race_cat, ethnicity, insurance, height, weight,
      bmi, home_antipsyc, charlson_total, frailty, frailty_f, icu_rsn,
      icu_rsn_other, apache_adm, apache_adm_only, apache_aps_adm,
      apache_aps_adm_only, sofa_adm, sofa_adm_only, sofa_mod_adm,
      sofa_mod_adm_only, cv_sofa_adm_f
    ),
    meds_admconsent,
    ph_form
  ),
  left_join,
  by = "id")

saveRDS(adm_df, file = "analysisdata/rds/admission.rds")
write_csv(adm_df, path = "analysisdata/csv/admission.csv")
