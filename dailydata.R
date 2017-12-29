################################################################################
## Data management for Daily Data Collection form
## This form is filled out once daily through Intervention Day 14, and possibly
##   through Intervention Day 18 if needed for four days post-study drug
################################################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(assertr)
library(googlesheets) ## For reading in drug abbreviations

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

## -- Read in our standardized drug abbreviations, stored in Google Sheet ------
## Only need columns for full drug name + abbreviation
icudel_meds <- gs_key("1ZGfxAmTFxGgfzjwE0trrpKEn_21QDwCSGJIv-E5kiE4") %>%
  gs_read(ws = "Sheet1", range = cell_cols(c(1, 3))) %>%
  select(-2)
names(icudel_meds) <- c("med_name", "med_abbrev")

## -- Download variables from dates tracking form ------------------------------
daily_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "redcap_event_name", "abcde_icu"),
  forms = c("daily_data_collection_form"),
  events = setdiff(
    ih_events$unique_event_name,
    c("randomization_arm_1", "prior_to_hospital_arm_1")
  )
) %>%
  ## Remove unneeded variables
  select(-daily_data_collection_form_complete) %>%
  ## Restrict to "real" events, as determined in ptevents.R
  right_join(allpts_events, by = c("id", "redcap_event_name"))

## -- Medications --------------------------------------------------------------
## Medications collected as "1. What was med 1? 2. What was dose of med 1?"
## For each day, reshape data to one record per drug given, get total of each
## drug, and reshape back to one record per day.

med_df <- daily_raw %>%
  ## Reshaping to long format
  select(id, redcap_event_name, matches("^daily\\_med\\_[1-9]+[\\_amt]*$")) %>%
  gather(key = med_var, value = med_value, daily_med_1:daily_med_8_amt) %>%
  separate(med_var, into = c("junk1", "junk2", "med_num", "amt_not"), sep = "_") %>%
  mutate(amt_not = ifelse(is.na(amt_not), "med_name", paste0("med_", amt_not))) %>%
  select(-matches("^junk")) %>%
  spread(key = amt_not, value = med_value) %>%
  
  ## Make factor out of med_name; all daily_med_xxxx variables have same levels
  mutate(
    med_name = factor(
      med_name,
      levels = get_levels_ih("daily_med_1"),
      labels = names(get_levels_ih("daily_med_1"))
    ),
    
    ## COMBINE Olanzapine, Olanzapine/Fluoxetine for daily totals (EWE Dec 2017)
    med_name = fct_collapse(
      med_name,
      Olanzapine = c("Olanzapine", "Olanzapine/Fluoxetine")
    )
  ) %>%
  
  ## Just in case a drug got entered twice on the same day: final drug doses per
  ## day = total of each (this removed four rows)
  group_by(id, redcap_event_name, med_name) %>%
  summarise(med_total = sum_na(med_amt)) %>%
  ungroup() %>%
  
  ## Merge on drug abbreviations
  left_join(icudel_meds, by = "med_name") %>%
  mutate(med_abbrev = paste0("daily_", med_abbrev)) %>%
  select(-med_name) %>%
  ## Note: We need the daily_NA level here to keep at least one record per
  ##  hospitalization day; we'll throw out that variable later
  
  ## Reshape back to wide format, one record per day with one column per med
  spread(key = med_abbrev, value = med_total) %>%
  
  ## Assume that if no dose is recorded, patient got none that day
  mutate_at(vars(starts_with("daily_")), funs(ifelse(is.na(.), 0, .))) %>%
  
  ## Calculate total benzodiazepines, opioids, open-label antipsychotics
  ## (Reference formulas for conversions stored in Google Sheet)
  
  ## Antipsychotics: Haloperidol equivalents
  ## H =
  ##    (quetiapine / 88.16)^(1 / 0.786)
  ##    (aripiprazole / 4.343)^(1 / 0.645)
  ##    (olanzapine / 2.9)^(1 / 0.805)
  ##    (risperidone / 0.79)^(1 / 0.851)
  ##    (ziprasidone / 35.59)^(1 / 0.578)
  mutate(
    ## Convert non-haloperidol antipsychotics to haloperidol equivalents
    daily_quet_halop   = (daily_quet / 88.16) ^ (1 / 0.786),
    daily_ari_halop    = (daily_ari / 4.343) ^ (1 / 0.645),
    daily_olanz_halop  = (daily_olanz / 2.9) ^ (1 / 0.805),
    daily_risp_halop   = (daily_risp / 0.79) ^ (1 / 0.851),
    daily_zipras_halop = (daily_zipras / 35.59) ^ (1 / 0.578),
    
    ## Get total dose of antipsychotics today in haloperidol equivalents
    daily_antipsyc = daily_halop + daily_quet_halop + daily_ari_halop +
      daily_olanz_halop + daily_risp_halop + daily_zipras_halop
  ) %>%
  
  ## Benzodiazepines: Midazolam equivalents
  ## midazolam = (lorazepam * 2.5) = (diazepam / 2)
  mutate(
    daily_loraz_midaz = daily_loraz * 2.5,
    daily_diaz_midaz  = daily_diaz / 2,
    
    daily_benzo = daily_midaz + daily_loraz_midaz + daily_diaz_midaz
  ) %>%
  
  ## Opioids: Fentanyl equivalents (methadone not collected)
  ## fentanyl =
  ##   (morphine / 50)*1000 
  ##   (hydromorphone / 7.5)*1000
  mutate(
    daily_morph_fent      = (daily_morph / 50) * 1000,
    daily_hydromorph_fent = (daily_hydromorph / 7.5) * 1000,
    
    daily_opioid = daily_fent + daily_morph_fent + daily_hydromorph_fent
  )

# ## Visual data checks of drug conversions
# ## Summary: There are some patients with REALLY high values on some days, but
# ##   looking at the data clean databases, these have been checked and are
# ##   extreme but correct.
# check_drug_conversions <- function(drug_vars){
#   tmp <- med_df %>% select(one_of(drug_vars)) %>%
#     gather(key = drug_name, value = drug_dose)
#   
#   print(
#     ggplot(data = tmp, aes(x = drug_dose)) +
#       facet_wrap(~ drug_name) +
#       geom_histogram()
#   )
# }
# 
# drug_sets <- list(
#   "benzos" =
#     c("daily_midaz", "daily_loraz_midaz", "daily_diaz_midaz", "daily_benzo"),
#   "opioids" =
#     c("daily_fent", "daily_morph_fent", "daily_hydromorph_fent", "daily_opioid"),
#   "antipsychotics" =
#     c("daily_halop", "daily_quet_halop", "daily_ari_halop", "daily_ari",
#       "daily_olanz_halop", "daily_risp_halop", "daily_zipras_halop",
#       "daily_antipsyc")
# )
# 
# walk(drug_sets, check_drug_conversions)

med_df <- med_df %>%
  ## Remove converted versions and meaningless daily_NA from final dataset
  ## (daily_NA is an artifact of the reshaping process)
  select(-daily_NA, -matches("^daily\\_[a-z]+\\_")) %>%
  ## Add yes/no daily medications back on, create more meaningful variable names
  left_join(daily_raw %>%
              select(id, redcap_event_name,
                     matches("^addl\\_meds\\_cat\\_daily\\_[1, 2, 4, 6]")),
            by = c("id", "redcap_event_name")) %>%
  rename(daily_abx      = "addl_meds_cat_daily_1",
         daily_anxio    = "addl_meds_cat_daily_2",
         daily_opioidpo = "addl_meds_cat_daily_4",
         daily_statin   = "addl_meds_cat_daily_6") %>%
  right_join(allpts_events, by = c("id", "redcap_event_name"))

## Note: "Narcotics PO" as an option actually refers to several opioid
##   medications; we use "opioids" here to increase precision. Specifically,
##   this asks whether any of the following are given: hydrocodone+acetaminophen,
##   oxycodone, oxycodone+acetaminophen, and/or tramadol.

## -- Calculate summary drug variables: mean daily and total during... ---------
## - Entire hospitalization
## - Pre-randomization period (consent -> randomization)
## - Intervention period (randomization + next 13 days, as long as hospitalized)
## - All ICU days, regardless of time period
## - All ICU days during intervention period

## NOTE: For patients who received each drug, "all" and "exposed" values of days
##    and total dose will be the same. This is **not** true for mean dose.
##    mean_all = total dose / number of days *in the time period* (eg, in
##      hospital). We may use this as a covariate in models for long-term
##      outcomes, for example, to describe the average daily sedative given
##      while in the hospital; patients who never received the drug have a 0.
##    mean_exp = total dose / number of days *received the drug during the time
##      period*. This is a more accurate representation for *describing* what
##      happens during the study - when patients got Drug X, how much did they
##      get on an average day?

## Prep: Medication categories will be treated differently than med doses
med_cats <- c("abx", "anxio", "opioidpo", "statin")
med_cats_regex <- paste(med_cats, collapse = "|")

## Write a function to calculate the same drug variables for a given time period
calc_med_vars <- function(
  df,    ## data.frame including only desired patient-days
  suffix ## suffix to add to variable names to indicate time period
){
  df %>%
    group_by(id) %>%
    summarise_at(
      vars(starts_with("daily_")),
      funs(days_all = sum_na(. > 0),     ## number of days received drug
           prop_days = mean_na(. > 0),   ## proportion of days received drug
           mean_all = mean_na(.),        ## total dose / days in hospital
           mean_exp = mean_na(.[. > 0]), ## mean dose on days *received* drug
           total_all = sum_na(.))        ## total dose, all patients
    ) %>%
    ## Strip "daily_" from all variable names
    rename_all(funs(str_replace(., "^daily\\_", ""))) %>%
    ## Create additional "among exposed" variables
    ## Days
    mutate_at(
      vars(matches("\\_days\\_all$")), funs(days_exp = ifelse(. > 0, ., NA))
    ) %>%
    rename_at(
      vars(matches("\\_days\\_all\\_days\\_exp")),
      funs(str_replace(., "days\\_all\\_", ""))
    ) %>%
    ## Total
    mutate_at(
      vars(matches("\\_total\\_all$")), funs(total_exp = ifelse(. > 0, ., NA))
    ) %>%
    rename_at(
      vars(matches("\\_total\\_all\\_total\\_exp")),
      funs(str_replace(., "total\\_all\\_", ""))
    ) %>%
    ## For medication categories, mean & total variables are duplicates of days
    ##   and proportion variables; remove unneeded columns
    select(
      ## Remove "mean among exposed"; always 1
      -matches(sprintf("(%s)\\_mean\\_exp$", med_cats_regex)),
      ## Remove "total"; same as "days"
      -matches(sprintf("(%s)\\_total\\_(all|exp)$", med_cats_regex)),
      ## Remove "mean among all"; same as "prop_days"
      -matches(sprintf("(%s)\\_mean\\_all$", med_cats_regex))
    ) %>%
    ## Taking the mean of all NAs (_exp) results in NaN; turn these into NA
    mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
    ## Add _ih suffix to everything
    rename_at(vars(-id), funs(paste(., suffix, sep = "_"))) %>%
    ungroup()
}

med_ih <- calc_med_vars(med_df, "ih")
med_pr <- calc_med_vars(med_df %>% filter(prerandom), "pr")
med_int <- calc_med_vars(med_df %>% filter(intervention), "int")
med_icu <- calc_med_vars(med_df %>% filter(in_icu), "icu")
med_int_icu <- calc_med_vars(med_df %>% filter(intervention & in_icu), "int_icu")

## Join all medication summary variables into a single, gigantic dataset
med_summary <- reduce(
  list(med_ih, med_pr, med_int, med_icu, med_int_icu),
  left_join,
  by = "id"
)

## -- Daily SOFA ---------------------------------------------------------------
## References:
##   Original SOFA paper, Vincent et al (https://doi.org/10.1007/BF01709751)
##   S/F as substitute for unavailable P/F:
##     Pandharipande et al https://www.ncbi.nlm.nih.gov/pubmed/19242333
##   RASS as substitute for unavailable GCS:
##     Vasilevskis et al https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4748963/
##     Method C has best predictive validity
sofa_df <- daily_raw %>%
  select(id, redcap_event_name, abcde_icu, pfratio_daily, sf_daily,
         plt_daily, bili_daily, gcs_daily, rass_daily_low, cr_daily, uo_daily,
         cv_sofa_daily) %>%
  ## Create factors of some variables, making "not available" NA
  mutate(
    gcs_daily = na_if(gcs_daily, 99),
    rass_daily_low =
      as.numeric(as.character(make_factor_ih(., "rass_daily_low"))),
    uo_daily = factor(
      ifelse(uo_daily == 0, NA, uo_daily),
      levels = 1:3,
      labels = names(get_levels_ih("uo_daily")[1:3])
    )
  )

# ## Look at missingness of variables required to calculate SOFA
# library(naniar)
# sofa_df_sub <- subset(sofa_df, select = -c(id, redcap_event_name))
# 
# vis_miss(
#   subset(sofa_df_sub, abcde_icu == 1, select = -abcde_icu) %>%
#     arrange(days_since_consent)
# ) + ggtitle("ICU Days Only")
# 
# vis_miss(
#   subset(sofa_df_sub, abcde_icu == 0, select = -abcde_icu) %>%
#     arrange(days_since_consent)
# ) + ggtitle("Non-ICU Days Only")
# 
# vis_miss(
#   subset(sofa_df_sub, is.na(abcde_icu), select = -abcde_icu) %>%
#     arrange(days_since_consent)
# ) + ggtitle("No ABCDE Form (Missing ICU Day Question)")

## Calculate *raw* SOFA component scores (no imputation)
sofa_df <- sofa_df %>%
  ## SOFA component scores
  mutate(
    ## Respiratory component: Use P/F if available; otherwise use S/F
    resp_sofa_raw = case_when(
      is.na(pfratio_daily) & is.na(sf_daily) ~ as.numeric(NA),
      pfratio_daily <= 100  ~ 4,
      pfratio_daily <= 200  ~ 3,
      pfratio_daily <= 300  ~ 2,
      pfratio_daily <= 400  ~ 1,
      !is.na(pfratio_daily) ~ 0,
      sf_daily <= 89        ~ 4,
      sf_daily <= 214       ~ 3,
      sf_daily <= 357       ~ 2,
      sf_daily <= 512       ~ 1,
      TRUE                  ~ 0
    ),

    ## Coagulation
    coag_sofa_raw = case_when(
      is.na(plt_daily) ~ as.numeric(NA),
      plt_daily <= 20  ~ 4,
      plt_daily <= 50  ~ 3,
      plt_daily <= 100 ~ 2,
      plt_daily <= 150 ~ 1,
      TRUE             ~ 0
    ),

    ## Liver
    liver_sofa_raw = case_when(
      is.na(bili_daily) ~ as.numeric(NA),
      bili_daily >= 12  ~ 4,
      bili_daily >= 6   ~ 3,
      bili_daily >= 2   ~ 2,
      bili_daily >= 1.2 ~ 1,
      TRUE              ~ 0
    ),

    ## Central nervous system: Use GCS if available; otherwise, use RASS
    cns_sofa_raw = case_when(
      is.na(gcs_daily) & is.na(rass_daily_low) ~ as.numeric(NA),
      !is.na(gcs_daily) & gcs_daily < 6        ~ 4,
      !is.na(gcs_daily) & gcs_daily < 10       ~ 3,
      !is.na(gcs_daily) & gcs_daily < 13       ~ 2,
      !is.na(gcs_daily) & gcs_daily < 15       ~ 1,
      rass_daily_low <= -4                     ~ 4,
      rass_daily_low == -3                     ~ 3,
      rass_daily_low == -2                     ~ 2,
      rass_daily_low == -1                     ~ 1,
      TRUE                                     ~ 0
    ),

    ## Renal: Look at highest creatinine *and* urine output
    renal_sofa_raw = case_when(
      is.na(cr_daily) ~ as.numeric(NA),
      cr_daily >= 5   | (!is.na(uo_daily) & uo_daily == "0-200")   ~ 4,
      cr_daily >= 3.5 | (!is.na(uo_daily) & uo_daily == "201-500") ~ 3,
      cr_daily >= 2                                                ~ 2,
      cr_daily >= 1.2                                              ~ 1,
      TRUE                                                         ~ 0
    ),

    ## CV: already in database; create new variable to match naming convention
    cv_sofa_raw = as.numeric(cv_sofa_daily)
  )

## For missing components, find closest available component score within +/-2
## days to impute. If length of time is the same (eg, missing day has a value
## the day before and after), prioritize the earlier one.
sofa_df <- sofa_df %>%
  group_by(id) %>%
  mutate_at(
    vars(ends_with("_sofa_raw")), funs(imp = impute_closest(.))
  ) %>%
  ungroup() %>%
  rename_at(
    vars(ends_with("_raw_imp")), funs(str_replace(., "\\_raw", ""))
  ) %>%
  ## For liver component *only*, if data is still missing after imputation,
  ## assign a value of 0 (normal); we assume that no bilirubin available
  ## indicates no clinical reason to suspect liver dysfunction.
  mutate(
    liver_sofa_imp = ifelse(is.na(liver_sofa_imp), 0, liver_sofa_imp)
  )
  
# ## Look at missingness of SOFA components
# library(naniar)
# sofa_comp_df <-
#   sofa_df[, grep("(abcde\\_icu|days\\_since\\_consent|\\_sofa\\_raw|\\_sofa\\_imp)$",
#                  names(sofa_df2))]
# 
# vis_miss(
#   sofa_comp_df %>%
#     filter(abcde_icu == 1) %>%
#     select(resp_sofa_raw, resp_sofa_imp, coag_sofa_raw, coag_sofa_imp,
#            cns_sofa_raw, cns_sofa_imp, cv_sofa_raw, cv_sofa_imp,
#            liver_sofa_raw, liver_sofa_imp, renal_sofa_raw, renal_sofa_imp)
# ) + ggtitle("ICU Days Only")
# 
# vis_miss(
#   sofa_comp_df %>%
#     filter(abcde_icu == 0) %>%
#     select(resp_sofa_raw, resp_sofa_imp, coag_sofa_raw, coag_sofa_imp,
#            cns_sofa_raw, cns_sofa_imp, cv_sofa_raw, cv_sofa_imp,
#            liver_sofa_raw, liver_sofa_imp, renal_sofa_raw, renal_sofa_imp)
# ) + ggtitle("Non-ICU Days Only")
# 
# vis_miss(
#   sofa_comp_df %>%
#     filter(is.na(abcde_icu)) %>%
#     select(resp_sofa_raw, resp_sofa_imp, coag_sofa_raw, coag_sofa_imp,
#            cns_sofa_raw, cns_sofa_imp, cv_sofa_raw, cv_sofa_imp,
#            liver_sofa_raw, liver_sofa_imp, renal_sofa_raw, renal_sofa_imp)
# ) + ggtitle("No ABCDE Form (Missing ICU Day Question)")

## Sum components to get versions of SOFA
## Vectors of SOFA variable prefixes
sofa_mod_prefixes <- c("resp", "coag", "liver", "renal", "cv")
sofa_all_prefixes <- c(sofa_mod_prefixes, "cns")

## Vectors of SOFA variable names
sofa_mod_vars_raw <- paste0(sofa_mod_prefixes, "_sofa_raw")
sofa_mod_vars_imp <- paste0(sofa_mod_prefixes, "_sofa_imp")
sofa_all_vars_raw <- paste0(sofa_all_prefixes, "_sofa_raw")
sofa_all_vars_imp <- paste0(sofa_all_prefixes, "_sofa_imp")

## How many components are available?
sofa_df$sofa_vars_raw <- rowSums(!is.na(sofa_df[, sofa_all_vars_raw]))
sofa_df$sofa_vars_imp <- rowSums(!is.na(sofa_df[, sofa_all_vars_imp]))
sofa_df$sofa_mod_vars_raw <- rowSums(!is.na(sofa_df[, sofa_mod_vars_raw]))
sofa_df$sofa_mod_vars_imp <- rowSums(!is.na(sofa_df[, sofa_mod_vars_imp]))

## Calculate totals; using na.rm = TRUE means that components still missing
## after looking ahead will be considered normal

## Overall SOFA
sofa_df$sofa_raw <- ifelse(
  sofa_df$sofa_vars_raw == 0, NA,
  rowSums(sofa_df[, sofa_all_vars_raw], na.rm = TRUE)
)
sofa_df$sofa_imp <- ifelse(
  sofa_df$sofa_vars_imp == 0, NA,
  rowSums(sofa_df[, sofa_all_vars_imp], na.rm = TRUE)
)

## Modified SOFA (no CNS component)
sofa_df$sofa_mod_raw <- ifelse(
  sofa_df$sofa_mod_vars_raw == 0, NA,
  rowSums(sofa_df[, sofa_mod_vars_raw], na.rm = TRUE)
)
sofa_df$sofa_mod_imp <- ifelse(
  sofa_df$sofa_mod_vars_imp == 0, NA,
  rowSums(sofa_df[, sofa_mod_vars_imp], na.rm = TRUE)
)

# ## Check distributions
# ggplot(data = sofa_df, aes(x = sofa_raw)) +
#   geom_bar(stat = "count") +
#   labs(
#     caption =
#       sprintf(
#         "SOFA using raw data only, with missing data = normal; %s (%s%%) have no components available",
#         sum(is.na(sofa_df$sofa_raw)),
#         round(mean(is.na(sofa_df$sofa_raw))*100)
#       )
#   )
# 
# ggplot(data = sofa_df, aes(x = sofa_imp)) +
#   geom_bar(stat = "count") +
#   labs(
#     caption =
#       sprintf(
#         "SOFA using imputed data, with missing data = normal; %s (%s%%) have no components available",
#         sum(is.na(sofa_df$sofa_imp)),
#         round(mean(is.na(sofa_df$sofa_imp))*100)
#       )
#   )
# 
# ggplot(data = sofa_df, aes(x = sofa_mod_raw)) +
#   geom_bar(stat = "count") +
#   labs(
#     caption =
#       sprintf(
#         "Modified SOFA using raw data only, with missing data = normal; %s (%s%%) have no components available",
#         sum(is.na(sofa_df$sofa_mod_raw)),
#         round(mean(is.na(sofa_df$sofa_mod_raw))*100)
#       )
#   )
# 
# ggplot(data = sofa_df, aes(x = sofa_mod_imp)) +
#   geom_bar(stat = "count") +
#   labs(
#     caption =
#       sprintf(
#         "Modified SOFA using imputed data, with missing data = normal; %s (%s%%) have no components available",
#         sum(is.na(sofa_df$sofa_mod_imp)),
#         round(mean(is.na(sofa_df$sofa_mod_imp))*100)
#       )
#   )

## -- Calculate summary SOFA variables: mean daily during... -------------------
## - Entire hospitalization
## - Pre-randomization period (consent -> randomization)
## - Intervention period (randomization + next 13 days, as long as hospitalized)
## - All ICU days, regardless of time period
## - All ICU days during intervention period

sofa_df <- sofa_df %>%
  left_join(
    select(allpts_events, id, redcap_event_name, prerandom, intervention, in_icu),
    by = c("id", "redcap_event_name")
  )

## Write a function to calculate the same SOFA variables for a given time period
calc_sofa_vars <- function(
  df,    ## data.frame including only desired patient-days
  suffix ## suffix to add to variable names to indicate time period
){
  df %>%
    group_by(id) %>%
    summarise_at(
      vars(sofa_raw:sofa_mod_imp),
      funs(mean = mean_na, ## mean during period
           max = max_na,   ## max during period
           min = min_na)   ## min during period
    ) %>%
    rename_at(vars(-id), funs(paste(., suffix, sep = "_"))) %>%
    ungroup()
}

sofa_ih  <- calc_sofa_vars(sofa_df, "ih")
sofa_pr  <- calc_sofa_vars(sofa_df %>% filter(prerandom), "pr")
sofa_int <- calc_sofa_vars(sofa_df %>% filter(intervention), "int")
sofa_icu <- calc_sofa_vars(sofa_df %>% filter(in_icu), "icu")
sofa_int_icu <-
  calc_sofa_vars(sofa_df %>% filter(intervention & in_icu), "int_icu")

## Join all medication summary variables into a single, gigantic dataset
sofa_summary <- reduce(
  list(sofa_ih, sofa_pr, sofa_int, sofa_icu, sofa_int_icu),
  left_join,
  by = "id"
)

## -- Combine daily, summary information into single datasets ------------------
daily_df <- reduce(
  list(
    med_df %>% select(-(days_since_consent:postint)),
    sofa_df %>% select(-abcde_icu, -(prerandom:in_icu))
  ),
  left_join, by = c("id", "redcap_event_name")
)

dailysum_df <- reduce(
  list(
    med_summary,
    sofa_summary
  ),
  left_join, by = c("id")
)

## -- Save analysis datasets to /analysisdata ----------------------------------
## Daily data
saveRDS(daily_df, file = "analysisdata/rds/dailydata.rds")
write_csv(daily_df, path = "analysisdata/csv/dailydata.csv")

## Summary variables
saveRDS(dailysum_df, file = "analysisdata/rds/dailysummary.rds")
write_csv(dailysum_df, path = "analysisdata/csv/dailysummary.csv")
