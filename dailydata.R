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
  select(-daily_data_collection_form_complete)

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
