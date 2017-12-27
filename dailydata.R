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
  ## Remove converted versions from final dataset
  select(-matches("^daily\\_[a-z]+\\_")) %>%
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
