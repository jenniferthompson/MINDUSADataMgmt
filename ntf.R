################################################################################
## Data management for Notes to File form
## This form is filled out as needed during the hospitalization
################################################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(assertr)

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

## -- Download NTF form --------------------------------------------------------
## Goals:
## - Report protocol noncompliance that increased risk to patient
##   (other noncompliance is systematically documented elsewhere in database -
##   eg, drug titration info - and is documented elsewhere in the SAP)
##   To fit criteria of increased risk, NTFs must have "Yes" for both:
##   Was event/issue considered by the VCC to...
##   1) Involve protocol noncompliance? (ntf_compliance_x)
##   2) Place the patient or others at greater risk of harm than previously
##      known/recognized? (ntf_vccrisk_x)
##   Describe these events by categories (ntf_reason_x; check all that apply)
ntf_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  forms = c("note_to_file_form"),
  events = "enrollment__day_0_arm_1"
) %>%
  ## Select only needed variables
  select(
    -note_to_file_form_complete
  )

## -- Reshape to long format: For each NTF, keep... ----------------------------
## - Categories (ntf_reason_x_1-7)
## - Whether event involved protocol noncompliance (ntf_noncompliance_x)
## - Whether event placed patient at greater risk (ntf_vccrisk_x)
## - Explanation (ntf_explain_x)
ntf_df <- ntf_raw %>%
  dplyr::select(
    id, ntf_number, starts_with("ntf_reason_"), starts_with("ntf_vccrisk_"),
    starts_with("ntf_noncompliance_"), starts_with("ntf_explain_")
  ) %>%
  ## -- Reshape from wide (many cols per NTF) to long (one row per NTF) --------
  ## One row per possible NTF, per piece of info
  gather(key = "ntf_var", value = "ntf_value", -id, -ntf_number) %>%
  ## Separate variable name into [variable name], [NTF number],
  ## [checkbox option] (checkbox option only relevant for ntf_reason variables;
  ## all others will have NA for ntf_option)
  separate(
    col = ntf_var,
    into = c("ntf_var", "ntf_event", "ntf_option"),
    sep = "\\_(?=[0-9])"
  ) %>%
  ## Add category suffix back onto ntf_reason variables
  unite(col = ntf_var, ntf_var, ntf_option) %>%
  mutate(ntf_var = str_remove(ntf_var, "\\_NA$")) %>%
  spread(key = ntf_var, value = ntf_value) %>%
  ## Change everything but ID, explanation back to numeric
  mutate_at(vars(-id, -ntf_explain), as.numeric) %>%
  ## Keep only real events: ntf_number is present and >= ntf_event
  ## (REDCap branching logic only shows as many sets of NTF variables as are
  ##  entered in ntf_number, so no possibility for "extra" events to be entered)
  filter(!is.na(ntf_number) & ntf_event <= ntf_number) %>%
  ## Rename category variables for clarity, make logicals
  rename(
    ntf_cat_incexc    = ntf_reason_1,
    ntf_cat_specimen  = ntf_reason_2,
    ntf_cat_titration = ntf_reason_3,
    ntf_cat_asmt      = ntf_reason_4,
    ntf_cat_saevcc    = ntf_reason_5,
    ntf_cat_saeirb    = ntf_reason_6,
    ntf_cat_other     = ntf_reason_7
  ) %>%
  mutate_at(vars(ntf_noncompliance:ntf_vccrisk), as.logical) %>%
  ## Remove PHI from explanations
  mutate(ntf_explain = remove_phi(ntf_explain))

## -- Dataset for only protocol noncompliance events ---------------------------
noncomp_df <- ntf_df %>%
  ## Filter to events that meet definitions of noncompliance w/ inc risk
  filter(ntf_noncompliance == 1 & ntf_vccrisk == 1)

## -- Save final noncompliance dataset -----------------------------------------
saveRDS(noncomp_df, file = "analysisdata/rds/noncompliance.rds")
write_csv(noncomp_df, path = "analysisdata/csv/noncompliance.csv")
