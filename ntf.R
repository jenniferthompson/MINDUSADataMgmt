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
    id, starts_with("ntf_reason_"), starts_with("ntf_vccrisk_"),
    starts_with("ntf_noncompliance_"), starts_with("ntf_explain_")
  ) %>%
  ## Reshape from wide to long format
  ## One row per possible NTF, per piece of info
  gather(key = "ntf_var", value = "ntf_value", -id) %>%
  ## Separate variable name into [variable name], [NTF number],
  ## [checkbox option] (checkbox option only relevant for ntf_reason variables;
  ## all others will have NA for ntf_option)
  separate(
    col = ntf_var,
    into = c("ntf_var", "ntf_event", "ntf_option"),
    sep = "\\_(?=[0-9])"
  )

