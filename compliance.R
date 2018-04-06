################################################################################
## Data management for data related to study drug administration
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

# ## -- Download randomization info ----------------------------------------------
# ptstatus_df <- readRDS("analysisdata/rds/ptstatus.rds")
# 
# ## Vector of all randomized patients never excluded
# rand_pts <- ptstatus_df %>%
#   filter(randomized & !excluded_ever) %>%
#   pull(id)

## -- Download data needed to determine whether patient ever got study drug ----
compliance_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c(
    "id", "redcap_event_name"
  ),
  forms = "abcde_protocol_compliance_tracking_form_ecbe",
  ## Compliance not tracked on "randomziation" or "prior to d/c" events
  events = setdiff(
    ih_events$unique_event_name,
    c("randomization_arm_1", "prior_to_hospital_arm_1")
  )
)

## -- Data management ----------------------------------------------------------
compliance_df <- compliance_raw %>%
  mutate(
    ## Dates/times
    abcde_date = ymd(abcde_date)
  ) %>%
  ## ICU, MV, SAT, SBT variables will all be used to determine denominators;
  ## make them logical
  mutate_at(
    vars(abcde_icu, vent_today, sat_today, sbt_today),
    as.logical
  ) %>%
  ## Definitions of compliance for each element, A-E
  mutate(
    ## Awakening:
    ## - Not applicable if patient not in ICU or not on MV
    ## - Compliant if 1) got an SAT; 2) did not, for reason other than "Other"
    ## - Otherwise, noncompliant
    comp_a = case_when(
      is.na(abcde_icu) | !abcde_icu |
        is.na(vent_today) | !vent_today ~ as.logical(NA),
      !is.na(sat_today) & sat_today                                 ~ TRUE,
      rowSums(.[, str_subset(names(.), "^sat\\_rsn\\_[1-7]$")]) > 0 ~ TRUE,
      TRUE                                                          ~ FALSE
    )
  )
