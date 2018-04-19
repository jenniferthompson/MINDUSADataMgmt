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

## -- Read in data frame of all patient events ---------------------------------
allptsevents_df <- readRDS("analysisdata/rds/allptevents.rds")

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
  ## Remove any patient-day that isn't "real" (see ptevents.R)
  right_join(
    dplyr::select(allptsevents_df, id, redcap_event_name),
    by = c("id", "redcap_event_name")
  ) %>%
  mutate(
    ## Dates/times
    abcde_date = ymd(abcde_date)
  ) %>%
  ## Convert numeric variables to logicals (all have Yes = 1, No = 0)
  mutate_at(
    vars(
      abcde_icu, vent_today, sat_today, sbt_today, starts_with("sat_rsn"),
      starts_with("sbt_rsn"), starts_with("nonpharm_int"), mobility_crit,
      starts_with("mobility_rsn"), starts_with("mobility_occur")
    ),
    as.logical
  ) %>%
  ## Convert highest level of mobility to factor
  mutate(
    mobility_highest = make_factor_ih(., "mobility_highest")
  ) %>%
  ## Definitions of compliance for each element, A-E
  mutate(
    ## Awakening: compliant if patient on MV received an SAT, or was
    ##    successfully screened for one
    ## - Not applicable if patient not in ICU or not on MV, or was off the unit
    ## - Compliant if 1) got an SAT; 2) did not, for reason other than "Other"
    ## - Otherwise, noncompliant
    elig_a = !is.na(abcde_icu) & abcde_icu & !is.na(vent_today) & vent_today &
      !(!is.na(sat_rsn_7) & sat_rsn_7),
    comp_a = case_when(
      !elig_a ~ as.logical(NA),
      !is.na(sat_today) & sat_today                                 ~ TRUE,
      rowSums(.[, str_subset(names(.), "^sat\\_rsn\\_[1-6]$")]) > 0 ~ TRUE,
      TRUE                                                          ~ FALSE
    ),
    ## Breathing: compliant if patient on MV received an SBT, or was
    ##    successfully screened for one
    ## - Not applicable if patient not in ICU or not on MV, or was off the unit,
    ##   or did not pass SAT and therefore did not progress to SBT
    ## - Compliant if 1) got an SBT; 2) did not, for reason other than "Other"
    ## - Otherwise, noncompliant
    elig_b = !is.na(abcde_icu) & abcde_icu & !is.na(vent_today) & vent_today &
      !((!is.na(sbt_rsn_1) & sbt_rsn_1) | (!is.na(sbt_rsn_2) & sbt_rsn_2)),
    comp_b = case_when(
      !elig_b ~ as.logical(NA),
      !is.na(sbt_today) & sbt_today                                 ~ TRUE,
      rowSums(.[, str_subset(names(.), "^sbt\\_rsn\\_[3-9]$")]) > 0 ~ TRUE,
      TRUE                                                          ~ FALSE
    ),
    ## Coordination of SAT, SBT: compliant if SBT was done while patient off
    ##   sedation
    ## - Not applicable if patient not in the ICU on the vent, or did not get
    ##   either SAT or SBT
    ## - Compliant if patient was off sedation during SBT (abc_paired)
    ## - Not compliant if patient was on sedation during SBT
    elig_c = !is.na(abcde_icu) & abcde_icu & !is.na(sat_today) & sat_today &
      !is.na(sbt_today) & sbt_today,
    comp_c = case_when(
      !elig_c ~ as.logical(NA),
      TRUE    ~ as.logical(abc_paired)
    ),
    
    ## Delirium: compliant if patient received at least one nonpharmacological
    ##   preventive intervention
    ## - Not applicable if patient not in the ICU, or was comatose
    ## - Compliant if at least one nonpharm intervention was done
    elig_d = !is.na(abcde_icu) & abcde_icu &
      !(!is.na(nonpharm_int_5) & nonpharm_int_5),
    
    ## Get number of nonpharmacological interventions
    nonpharm_int = ifelse(
      !elig_d, NA,
      rowSums(.[, paste0("nonpharm_int_", 1:4)], na.rm = TRUE)
    ),
    comp_d = case_when(
      !elig_d ~ as.logical(NA),
      TRUE    ~ as.logical(nonpharm_int > 0)
    ),
    
    ## Exercise: compliant if patient was successfully screened, and received
    ##   exercise therapy if personnel and patient were able
    ## - Not applicable if patient not in the ICU, or met criteria but was off
    ##     unit (for consistency with A/B elements)
    ## - Compliant if patient:
    ##   - had mobility
    ##   - didn't meet mobility criteria and has reason marked
    ##   - met criteria, but didn't get mobility due to sedatives, refusal, or
    ##     clinical refusal
    elig_e = !is.na(abcde_icu) & abcde_icu &
      !(!is.na(mobility_occur_rsn_3) & mobility_occur_rsn_3),
    
    ## If eligible but criteria not met, fill in F for whether mobility occurred
    mobility_occur = ifelse(elig_e & !mobility_crit, FALSE, mobility_occur),
    
    comp_e = case_when(
      !elig_e ~ as.logical(NA),
      is.na(mobility_crit) ~ FALSE,
      !is.na(mobility_occur) & mobility_occur ~ TRUE,
      !mobility_crit &
        rowSums(.[, paste0("mobility_rsn_", 1:6)], na.rm = TRUE) > 0 ~ TRUE,
      mobility_crit &
        !mobility_occur &
        rowSums(
          .[, paste0("mobility_occur_rsn_", c(0, 2, 4))],
          na.rm = TRUE
        ) > 0 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  ## Rename reasons for not getting SAT, SBT; nonpharm interventions
  rename(
    sat_rsn_seizures = sat_rsn_1,
    sat_rsn_alcohol = sat_rsn_2,
    sat_rsn_agitation = sat_rsn_3,
    sat_rsn_paralytics = sat_rsn_4,
    sat_rsn_mi = sat_rsn_5,
    sat_rsn_icp = sat_rsn_6,
    sbt_rsn_agitation = sbt_rsn_3,
    sbt_rsn_o2sat = sbt_rsn_4,
    sbt_rsn_fio2 = sbt_rsn_5,
    sbt_rsn_peep = sbt_rsn_6,
    sbt_rsn_mi = sbt_rsn_7,
    sbt_rsn_vasopressor = sbt_rsn_8,
    sbt_rsn_aprv = sbt_rsn_9,
    nonpharm_int_pain = nonpharm_int_1,
    nonpharm_int_orient = nonpharm_int_2,
    nonpharm_int_sensory = nonpharm_int_3,
    nonpharm_int_sleep = nonpharm_int_4,
    mobility_rsn_mi = mobility_rsn_4,
    mobility_rsn_arr = mobility_rsn_5,
    mobility_rsn_fio2 = mobility_rsn_2,
    mobility_rsn_peep = mobility_rsn_3,
    mobility_rsn_vasopressor = mobility_rsn_6,
    mobility_rsn_rass = mobility_rsn_1
  ) %>%
  dplyr::select(
    id, redcap_event_name, starts_with("elig_"), starts_with("comp_"),
    "sat_today", matches("^sat_rsn_[a-z]"),
    "sbt_today", matches("^sbt_rsn_[a-z]"),
    matches("^nonpharm_int_[a-z]"), nonpharm_int,
    "mobility_occur", matches("^mobility_rsn_[a-z]"), mobility_highest
  )

## -- Save final dataset -------------------------------------------------------
saveRDS(compliance_df, file = "analysisdata/rds/abcdecomp.rds")
write_csv(compliance_df, path = "analysisdata/csv/abcdecomp.csv")
