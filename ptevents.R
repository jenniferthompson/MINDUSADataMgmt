################################################################################
## Create dummy data set with IDs, REDCap event names, days since
##   enrollment/randomization for each event
## This will help with deciding which days to include in variables like DCFDs
################################################################################

library(tidyverse)
library(stringr)

## Source data management functions
source("data_functions.R")

## -- Import data dictionaries from REDCap -------------------------------------
## All tokens are stored in .Renviron
ih_events <- get_events("MINDUSA_IH_TOKEN") %>% mutate(event_num = 1:nrow(.))
## Add event_num to help with sorting events later
ih_mapping <- get_event_mapping("MINDUSA_IH_TOKEN")

## -- ptstatus_df = definitive source of those randomized w/ no exclusions -----
ptstatus_df <- readRDS("analysisdata/rds/ptstatus.rds")

## Vector of all randomized patients never excluded
rand_pts <- ptstatus_df %>%
  filter(randomized & !excluded_ever) %>%
  pull(id)

## -- Import REDCap event names, whether patient in ICU (from ABCDEF form), ----
## -- enrollment and randomization times if applicable -------------------------
events_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c("id", "redcap_event_name", "abcde_icu"),
  ## Randomization, hospital discharge events don't have daily data collection
  events = setdiff(
    ih_events$unique_event_name,
    c("randomization_arm_1", "prior_to_hospital_arm_1")
  )
) %>%
  ## Keep all consented patients who were not excluded
  filter(id %in% subset(ptstatus_df, consented & !excluded_ever)$id)

## Last day without "interventional" in event name = day of randomization
last_pr_day <- events_raw %>%
  select(id, redcap_event_name) %>%
  left_join(select(ih_events, unique_event_name, event_num),
            by = c("redcap_event_name" = "unique_event_name")) %>%
  filter(id %in% rand_pts & !str_detect(redcap_event_name, "^interventional")) %>%
  group_by(id) %>%
  arrange(event_num) %>%
  mutate(randomization_day = 1:n()) %>%
  slice(n()) %>%
  ungroup() %>%
  rename(randomization_event = "redcap_event_name")

## Add randomization event onto daily data; calculate days to each event
## 0 = day of consent/randomization
events_df <- events_raw %>%
  left_join(last_pr_day %>% select(-event_num), by = "id") %>%
  group_by(id) %>%
  mutate(
    days_since_consent = 1:n() - 1,
    days_since_randomization = (days_since_consent - randomization_day) + 1,
    in_icu = as.logical(abcde_icu)
  ) %>%
  select(
    id, redcap_event_name, days_since_consent, days_since_randomization, in_icu
  )

## Save to analysisdata
saveRDS(events_df, file = "analysisdata/rds/ptevents.rds")
write_csv(events_df, path = "analysisdata/csv/ptevents.csv")
