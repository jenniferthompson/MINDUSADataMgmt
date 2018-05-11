################################################################################
## Data management for treatment group assignments
##
## Most of our data is stored in REDCap and read in directly in each script
## using the REDCap API. For obvious reasons, the treatment assignments were not
## included in the original database, and were instead stored and then provided
## by the VUMC investigational pharmacy. They are stored in the data management
## directory in the rawdata/ folder (which is .gitignored).
################################################################################

library(tidyverse)
library(readxl)

## -- Read in treatment assignments, stored in xlsx file -----------------------
trts <- readxl::read_excel(
  path = "rawdata/MIND-USAUnblinding.xlsx",
  range = cell_cols(1:3)
) %>%
  set_names("id", "rand_num", "trt")

## -- Make sure there's a 1:1 match between treatment, randomized patients -----
ptstatus_df <- readRDS("analysisdata/rds/ptstatus.rds")

## Does everyone in trts have a value of `randomized` = TRUE?
ptstatus_df %>%
  filter(id %in% trts$id & !randomized)
## 4 patients in randomization list recorded as disqualified
## MET-021: Listed as ICU discharge before delirium
## MON-060: Withdrew the day of consent
## OSU-062: Listed as ICU discharge before delirium
## VAN-175: Listed as no delirium within 5 days of consent

## Per team discussion 5/10/2018, it was determined that these patients were
## never intended to receive treatment per the study coordinators, and did not
## have outcomes tracked after disqualification. They did take up spots on the
## randomization list. We will *describe* this situation, but these patients
## will not be included in the ITT population, as they were never intended to
## receive treatment.
## Though the treatment blind had been broken at this point, the treatment
## groups had never been merged with outcomes data, and no one involved in the
## discussion had access to treatment information except for JT.

## Does everyone listed as `randomized` = TRUE have an assignment?
ptstatus_df %>%
  filter(randomized & !(id %in% trts$id))
## :tada:

## -- Final dataset: include ID, indicator for "on rand. list," treatment ------
## Treatment key from file: A=Haloperidol  B=Ziprasidone C=Placebo
trt_df <- trts %>%
  left_join(
    ptstatus_df %>% dplyr::select(id, randomized),
    by = "id"
  ) %>%
  mutate(
    rand_list = TRUE,
    trt = case_when(
      trt == "A" ~ "Haloperidol",
      trt == "B" ~ "Ziprasidone",
      trt == "C" ~ "Placebo",
      TRUE ~ as.character(NA)
    )
  ) %>%
  dplyr::select(id, rand_list, randomized, trt)

saveRDS(trt_df, file = "analysisdata/rds/trt.rds")
write_csv(trt_df, path = "analysisdata/csv/trt.csv")
