################################################################################
## For in-hospital RCT analysis:
##   Combine datasets, create final analysis data files specific to this
##   project, and send to RCT project folder.
################################################################################

source("fake_make.R")

rm(list = ls())

## -- Read in datasets from /analysisdata --------------------------------------
ptstatus_df      <- readRDS("analysisdata/rds/ptstatus.rds")
allptevents_df   <- readRDS("analysisdata/rds/allptevents.rds")
randptevents_df  <- readRDS("analysisdata/rds/randptevents.rds")
admission_df     <- readRDS("analysisdata/rds/admission.rds")
datestrack_df    <- readRDS("analysisdata/rds/datestrack.rds")
ptdrug_df        <- readRDS("analysisdata/rds/ptdrug.rds")
doses_df         <- readRDS("analysisdata/rds/doses.rds")
padasmts_df      <- readRDS("analysisdata/rds/padasmts.rds")
paddaily_df      <- readRDS("analysisdata/rds/paddaily.rds")
padsummary_df    <- readRDS("analysisdata/rds/padsummary.rds")
dailydata_df     <- readRDS("analysisdata/rds/dailydata.rds")
dailysummary_df  <- readRDS("analysisdata/rds/dailysummary.rds")
abcdecomp_df     <- readRDS("analysisdata/rds/abcdecomp.rds")
safetydaily_df   <- readRDS("analysisdata/rds/safetydaily.rds")
safetysummary_df <- readRDS("analysisdata/rds/safetysummary.rds")
torsades_df      <- readRDS("analysisdata/rds/torsadesaes.rds")

## -- Temporary: Create dataset of fake treatment groups -----------------------
## Final treatment groups will be added once data clean is finalized, database
## is "locked" & final treatment groups received from investigational pharmacist

## Create data frame with only IDs and treatment groups
rand_pts <- sort(unique(subset(ptstatus_df, randomized)$id))
trt_df <- data.frame(
  id = rand_pts,
  trt = sample(
    LETTERS[1:3],
    size = length(rand_pts),
    prob = rep(1/3, 3),
    replace = TRUE
  )
)

## -- Idea here: Combine datasets, then restrict to only variables we need -----
## -- for main RCT analysis ----------------------------------------------------

## Create data frame of all randomized patients + treatment assignment
rand_df <- ptstatus_df %>%
  filter(randomized) %>%
  left_join(trt_df, by = "id") %>%
  dplyr::select(id, trt)

## Vector of IDs for all randomized patients
rand_pts <- rand_df %>% pull(id)

## -- All consented patients ---------------------------------------------------
## Want to describe demographics, ICU admission characteristics for all patients
## consented and not excluded
ptsummary_all_df <- left_join(ptstatus_df, admission_df, by = "id") %>%
  left_join(
    dplyr::select(dailysummary_df, id, sofa_imp_consent, cv_sofa_consent),
    by = "id"
  ) %>%
  filter(consented & !excluded_ever) %>%
  dplyr::select(-(screened:exc_other))

## Combine longitudinal datasets: One record per day in the hospital
daily_all_df <- reduce(
  list(
    allptevents_df,
    paddaily_df,
    dailydata_df,
    safetydaily_df,
    abcdecomp_df
  ),
  left_join,
  by = c("id", "redcap_event_name")
)

## -- All randomized patients --------------------------------------------------
## One record per randomized patient with baseline, summary variables
ptsummary_df <- reduce(
  list(
    ptsummary_all_df %>% filter(randomized) %>% left_join(rand_df, by = "id"),
    datestrack_df,
    padsummary_df,
    dailysummary_df,
    safetysummary_df,
    ptdrug_df
  ),
  left_join,
  by = "id"
)

## One record per day after randomization, including post-discharge/death
daily_int_df <- reduce(
  list(
    randptevents_df %>% left_join(trt_df, by = "id"),
    paddaily_df,
    dailydata_df,
    safetydaily_df,
    abcdecomp_df
  ),
  left_join,
  by = c("id", "redcap_event_name")
)

## Add treatment group to Torsades info
torsades_df <- right_join(trt_df, torsades_df, by = "id")

## -- Save datasets to final RCT .Rdata file -----------------------------------
save(ptstatus_df, rand_df, rand_pts,
     ptsummary_all_df, daily_all_df,
     ptsummary_df, daily_int_df, doses_df, ptdrug_df, torsades_df,
     file = "../MINDUSARCT/analysisdata/rct.Rdata")
