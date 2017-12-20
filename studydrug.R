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

## -- Download data needed to determine whether patient ever got study drug ----
drug_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c(
    "id", "redcap_event_name",
    paste0("study_drug_given_", c(1:2, "3a")),
    paste0("dose_given_yes_", 1:3),
    "dose_y_other", "dose_y_other2", "dose_y_other3",
    paste0("dose_", 1:3),
    paste0("pre_dose_qtc_", 1:3),
    paste0("ecg_ordered_", 1:3),
    paste0("ecg_result_", 1:3),
    paste0("post_dose_qtc_", 1:3),
    paste0("no_dose_why_", 1:3),
    paste0("dose_held_reason_", 1:3),
    paste0("oversedation_", 1:3, "_actual"),
    paste0("decrease_other_", 1:3),
    paste0("permanent_stop_why_", 1:3),
    paste0("permanent_stop_other_", 1:3)
  )
) %>%
  ## Study drug could not be given on "randomziation" or "prior to d/c" events
  filter(!(redcap_event_name %in%
             c("randomization_arm_1", "prior_to_hospital_arm_1")))

## -- assertr checks for raw data ----------------------------------------------
drug_raw_checks <- drug_raw %>%
  assert(in_set(0:1), matches("^study\\_drug\\_given\\_")) %>%
  assert(within_bounds(0, 700), matches("^pre\\_dose\\_qtc")) %>%
  assert(in_set(c(1:10, 99)), matches("^dose\\_held\\_reason")) %>%
  assert(in_set(1:2), matches("^no\\_dose\\_why")) # %>%
## PEN-009 used old option; asked coordinators to fix
# assert(in_set(c(1, 3, 5:13, 99)), matches("^permanent\\_stop\\_why"))

## -- Create one data set with one record per **study drug dose** --------------
doses_df <- drug_raw %>%
  ## Rename 3rd study drug dose indicator, oversedation for consistency, others
  ## for sense
  rename(
    study_drug_given_3 = "study_drug_given_3a",
    dose_type_other_1 = "dose_y_other",
    dose_type_other_2 = "dose_y_other2",
    dose_type_other_3 = "dose_y_other3",
    held_other_exp_1 = "decrease_other_1",
    held_other_exp_2 = "decrease_other_2",
    held_other_exp_3 = "decrease_other_3",
    oversed_actual_1 = "oversedation_1_actual",
    oversed_actual_2 = "oversedation_2_actual",
    oversed_actual_3 = "oversedation_3_actual",
    permdc_other_exp_1 = "permanent_stop_other_1",
    permdc_other_exp_2 = "permanent_stop_other_2",
    permdc_other_exp_3 = "permanent_stop_other_3"
  ) %>%
  ## Reshape data to one row per dose
  gather(key = drug_var, value = drug_val, -id, -redcap_event_name) %>%
  separate(drug_var, into = c("drug_var", "dose_num"), sep = "\\_(?=[0-9]$)") %>%
  spread(key = drug_var, value = drug_val) %>%
  ## Renaming for length/sense
  rename(dose_amt = "dose",
         dose_type = "dose_given_yes",
         dose_permdc_reason = "permanent_stop_why") %>%
  mutate(
    study_drug_given = factor(
      study_drug_given,
      levels = get_levels_ih("study_drug_given_1"),
      labels = names(get_levels_ih("study_drug_given_1"))
    ),
    dose_amt = factor(
      dose_amt,
      levels = get_levels_ih("dose_1"),
      labels = names(get_levels_ih("dose_1"))
    ),
    dose_type = factor(
      dose_type,
      levels = get_levels_ih("dose_given_yes_1"),
      labels = names(get_levels_ih("dose_given_yes_1"))
    ),
    ecg_ordered = factor(
      ecg_ordered,
      levels = get_levels_ih("ecg_ordered_1"),
      labels = names(get_levels_ih("ecg_ordered_1"))
    ),
    dose_held_reason = factor(
      dose_held_reason,
      levels = get_levels_ih("dose_held_reason_1"),
      labels = names(get_levels_ih("dose_held_reason_1"))
    ),
    no_dose_why = factor(
      no_dose_why,
      levels = get_levels_ih("no_dose_why_1"),
      labels = names(get_levels_ih("no_dose_why_1"))
    ),
    oversed_actual = factor(
      oversed_actual,
      levels = get_levels_ih("oversedation_1_actual"),
      labels = names(get_levels_ih("oversedation_1_actual"))
    ),
    dose_permdc_reason = factor(
      dose_permdc_reason,
      levels = get_levels_ih("permanent_stop_why_1"),
      labels = names(get_levels_ih("permanent_stop_why_1"))
    )
  ) %>%
  ## Only want doses which were entered in the database
  filter(!is.na(study_drug_given)) %>%
  mutate(
    ## Numeric versions of QTc, dose amount; dose units are mL, don't need that
    pre_dose_qtc = as.numeric(pre_dose_qtc),
    post_dose_qtc = as.numeric(post_dose_qtc),
    dose_amt = as.numeric(gsub(" mL", "", dose_amt)),
    ## -- Create T/F indicators for each dose ----------------------------------
    ## Was drug given?
    drug_given = study_drug_given == "Yes",
    ## Was drug held? Note: NOT considered "held" if held d/t delirium
    ## resolution, off floor, discharge (team meeting Dec 4 2017)
    drug_held = case_when(
      study_drug_given == "Yes" ~ FALSE,
      study_drug_given == "No" &
        no_dose_why == "Held/missed" &
        !is.na(dose_held_reason) &
        !(dose_held_reason %in%
            c("Delirium resolving/resolved",
              "ICU Discharge",
              "Patient off floor")) ~ TRUE,
      TRUE ~ FALSE
    ),
    ## Was drug permanently discontinued? Note: NOT considered "discontinued" if
    ## not given d/t death, discharge, study withdrawal, comfort measures/hospice
    drug_permdc = case_when(
      study_drug_given == "Yes" ~ FALSE,
      study_drug_given == "No" &
        no_dose_why == "Permanently discontinued" &
        !is.na(dose_permdc_reason) &
        !(dose_permdc_reason %in%
            c("Hospital Discharge", "Study withdraw", "Patient died",
              "Patient's status is comfort measures only/hospice")) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    ## If drug held or discontinued, indicators for why
    held_qtc = drug_held & dose_held_reason == "QTc Prolongation",
    held_oversed = drug_held & dose_held_reason == "Oversedation",
    held_eps = drug_held & dose_held_reason == "Extrapyramidal symptoms",
    held_dystonia = drug_held & dose_held_reason == "Dystonia",
    held_ae = drug_held & dose_held_reason == "Adverse event",
    held_refuseteam = drug_held & dose_held_reason == "Managing team refused",
    held_refuseptfam = drug_held & dose_held_reason == "Patient/family refused",
    held_other = drug_held & dose_held_reason == "Other",
    
    permdc_nms = drug_permdc &
      dose_permdc_reason == "Neuroleptic malignant syndrome",
    permdc_react = drug_permdc &
      dose_permdc_reason == "Drug Reaction with Eosinophilia and Systemic Symptoms",
    permdc_torsades = drug_permdc & dose_permdc_reason == "Torsades de pointes",
    permdc_coma = drug_permdc &
      dose_permdc_reason == "Coma due to a structural brain disease",
    permdc_ae = drug_permdc & dose_permdc_reason == "Adverse event",
    permdc_refuseptfam = drug_permdc &
      dose_permdc_reason == "Surrogate/patient refuse further study drug administration",
    permdc_refuseteam = drug_permdc &
      dose_permdc_reason == "Physician refused further study drug administration",
    permdc_other = drug_permdc & dose_permdc_reason == "Other"
  ) %>%
  ## Reorder columns
  select(
    id, redcap_event_name, dose_num, drug_given,
    ## Dose, QTc/ECG info
    dose_amt, dose_type, dose_type_other, pre_dose_qtc, ecg_ordered, ecg_result,
    post_dose_qtc,
    ## Hold info
    drug_held, dose_held_reason, matches("^held\\_[a-z]+$"), held_other_exp,
    oversed_actual,
    ## Discontinuation info
    drug_permdc, dose_permdc_reason, matches("^permdc\\_[a-z]+$"),
    permdc_other_exp
  )


## -- Create one data set with one record per **patient**, summarizing ---------
## -- doses, holds, discontinuation --------------------------------------------
## How many *days* did patients get study drug?
ptdays_df <- doses_df %>%
  group_by(id, redcap_event_name) %>%
  summarise(
    drug_given_day = sum_na(drug_given) > 0,
    total_drug_daily = sum_na(dose_amt)
  ) %>%
  mutate(
    total_drug_daily = ifelse(total_drug_daily == 0, NA, total_drug_daily)
  ) %>%
  ungroup %>%
  ## How many days did the patient get drug? What was average total daily dose?
  group_by(id) %>%
  summarise(
    num_drug_days = sum_na(drug_given_day),
    mean_drug_daily = ifelse(num_drug_days == 0, NA, mean_na(total_drug_daily))
  ) %>%
  ungroup

## Summarize *doses*
ptdoses_df <- doses_df %>%
  ## Summarize drug doses per patient
  group_by(id) %>%
  summarise(
    ever_studydrug = sum_na(drug_given) > 0,
    num_drug_doses = sum_na(drug_given),
    
    ## Describe drug dose amounts
    max_drug_dose = ifelse(num_drug_doses > 0, max_na(dose_amt), NA),
    min_drug_dose = ifelse(num_drug_doses > 0, min_na(dose_amt), NA),
    q25_drug_dose = ifelse(num_drug_doses > 0, q25(dose_amt), NA),
    q50_drug_dose = ifelse(num_drug_doses > 0, q50(dose_amt), NA),
    q75_drug_dose = ifelse(num_drug_doses > 0, q75(dose_amt), NA),
    mean_drug_dose = ifelse(num_drug_doses > 0, mean_na(dose_amt), NA),
    sd_drug_dose = ifelse(num_drug_doses > 0, sd_na(dose_amt), NA),
    
    ## Indicator for whether drug was ever temporarily held
    ever_drug_held = sum_na(drug_held) > 0,
    
    ## How many times was drug temporarily held? How often for each reason?
    times_drug_held = sum_na(drug_held),
    times_held_qtc = sum_na(held_qtc),
    times_held_oversed = sum_na(held_oversed),
    times_held_eps = sum_na(held_eps),
    times_held_dystonia = sum_na(held_dystonia),
    times_held_ae = sum_na(held_ae),
    times_held_refuseteam = sum_na(held_refuseteam),
    times_held_refuseptfam = sum_na(held_refuseptfam),
    times_held_other = sum_na(held_other),
    
    ## Indicator for whether drug ever permanently discontinued
    ever_drug_permdc = sum_na(drug_permdc) > 0
  ) %>%
  ## Create indicators for whether patient ever had drug held for each reason
  mutate_at(vars(matches("^times\\_held")), funs(ever = . > 0)) %>%
  rename_at(vars(matches("^times\\_held\\_.*\\_ever$")),
            funs(gsub("\\_ever$", "", gsub("times", "ever", .))))

## Find reason for 1st discontinuation for each patient
first_dc <- doses_df %>%
  filter(drug_permdc) %>%
  left_join(select(ih_events, unique_event_name, event_num),
            by = c("redcap_event_name" = "unique_event_name")) %>%
  arrange(id, event_num) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup %>%
  select(id, dose_permdc_reason:permdc_other_exp)

## Combine days, doses, and discontinuation information; one record per patient
ptdrug_df <- ptdays_df %>%
  left_join(ptdoses_df, by = "id") %>%
  left_join(first_dc, by = "id") %>%
  mutate_at(
    vars(matches("^permdc\\_[a-z]+$")), funs(ifelse(is.na(.), FALSE, .))
  ) %>%
  select(id, ever_studydrug, num_drug_days, mean_drug_daily,
         num_drug_doses:times_drug_held, ever_held_qtc, times_held_qtc,
         ever_held_oversed, times_held_oversed, ever_held_eps, times_held_eps,
         ever_held_dystonia, times_held_dystonia, ever_held_ae, times_held_ae,
         ever_held_refuseteam, times_held_refuseteam, ever_held_refuseptfam,
         times_held_refuseptfam, ever_held_other, times_held_other,
         ever_drug_permdc, dose_permdc_reason, everything())

## -- Data checks: Review this text file each time -----------------------------
## Write notes for "other" reasons for hold and permanent d/c to CSV for
## coordinators to check
write_csv(
  subset(doses_df,
         held_other,
         select = c(id, redcap_event_name, dose_num, held_other_exp)),
  path = "sd_hold_other.csv"
)

write_csv(
  subset(doses_df,
         permdc_other,
         select = c(id, redcap_event_name, dose_num, permdc_other_exp)),
  path = "sd_permdc_other.csv"
)

sink(file = "studydrug_checks.txt")

## Each held dose should have exactly 1 reason for hold; each permanently
## discontinued dose should have exactly 1 reason for d/c
doses_df_ck <- doses_df
doses_df_ck$rsns_held <-
  ifelse(!doses_df_ck$drug_held, NA,
         rowSums(doses_df_ck[,grep("^held\\_[a-z]+$", names(doses_df_ck))],
                 na.rm = TRUE)
  )
doses_df_ck$rsns_permdc <-
  ifelse(!doses_df_ck$drug_permdc, NA,
         rowSums(doses_df_ck[,grep("^permdc\\_[a-z]+$", names(doses_df_ck))],
                 na.rm = TRUE)
  )

cat("Patients with study drug held should have exactly one reason for hold:\n\n")
print(with(doses_df_ck, table(drug_held, rsns_held, useNA = "ifany")))

cat("\n\n\nPatients with study drug permanently discontinued should have exactly one reason for discontinuation:\n\n")
print(with(doses_df_ck, table(drug_permdc, rsns_permdc, useNA = "ifany")))


sink()

## -- Save both files to rds, csv ----------------------------------------------
saveRDS(doses_df, file = "analysisdata/rds/doses.rds")
write_csv(doses_df, path = "analysisdata/csv/doses.csv")

saveRDS(ptdrug_df, file = "analysisdata/rds/ptdrug.rds")
write_csv(ptdrug_df, path = "analysisdata/csv/ptdrug.csv")
