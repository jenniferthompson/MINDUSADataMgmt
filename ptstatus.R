################################################################################
## Data management for data frame of patient status at various time points,
## along with reasons for exclusion, disqualification and withdrawal
################################################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(assertr)

## Source data management functions
source("data_functions.R")

## Create partial-ed version of sum()
sum_na <- partial(sum, na.rm = TRUE)

## -- Import data dictionaries from REDCap -------------------------------------
## All tokens are stored in .Renviron
exc_dd <- get_datadict("MINDUSA_EXC_TOKEN")
ih_dd <- get_datadict("MINDUSA_IH_TOKEN")
ih_events <- get_events("MINDUSA_IH_TOKEN") %>% mutate(event_num = 1:nrow(.))
  ## Add event_num to help with sorting events later
ih_mapping <- get_event_mapping("MINDUSA_IH_TOKEN")

get_levels_ih <-
  function(varname){ get_factor_levels(ddict = ih_dd, varname = varname) }

## -- Import, check exclusion log data -----------------------------------------

## Get factor names/levels for study site
exc_site_levels <- get_factor_levels(ddict = exc_dd, varname = "exc_site")

## Read in all variables as numeric; only one factor that we need, and 0/1 for
## exclusion checkboxes is helpful
exc_raw <- import_df(
  "MINDUSA_EXC_TOKEN",
  id_field = "exc_id",
  forms = "exclusion_log",
  export_labels = "none"
)

## -- Download variables needed to determine patient status at enrollment, -----
## -- disqualification, randomization, discharge -------------------------------

## From enrollment qualification form: All variables
## From prehospital form: Only IQCODE (patient should be excluded if >= 4.5)
iqcode_vars <- sprintf("iqcode_%s_ph", 1:16)

## From dates tracking form:
## Dates/times for enrollment, randomization, disqualification, death,
## withdrawal, ICU/hospital discharge; whether patient was randomized, died,
## withdrew, was discharged
datestrack_vars <- c(
  "enroll_time", "randomized_yn", "randomization_time", "disqualification_time",
  "death", "death_time", "studywd", "studywd_time", "hospdis", "hospdis_time"
)

inhosp_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  forms = c("enrollment_qualification_form"),
  fields = c(iqcode_vars, datestrack_vars),
  events = "enrollment__day_0_arm_1"
) %>%
  select(-redcap_event_name, -enrollment_qualification_form_complete) %>%
  rename(
    ## Inclusions at **enrollment**
    enroll_mv = "organ_failures_present_1",
    enroll_nippv = "organ_failures_present_2",
    enroll_shock = "organ_failures_present_3",
    ## Exclusions (should match exc_df)
    exc_1_resolving = "rapidly_resolving",
    exc_2a_pregnancy = "pregnancy",
    exc_2b_breastfeed = "breastfeed",
    exc_3_dementia = "dementia_neurodz",
    exc_4_deficit = "neuro_event",
    exc_5_torsadesqtc = "torsades",
    exc_6_maintmeds = "maint_antipsy",
    exc_7_nmsallergy = "nms_allergy",
    exc_8_death24 = "moribund",
    exc_9a_refusal_md = "attending_refuse",
    exc_9b_refusal_ptsurr = "ptsurr_refuse",
    exc_9c_unable_nosurr = "nosurr_avail",
    exc_9d_72h_noscreen = "exc_scrn_period",
    exc_9e_72h_nosurr = "nosurr_noncoma",
    exc_9f_120h_coma = "nosurr_coma",
    exc_10_blind_lang = "blind_deaf",
    exc_11_prison = "incarcerated",
    exc_12_coenroll = "coenrollment",
    exc_99_other = "enr_qual_other",
    exc_other = "enr_qual_other_rsn"
  )
  
randqual_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  fields = c(
    "id", "organ_failure_present", "randomized_no_reason", "pregnancy_random",
    "breastfeed_random", "neuro_event_random", "qtc_rand",
    "maint_antipsy_random", "nms_allergy_random", "moribund_random",
    "blind_deaf_random", "incarcerated_random", "coenrollment_random",
    "rand_qual_other", "rand_qual_other_rsn"
  ),
  events = "randomization_arm_1"
) %>%
  rename(rand_mv = "organ_failure_present_1",
         rand_nippv = "organ_failure_present_2",
         rand_shock = "organ_failure_present_3",
         rand_noorgfailure = "organ_failure_present_0",
         exc_2a_pregnancy = "pregnancy_random",
         exc_2b_breastfeed = "breastfeed_random",
         exc_4_deficit = "neuro_event_random",
         exc_5_torsadesqtc = "qtc_rand",
         exc_6_maintmeds = "maint_antipsy_random",
         exc_7_nmsallergy = "nms_allergy_random",
         exc_8_death24 = "moribund_random",
         exc_10_blind_lang = "blind_deaf_random",
         exc_11_prison = "incarcerated_random",
         exc_12_coenroll = "coenrollment_random",
         exc_99_other = "rand_qual_other",
         exc_other = "rand_qual_other_rsn"
  )

## -- assertr checks for raw data ----------------------------------------------
## - All variables should always be present except for exc_other, exc_notes
## - All exclusion variables should be non-missing, numeric, 0/1
## - Exclusion date should be in YYYY-MM-DD format
exc_raw_checks <- exc_raw %>%
  # assert(not_na, -exc_other, -exc_notes) %>%
  ## We're missing some sites and exclusion numbers, but everyone has an ID,
  ## so we can figure those out
  ## We're also missing some dates; these go along with patients who have no
  ## exclusions entered (checked later), so hopefully will be addressed
  ## Will comment out this assertion for now, but come back to it after
  ## "no exclusion" patients are handled
  assert(in_set(0, 1), matches("^exc\\_rsn\\_[0-9]+$"))

randqual_raw_checks <- randqual_raw %>%
  assert(not_na, rand_mv:rand_noorgfailure) %>%
  assert(in_set(0:2), "exc_2a_pregnancy") %>%
  assert(in_set(0, 1), "exc_2b_breastfeed", matches("^exc\\_[1, 4-9]"))

inhosp_raw_checks <- inhosp_raw %>%
  ## Remove test patients
  ## ***** YAL-060 has lots of issues caught in data clean - recheck this!
  filter(!str_detect(toupper(id), "TEST") & !(id == "YAL-060")) %>%
  ## Exclusions 9cdef are protocol-specific and will be missing on some;
  ##  don't include here
  assert(
    not_na,
    id:exc_9b_refusal_ptsurr, exc_10_blind_lang:exc_99_other,
    enroll_time:randomized_yn
  ) %>%
  # 48 patients missing IQCODE variables
  assert(in_set(1:10), protocol) %>%
  assert(in_set(0:1), adult_patient:exc_99_other) %>%
  assert(in_set(1:6), matches("^iqcode\\_[0-9]+\\_ph$"))

## -- Data management for exclusions -------------------------------------------
## Exclusions in exclusion log have partners in in-hospital database. Rename
## variables so these a) are more meaningful and b) match up.
## Also: remove test pts; force IDs to have "-" as separator and be in all caps

## Exclusion log
exc_df <- exc_raw %>%
  mutate(
    ## Make exclusion site a factor, using levels from data dictionary
    study_site = factor(exc_site,
                        levels = exc_site_levels,
                        labels = names(exc_site_levels)),
    ## Format exclusion date
    exc_date = ymd(exc_date),
    ## Some exclusion IDs have separators other than "-"; replace them and
    ## force ID to use all caps
    id = str_replace_all(exc_id, c("[=_]" = "-", "\\." = "")) %>% toupper
  ) %>%
  select(-exc_id, -exc_number, -exc_site, -exclusion_log_complete) %>%
  rename(
    exc_1_resolving = "exc_rsn_1",
    exc_2a_pregnancy = "exc_rsn_2",
    exc_2b_breastfeed = "exc_rsn_3",
    exc_3_dementia = "exc_rsn_4",
    exc_4_deficit = "exc_rsn_5",
    exc_5_torsadesqtc = "exc_rsn_6",
    exc_6_maintmeds = "exc_rsn_7",
    exc_7_nmsallergy = "exc_rsn_8",
    exc_8_death24 = "exc_rsn_9",
    exc_9a_refusal_md = "exc_rsn_10",
    exc_9b_refusal_ptsurr = "exc_rsn_11",
    exc_9c_unable_nosurr = "exc_rsn_12",
    exc_9d_72h_noscreen = "exc_rsn_16",
    exc_9e_72h_nosurr = "exc_rsn_17",
    exc_9f_120h_coma = "exc_rsn_18",
    exc_10_blind_lang = "exc_rsn_13",
    exc_11_prison = "exc_rsn_14",
    exc_12_coenroll = "exc_rsn_15",
    exc_99_other = "exc_rsn_99"
  )

## Combine exclusions from enrollment qualification + randomization
##  qualification forms
## Plan: create two rows per patient (one from each form) with all exclusions
##       available at both time points; create total by summing two rows, turn
##       into yes/no (1/0)
exclusion_vars <- c(
  "exc_1_resolving", "exc_2a_pregnancy", "exc_2b_breastfeed", "exc_3_dementia",
  "exc_4_deficit", "exc_5_torsadesqtc", "exc_6_maintmeds", "exc_7_nmsallergy",
  "exc_8_death24", "exc_9a_refusal_md", "exc_9b_refusal_ptsurr",
  "exc_9c_unable_nosurr", "exc_9d_72h_noscreen", "exc_9e_72h_nosurr",
  "exc_9f_120h_coma", "exc_10_blind_lang", "exc_11_prison", "exc_12_coenroll",
  "exc_99_other"
)

inhosp_exc <- bind_rows(
  select(inhosp_raw, id, one_of(exclusion_vars)),
  select(randqual_raw, id, one_of(exclusion_vars))
) %>%
  ## In randomization qualification form, pregnancy exclusion could have
  ## value 2 for N/A; change this to actual NA
  mutate_at("exc_2a_pregnancy", ~ ifelse(. == 2, NA, .)) %>%
  ## Summarize exclusions by patient: Get sum of times patient met each
  ## exclusion then turn into 0/1 (a couple of patients have exclusions
  ## documented in both forms, so total = 2; NA equivalent to 0)
  group_by(id) %>%
  summarize_at(vars(exclusion_vars), sum_na) %>%
  mutate_at(vars(exclusion_vars), ~ as.numeric(. > 0)) %>%
  ungroup

inhosp_df <- inhosp_raw %>%
  ## Remove exclusions from enrollment qualification forms; replace these with
  ## *combined* exclusions from EQ + RQ forms
  select(-one_of(exclusion_vars)) %>%
  left_join(inhosp_exc, by = "id") %>%
  ## Add organ failure info and explanation for "other" exclusion from
  ## randomization qualification form (no one excluded on enrollment
  ## qualification form was excluded for "other"; no need to deal with that)
  left_join(
    randqual_raw %>%
      select(id, starts_with("rand_"), randomized_no_reason, exc_other),
    by = "id"
  ) %>%
  mutate(
    ## Make study site, protocol, reason for disqualification factors
    ##   using levels from data dictionary
    study_site = factor(study_site,
                        levels = get_levels_ih("study_site"),
                        labels = names(get_levels_ih("study_site"))),
    protocol = factor(
      protocol,
      levels = get_levels_ih("protocol"),
      labels = names(get_levels_ih("protocol"))
    ),
    randomized_no_reason = factor(
      randomized_no_reason,
      levels = get_levels_ih("randomized_no_reason"),
      ## Labels in database are long; use custom ones
      labels = c("No delirium within 5 days",
                 "Exclusion identified",
                 "ICU discharge before delirium",
                 "Died/withdrew before delirium")
    ),
    
    ## Force IDs to use "-" as separator, be in all caps
    id = str_replace_all(id, c("[=_]" = "-", "\\." = "")) %>% toupper,
    
    ## Calculate IQCODE score
    iqcode_score = rowMeans(.[, paste("iqcode", 1:16, "ph", sep = "_")])
  ) %>%
  select(-patient_id, -matches("^iqcode\\_[0-9]+\\_ph$"))

## -- Combine exclusion, in-hospital data for one master dataset ---------------
ptstatus_df <- bind_rows(exc_df, inhosp_df) %>%
  ## Filter out test patients
  filter(!str_detect(id, "TEST")) %>%
  select(id, study_site, protocol, exc_date:exc_notes,
         inclusion_met_date:enroll_shock, iqcode_score,
         everything()) %>%
  
  ## Format dates/times
  mutate_at(c("inclusion_met_date", "enroll_time", "randomization_time",
              "disqualification_time", "death_time", "hospdis_time"),
            ymd_hm) %>%
  mutate_at(c("exc_date", "studywd_time"), ymd) %>%
  rename(studywd_date = "studywd_time") %>% ## no time included
  mutate(enroll_date = as_date(enroll_time),
         randomization_date = as_date(randomization_time),
         disqualification_date = as_date(disqualification_time),
         hospdis_date = as_date(hospdis_time),
         death_date = as_date(death_time)) %>%
  
  ## Add exclusions for high IQCODE, protocol violation; combine related exclusions
  mutate(
    exc_13_iqcode = as.numeric(
      (!is.na(iqcode_score) & iqcode_score >= 4.5) & is.na(randomization_time)
    ),
    ## Currently only one patient meets this exclusion; documented in REDCap
    ##   post-it + NTF. Will be excluded d/t screening failure.
    exc_14_screen = as.numeric(id == "VAN-245"),
    ## Categories per TGirard, 11-8-2017: 2a+2b; 3+4; 9c+9e+9f
    exc_2_pregbf = as.numeric(exc_2a_pregnancy | exc_2b_breastfeed),
    exc_34_neuro = as.numeric(exc_3_dementia | exc_4_deficit),
    exc_9cef_time = as.numeric(
      exc_9c_unable_nosurr | exc_9e_72h_nosurr | exc_9f_120h_coma
    )
  )

## -- Create indicators for status at specific time points ---------------------
## Definitions:
##
## Screening + enrollment:
## Screened: Screened within 72 hours of meeting inclusion criteria. Leaves
##           out exclusion 1 (rapidly resolving organ failure) and 9d (72h
##           eligibility period exceeded before screening)
## Excluded, ever: Screened, and met >=1 exclusion criteria (including IQCODE
##                 >= 4.5) without being randomized (subset of screened) **** not currently true - YAL-060
## Excluded immediately: Screened, and met >=1 exclusion before being consented
##                       (no enrollment time)
## Excluded later: Screened, consented, and was found to meet >=1 exclusion
##                 after consent (eg, high IQCODE, or discovery of
##                 maintenance antipsychotic use) without being randomized
##   excluded immediately + excluded later = excluded ever
## Approached: Screened, not immediately excluded
##             (excluded imm. + approached = screened; **** not currently true - MON-071)
## Refused: Screened, approached, & pt/surrogate approached but refused consent
##          (subset of approached)
## Consented: Screened/approached, no refusal, enrollment date recorded
##            (refused + consent = approached) **** not currently true d/t 44 patients with no exclusion; currently approached, but neither consented nor randomized
##
## Randomization + study drug:
## Disqualified: Consented, but not randomized + disqualification date
## Randomized: Consented, randomized + randomization date
##             (disqualified + randomized = consented)
## Ever excluded + refused + randomized + disqualified = screened
## **** not currently true; recheck this after those no-exclusion patients are handled ****
## Received study drug: Randomized and received at least one dose of study drug
## Did not receive study drug: Randomized, but never received a dose of s. drug
##   (subset of randomized) **** not currently true; MON-071
##
## End of in-hospital phase:
## Eligible for followup: Randomized + discharged alive from hospital, with no
##                        withdrawal date
## Died in hospital: Randomized + death date prior to hospital discharge date,
##                   or no hospital discharge date recorded
## Withdrew in hospital: Randomized + withdrawal date prior to hospital
##                       discharge date, or no hospital discharge date recorded
## Elig for f/u + died in hosp. + w/d [and did not die] in hospital = randomized

## Prep: Create list of all "official" exclusions. Leaves out:
## - 1: rapidly resolving organ failure (patient does not meet incl. criteria)
## - 9d: no screening within 72h of meeting inclusion
## - 9b: patient/surrogate refusal
## 1, 9d don't meet screening def of "met inc criteria + screened within 72h"
## 9b is not an exclusion in the CONSORT sense; patient met all inclusion
## criteria, no other exclusions, and was approached
not_exclusions <- c("1", "9b", "9d")

## Variables listed as exclusion options in database
possible_exclusions <- str_subset(names(ptstatus_df), "^exc\\_[0-9]+")

## Remove variables which are not considered official exclusion criteria
exclusion_rsns <-
  possible_exclusions[
    !(map_chr(str_split(possible_exclusions, fixed("_")), ~ .[2]) %in%
        not_exclusions)
    ]

## Create indicators
ptstatus_df <- ptstatus_df %>%
  mutate(
    ## Screened: met inclusion criteria & screened within 72h, *or* had
    ##   randomization time
    ## (1 patient was consented 7h past 72h window and was eventually randomized)
    screened = ifelse(
      is.na(exc_1_resolving) & is.na(exc_9d_72h_noscreen), NA,
      (!((!is.na(exc_1_resolving) & exc_1_resolving) |
           (!is.na(exc_9d_72h_noscreen) & exc_9d_72h_noscreen))) |
        !is.na(enroll_time)
    ),
    ## Ever excluded:
    ##  Screened + met any exc criteria other than refusal + not randomized
    excluded_ever = ifelse(
      rowSums(!is.na(.[, exclusion_rsns])) == 0, NA,
      screened &
        rowSums(.[, exclusion_rsns], na.rm = TRUE) > 0 &
        is.na(randomization_time)
    ),
    ## Excluded immediately:
    ##   Screened + met any exclusion but refusal + was not consented
    excluded_imm = ifelse(
      rowSums(!is.na(.[, exclusion_rsns])) == 0, NA,
      screened &
        rowSums(.[, exclusion_rsns], na.rm = TRUE) > 0 &
        is.na(enroll_time)
    ),
    ## Excluded later: Consented, but met >=1 exclusion criteria (including
    ## IQCODE) without being randomized
    excluded_later = ifelse(
      rowSums(!is.na(.[, exclusion_rsns])) == 0, NA,
      screened & !is.na(enroll_time) & is.na(randomization_time) &
        rowSums(.[, exclusion_rsns], na.rm = TRUE) > 0
    ),
    ## Approached: screened and not immediately excluded
    approached = ifelse(
      is.na(exc_9b_refusal_ptsurr), NA, screened & !excluded_imm
    ),
    ## Refused: Screened, not immediately excluded, approached, refusal
    refused = screened & !excluded_imm & approached & exc_9b_refusal_ptsurr,
    ## Consented: Screened, approached, did not refuse, enrollment time rec.
    consented = screened & approached & !refused & !is.na(enroll_time),
    
    ## Randomization vs disqualification
    ## Disqualified: screened, approached, did not refuse, consented,
    ##   not randomized + DQ time entered
    disqualified = screened & approached & !refused & !excluded_ever &
      consented & !randomized_yn & !is.na(disqualification_time),
    ## Randomized: screened, approached, did not refuse, consented,
    ##   randomized + time recorded
    randomized = screened & approached & !refused & consented &
      randomized_yn & !is.na(randomization_time),
    
    ## End of in-hospital phase
    ## Died in hospital: randomized + died + died prior to hospital discharge
    ## Followup deaths are recorded in this database, so need to differentiate
    died_inhosp = randomized &
      death == 1 & !is.na(death_time) &
      (is.na(hospdis_time) | death_time <= hospdis_time),
    
    ## Withdrew in hospital: randomized + withdrew + w/d date prior to discharge
    wd_inhosp = randomized &
      studywd == 1 & !is.na(studywd_date) &
      (is.na(hospdis_date) | studywd_date <= hospdis_date),
    
    ## Patients can both die and withdraw in-hospital
    
    ## Eligible for followup: randomized, survived hospital stay, no withdrawal
    elig_fu = randomized & !died_inhosp & !wd_inhosp & hospdis == 1,
    
    ## Overall status at end of in-hospital period for randomized patients
    ##  If patient both died and withdrew in hospital, considered deceased (we
    ##  know what happened to them)
    dc_status = factor(
      ifelse(!randomized, NA,
      ifelse(elig_fu, 1,
      ifelse(died_inhosp, 2, 3))),
      levels = 1:3,
      labels = c("Eligible for follow-up",
                 "Died in hospital",
                 "Withdrew in hospital")
    )
    
  ) %>%
  
  ## Change exclusion indicators to logicals, not 0/1
  mutate_at(
    vars(
      adult_patient:enroll_shock, ## qualification criteria
      matches("^rand\\_"),        ## organ failure(s) at randomization
      matches("^exc\\_[0-9]")     ## exclusions
    ),
    as.logical
  )

## -- Data checks --------------------------------------------------------------

## All results are written to a text file, ptstatus_checks.txt
sink("ptstatus_checks.txt")

## All randomized patients should have status of died in-hospital, withdrew
##  in-hospital, or eligible for followup; some will both withdraw and die in
##  the hospital, but we want to check anyone else with >1 of these statuses.
conflicting_dcstatus <- ptstatus_df %>%
  mutate(dc_statuses = died_inhosp + wd_inhosp + elig_fu,
         issue_flag = dc_statuses > 1 & !(died_inhosp & wd_inhosp)) %>%
  filter(randomized & issue_flag)

print_datachecks(
  "Patients with >1 of died in hospital, w/d in hospital, & eligible for followup:",
  conflicting_dcstatus
)

## -- Summarize exclusions + statuses among those officially screened ----------
test_df <- ptstatus_df %>%
  mutate(
    ## Calculate total number of exclusions
    exc_number = rowSums(.[, exclusion_rsns]),
    exc_none = screened & !excluded_ever & !refused & !consented
  )

# ## No one should have 0 exclusions checked. Write a subset of these IDs to CSV
# write_csv(
#   subset(test_df,
#          exc_none,
#          select = c(id, study_site, exc_date, exc_other, exc_notes)),
#   path = "no_exclusions.csv"
# )

# ## List all exclusions due to "other" in randomization qualification form
# write_csv(
#   x = subset(randqual_raw, exc_99_other == 1, select = c(id, exc_other)),
#   path = "randqual_otherexc.csv"
# )

## Summarize % of exclusions
main_exclusions <- c(
  paste(
    "exc",
    c("1_resolving", "2_pregbf", "34_neuro", "5_torsadesqtc", "6_maintmeds",
      "7_nmsallergy", "8_death24", "9a_refusal_md", # "9b_refusal_ptsurr",
      "9d_72h_noscreen", "9cef_time", "10_blind_lang", "11_prison",
      "12_coenroll", "13_iqcode", "14_screen", "99_other", "none"),
    sep = "_"
  ),
  "screened", "excluded_ever", "excluded_imm", "approached", "refused",
  "consented", "excluded_later", "disqualified", "randomized",
  "died_inhosp", "wd_inhosp", "elig_fu"
)

summarize_exc <- test_df %>%
  filter(screened) %>%
  select(one_of(main_exclusions)) %>%
  gather(key = exclusion, value = yn) %>%
  group_by(exclusion) %>%
  summarise_all(funs(pts = sum, pct = mean), na.rm = TRUE) %>%
  mutate(pct = pct * 100,
         order = ifelse(exclusion == "screened", 1,
                 ifelse(exclusion == "excluded_ever", 2,
                 ifelse(exclusion == "excluded_imm", 3,
                 ifelse(exclusion == "approached", 5,
                 ifelse(exclusion == "refused", 6,
                 ifelse(exclusion == "consented", 7,
                 ifelse(exclusion == "excluded_later", 8,
                 ifelse(exclusion == "disqualified", 9,
                 ifelse(exclusion == "randomized", 10,
                 ifelse(exclusion == "ever_studydrug", 11,
                 ifelse(exclusion == "died_inhosp", 12,
                 ifelse(exclusion == "wd_inhosp", 13,
                 ifelse(exclusion == "elig_fu", 14, 4))))))))))))),
         exclusion =
           ifelse(exclusion == "exc_none", "exc_none [CHECK THESE]", exclusion)
  ) %>%
  arrange(order, desc(pct)) %>%
  select(-order)

print_datachecks("Summary of exclusions and patient statuses:", summarize_exc)

## Data weirdness thus far:
## - YAL-060: missing screened indicator + others d/t incomplete enroll. qual form
## - MON-071: missing approached ind. + others d/t missing exclusion 9b

## -- Check totals of statuses -------------------------------------------------
cat(
  glue(
    "Screening and Exclusions\n",
    "Total patients screened: {sum_na(ptstatus_df$screened)}",
    "  Patients excluded immediately: {sum_na(ptstatus_df$excluded_imm)}",
    "  Patients approached: {sum_na(ptstatus_df$approached)}",
    "  Should equal line 1: {with(ptstatus_df, sum_na(excluded_imm) + sum_na(approached))}",
    .sep = "\n"
  )
)

cat(
  glue(
    "\n\n\nExclusion Details\n",
    "Total patients excluded: {sum_na(ptstatus_df$excluded_ever)}",
    "  Excluded at screening: {sum_na(ptstatus_df$excluded_imm)}",
    "  Excluded after consent: {sum_na(ptstatus_df$excluded_later)}",
    "  Should equal line 1: {with(ptstatus_df, sum_na(excluded_imm) + sum_na(excluded_later))}",
    .sep = "\n"
  )
)

cat(
  glue(
    "\n\n\nApproaches, Refusals & Consent\n",
    "Total patients approached: {sum_na(ptstatus_df$approached)}",
    "  Patient/surrogate refusal: {sum_na(ptstatus_df$refused)}",
    "  Consented: {sum_na(ptstatus_df$consented)}",
    "  Should equal line 1: {with(ptstatus_df, sum_na(consented) + sum_na(refused))}",
    .sep = "\n"
  )
)

cat(
  glue(
    "\n\n\nRandomization & Disqualification\n",
    "Total patients consented: {sum_na(ptstatus_df$consented)}",
    "  Excluded after consent: {sum_na(ptstatus_df$excluded_later)}",
    "  Disqualified: {sum_na(ptstatus_df$disqualified)}",
    "  Randomized: {sum_na(ptstatus_df$randomized)}",
    "  Should equal line 1: {with(ptstatus_df, sum_na(excluded_later) + sum_na(disqualified) + sum_na(randomized))}",
    .sep = "\n"
  )
)

cat(
  glue(
    "\n\n\nTotal Screening through Randomization\n",
    "Total patients screened: {sum_na(ptstatus_df$screened)}",
    "  Excluded immediately: {sum_na(ptstatus_df$excluded_imm)}",
    "  Refused consent: {sum_na(ptstatus_df$refused)}",
    "  Consented: {sum_na(ptstatus_df$consented)}",
    "    Excluded after consent: {sum_na(ptstatus_df$excluded_later)}",
    "    Randomized: {sum_na(ptstatus_df$randomized)}",
    "    Disqualified: {sum_na(ptstatus_df$disqualified)}",
    "  Should equal line 1: {with(ptstatus_df, sum_na(excluded_imm) + sum_na(excluded_later) + sum_na(refused) + sum_na(disqualified) + sum_na(randomized))}",
    .sep = "\n"
  )
)

cat(
  glue(
    "End of In-Hospital Phase\n",
    "Total patients randomized: {sum_na(ptstatus_df$randomized)}",
    "  Died in hospital: {sum_na(ptstatus_df$dc_status == 'Died in hospital')}",
    "  Withdrew in hospital: {sum_na(ptstatus_df$dc_status == 'Withdrew in hospital')}",
    "  Eligible for follow-up: {sum_na(ptstatus_df$dc_status == 'Eligible for follow-up')}",
    "  Should equal line 1: {sum(!is.na(ptstatus_df$dc_status))}",
    .sep = "\n"
  )
)

sink()

## -- Save final dataset :tada: ------------------------------------------------
## Keep only what we need for this dataset: month/year of screening, enrollment,
##  randomization; status indicators; reasons for exclusion, disqualification
ptstatus_df <- ptstatus_df %>%
  mutate(exc_month = month(exc_date),
         exc_year = year(exc_date),
         inc_month = month(inclusion_met_date),
         inc_year = year(inclusion_met_date),
         enroll_month = month(enroll_time),
         enroll_year = year(enroll_time),
         randomized_month = month(randomization_time),
         randomized_year = year(randomization_time)) %>%
  select(id, study_site, protocol,
         screened, excluded_imm, excluded_later, excluded_ever,
         exc_month, exc_year, matches("^exc\\_[0-9]+"), exc_other,
         inc_month, inc_year, approached, refused,
         enroll_mv, enroll_nippv, enroll_shock,
         consented, enroll_month, enroll_year,
         randomized, randomized_month, randomized_year,
         rand_mv, rand_nippv, rand_shock,
         disqualified, randomized_no_reason,
         died_inhosp, wd_inhosp, elig_fu, dc_status) %>%
  rename(dq_reason = "randomized_no_reason")

saveRDS(ptstatus_df, file = "analysisdata/rds/ptstatus.rds")
write_csv(ptstatus_df, path = "analysisdata/csv/ptstatus.csv")
