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

## -- Import data dictionaries from REDCap -------------------------------------
## All tokens are stored in .Renviron
exc_dd <- get_datadict("MINDUSA_EXC_TOKEN")
ih_dd <- get_datadict("MINDUSA_IH_TOKEN")
ih_events <- get_events("MINDUSA_IH_TOKEN")
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
  "death", "death_time", "studywd", "studywd_time", "hospdis", "hospdis_time",
  "icudis_1_time"
)

inhosp_raw <- import_df(
  rctoken = "MINDUSA_IH_TOKEN",
  id_field = "id",
  export_labels = "none",
  forms = c("enrollment_qualification_form"),
  fields = c(iqcode_vars, datestrack_vars),
  events = "enrollment__day_0_arm_1"
) %>%
  select(-redcap_event_name, -enrollment_qualification_form_complete)

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

inhosp_raw_checks <- inhosp_raw %>%
  ## Remove test patients
  ## ***** YAL-060 has lots of issues caught in data clean - recheck this!
  filter(!str_detect(toupper(id), "TEST") & !(id == "YAL-060")) %>%
  # assert(not_na,
  #        id:ptsurr_refuse, blind_deaf:enr_qual_other, enroll_time:randomized_yn) %>%
  ## 48 patients missing IQCODE variables
  ## MON-071 missing ptsurr_refuse
  assert(in_set(1:10), protocol) %>%
  assert(in_set(0:1), adult_patient:nosurr_avail) %>%
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

inhosp_df <- inhosp_raw %>%
  mutate(
    ## Make study site and protocol factors, using levels from data dictionary
    study_site = factor(study_site,
                        levels = get_levels_ih("study_site"),
                        labels = names(get_levels_ih("study_site"))),
    protocol = factor(
      protocol,
      levels = get_levels_ih("protocol"),
      labels = names(get_levels_ih("protocol"))
    ),
    
    ## Force IDs to use "-" as separator, be in all caps
    id = str_replace_all(id, c("[=_]" = "-", "\\." = "")) %>% toupper,
    
    ## Calculate IQCODE score
    iqcode_score = rowMeans(.[, paste("iqcode", 1:16, "ph", sep = "_")])
  ) %>%
  rename(
    ## Inclusions
    orgfail_mv = "organ_failures_present_1",
    orgfail_nippv = "organ_failures_present_2",
    orgfail_shock = "organ_failures_present_3",
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
  ) %>%
  select(-patient_id, -matches("^iqcode\\_[0-9]+\\_ph$"))

## -- Combine exclusion, in-hospital data for one master dataset ---------------
ptstatus_df <- bind_rows(exc_df, inhosp_df) %>%
  select(id, study_site, protocol, exc_date:exc_notes,
         inclusion_met_date:orgfail_shock, iqcode_score,
         everything())
## Next up: Indicators for status at each time point; format dates/times












# ## Prep to summarize exclusions for CONSORT table
# ## Create list of all exclusions, except:
# ## - 1: rapidly resolving organ failure (patient does not meet incl. criteria)
# ## - 9d: no screening within 72h of meeting inclusion
# ## - 9b: patient/surrogate refusal
# ## 1, 9d don't meet screening def of "met inc criteria + screened within 72h"
# ## 9b is not an exclusion in the CONSORT sense; patient met all inclusion
# ## criteria, no other exclusions, and was approached
# not_exclusions <- c("1", "9b", "9d")
# 
# ## Variables listed as exclusion options in database
# possible_exclusions <- str_subset(names(exc_df), "^exc\\_[0-9]+")
# 
# ## Remove variables which are not considered official exclusion criteria
# final_exclusions <-
#   possible_exclusions[
#     !(map_chr(str_split(possible_exclusions, fixed("_")), ~ .[2]) %in%
#         not_exclusions)
#     ]
# 
# exc_df <- exc_df %>%
#   mutate(
#     ## Combine related exclusions
#     ## Categories per TGirard, 11-8-2017: 2a+2b; 3+4; 9c+9e+9f
#     exc_2_pregbf = as.numeric(exc_2a_pregnancy | exc_2b_breastfeed),
#     exc_34_neuro = as.numeric(exc_3_dementia | exc_4_deficit),
#     exc_9cef_time = as.numeric(
#       exc_9c_unable_nosurr | exc_9e_72h_nosurr | exc_9f_120h_coma
#     ),
#     ## Indicator for no exclusions checked, including rapidly reversible,
#     ## 72h, refusal - this shouldn't happen
#     exc_none = rowSums(.[, possible_exclusions]) == 0,
#     
#     ## How many official exclusions checked? (not counting 1, 9d, 9b)
#     exc_number = rowSums(.[, final_exclusions]),
#     
#     ## Indicator for "screened": must be screened within 72 hours of meeting
#     ## inclusion criteria. Leaves out exclusion 1 (rapidly resolving organ
#     ## failure) and 9d (72h eligibility period exceeded before screening).
#     screened = !(exc_1_resolving | exc_9d_72h_noscreen),
#     
#     ## Indicator for "excluded": must be screened and meet >=1 exc criteria
#     excluded = screened & rowSums(.[,final_exclusions]) > 0,
#     
#     ## Indicator for "approached": Patient had no other exclusions and they or
#     ## surrogate was approached for consent
#     approached = screened & !excluded & exc_9b_refusal_ptsurr,
#     
#     ## Indicator for "refused": Patient/surrogate was approached but refused
#     ## consent (should match `approached` until we add in enrollment log)
#     refused = screened & !excluded & approached & exc_9b_refusal_ptsurr
#   )
# 
# ## -- Data checks --------------------------------------------------------------
# ## Tell me about the patients approached but who also had an exclusion - want to
# ## make sure these make sense (often, exclusion developed while family was
# ## considering decision)
# approached_exc <- exc_df %>%
#   filter(exc_9b_refusal_ptsurr & exc_number > 0)
# 
# ## No one should have 0 exclusions checked. Write a subset of these IDs to CSV
# write_csv(
#   subset(exc_df,
#          exc_none,
#          select = c(exc_id, exc_site, exc_date, exc_other, exc_notes)),
#   path = "no_exclusions.csv"
# )
# 
# ## -- Summarize all recorded exclusions among patients officially screened -----
# ## Summarize % of exclusions
# main_exclusions <- c(
#   paste(
#     "exc",
#     c("1_resolving", "2_pregbf", "34_neuro", "5_torsadesqtc", "6_maintmeds",
#       "7_nmsallergy", "8_death24", "9a_refusal_md", # "9b_refusal_ptsurr",
#       "9d_72h_noscreen", "9cef_time", "10_blind_lang", "11_prison",
#       "12_coenroll", "99_other", "none"),
#     sep = "_"
#   ),
#   "screened", "excluded", "approached", "refused"
# )
# 
# summarize_exc <- exc_df %>%
#   filter(screened) %>%
#   select(one_of(main_exclusions)) %>%
#   gather(key = exclusion, value = yn) %>%
#   group_by(exclusion) %>%
#   summarise_all(funs(pts = sum, pct = mean)) %>%
#   mutate(pct = pct * 100,
#          order = ifelse(exclusion == "screened", 1,
#                  ifelse(exclusion == "excluded", 2,
#                  ifelse(exclusion == "approached", 4,
#                  ifelse(exclusion == "refused", 5, 3))))) %>%
#   arrange(order, desc(pct))
# 
# ## -- Save final exclusions dataset --------------------------------------------
# # save(exc_df, file = "analysisdata/exclusions.Rdata")
# 
