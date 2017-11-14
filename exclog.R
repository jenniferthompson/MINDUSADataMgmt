################################################################################
## Data management for MINDUSA exclusion log
################################################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(assertr)

## Source data management functions
source("data_functions.R")

## -- Import data dictionary, raw data from REDCap -----------------------------
## All tokens are stored in .Renviron
exc_dd <- get_datadict("MINDUSA_EXC_TOKEN")

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

## -- Data management ----------------------------------------------------------
## Properly format IDs; dump vars we'll never use; rename exc's for readability
exc_df <- exc_raw %>%
  mutate(
    ## Make exclusion site a factor, using levels from data dictionary
    exc_site = factor(exc_site,
                      levels = exc_site_levels,
                      labels = names(exc_site_levels)),
    ## Format exclusion date
    exc_date = ymd(exc_date),
    ## Some exclusion IDs have separators other than "-"; replace them and
    ## force ID to use all caps
    exc_id = str_replace_all(exc_id, c("[=_]" = "-", "\\." = "")) %>% toupper
  ) %>%
  ## Create correctly formatted IDs:
  ##  Some have too few/too many numbers; some don't include site
  ## 1. ID should have two pieces separated by "-"; all IDs missing a piece
  ##    were missing the site, so fill in lefthand missings with NA
  separate(exc_id, into = c("org_site", "org_idnum"), fill = "left") %>%
  mutate(
    ## 2. If original ID number had width <4, pad with 0s on lefthand side
    ##    (eg, "324" -> "0324"); if it had width >4, take last four digits
    org_idnum =
      str_pad(org_idnum, width = 4, side = "left", pad = "0") %>%
      str_sub(-4),
    ## 3. If no site code included in ID, replace with exclusion site value
    org_site = ifelse(is.na(org_site), as.character(exc_site), org_site)
  ) %>%
  ## 4. Recombine new site, numbers into properly formatted exc_id
  unite("exc_id", org_site, org_idnum, sep = "-") %>%
  ## We don't need these variables
  select(-exc_number, -exclusion_log_complete) %>%
  ## Rename exclusions to match CRF + include meaningful terms
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

## Prep to summarize exclusions for CONSORT table
## Create list of all exclusions, except:
## - 1: rapidly resolving organ failure (patient does not meet incl. criteria)
## - 9d: no screening within 72h of meeting inclusion
## - 9b: patient/surrogate refusal
## 1, 9d don't meet screening def of "met inc criteria + screened within 72h"
## 9b is not an exclusion in the CONSORT sense; patient met all inclusion
## criteria, no other exclusions, and was approached
not_exclusions <- c("1", "9b", "9d")

## Variables listed as exclusion options in database
possible_exclusions <- str_subset(names(exc_df), "^exc\\_[0-9]+")

## Remove variables which are not considered official exclusion criteria
final_exclusions <-
  possible_exclusions[
    !(map_chr(str_split(possible_exclusions, fixed("_")), ~ .[2]) %in%
        not_exclusions)
    ]

exc_df <- exc_df %>%
  mutate(
    ## Combine related exclusions
    ## Categories per TGirard, 11-8-2017: 2a+2b; 3+4; 9c+9e+9f
    exc_2_pregbf = as.numeric(exc_2a_pregnancy | exc_2b_breastfeed),
    exc_34_neuro = as.numeric(exc_3_dementia | exc_4_deficit),
    exc_9cef_time = as.numeric(
      exc_9c_unable_nosurr | exc_9e_72h_nosurr | exc_9f_120h_coma
    ),
    ## Indicator for no exclusions checked, including rapidly reversible,
    ## 72h, refusal - this shouldn't happen
    exc_none = rowSums(.[, possible_exclusions]) == 0,
    
    ## How many official exclusions checked? (not counting 1, 9d, 9b)
    exc_number = rowSums(.[, final_exclusions]),
    
    ## Indicator for "screened": must be screened within 72 hours of meeting
    ## inclusion criteria. Leaves out exclusion 1 (rapidly resolving organ
    ## failure) and 9d (72h eligibility period exceeded before screening).
    screened = !(exc_1_resolving | exc_9d_72h_noscreen),
    
    ## Indicator for "excluded": must be screened and meet >=1 exc criteria
    excluded = screened & rowSums(.[,final_exclusions]) > 0,
    
    ## Indicator for "approached": Patient had no other exclusions and they or
    ## surrogate was approached for consent
    approached = screened & !excluded & exc_9b_refusal_ptsurr,
    
    ## Indicator for "refused": Patient/surrogate was approached but refused
    ## consent (should match `approached` until we add in enrollment log)
    refused = screened & !excluded & approached & exc_9b_refusal_ptsurr
  )

## -- Data checks --------------------------------------------------------------
## Tell me about the patients approached but who also had an exclusion - want to
## make sure these make sense (often, exclusion developed while family was
## considering decision)
approached_exc <- exc_df %>%
  filter(exc_9b_refusal_ptsurr & exc_number > 0)

## No one should have 0 exclusions checked. Write a subset of these IDs to CSV
write_csv(
  subset(exc_df,
         exc_none,
         select = c(exc_id, exc_site, exc_date, exc_other, exc_notes)),
  path = "no_exclusions.csv"
)

## -- Summarize all recorded exclusions among patients officially screened -----
## Summarize % of exclusions
main_exclusions <- c(
  paste(
    "exc",
    c("1_resolving", "2_pregbf", "34_neuro", "5_torsadesqtc", "6_maintmeds",
      "7_nmsallergy", "8_death24", "9a_refusal_md", # "9b_refusal_ptsurr",
      "9d_72h_noscreen", "9cef_time", "10_blind_lang", "11_prison",
      "12_coenroll", "99_other", "none"),
    sep = "_"
  ),
  "screened", "excluded", "approached", "refused"
)

summarize_exc <- exc_df %>%
  filter(screened) %>%
  select(one_of(main_exclusions)) %>%
  gather(key = exclusion, value = yn) %>%
  group_by(exclusion) %>%
  summarise_all(funs(pts = sum, pct = mean)) %>%
  mutate(pct = pct * 100,
         order = ifelse(exclusion == "screened", 1,
                 ifelse(exclusion == "excluded", 2,
                 ifelse(exclusion == "approached", 4,
                 ifelse(exclusion == "refused", 5, 3))))) %>%
  arrange(order, desc(pct))

## -- Save final exclusions dataset --------------------------------------------
save(exc_df, file = "analysisdata/exclusions.Rdata")
