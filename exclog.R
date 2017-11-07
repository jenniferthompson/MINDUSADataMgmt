################################################################################
## Data management for MINDUSA exclusion log
################################################################################

library(tidyverse)
library(stringr)

## Source data management functions
source("data_functions.R")

## -- Import data dictionary, raw data from REDCap -----------------------------
## All tokens are stored in .Renviron
exc_dd <- get_datadict("MINDUSA_EXC_TOKEN")

## Get factor names/levels for study site
exc_site_levels <- get_factor_levels(ddict = exc_dd, varname = "exc_site")

## Read in all variables as numeric; only one factor that we need, and 0/1 for
## exclusion checkboxes is helpful
exc_df <- import_df(
  "MINDUSA_EXC_TOKEN",
  id_field = "exc_id",
  forms = "exclusion_log",
  export_labels = "none"
) %>%
  mutate(exc_site = factor(exc_site,
                           levels = exc_site_levels,
                           labels = names(exc_site_levels)))
