################################################################################
## Functions for use in MINDUSA data management
################################################################################

library(RCurl)
library(glue)

## -- Create CSV out of postForm() object --------------------------------------
get_csv <- function(pF){
  read.csv(file = textConnection(pF), na.strings = "", stringsAsFactors = FALSE)
}

## -- Exporting data dictionary ------------------------------------------------
## rctoken must be a REDCap API token stored as a named object in .Renviron
get_datadict <- function(rctoken){
  tmp <- postForm(
    "https://redcap.vanderbilt.edu/api/", ## URL for REDCap instance
    token = Sys.getenv(rctoken),          ## token for specific database
    content = "metadata",                 ## export metadata
    format = "csv"                        ## export as CSV
  )
  return(get_csv(tmp))
}

## Example: get data dictionary for exclusion log
## exc_dd <- get_datadict("MINDUSA_EXC_TOKEN")

## -- Exporting REDCap event names ---------------------------------------------
## rctoken must be a REDCap API token stored as a named object in .Renviron
## Database must be longitudinal; otherwise you'll get a Bad Request error
get_events <- function(rctoken){
  tmp <- postForm(
    "https://redcap.vanderbilt.edu/api/", ## URL for REDCap instance
    token = Sys.getenv(rctoken),          ## token for specific database
    content = "event",                   ## export metadata
    format = "csv"                        ## export as CSV
  )
  return(get_csv(tmp))
}

## Example: get events for in-hospital database
## inhosp_events <- get_events("MINDUSA_IH_TOKEN")

## -- Exporting REDCap form -> event mapping -----------------------------------
## rctoken must be a REDCap API token stored as a named object in .Renviron
## Database must be longitudinal; otherwise you'll get a Bad Request error
get_event_mapping <- function(rctoken){
  tmp <- postForm(
    uri = "https://redcap.vanderbilt.edu/api/",
    token = Sys.getenv(rctoken),
    content = "formEventMapping",
    format = "csv",
    returnFormat = "csv"
  )
  
  ## Our databases are the same for each arm; don't need first column
  return(get_csv(tmp)[, -1])
}

## -- Importing data from REDCap databases -------------------------------------
## - rctoken: REDCap API token stored as a named object in .Renviron
## - export_labels:
##   - "all" = export labels for factors and checkboxes (unchecked boxes = NA)
##   - "factors" = export labels for factors; checkboxes = Unchecked/Checked
##   - "none" = export numeric codes for factors and checkboxes (0/1)
##   I don't think there's a way to export labels for factors + 0/1 for CBs.
## - id_field: character vector of length 1; represents patient ID field name
## - forms: character vector specifying which form(s) to export; default = all
## - fields: character vector specifying which field(s) to export; default = all
## - events: character vector specifying which event(s) to export; default = all
##     (orig. REDCap name; eg, enrollment__day_0_arm_1, not Enrollment / Day 0)
get_pF <- function(
  rctoken,
  export_labels = c("all", "factors", "none"),
  id_field,
  forms = NULL,
  fields = NULL,
  events = NULL
){
  ## Should we export raw or label versions of factors and checkboxes
  export_labels <- match.arg(export_labels)
  
  factor_export <- ifelse(export_labels %in% c("all", "factors"), "label", "raw")
  cb_export <- ifelse(export_labels == "all", TRUE, FALSE)
  
  ## -- Construct postForm call ------------------------------------------------
  ## If events, fields, and/or forms are specified, add line to specify which
  ## of each to import
  forms_line <-
    ifelse(is.null(forms), "", 'forms = paste(forms, collapse = ","),')
  fields_line <-
    ifelse(is.null(fields),
           "fields = id_field,", ## ID variable - need on every row
           'fields = paste(fields, collapse = ","),')
  events_line <-
    ifelse(is.null(events), "", 'events = paste(events, collapse = ","),')
  
  ## Use glue() to construct a postForm call
  pF_call <- glue(
    'postForm(
       "https://redcap.vanderbilt.edu/api/",   ## URL for REDCap instance
       token = Sys.getenv("{rctoken}"),        ## token for specific database
       content = "record",                     ## export records
       format = "csv",                         ## export as CSV
       {forms_line}                            ## which forms to export
       {fields_line}                           ## which fields to export
       {events_line}                           ## which events to export
       rawOrLabel = "{factor_export}",         ## exp. factor labels vs numbers
       exportCheckboxLabel = {cb_export},      ## exp. NA/checkbox labels vs U/C
       exportDataAccessGroups = FALSE          ## do not need data access grps
     )'
  )
  
  ## Return evaluated postForm() call
  return(eval(parse(text = pF_call)))
}

## Example: Read in enrollment qual + dates tracking from only enrollment day
## tmp <- get_pF(
##   rctoken = "MINDUSA_IH_TOKEN",
##   export_labels = "all",
##   id_field = "id",
##   forms = c("enrollment_qualification_form", "dates_tracking_form"),
##   events = c("enrollment__day_0_arm_1")
## )
## tmp_df <- get_csv(tmp)
## head(tmp_df)

## -- Wrapper to read in data, turn into data.frame, remove extra _s -----------
import_df <- function(rctoken, id_field, ...){
  tmp_pF <- get_pF(rctoken, id_field = id_field, ... )
  tmp_csv <- get_csv(tmp_pF)
  
  ## REDCap loves to use so many underscores; one per instance seems like plenty
  names(tmp_csv) <- gsub("_+", "_", names(tmp_csv))
  
  tmp_csv
}

## Example: Read in enrollment qual + dates tracking from only enrollment day
## tmp <- import_df(
##   rctoken = "MINDUSA_IH_TOKEN",
##   export_labels = "none",
##   id_field = "id",
##   forms = c("enrollment_qualification_form", "dates_tracking_form"),
##   events = c("enrollment__day_0_arm_1")
## )
## head(tmp)

## -- Get factor levels from string in REDCap data dictionary ------------------
## Returns a numeric vector of all possible factor levels in order listed in
## REDCap, with names() = factor levels
get_factor_levels <- function(ddict, varname){
  tmp <- ddict %>%
    filter(field_name == varname) %>%
    pull(select_choices_or_calculations)
  
  flevels <- str_extract_all(tmp, "[0-9]+(?=,)")[[1]] %>% as.numeric
  names(flevels) <-
    str_extract_all(tmp, "(?<=, )[a-z, A-Z, 0-9, ., /, ', \\-, +, (, )]+")[[1]] %>%
    trimws
  
  return(flevels)
}

## Example: Exclusion site factor labels
## exc_site_levels <- get_factor_levels(
##   ddict = get_datadict("MINDUSA_EXC_TOKEN"),
##   varname = "exc_site"
## )

## -- Function to print dataset + a cat() message above ------------------------
## Intended use case: printing datasets with values to be inspected to text file
print_datachecks <- function(msg, df){
  cat(msg, "\n\n")
  if(nrow(df) == 0){
    cat("<none>")
  } else{
    print(as.data.frame(df), digits = 2)
  }
  cat("\n\n\n")
}

## -- Helper functions ---------------------------------------------------------
max_na <- function(x){ max(x, na.rm = TRUE) }
min_na <- function(x){ min(x, na.rm = TRUE) }
sum_na <- function(x){ sum(x, na.rm = TRUE) }
mean_na <- function(x){ mean(x, na.rm = TRUE) }
sd_na <- function(x){ sd(x, na.rm = TRUE) }
q25 <- function(x){ quantile(x, probs = 0.25, na.rm = TRUE) }
q50 <- function(x){ quantile(x, probs = 0.50, na.rm = TRUE) }
q75 <- function(x){ quantile(x, probs = 0.75, na.rm = TRUE) }
