################################################################################
## Functions for use in MINDUSA data management
################################################################################

library(RCurl)

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

inhosp_dd <- get_datadict("MINDUSA_IH_TOKEN")

## -- Exporting REDCap event names ---------------------------------------------
## rctoken must be a REDCap API token stored as a named object in .Renviron
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

## -- Importing data from REDCap databases -------------------------------------
## - rctoken: REDCap API token stored as a named object in .Renviron
## - export_labels:
##   - "all" = export labels for factors and checkboxes (unchecked boxes = NA)
##   - "factors" = export labels for factors; checkboxes = Unchecked/Checked
##   - "none" = export numeric codes for factors and checkboxes (0/1)
##   I don't think there's a way to export labels for factors + 0/1 for CBs.
## - id_field: character vector of length 1; represents patient ID field name
## - forms: character vector specifying which form(s) to export
## - events: character vector specifying which event(s) to export
##   (original REDCap name; eg, enrollment__day_0_arm_1, not Enrollment / Day 0)
get_pF <- function(
  rctoken,
  export_labels = c("all", "factors", "none"),
  id_field,
  forms,
  events
){
  ## Should we export raw or label versions of factors and checkboxes
  export_labels <- match.arg(export_labels)
  
  factor_export <- ifelse(export_labels %in% c("all", "factors"), "label", "raw")
  cb_export <- ifelse(export_labels == "all", TRUE, FALSE)
  
  postForm(
    "https://redcap.vanderbilt.edu/api/",   ## URL for REDCap instance
    token = Sys.getenv(rctoken),            ## token for specific database
    content = "record",                     ## export records
    format = "csv",                         ## export as CSV
    fields = id_field,                      ## ID variable - need on every row
    forms = paste(forms, collapse = ","),   ## which form(s) to export?
    events = paste(events, collapse = ","), ## which event(s) to export?
    rawOrLabel = factor_export,             ## exp. factor labels vs numbers
    exportCheckboxLabel = cb_export,        ## exp. NA/checkbox labels vs U/C
    exportDataAccessGroups = FALSE          ## don't need data access grps
  )
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
import_df <- function(rctoken, id_field, forms, events, ...){
  tmp_pF <- get_pF(
    rctoken, id_field = id_field, forms = forms, events = events, ...
  )
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
