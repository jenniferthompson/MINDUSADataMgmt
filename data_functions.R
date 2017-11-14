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
## - forms: character vector specifying which form(s) to export
## - events: character vector specifying which event(s) to export
##   (original REDCap name; eg, enrollment__day_0_arm_1, not Enrollment / Day 0)
get_pF <- function(
  rctoken,
  export_labels = c("all", "factors", "none"),
  id_field,
  forms,
  events = NULL
){
  ## Should we export raw or label versions of factors and checkboxes
  export_labels <- match.arg(export_labels)
  
  factor_export <- ifelse(export_labels %in% c("all", "factors"), "label", "raw")
  cb_export <- ifelse(export_labels == "all", TRUE, FALSE)
  
  ## If database is not longitudinal or events not specified, take all events
  if(is.null(events)){
    postForm(
      "https://redcap.vanderbilt.edu/api/",   ## URL for REDCap instance
      token = Sys.getenv(rctoken),            ## token for specific database
      content = "record",                     ## export records
      format = "csv",                         ## export as CSV
      fields = id_field,                      ## ID variable - need on every row
      forms = paste(forms, collapse = ","),   ## which form(s) to export?
      rawOrLabel = factor_export,             ## exp. factor labels vs numbers
      exportCheckboxLabel = cb_export,        ## exp. NA/checkbox labels vs U/C
      exportDataAccessGroups = FALSE          ## don't need data access grps
    )
  ## Otherwise, take only specified events
  } else{
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
import_df <- function(rctoken, id_field, forms, ...){
  tmp_pF <- get_pF(
    rctoken, id_field = id_field, forms = forms, ...
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

## -- Get factor levels from string in REDCap data dictionary ------------------
## Returns a numeric vector of all possible factor levels in order listed in
## REDCap, with names() = factor levels
get_factor_levels <- function(ddict, varname){
  tmp <- ddict %>%
    filter(field_name == varname) %>%
    pull(select_choices_or_calculations)
  
  flevels <- str_extract_all(tmp, "[0-9]+(?=,)")[[1]] %>% as.numeric
  names(flevels) <- str_extract_all(tmp, "(?<=, )[a-z, A-Z, 0-9]+")[[1]] %>%
    trimws
  
  return(flevels)
}

## Example: Exclusion site factor labels
## exc_site_levels <- get_factor_levels(
##   ddict = get_datadict("MINDUSA_EXC_TOKEN"),
##   varname = "exc_site"
## )
