################################################################################
## For in-hospital RCT analysis:
##   Combine datasets, create final analysis data files specific to this
##   project, and send to RCT project folder.
################################################################################

save(ptstatus_df, file = "../MINDUSARCT/analysisdata/rct.Rdata")
