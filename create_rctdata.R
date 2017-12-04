################################################################################
## For in-hospital RCT analysis:
##   Combine datasets, create final analysis data files specific to this
##   project, and send to RCT project folder.
################################################################################

## -- Temporary: Add fake treatment groups to datasets -------------------------
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

## Add treatment group to datasets
ptstatus_df <- left_join(ptstatus_df, trt_df, by = "id")

## -- Save datasets to final RCT .Rdata file -----------------------------------
save(ptstatus_df, file = "../MINDUSARCT/analysisdata/rct.Rdata")
