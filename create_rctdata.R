################################################################################
## For in-hospital RCT analysis:
##   Combine datasets, create final analysis data files specific to this
##   project, and send to RCT project folder.
################################################################################

## -- Read in datasets from /analysisdata --------------------------------------
ptstatus_df <- readRDS("analysisdata/rds/ptstatus.rds")
ptdrug_df <- readRDS("analysisdata/rds/ptdrug.rds")
doses_df <- readRDS("analysisdata/rds/doses.rds")

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
add_trt <- function(df){
  left_join(df, trt_df, by = "id")
}

ptstatus_df <- add_trt(ptstatus_df)
ptdrug_df <- add_trt(ptdrug_df)
doses_df <- add_trt(doses_df)

## -- Save datasets to final RCT .Rdata file -----------------------------------
save(ptstatus_df, ptdrug_df, doses_df,
     file = "../MINDUSARCT/analysisdata/rct.Rdata")
