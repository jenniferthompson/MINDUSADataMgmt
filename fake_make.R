## -- Sources everything, in order (poor man on a deadline's make file) --------
## Yes, I'm well aware this isn't best practice; I'll figure out make after the
## first big deadline

## Have to run ptstatus.R first; it is the source of truth
rm(list = ls())
source("ptstatus.R")

## Have to run ptevents.R next; it is used in other scripts
rm(list = ls())
source("ptevents.R")

## From here on out these should stand alone
rm(list = ls())
source("demog.R")

rm(list = ls())
source("datestrack.R")

rm(list = ls())
source("studydrug.R")

rm(list = ls())
source("pad.R")

rm(list = ls())
source("dailydata.R")
