# MINDUSADataMgmt
## Data management code for the MINDUSA trial

Each script creates a single data.frame, which are then stored in the `analysisdata/` subfolder in both `.rds` and `.csv` formats.

Each script also produces a text file with automated data checks. The user should examine each of these files for anomalies when recreating the datasets; to preserve patient confidentiality, these files are **not** tracked with git.

Scripts:

- [ptstatus.R](ptstatus.R): Combines data from exclusion log and in-hospital database to create a dataset of patient status at various time points + related information. Automated data checks: `ptstatus_checks.txt`.
