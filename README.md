# MINDUSADataMgmt
## Data management code for the MINDUSA trial

Each script creates a single data.frame, which are then stored in the `analysisdata/` subfolder in both `.rds` and `.csv` formats.

Each script also produces a text file with automated data checks. The user should examine each of these files for anomalies when recreating the datasets; to preserve patient confidentiality, these files are **not** tracked with git.

Scripts:

- [fake_make.R](fake_make.R): Sources all following scripts in proper order. One day after a deadline I'll figure out how to do a real makefile. Till then, this'll do.
- [ptstatus.R](ptstatus.R): Combines data from exclusion log and in-hospital database to create a dataset, `ptstatus_df`, of patient status at various time points + related information. Automated data checks: `ptstatus_checks.txt`.
- [ptevents.R](ptevents.R): Creates single dataset with all events for each patient, days since consent and randomization (if appropriate), and whether patient was known to be in ICU each day.
- [demog.R](demog.R): Creates data frame, `admission_df`, of demographic, pre-hospital, and ICU admission information for all consented patients.
- [datestrack.R](datestrack.R): Creates data frame, `datestrack_df`, of information calculated from Dates Tracking form: ICU and hospital stay variables; mechanical ventilation variables; death, hospital discharge and withdrawal variables. No actual dates are included as a precautionary measure for privacy.
- [studydrug.R](studydrug.R): Creates two data frames related to study drug administration:
    - `ptdrug_df`: one record per patient, describing number of days and doses of study drug; summarizing amounts of drug and number of times held for various reasons; and reason study drug was first permanently discontinued
    - `doses_df`: one record per potential dose of study drug (up to 3 per day), describing amounts, holds/discontinuations, QTc
- [create_rctdata.R](create_rctdata.R): Sources `fake_make.R` and saves fresh datasets needed to conduct analysis of primary in-hospital aims of the study to a separate repository. [If I use a `make` approach, this should **not** be included, because once the final manuscript version of this dataset is created it will stay constant.]