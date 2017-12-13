# MINDUSADataMgmt
## Data management code for the MINDUSA trial

Each script creates a single data.frame, which are then stored in the `analysisdata/` subfolder in both `.rds` and `.csv` formats.

Each script also produces a text file with automated data checks. The user should examine each of these files for anomalies when recreating the datasets; to preserve patient confidentiality, these files are **not** tracked with git.

Scripts:

- [ptstatus.R](ptstatus.R): Combines data from exclusion log and in-hospital database to create a dataset, `ptstatus_df`, of patient status at various time points + related information. Automated data checks: `ptstatus_checks.txt`.
- [demog.R](demog.R): Creates data frame, `admission_df`, of demographic, pre-hospital, and ICU admission information for all consented patients.
- [studydrug.R](studydrug.R): Creates two data frames related to study drug administration:
    - `ptdrug_df`: one record per patient, describing number of days and doses of study drug; summarizing amounts of drug and number of times held for various reasons; and reason study drug was first permanently discontinued
    - `doses_df`: one record per potential dose of study drug (up to 3 per day), describing amounts, holds/discontinuations, QTc
- [create_rctdata.R](create_rctdata.R): Saves datasets needed to conduct analysis of primary in-hospital aims of the study to a separate repository. [If I use a `make` approach, this should **not** be included, because once the final manuscript version of this dataset is created it will stay constant.]