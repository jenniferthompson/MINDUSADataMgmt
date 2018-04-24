# MINDUSADataMgmt
## Data management code for the MINDUSA trial

Each script creates a single data.frame, which are then stored in the
`analysisdata/` subfolder in both `.rds` and `.csv` formats.

Each script also produces a text file with automated data checks. The user
should examine each of these files for anomalies when recreating the datasets;
to preserve patient confidentiality, these files are **not** tracked with git.

### Naming conventions

The following variable naming conventions are used:

- `_exp`: "among those exposed" *(eg, time on vent among patients ever on MV)*
- `_all`: "all patients" *(eg, patients never on MV get 0 for this version)*
- `_ih`: "in hospital" *(eg, in-hospital death vs death at any point, or total
drug doses on all hospital days, vs only intervention or only ICU days)*
- `_pr`: "pre-randomization" *(period between consent and randomization)*
- `_int`: "intervention" *(randomization + 13 following days)*
- `_icu`: "ICU" *(eg, only on ICU days, vs days on the floor)*
- `_adm`: "at ICU admission" *(eg, SOFA at ICU admission, vs daily in ICU)*
- `_rand`: "at randomization" *(eg, SOFA or RASS on the day of randomization)*

### Scripts

*Some scripts need to be run before others; these cases are noted*

#### Utilities

- [data_functions.R](data_functions.R): Includes functions used in multiple data
management scripts. Subject areas:
    - Dealing with REDCap (eg, wrappers to download raw data and data
    dictionaries)
    - Working with factors + REDCap data dictionaries
    - Base functions with partial arguments supplied (eg, `sum_na <-
    function(x){ sum(x, na.rm = TRUE, ...) }`)
    - More complex functions for operations common with this data
- [fake_make.R](fake_make.R): Sources all following scripts in proper order. One
day after a deadline I'll figure out how to do a real makefile. Till then,
this'll do.
- [create_rctdata.R](create_rctdata.R): Sources `fake_make.R` and saves fresh
datasets needed to conduct analysis of primary in-hospital aims of the study to
a separate repository. [If I use a `make` approach, this should **not** be
included, because once the final manuscript version of this dataset is created
it will stay constant.]

#### Data management

- [ptstatus.R](ptstatus.R): Combines data from exclusion log and in-hospital
database to create a dataset, `ptstatus_df`, of patient status at various time
points + related information. **Should be run first**, as lists of randomized
and consented patients are pulled from this dataset for use in other scripts.
Automated data checks: `datachecks/ptstatus_checks.txt`.
- [ptevents.R](ptevents.R): **Should be run second**, as these datasets are used
in other scripts. Creates two datasets:
    - `allpts_events`: All REDCap events for each patient, days since consent
    and randomization (if appropriate), and whether patient was known to be in
    ICU each day
    - `randpts_events`: A record for every day of the
    intervention/post-intervention period for every randomized patient,
    beginning on day of randomization; this includes indicators for patient
    status each day (in ICU, in REDCap, hospitalized, etc), as well as an
    overall variable for study status: intervention period; post-intervention
    period; hospitalized, but no longer being tracked daily; discharged;
    deceased; or withdrawn.
- [demog.R](demog.R): Creates data frame, `admission_df`, of demographic,
pre-hospital, and ICU admission information for all consented patients.
- [datestrack.R](datestrack.R): Creates data frame, `datestrack_df`, of
information calculated from Dates Tracking form: ICU and hospital stay
variables; mechanical ventilation variables; death, hospital discharge and
withdrawal variables. No actual dates are included as a precautionary measure
for privacy.
- [studydrug.R](studydrug.R): Creates two data frames related to study drug
administration:
    - `ptdrug_df`: one record per patient, describing number of days and doses
    of study drug; summarizing amounts of drug and number of times held for
    various reasons; and reason study drug was first permanently discontinued
    - `doses_df`: one record per potential dose of study drug (up to 3 per day),
    describing amounts, holds/discontinuations, QTc
- [dailydata.R](dailydata.R): Creates two data frames related to data from the
Daily Data Collection Form:
    - `daily_df`: one record per day in the hospital, with variables for NMS
    (indicator), medication doses, variables used for SOFA calculation and total
    SOFA scores (full + modified, using raw data + imputed).
    - `dailysum_df`: one record per patient, with:
        - Indicators for whether patient had NMS, ever and during intervention
        - Medication variables:
            - indicators for whether patient had Drug X ever in the hospital,
            during the pre-randomization period, during the intervention period,
            ever in the ICU, in the ICU during intervention
            - proportion of days the patient had Drug X during the time periods
            listed above
            - mean dose of Drug X during the time periods listed above, among
            all patients and among those exposed
            - total dose of Drug X during the time periods listed above, among
            all patients and among those exposed
        - SOFA variables (with versions using both original, raw data and using
        imputation rules):
            - Full, modified, and CV SOFA scores on the day of consent
            - Full, modified, and CV SOFA scores on the day of randomization
            - Mean, maximum, minimum full and modified SOFA scores during the
            time periods listed above for medications
- [pad.R](pad.R): Creates three data frames with various levels of detail about
the Pain, Agitation and Delirium form:
    - `pad_long`: Every single assessment done (or that should have been done)
    during the entire hospitalization
    - `pad_daily`: One record per *day* during the hospitalization, summarizing
    whether patient was delirious, comatose, etc that day
    - `pad_summary`: One record per *patient*, summarizing coma, delirium, and
    DCFDs during the entire intervention period (study days 0-13)
    - **Should be run after demog.R and dailydata.R**, because we use variables
    from those datasets to impute missing mental status.
- [safety.R](safety.R): Creates data frames related to safety outcomes:
    - `torsades_events`: Descriptions of adverse events determined to be
    Torsades de pointes, for direct inclusion in eventual report. PHI has been
    removed from text fields.
    - `safety_df`: One record per *day* for all consented patients, with
    information on extrapyramidal symptoms (EPS) and Torsades de pointes.
    - `safety_summary`: One record per *patient* for all consented patients,
    summarizing days and instances of EPS symptoms and Torsades de pointes
    during the entire hospitalization and specifically during the intervention
    period (if applicable).
- [compliance.R](compliance.R): Creates one long data frame with information
about compliance on the A-E elements related to ICU liberation (early version of
the [ICU Liberation ABCDEF Bundle](iculiberation.org/Bundles/Pages/default.aspx)
from the Society of Critical Care Medicine). One record per *day* during the
hospitalization.