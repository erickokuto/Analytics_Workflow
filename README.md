# Analytics_Workflow — Supply Chain Analytics Workflow

An R Shiny interface backed by the original fitted Python artefacts. Synthetic data only. No SAP BTP or IBP connection, no automatic retraining, and no enterprise deployment. The historical analytical evidence is kept separate from the current uploaded batch.

## Run locally (Windows / RStudio)

1. Extract the whole ZIP into one directory. Do not move app.R away from its data/backend folders.
2. Install R, RStudio and Python 3.13 (64-bit). Open a terminal in the extracted Analytics_Workflow directory. Create a virtual environment and install dependencies:

```powershell
py -3.13 -m venv .venv
.\.venv\Scripts\python.exe -m pip install -r requirements.txt
```

3. In RStudio, set the working directory to the extracted Analytics_Workflow folder (change the example path). Install packages once, select the Python interpreter, and launch:

```r
install.packages(c('shiny','DT','ggplot2'))
setwd('D:/Projects/Analytics_Workflow')
Sys.setenv(SUPPLY_PYTHON = normalizePath('.venv/Scripts/python.exe', winslash='/'))
shiny::runApp('.')
```

On macOS/Linux use `python3.13 -m venv .venv`, `.venv/bin/python -m pip install -r requirements.txt`, and set SUPPLY_PYTHON to the absolute path to `.venv/bin/python`. The Python interpreter must have the pinned packages; older versions of scikit-learn may not load the fitted models. Use only the bundled, trusted joblib files. Do not load externally supplied model artefacts.

## Workflow
The application starts with the original 1,000 open orders. In Data intake, choose an included dataset or upload a compatible CSV, then click Validate and score. Browse automatically selects My CSV file. To switch back, select Bundled dataset and choose the required file. Upload_Example.csv is a small 12-order sample. Optional decision simulation is limited to 1,000 orders per run for memory and performance. In Predictions, filter by transport and alert threshold; choose nominal interval level and probability version in Uncertainty. The original logistic probabilities are the default. Current batch quality, EDA, anomaly flags, predictions and optional decisions change after successful validation. Fixed historical model comparisons, supplier PCA and policy evidence are labelled as such. Download results and reports from the tabs. A failed validation clears the active results.

## Data contract
The data dictionary defines all 46 original fields. Forty fields are required; receipt_date, actual_lead_days and late_flag may be absent for open orders (or all three present on labelled rows), and dataset_split, disruption_regime and order_status are optional audit fields. Keep date values in YYYY-MM-DD format, order identifiers as text, and units identical. The model is order-time only: order_date equals as_of_date, and signal_date cannot be later than the cutoff. Supplier history must genuinely predate the cutoff; schema checks cannot prove the provenance of externally uploaded historical summaries. Unseen categories are flagged; unseen suppliers may lack a cluster. The backend validates data types, key uniqueness, selected ranges, timestamps and outcome consistency. It accepts up to 10,000 records per predictive batch; it is not designed to ingest raw SAP extracts without preprocessing.

## Direct backend use
From Analytics_Workflow, after installing the requirements:

```powershell
.\.venv\Scripts\python.exe backend/run_batch.py data/Scoring.csv results_demo --decisions
```

For other inputs, replace the input path and use a new output directory. Leave off --decisions for batches over 1,000. Output includes validated.csv, quality.csv, unsupervised.csv, uncertainty.csv and results.csv; decisions.csv is included only if requested.

## Verification and limits
Python backend executed with the pinned dependencies. Original scoring batch (1,000) matched the prior point forecasts, original risk probabilities, upper 90% interval bounds, actions and costs. Complete dataset (10,000) scored successfully; outcomes-absent upload also scored. Rejection tests passed for missing supplier column, duplicate order, negative demand and future-dated signal. R runtime is unavailable in the authoring environment; app.R received a delimiter balance check but was NOT parsed or executed by R, browser-tested or deployed. Local end-to-end acceptance testing remains necessary. The app runs Python synchronously per session, appropriate for a local proof of concept, not hardened concurrent production. The optional Monte Carlo engine uses the Prescriptive_Analytics independent-order assumptions and limits; estimated savings are not realised savings. No new training is performed. Resilience_Command_Centre will focus on a separate operational command centre, rather than repeating this technical workflow.

## Using the dashboard
Use data/Scoring.csv for open-order forecasts. Use Test.csv for observed outcomes in the Analytics Workflow. Do not upload support tables or prediction exports. The user guide in the reviewer package explains all tabs.
