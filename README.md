# Maturity-Matched LRP-Option Analysis

This folder contains the updated maturity- and moneyness-matched LRP-option analysis used in the revised manuscript.

## To Run

Open the R project first:

```text
maturity_matched_reanalysis/maturity_matched_reanalysis.Rproj
```

Then run the manuscript scripts in the `R/` folder in the order listed below. The scripts are written to be run from this R project directory. If a script is run from another working directory, it may stop with a message asking the user to open the `maturity_matched_reanalysis` R project.

The run order for the current manuscript is:

```text
R/00_rebuild_maturity_matching_full_history.R
R/01_calculate_matched_iv.R
R/02_build_analysis_panels.R
R/03_descriptive_outputs.R
R/04_build_iv_curves_fpca.R
R/05_run_var_irf.R
R/06_run_lrp_to_pc_local_projections.R
R/08_run_shock_specification_robustness.R
R/11_run_direct_iv_metric_lps.R
R/14_cell_descriptive_outputs.R
R/18_cell_mechanical_premium_channel.R
R/19_cell_bin_heterogeneity_figures_v2.R
R/20_run_direct_cell_main_models.R
R/21_cell_policy_event_placebo_tests.R
R/22_make_submission_tables_figures.R
R/23_make_appendix_tables_figures.R
R/25_close_contract_subanalysis.R
R/26_mechanism_closeness_premium_channel.R
```

Some later scripts use outputs created by earlier scripts, so the numerical order matters.

Scripts that were exploratory or superseded during revision are stored in:

```text
R/archive_not_used_current_draft/
```

Those archived scripts are kept for record-keeping but are not needed to reproduce the current manuscript tables and figures.

## Source Files

The source-file map is stored in:

```text
data_raw/source_paths.csv
```

The paths in that file are relative to this project folder. The current source inputs are:

| Key | Relative path | Description |
|---|---|---|
| `matched_endorsements` | `../full data/maturity matching/lrp_option_maturity_matched_endorsements.csv` | Endorsement-level LRP-option maturity-matched data |
| `monthly_maturity_panel` | `../full data/maturity matching/monthly_maturity_bucket_panel.csv` | Existing monthly maturity-bucket panel from the maturity-matching scripts |
| `futures_curve_long` | `../full data/Futures_curve/futures_curve_long.csv` | CME futures curve data in long format |
| `options_with_futures_curve` | `../full data/Futures_curve/options_with_futures_curve.csv` | CME option trades matched to futures curve prices |
| `treasury_3m` | `../t_bill_3m_2001on.csv` | Three-month Treasury rate data used in implied-volatility construction |
| `vix` | `../VIXCLS.csv` | Daily VIX data used for market volatility controls in market-level robustness checks |
| `jcm_helpers` | `../JCM_revised_R_pipeline/jcm_helpers.R` | Helper functions, including Black-76 implied-volatility routines |

The mechanical premium-channel scripts also use:

```text
../lrp_policy_subsidy_schedule.csv
```

This file records the LRP subsidy schedule used to construct policy-induced producer-premium cuts. In the current version, the July 2019 to June 2020 reform is coded as a flat 20 percent subsidy rate across coverage levels, and the July 2020 expansion is coded as tiered rates by coverage level.

The full-history matching rebuild also uses the annual LRP endorsement files in:

```text
../full data/lrp_csv/
```

## Main Outputs

Processed analysis data are written to:

```text
data_processed/
```

Regression tables, summaries, and generated LaTeX tables are written to:

```text
outputs/
```

Generated figures are written to:

```text
figures/
```


