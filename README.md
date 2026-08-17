# GRIR statutory coverage-tier data project

## Confirmed data construction

- Stable coverage bins over time: 70--79.99, 80--84.99, 85--89.99, 90--94.99, and 95--100 percent.
- Official statutory schedule rate is the treatment.
- Endorsement-level `subsidy / total premium` validates the schedule but does not define treatment.
- Active county-month-tier panel includes zero-demand cells for the five fixed bins in every positive-LRP county-month.
- Full county-month risk-set panel expands all ever-valid-LRP counties by month and keeps inactive county-months as zero-demand observations.
- Feeder and fed cattle are estimated separately; feeder cattle is the main sample.

The active-purchase analysis studies insured-head and endorsement shares across
fixed tiers within active county-months. The full risk-set analysis studies
activity and scale with inactive county-months retained. None of these scripts
generate the paper; they write diagnostic outputs for reviewing the revised
identification strategy.

## Important manual check

The project reads the policy schedule supplied in `data_manual/lrp_policy_subsidy_schedule.csv`. The code verifies that commodity codes 801 and 802 have the same rate for every period and fixed coverage bin before collapsing them to the cattle-type panel schedule.

## How to run

1. Open `GRIR_statutory_tier_pilot.Rproj` in RStudio.
2. Review `data_manual/lrp_policy_subsidy_schedule.csv`.
3. Run `RUN_PILOT.R`.


Run scripts individually if preferred:

1. `R/01_validate_statutory_rates.R`
2. `R/02_build_county_month_tier_panel.R`
3. `R/03_run_active_purchase_share_models.R`
4. `R/04_setups.R`
5. `R/05_PPML_count_models.R`
6. `R/06_identification_grid.R`
7. `R/07_build_full_county_month_panel.R`
8. `R/08_full_county_month_models.R`
9. `R/09_active_tier_stacked_design.R`

Results are written to `results/`, figures to `figures/`, processed data to `data_processed/`, and the run log to `logs/RUN_PILOT.log`.
