if (!file.exists("GRIR_statutory_tier_pilot.Rproj")) {
  stop("Open GRIR_statutory_tier_pilot.Rproj before running RUN_PILOT.R.")
}
source("config.R")
scripts <- c(
  "R/01_validate_statutory_rates.R",
  "R/02_build_county_month_tier_panel.R",
  "R/03_run_active_purchase_share_models.R",
  "R/04_setups.R",
  "R/05_PPML_count_models.R",
  "R/06_identification_grid.R",
  "R/07_build_full_county_month_panel.R",
  "R/08_full_county_month_models.R",
  "R/09_active_tier_stacked_design.R"
)
log_file <- file.path(PATHS$logs, "RUN_PILOT.log")
con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(con, type = "output"); sink(con, type = "message")
on.exit({sink(type = "message"); sink(type = "output"); close(con)}, add = TRUE)
cat("Statutory-tier pilot started:", format(Sys.time()), "\n")
for (script in scripts) {
  cat("\n=====", script, "=====\n")
  source(script, local = new.env(parent = globalenv()))
}
cat("\nPilot completed:", format(Sys.time()), "\n")
