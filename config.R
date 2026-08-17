PROJECT_DIR <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(PROJECT_DIR, "GRIR_statutory_tier_pilot.Rproj"))) {
  stop("Open GRIR_statutory_tier_pilot.Rproj before running the pilot.")
}
WORKSPACE_ROOT <- normalizePath(file.path(PROJECT_DIR, ".."), winslash = "/", mustWork = TRUE)

PATHS <- list(
  endorsements = file.path(WORKSPACE_ROOT, "GRIR_corrected_reanalysis", "data_processed",
                           "full_lrp_endorsements_2005_2026.csv"),
  schedule = file.path(PROJECT_DIR, "data_manual", "lrp_policy_subsidy_schedule.csv"),
  processed = file.path(PROJECT_DIR, "data_processed"),
  results = file.path(PROJECT_DIR, "results"),
  figures = file.path(PROJECT_DIR, "figures"),
  logs = file.path(PROJECT_DIR, "logs")
)
for (d in PATHS[c("processed", "results", "figures", "logs")]) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

SAMPLE <- list(start = as.Date("2005-01-01"), end = as.Date("2024-12-31"))
REFORMS <- list(r2019 = as.Date("2019-07-01"), r2020 = as.Date("2020-07-01"))
EVENT_WINDOW <- 12L
