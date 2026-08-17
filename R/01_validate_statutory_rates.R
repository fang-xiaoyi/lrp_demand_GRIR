source("config.R")
source("R/00_helpers.R")

cat("Validating endorsement-level statutory subsidy rates...\n")
d <- fread(PATHS$endorsements, select = c(
  "endorsement_id_full", "policy_effective_date", "year", "cattle_type",
  "cov_level_pct", "subsidy_amt", "total_prem_amt", "prod_prem_amt", "net_head"
))
d[, policy_effective_date := as.IDate(policy_effective_date)]
d <- d[policy_effective_date >= SAMPLE$start & policy_effective_date <= SAMPLE$end &
         total_prem_amt > 0 & subsidy_amt >= 0 & cov_level_pct >= .70 & cov_level_pct <= 1]
d[, coverage_bin := assign_bin(cov_level_pct)]
d[, observed_rate := subsidy_amt / total_prem_amt]
d[, premium_identity_error := total_prem_amt - subsidy_amt - prod_prem_amt]
d <- attach_schedule(d, "policy_effective_date")
d[, rate_gap := observed_rate - statutory_rate]
d[, rate_matches_1pp := abs(rate_gap) <= .01]

audit <- d[, .(
  endorsements = .N,
  insured_head = sum(net_head, na.rm = TRUE),
  observed_rate_median = median(observed_rate, na.rm = TRUE),
  statutory_rate = first(statutory_rate),
  match_share_1pp = mean(rate_matches_1pp, na.rm = TRUE),
  zero_subsidy_share = mean(subsidy_amt == 0, na.rm = TRUE),
  max_abs_premium_identity_error = max(abs(premium_identity_error), na.rm = TRUE)
), by = .(schedule_start, schedule_end, coverage_bin, cattle_type)]
setorder(audit, cattle_type, schedule_start, coverage_bin)
fwrite(audit, file.path(PATHS$results, "01_statutory_rate_validation.csv"))

exceptions <- d[rate_matches_1pp == FALSE, .(
  endorsement_id_full, policy_effective_date, cattle_type, cov_level_pct, coverage_bin,
  subsidy_amt, total_prem_amt, prod_prem_amt, observed_rate, statutory_rate, rate_gap
)]
fwrite(exceptions, file.path(PATHS$results, "01_statutory_rate_exceptions.csv"))

plot_dt <- d[, .(observed_rate = weighted.mean(observed_rate, net_head, na.rm = TRUE),
                 statutory_rate = first(statutory_rate)),
             by = .(month = as.IDate(format(policy_effective_date, "%Y-%m-01")), coverage_bin)]
p1 <- ggplot(plot_dt, aes(month, observed_rate, color = coverage_bin)) +
  geom_line(linewidth = .65) + geom_step(aes(y = statutory_rate), linetype = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = "Subsidy rate", color = "Fixed coverage bin",
       title = "Observed and statutory LRP subsidy rates",
       subtitle = "Solid: insured-head-weighted observed ratio; dashed: schedule rate") +
  theme_minimal(base_size = 11)
save_plot(p1, "01_observed_vs_statutory_rates", 10, 6)

heat <- d[, .(endorsements = .N),
          by = .(month = as.IDate(format(policy_effective_date, "%Y-%m-01")),
                 coverage_bin, rate = round(observed_rate, 2))]
p2 <- ggplot(heat, aes(month, factor(rate), fill = log1p(endorsements))) +
  geom_tile() + facet_wrap(~coverage_bin, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "Observed subsidy rate", fill = "log(1 + N)",
       title = "Observed subsidy-rate support by fixed coverage bin") +
  theme_minimal(base_size = 10)
save_plot(p2, "01_subsidy_rate_heatmap", 10, 10)

cat("Saved validation results. Overall 1pp match share:",
    sprintf("%.2f%%", 100 * mean(d$rate_matches_1pp)), "\n")
