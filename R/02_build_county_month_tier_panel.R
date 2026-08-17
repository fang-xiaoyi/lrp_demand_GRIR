source("config.R")
source("R/00_helpers.R")

cat("Building county-month-fixed-tier panel...\n")
d <- fread(PATHS$endorsements, select = c(
  "endorsement_id_full", "policy_effective_date", "fips", "state_fips", "state_abbr",
  "cattle_type", "cov_level_pct", "net_head", "liab_amt", "subsidy_amt", "total_prem_amt"
))
d[, policy_effective_date := as.IDate(policy_effective_date)]
d <- d[policy_effective_date >= SAMPLE$start & policy_effective_date <= SAMPLE$end &
         cattle_type %chin% c("feeder", "fed") & fips > 1000 & fips < 99000 &
         net_head > 0 & total_prem_amt > 0 & cov_level_pct >= .70 & cov_level_pct <= 1]
d[, `:=`(month = as.IDate(format(policy_effective_date, "%Y-%m-01")),
         coverage_bin = assign_bin(cov_level_pct))]
d <- d[!is.na(coverage_bin)]

cells <- d[, .(
  insured_head = sum(net_head, na.rm = TRUE),
  endorsements = .N,
  liability = sum(liab_amt, na.rm = TRUE),
  subsidy = sum(subsidy_amt, na.rm = TRUE),
  total_premium = sum(total_prem_amt, na.rm = TRUE)
), by = .(fips, state_fips, state_abbr, cattle_type, month, coverage_bin)]

active <- unique(cells[, .(fips, state_fips, state_abbr, cattle_type, month)])
active[, cross_key___ := 1L]
bins_for_join <- copy(coverage_bins)[, cross_key___ := 1L]
panel <- merge(active, bins_for_join, by = "cross_key___", allow.cartesian = TRUE)
panel[, cross_key___ := NULL]
panel <- merge(panel, cells, by = c("fips", "state_fips", "state_abbr", "cattle_type", "month", "coverage_bin"),
               all.x = TRUE)
for (v in c("insured_head", "endorsements", "liability", "subsidy", "total_premium")) {
  set(panel, which(is.na(panel[[v]])), v, 0)
}
panel <- attach_schedule(panel, "month")
panel[, `:=`(
  subsidy_rate_10pp = statutory_rate / .10,
  county_month = interaction(fips, cattle_type, month, drop = TRUE),
  county_tier = interaction(fips, cattle_type, coverage_bin, drop = TRUE),
  tier_calmonth = interaction(coverage_bin, format(month, "%m"), drop = TRUE),
  within_month_head_share = insured_head / sum(insured_head),
  within_month_endorsement_share = endorsements / sum(endorsements)
), by = .(fips, cattle_type, month)]
setorder(panel, cattle_type, fips, month, bin_order)

fwrite(panel, file.path(PATHS$processed, "county_month_fixed_tier_panel_2005_2024.csv"))

summary <- panel[, .(
  observations = .N, counties = uniqueN(fips), active_county_months = uniqueN(county_month),
  zero_head_share = mean(insured_head == 0), zero_endorsement_share = mean(endorsements == 0),
  total_head = sum(insured_head), total_endorsements = sum(endorsements)
), by = cattle_type]
fwrite(summary, file.path(PATHS$results, "02_panel_summary.csv"))

monthly <- panel[, .(insured_head = sum(insured_head), endorsements = sum(endorsements)),
                 by = .(cattle_type, month, coverage_bin, coverage_label, bin_order)]
monthly[, `:=`(head_share = insured_head / sum(insured_head),
               endorsement_share = endorsements / sum(endorsements)),
        by = .(cattle_type, month)]
fwrite(monthly, file.path(PATHS$results, "02_monthly_tier_shares.csv"))

p <- ggplot(monthly[cattle_type == "feeder"], aes(month, head_share, color = coverage_label)) +
  geom_line(linewidth = .65) + scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = "Share of insured head", color = "Fixed coverage bin",
       title = "Feeder-cattle insured-head allocation across fixed coverage bins") +
  theme_minimal(base_size = 11)
save_plot(p, "02_feeder_monthly_head_shares", 10, 6)
cat("Saved panel:", nrow(panel), "rows.\n")
