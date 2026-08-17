source("config.R")
source("R/00_helpers.R")

cat("Building full county-month risk-set panels...\n")

raw <- fread(PATHS$endorsements, select = c(
  "endorsement_id_full", "policy_effective_date", "fips", "state_fips", "state_abbr",
  "cattle_type", "cov_level_pct", "net_head", "liab_amt", "subsidy_amt", "total_prem_amt"
))
raw[, policy_effective_date := as.IDate(policy_effective_date)]
raw <- raw[
  policy_effective_date >= SAMPLE$start & policy_effective_date <= SAMPLE$end &
    cattle_type %chin% c("feeder", "fed") &
    fips > 1000 & fips < 99000 &
    net_head > 0 & total_prem_amt > 0 &
    cov_level_pct >= .70 & cov_level_pct <= 1
]
raw[, `:=`(
  month = as.IDate(format(policy_effective_date, "%Y-%m-01")),
  coverage_bin = assign_bin(cov_level_pct)
)]
raw <- raw[!is.na(coverage_bin)]

months <- data.table(month = seq(as.IDate(format(SAMPLE$start, "%Y-%m-01")),
                                 as.IDate(format(SAMPLE$end, "%Y-%m-01")),
                                 by = "1 month"))

# Risk set: counties that ever appear in valid LRP endorsements during the sample.
# This is a transparent LRP-county universe. It can later be replaced by a NASS
# cattle-county universe if we want broader nonuser counties.
risk_counties <- unique(raw[, .(fips, state_fips, state_abbr, cattle_type)])
risk_counties[, cross_key___ := 1L]
months[, cross_key___ := 1L]
county_month <- merge(risk_counties, months, by = "cross_key___", allow.cartesian = TRUE)
county_month[, cross_key___ := NULL]

cm_cells <- raw[, .(
  endorsements = .N,
  insured_head = sum(net_head, na.rm = TRUE),
  liability = sum(liab_amt, na.rm = TRUE),
  subsidy = sum(subsidy_amt, na.rm = TRUE),
  total_premium = sum(total_prem_amt, na.rm = TRUE),
  cov_level_avg = weighted.mean(cov_level_pct, net_head, na.rm = TRUE),
  share_b70_79_head = sum(net_head[coverage_bin == "b70_79"], na.rm = TRUE) / sum(net_head, na.rm = TRUE),
  share_b80_84_head = sum(net_head[coverage_bin == "b80_84"], na.rm = TRUE) / sum(net_head, na.rm = TRUE),
  share_b85_89_head = sum(net_head[coverage_bin == "b85_89"], na.rm = TRUE) / sum(net_head, na.rm = TRUE),
  share_b90_94_head = sum(net_head[coverage_bin == "b90_94"], na.rm = TRUE) / sum(net_head, na.rm = TRUE),
  share_b95_100_head = sum(net_head[coverage_bin == "b95_100"], na.rm = TRUE) / sum(net_head, na.rm = TRUE)
), by = .(fips, state_fips, state_abbr, cattle_type, month)]

county_month <- merge(
  county_month, cm_cells,
  by = c("fips", "state_fips", "state_abbr", "cattle_type", "month"),
  all.x = TRUE
)
for (v in c("endorsements", "insured_head", "liability", "subsidy", "total_premium")) {
  set(county_month, which(is.na(county_month[[v]])), v, 0)
}
for (v in c("share_b70_79_head", "share_b80_84_head", "share_b85_89_head",
            "share_b90_94_head", "share_b95_100_head")) {
  set(county_month, which(is.na(county_month[[v]])), v, 0)
}
county_month[, `:=`(
  any_lrp = as.integer(endorsements > 0),
  log1p_head = log1p(insured_head),
  log1p_endorsements = log1p(endorsements),
  log1p_liability = log1p(liability),
  county_id = interaction(fips, cattle_type, drop = TRUE),
  month_fe = factor(month),
  calendar_month = format(month, "%m")
)]

base <- raw[month >= as.IDate("2017-07-01") & month <= as.IDate("2019-06-01"),
            .(base_head = sum(net_head), base_endorsements = .N),
            by = .(fips, cattle_type, coverage_bin)]
base[, `:=`(
  total_base_head = sum(base_head),
  total_base_endorsements = sum(base_endorsements)
), by = .(fips, cattle_type)]
base[, `:=`(
  base_head_share = fifelse(total_base_head > 0, base_head / total_base_head, 0),
  base_endorsement_share = fifelse(total_base_endorsements > 0, base_endorsements / total_base_endorsements, 0)
)]

rate_month <- CJ(month = months$month, coverage_bin = coverage_bins$coverage_bin)
rate_month <- attach_schedule(rate_month, "month")
rate_month[, rate_delta_from_pre2019_10pp := (statutory_rate - .13) / .10]

pred <- merge(base, rate_month, by = "coverage_bin", allow.cartesian = TRUE)
pred[, `:=`(
  pred_head_exposure_10pp = sum(base_head_share * rate_delta_from_pre2019_10pp),
  pred_endorsement_exposure_10pp = sum(base_endorsement_share * rate_delta_from_pre2019_10pp)
), by = .(fips, cattle_type, month)]
pred <- unique(pred[, .(fips, cattle_type, month,
                        pred_head_exposure_10pp, pred_endorsement_exposure_10pp)])

county_month[pred, on = .(fips, cattle_type, month),
             `:=`(pred_head_exposure_10pp = i.pred_head_exposure_10pp,
                  pred_endorsement_exposure_10pp = i.pred_endorsement_exposure_10pp)]
county_month[is.na(pred_head_exposure_10pp), `:=`(
  pred_head_exposure_10pp = 0,
  pred_endorsement_exposure_10pp = 0
)]

tier_cells <- raw[, .(
  endorsements = .N,
  insured_head = sum(net_head, na.rm = TRUE),
  liability = sum(liab_amt, na.rm = TRUE),
  subsidy = sum(subsidy_amt, na.rm = TRUE),
  total_premium = sum(total_prem_amt, na.rm = TRUE)
), by = .(fips, state_fips, state_abbr, cattle_type, month, coverage_bin)]

county_month[, cross_key___ := 1L]
bins <- copy(coverage_bins)[, cross_key___ := 1L]
county_month_tier <- merge(
  county_month[, .(fips, state_fips, state_abbr, cattle_type, month, cross_key___)],
  bins, by = "cross_key___", allow.cartesian = TRUE
)
county_month[, cross_key___ := NULL]
county_month_tier[, cross_key___ := NULL]
county_month_tier <- merge(
  county_month_tier, tier_cells,
  by = c("fips", "state_fips", "state_abbr", "cattle_type", "month", "coverage_bin"),
  all.x = TRUE
)
for (v in c("endorsements", "insured_head", "liability", "subsidy", "total_premium")) {
  set(county_month_tier, which(is.na(county_month_tier[[v]])), v, 0)
}
county_month_tier <- attach_schedule(county_month_tier, "month")
county_month_tier[, `:=`(
  subsidy_rate_10pp = statutory_rate / .10,
  county_month = interaction(fips, cattle_type, month, drop = TRUE),
  county_tier = interaction(fips, cattle_type, coverage_bin, drop = TRUE),
  tier_calmonth = interaction(coverage_bin, format(month, "%m"), drop = TRUE)
)]

setorder(county_month, cattle_type, fips, month)
setorder(county_month_tier, cattle_type, fips, month, bin_order)

fwrite(county_month, file.path(PATHS$processed, "full_county_month_risk_panel_2005_2024.csv"))
fwrite(county_month_tier, file.path(PATHS$processed, "full_county_month_tier_panel_2005_2024.csv"))

support <- county_month[, .(
  observations = .N,
  counties = uniqueN(fips),
  months = uniqueN(month),
  active_county_months = sum(any_lrp),
  active_share = mean(any_lrp),
  total_head = sum(insured_head),
  total_endorsements = sum(endorsements),
  risk_set = "ever-valid-LRP county by cattle type, expanded monthly"
), by = cattle_type]
fwrite(support, file.path(PATHS$results, "07_full_county_month_panel_support.csv"))

cat("Wrote full county-month and county-month-tier risk-set panels.\n")
