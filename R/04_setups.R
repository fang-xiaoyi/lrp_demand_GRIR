source("config.R")
source("R/00_helpers.R")

cat("Comparing tier-choice setups (tables only)...\n")

d <- fread(file.path(PATHS$processed, "county_month_fixed_tier_panel_2005_2024.csv"))
d[, month := as.IDate(month)]
d <- d[month >= as.IDate("2017-07-01") & month <= as.IDate("2020-06-01")]
d[, `:=`(
  linear_month = month_index(month, as.IDate("2019-07-01")),
  post_2019 = as.integer(month >= as.IDate("2019-07-01")),
  total_head_cm = sum(insured_head),
  total_endorsements_cm = sum(endorsements),
  cal_month = format(month, "%m")
), by = .(fips, cattle_type, month)]

rate_lookup <- unique(d[month == as.IDate("2019-07-01"),
                        .(coverage_bin, post_rate_2019 = statutory_rate)])
d[rate_lookup, on = "coverage_bin", post_rate_2019 := i.post_rate_2019]
d[, `:=`(
  delta_rate_10pp = (post_rate_2019 - .13) / .10,
  post_delta_10pp = post_2019 * (post_rate_2019 - .13) / .10,
  tier_month_of_year = interaction(coverage_bin, cal_month, drop = TRUE)
)]

outcomes <- list(
  head_share = list(y = "within_month_head_share", weight = "total_head_cm"),
  endorsement_share = list(y = "within_month_endorsement_share", weight = "total_endorsements_cm")
)

models <- list()
results <- list()

extract_term <- function(model, term, metadata) {
  tab <- coeftable(model)
  if (!term %in% rownames(tab)) {
    return(cbind(metadata, data.table(term = term, coefficient = NA_real_, std_error = NA_real_,
                                      p_value = NA_real_, nobs = nobs(model), status = "term_not_identified")))
  }
  cbind(metadata, data.table(term = term, coefficient = tab[term, 1], std_error = tab[term, 2],
                             p_value = tab[term, 4], nobs = nobs(model), status = "estimated"))
}

for (ct in c("feeder", "fed")) {
  x <- d[cattle_type == ct]
  for (outcome_name in names(outcomes)) {
    y <- outcomes[[outcome_name]]$y
    weight_var <- outcomes[[outcome_name]]$weight
    for (weighting in c("equal_county_month", "volume_weighted")) {
      w <- if (weighting == "equal_county_month") NULL else x[[weight_var]]
      formulas <- list(
        seasonal_fe = as.formula(paste0(
          y, " ~ post_delta_10pp | county_month + county_tier + tier_month_of_year")),
        seasonal_fe_linear_tier_trend = as.formula(paste0(
          y, " ~ post_delta_10pp + i(coverage_bin, linear_month, ref = 'b95_100') | ",
          "county_month + county_tier + tier_month_of_year"))
      )
      for (setup in names(formulas)) {
        m <- feols(formulas[[setup]], data = x, weights = w, cluster = ~fips)
        key <- paste(ct, outcome_name, weighting, setup, sep = "__")
        models[[key]] <- m
        results[[length(results) + 1L]] <- extract_term(
          m, "post_delta_10pp",
          data.table(cattle_type = ct, outcome = outcome_name, weighting = weighting,
                     setup = setup, counties = uniqueN(x$fips))
        )
      }
    }
  }
}

# Year-over-year share changes require the same county-tier to be active in the
# current month and 12 months earlier. The reform-year change is compared with
# the preceding placebo-year change.
lagged <- copy(d)
lagged[, month := as.IDate(seq(as.Date(month), by = "12 months", length.out = 2L)[2L]), by = seq_len(nrow(lagged))]
setnames(lagged,
         c("within_month_head_share", "within_month_endorsement_share", "total_head_cm", "total_endorsements_cm"),
         c("lag_head_share", "lag_endorsement_share", "lag_total_head_cm", "lag_total_endorsements_cm"))
keep_lag <- c("fips", "cattle_type", "month", "coverage_bin", "lag_head_share",
              "lag_endorsement_share", "lag_total_head_cm", "lag_total_endorsements_cm")
yoy <- merge(d, lagged[, ..keep_lag], by = c("fips", "cattle_type", "month", "coverage_bin"))
yoy <- yoy[month >= as.IDate("2018-07-01") & month <= as.IDate("2020-06-01")]
yoy[, `:=`(
  change_period = factor(ifelse(month < as.IDate("2019-07-01"), "placebo", "reform"),
                         levels = c("placebo", "reform")),
  reform_delta_10pp = as.integer(month >= as.IDate("2019-07-01")) * delta_rate_10pp,
  delta_head_share = within_month_head_share - lag_head_share,
  delta_endorsement_share = within_month_endorsement_share - lag_endorsement_share,
  avg_head_volume = (total_head_cm + lag_total_head_cm) / 2,
  avg_endorsement_volume = (total_endorsements_cm + lag_total_endorsements_cm) / 2,
  county_change_month = interaction(fips, cattle_type, month, drop = TRUE),
  tier_month_of_year = interaction(coverage_bin, format(month, "%m"), drop = TRUE)
)]

yoy_outcomes <- list(
  head_share = list(y = "delta_head_share", weight = "avg_head_volume"),
  endorsement_share = list(y = "delta_endorsement_share", weight = "avg_endorsement_volume")
)
for (ct in c("feeder", "fed")) {
  x <- yoy[cattle_type == ct]
  if (!nrow(x)) next
  for (outcome_name in names(yoy_outcomes)) {
    y <- yoy_outcomes[[outcome_name]]$y
    for (weighting in c("equal_county_month", "volume_weighted")) {
      if (ct == "fed") {
        results[[length(results) + 1L]] <- data.table(
          cattle_type = ct, outcome = outcome_name, weighting = weighting,
          setup = "year_over_year_change", counties = uniqueN(x$fips),
          term = "reform_delta_10pp", coefficient = NA_real_, std_error = NA_real_,
          p_value = NA_real_, nobs = NA_integer_,
          status = "not_identified: fed overlap sample is all fixed-effect singletons"
        )
        next
      }
      w <- if (weighting == "equal_county_month") NULL else x[[yoy_outcomes[[outcome_name]]$weight]]
      f <- as.formula(paste0(
        y, " ~ reform_delta_10pp | county_change_month + county_tier + tier_month_of_year"))
      key <- paste(ct, outcome_name, weighting, "year_over_year_change", sep = "__")
      m <- tryCatch(feols(f, data = x, weights = w, cluster = ~fips), error = identity)
      if (inherits(m, "error")) {
        results[[length(results) + 1L]] <- data.table(
          cattle_type = ct, outcome = outcome_name, weighting = weighting,
          setup = "year_over_year_change", counties = uniqueN(x$fips),
          term = "reform_delta_10pp", coefficient = NA_real_, std_error = NA_real_,
          p_value = NA_real_, nobs = NA_integer_, status = paste0("not_identified: ", conditionMessage(m))
        )
        next
      }
      models[[key]] <- m
      results[[length(results) + 1L]] <- extract_term(
        m, "reform_delta_10pp",
        data.table(cattle_type = ct, outcome = outcome_name, weighting = weighting,
                   setup = "year_over_year_change", counties = uniqueN(x$fips))
      )
    }
  }
}

result_table <- rbindlist(results, fill = TRUE)
support <- rbindlist(list(
  d[, .(setup = "level_share", observations = .N, county_months = uniqueN(county_month),
         counties = uniqueN(fips)), by = cattle_type],
  yoy[, .(setup = "year_over_year_change", observations = .N,
           county_months = uniqueN(county_change_month), counties = uniqueN(fips)), by = cattle_type]
), fill = TRUE)

fwrite(result_table, file.path(PATHS$results, "04_setup_comparison.csv"))
fwrite(support, file.path(PATHS$results, "04_setup_support.csv"))
saveRDS(models, file.path(PATHS$results, "04_setup_models.rds"))
print(support)
print(result_table)
cat("Saved setup comparison; no figures generated.\n")
