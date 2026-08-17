source("config.R")
source("R/00_helpers.R")

cat("Running active-purchase county-month tier-share models...\n")

d <- fread(file.path(PATHS$processed, "county_month_fixed_tier_panel_2005_2024.csv"))
d[, month := as.IDate(month)]
d <- d[month >= as.IDate("2017-07-01") & month <= as.IDate("2020-06-01")]

# The panel contains only county-months with positive LRP activity. The five rows
# within each county-month are the fixed coverage alternatives, including zero shares.
d[, `:=`(
  event_month = month_index(month, as.IDate("2019-07-01")),
  linear_month = month_index(month, as.IDate("2019-07-01")),
  post_2019 = as.integer(month >= as.IDate("2019-07-01")),
  total_head_cm = sum(insured_head),
  total_endorsements_cm = sum(endorsements)
), by = .(fips, cattle_type, month)]

rate_lookup <- unique(d[month == as.IDate("2019-07-01"),
                        .(coverage_bin, post_rate_2019 = statutory_rate)])
if (nrow(rate_lookup) != 5L) stop("The July 2019 schedule must contain all five fixed tiers.")
d[rate_lookup, on = "coverage_bin", post_rate_2019 := i.post_rate_2019]
d[, `:=`(
  delta_rate_10pp = (post_rate_2019 - .13) / .10,
  post_delta_10pp = post_2019 * (post_rate_2019 - .13) / .10,
  county_id = factor(fips),
  event_month_factor = factor(event_month)
)]

stopifnot(d[, all(abs(sum(within_month_head_share) - 1) < 1e-9),
              by = .(fips, cattle_type, month)]$V1,
          d[, all(abs(sum(within_month_endorsement_share) - 1) < 1e-9),
              by = .(fips, cattle_type, month)]$V1)

outcomes <- list(
  head_share = list(variable = "within_month_head_share", weight = "total_head_cm"),
  endorsement_share = list(variable = "within_month_endorsement_share", weight = "total_endorsements_cm")
)

models <- list()
rows <- list()
events <- list()
pretrend_tests <- list()
support <- d[, .(positive_head_tiers = sum(insured_head > 0),
                 positive_endorsement_tiers = sum(endorsements > 0)),
             by = .(cattle_type, fips, month)][, .(
  county_months = .N,
  counties = uniqueN(fips),
  share_one_positive_head_tier = mean(positive_head_tiers == 1L),
  share_two_plus_positive_head_tiers = mean(positive_head_tiers >= 2L),
  share_one_positive_endorsement_tier = mean(positive_endorsement_tiers == 1L),
  share_two_plus_positive_endorsement_tiers = mean(positive_endorsement_tiers >= 2L)
), by = cattle_type]

for (ct in c("feeder", "fed")) {
  x <- d[cattle_type == ct]
  if (!nrow(x)) next
  available_pre_months <- sort(unique(x[event_month < 0, event_month]))
  if (!length(available_pre_months)) {
    warning("Skipping ", ct, ": no active pre-reform month is available.")
    next
  }
  reference_month <- max(available_pre_months)
  for (outcome_name in names(outcomes)) {
    outcome <- outcomes[[outcome_name]]$variable
    weight_var <- outcomes[[outcome_name]]$weight
    for (weighting in c("equal_county_month", "volume_weighted")) {
      w <- if (weighting == "equal_county_month") NULL else x[[weight_var]]
      static_formula <- as.formula(paste0(
        outcome,
        " ~ post_delta_10pp + i(coverage_bin, linear_month, ref = 'b95_100') | county_month + county_tier"
      ))
      event_formula <- as.formula(paste0(
        outcome, " ~ i(event_month, delta_rate_10pp, ref = ", reference_month, ") + ",
        "i(coverage_bin, linear_month, ref = 'b95_100') | county_month + county_tier"
      ))
      m_static <- feols(static_formula, data = x, weights = w, cluster = ~fips)
      m_event <- feols(event_formula, data = x, weights = w, cluster = ~fips)
      key <- paste(ct, outcome_name, weighting, sep = "__")
      models[[paste0(key, "__static")]] <- m_static
      models[[paste0(key, "__event")]] <- m_event

      ctab <- coeftable(m_static)
      rows[[length(rows) + 1L]] <- data.table(
        cattle_type = ct,
        outcome = outcome_name,
        weighting = weighting,
        term = "post_delta_10pp",
        coefficient = ctab["post_delta_10pp", 1],
        std_error = ctab["post_delta_10pp", 2],
        p_value = ctab["post_delta_10pp", 4],
        nobs = nobs(m_static),
        counties = uniqueN(x$fips),
        event_reference_month = reference_month
      )

      et <- as.data.table(coeftable(m_event), keep.rownames = "term")
      setnames(et, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
               c("coefficient", "std_error", "statistic", "p_value"), skip_absent = TRUE)
      et <- et[grepl("^event_month::", term)]
      et[, event_month := as.integer(sub("^event_month::(-?[0-9]+):.*$", "\\1", term))]
      et[, `:=`(cattle_type = ct, outcome = outcome_name, weighting = weighting,
                event_reference_month = reference_month)]
      events[[length(events) + 1L]] <- et[, .(cattle_type, outcome, weighting, event_month,
                                             event_reference_month, coefficient, std_error, p_value)]

      b <- coef(m_event)
      event_terms <- names(b)[grepl("^event_month::", names(b))]
      event_times <- as.integer(sub("^event_month::(-?[0-9]+):.*$", "\\1", event_terms))
      pre_terms <- event_terms[event_times <= -2L]
      if (length(pre_terms)) {
        v <- vcov(m_event)[pre_terms, pre_terms, drop = FALSE]
        bb <- b[pre_terms]
        eig <- eigen((v + t(v)) / 2, symmetric = TRUE)
        keep_eig <- eig$values > max(eig$values) * 1e-8
        joint_df <- sum(keep_eig)
        projected_b <- crossprod(eig$vectors[, keep_eig, drop = FALSE], bb)
        joint_stat <- sum(projected_b^2 / eig$values[keep_eig])
        joint_p <- pchisq(joint_stat, df = joint_df, lower.tail = FALSE)
      } else {
        joint_stat <- joint_df <- joint_p <- NA_real_
      }
      pretrend_tests[[length(pretrend_tests) + 1L]] <- data.table(
        cattle_type = ct, outcome = outcome_name, weighting = weighting,
        reference_month = reference_month, pre_months_tested = length(pre_terms),
        chi_square = joint_stat, degrees_freedom = joint_df, p_value = joint_p
      )
    }
  }
}

result_table <- rbindlist(rows, fill = TRUE)
event_table <- rbindlist(events, fill = TRUE)
pretrend_table <- rbindlist(pretrend_tests, fill = TRUE)
fwrite(result_table, file.path(PATHS$results, "03_active_purchase_share_results.csv"))
fwrite(event_table, file.path(PATHS$results, "03_active_purchase_share_event_coefficients.csv"))
fwrite(pretrend_table, file.path(PATHS$results, "03_active_purchase_share_pretrend_tests.csv"))
fwrite(support, file.path(PATHS$results, "03_active_purchase_share_support.csv"))
saveRDS(models, file.path(PATHS$results, "03_active_purchase_share_models.rds"))

print(support)
print(result_table)
print(pretrend_table)
cat("Saved active-purchase tier-share results.\n")
