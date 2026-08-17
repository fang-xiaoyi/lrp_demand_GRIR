source("config.R")
source("R/00_helpers.R")

cat("Running predefined identification grid...\n")

d <- fread(file.path(PATHS$processed, "county_month_fixed_tier_panel_2005_2024.csv"))
d[, month := as.IDate(month)]
d <- d[month >= as.IDate("2016-07-01") & month <= as.IDate("2021-06-01")]
d[, `:=`(
  cal_month = format(month, "%m"),
  tier_month_of_year = interaction(coverage_bin, format(month, "%m"), drop = TRUE),
  subsidy_rate_10pp = statutory_rate / .10,
  county_month = interaction(fips, cattle_type, month, drop = TRUE),
  county_tier = interaction(fips, cattle_type, coverage_bin, drop = TRUE),
  total_head_cm = sum(insured_head),
  total_endorsements_cm = sum(endorsements)
), by = .(fips, cattle_type, month)]

schedule_events <- data.table(
  event_name = c("pm19_coverage_dependent", "pm20_coverage_expansion"),
  event_date = as.IDate(c("2019-07-01", "2020-07-01")),
  pre_rate = c(.13, NA_real_)
)

extract_term <- function(model, term, metadata) {
  tab <- coeftable(model)
  if (!term %in% rownames(tab)) {
    return(cbind(metadata, data.table(
      term = term, coefficient = NA_real_, std_error = NA_real_,
      p_value = NA_real_, nobs = nobs(model), status = "term_not_identified"
    )))
  }
  cbind(metadata, data.table(
    term = term,
    coefficient = tab[term, 1],
    std_error = tab[term, 2],
    p_value = tab[term, 4],
    nobs = nobs(model),
    status = "estimated"
  ))
}

joint_pretrend <- function(model, prefix_regex, time_regex = NULL) {
  b <- coef(model)
  terms <- names(b)[grepl(prefix_regex, names(b))]
  if (!length(terms)) {
    return(data.table(pre_terms = 0L, chi_square = NA_real_, degrees_freedom = NA_integer_,
                      pretrend_p_value = NA_real_))
  }
  if (!is.null(time_regex)) {
    times <- suppressWarnings(as.integer(sub(time_regex, "\\1", terms)))
    terms <- terms[!is.na(times) & times <= -2L]
  }
  if (!length(terms)) {
    return(data.table(pre_terms = 0L, chi_square = NA_real_, degrees_freedom = NA_integer_,
                      pretrend_p_value = NA_real_))
  }
  v <- vcov(model)[terms, terms, drop = FALSE]
  bb <- b[terms]
  eig <- eigen((v + t(v)) / 2, symmetric = TRUE)
  keep <- eig$values > max(eig$values) * 1e-8
  if (!any(keep)) {
    return(data.table(pre_terms = length(terms), chi_square = NA_real_,
                      degrees_freedom = NA_integer_, pretrend_p_value = NA_real_))
  }
  projected_b <- crossprod(eig$vectors[, keep, drop = FALSE], bb)
  stat <- sum(projected_b^2 / eig$values[keep])
  data.table(pre_terms = length(terms), chi_square = stat,
             degrees_freedom = sum(keep), pretrend_p_value = pchisq(stat, sum(keep), lower.tail = FALSE))
}

run_feols <- function(formula, data, term, metadata, weights = NULL) {
  m <- tryCatch(feols(formula, data = data, weights = weights, cluster = ~fips), error = identity)
  if (inherits(m, "error")) {
    return(list(model = NULL, result = cbind(metadata, data.table(
      term = term, coefficient = NA_real_, std_error = NA_real_, p_value = NA_real_,
      nobs = NA_integer_, status = paste0("not_estimated: ", conditionMessage(m))
    ))))
  }
  list(model = m, result = extract_term(m, term, metadata))
}

run_fepois <- function(formula, data, term, metadata) {
  m <- tryCatch(fepois(formula, data = data, cluster = ~fips), error = identity)
  if (inherits(m, "error")) {
    return(list(model = NULL, result = cbind(metadata, data.table(
      term = term, coefficient = NA_real_, std_error = NA_real_, p_value = NA_real_,
      nobs = NA_integer_, status = paste0("not_estimated: ", conditionMessage(m))
    ))))
  }
  list(model = m, result = extract_term(m, term, metadata))
}

models <- list()
results <- list()
pretrends <- list()
support <- list()

outcome_specs <- list(
  head_share = list(y = "within_month_head_share", family = "ols", weight = "total_head_cm"),
  endorsement_share = list(y = "within_month_endorsement_share", family = "ols", weight = "total_endorsements_cm"),
  insured_head = list(y = "insured_head", family = "ppml", weight = NULL),
  endorsements = list(y = "endorsements", family = "ppml", weight = NULL)
)

for (ct in c("feeder", "fed")) {
  x_ct <- d[cattle_type == ct]
  if (!nrow(x_ct)) next

  for (event_i in seq_len(nrow(schedule_events))) {
    ev <- schedule_events[event_i]
    ev_date <- ev$event_date
    ev_name <- ev$event_name

    rates_pre <- unique(d[month == as.IDate(seq(as.Date(ev_date), by = "-1 month", length.out = 2L)[2L]),
                          .(coverage_bin, pre_rate = statutory_rate)])
    rates_post <- unique(d[month == ev_date, .(coverage_bin, post_rate = statutory_rate)])
    rate_change <- merge(rates_pre, rates_post, by = "coverage_bin", all = FALSE)
    rate_change[, delta_rate_10pp := (post_rate - pre_rate) / .10]

    for (window in c(6L, 12L)) {
      x <- copy(x_ct[month >= as.IDate(seq(as.Date(ev_date), by = paste0("-", window, " months"), length.out = 2L)[2L]) &
                       month <= as.IDate(seq(as.Date(ev_date), by = paste0(window - 1L, " months"), length.out = 2L)[2L])])
      if (!nrow(x)) next
      x[rate_change, on = "coverage_bin", delta_rate_10pp := i.delta_rate_10pp]
      x[, `:=`(
        event_month = month_index(month, ev_date),
        post_event = as.integer(month >= ev_date),
        post_delta_10pp = as.integer(month >= ev_date) * delta_rate_10pp
      )]
      support[[length(support) + 1L]] <- x[, .(
        design = "local_reform_window",
        event_name = ev_name,
        window_months = window,
        observations = .N,
        counties = uniqueN(fips),
        county_months = uniqueN(county_month),
        zero_head_share = mean(insured_head == 0),
        zero_endorsement_share = mean(endorsements == 0)
      ), by = cattle_type]

      for (outcome_name in names(outcome_specs)) {
        spec <- outcome_specs[[outcome_name]]
        metadata <- data.table(cattle_type = ct, outcome = outcome_name,
                               design = "local_reform_window", event_name = ev_name,
                               window_months = window, weighting = "unweighted")
        f <- as.formula(paste0(spec$y, " ~ post_delta_10pp | county_month + county_tier + tier_month_of_year"))
        fit <- if (spec$family == "ppml") {
          run_fepois(f, x, "post_delta_10pp", metadata)
        } else {
          run_feols(f, x, "post_delta_10pp", metadata)
        }
        results[[length(results) + 1L]] <- fit$result
        if (!is.null(fit$model)) {
          key <- paste(ct, outcome_name, "local", ev_name, window, sep = "__")
          models[[key]] <- fit$model
        }
      }
    }
  }
}

stacked <- list()
for (ct in c("feeder", "fed")) {
  x_ct <- d[cattle_type == ct]
  for (event_i in seq_len(nrow(schedule_events))) {
    ev <- schedule_events[event_i]
    ev_date <- ev$event_date
    rates_pre <- unique(d[month == as.IDate(seq(as.Date(ev_date), by = "-1 month", length.out = 2L)[2L]),
                          .(coverage_bin, pre_rate = statutory_rate)])
    rates_post <- unique(d[month == ev_date, .(coverage_bin, post_rate = statutory_rate)])
    rate_change <- merge(rates_pre, rates_post, by = "coverage_bin", all = FALSE)
    rate_change[, delta_rate_10pp := (post_rate - pre_rate) / .10]
    x <- copy(x_ct[month >= as.IDate(seq(as.Date(ev_date), by = "-12 months", length.out = 2L)[2L]) &
                     month <= as.IDate(seq(as.Date(ev_date), by = "11 months", length.out = 2L)[2L])])
    if (!nrow(x)) next
    x[rate_change, on = "coverage_bin", delta_rate_10pp := i.delta_rate_10pp]
    x[, `:=`(
      event_name = ev$event_name,
      event_month = month_index(month, ev_date),
      post_event = as.integer(month >= ev_date),
      post_delta_10pp = as.integer(month >= ev_date) * delta_rate_10pp,
      stack_county_month = interaction(ev$event_name, fips, cattle_type, month, drop = TRUE),
      stack_county_tier = interaction(ev$event_name, fips, cattle_type, coverage_bin, drop = TRUE),
      stack_tier_calmonth = interaction(ev$event_name, coverage_bin, cal_month, drop = TRUE)
    )]
    stacked[[length(stacked) + 1L]] <- x
  }
}
stacked <- rbindlist(stacked, fill = TRUE)

if (nrow(stacked)) {
  support[[length(support) + 1L]] <- stacked[, .(
    design = "stacked_two_reforms",
    event_name = "pm19_plus_pm20",
    window_months = 12L,
    observations = .N,
    counties = uniqueN(fips),
    county_months = uniqueN(stack_county_month),
    zero_head_share = mean(insured_head == 0),
    zero_endorsement_share = mean(endorsements == 0)
  ), by = cattle_type]

  for (ct in c("feeder", "fed")) {
    x <- stacked[cattle_type == ct]
    for (outcome_name in names(outcome_specs)) {
      spec <- outcome_specs[[outcome_name]]
      metadata <- data.table(cattle_type = ct, outcome = outcome_name,
                             design = "stacked_two_reforms", event_name = "pm19_plus_pm20",
                             window_months = 12L, weighting = "unweighted")
      f <- as.formula(paste0(spec$y, " ~ post_delta_10pp | stack_county_month + stack_county_tier + stack_tier_calmonth"))
      fit <- if (spec$family == "ppml") {
        run_fepois(f, x, "post_delta_10pp", metadata)
      } else {
        run_feols(f, x, "post_delta_10pp", metadata)
      }
      results[[length(results) + 1L]] <- fit$result

      if (!is.null(fit$model)) {
        key <- paste(ct, outcome_name, "stacked_two_reforms", sep = "__")
        models[[key]] <- fit$model
      }

      pre_ref <- -1L
      event_formula <- as.formula(paste0(
        spec$y, " ~ i(event_month, delta_rate_10pp, ref = ", pre_ref, ") | ",
        "stack_county_month + stack_county_tier + stack_tier_calmonth"
      ))
      m_event <- tryCatch(
        if (spec$family == "ppml") fepois(event_formula, data = x, cluster = ~fips) else feols(event_formula, data = x, cluster = ~fips),
        error = identity
      )
      if (inherits(m_event, "error")) {
        pretrends[[length(pretrends) + 1L]] <- cbind(metadata, data.table(
          pre_terms = NA_integer_, chi_square = NA_real_, degrees_freedom = NA_integer_,
          pretrend_p_value = NA_real_, status = paste0("not_estimated: ", conditionMessage(m_event))
        ))
      } else {
        key <- paste(ct, outcome_name, "stacked_two_reforms_event", sep = "__")
        models[[key]] <- m_event
        pretrends[[length(pretrends) + 1L]] <- cbind(
          metadata,
          joint_pretrend(m_event, "^event_month::", "^event_month::(-?[0-9]+):.*$"),
          data.table(status = "estimated")
        )
      }
    }
  }
}

# Predetermined exposure design: baseline county shares predict each county-month's
# schedule-driven subsidy change. This is a scale-margin design over active county-months.
base <- d[month >= as.IDate("2017-07-01") & month <= as.IDate("2019-06-01"),
          .(base_head = sum(insured_head), base_endorsements = sum(endorsements)),
          by = .(fips, cattle_type, coverage_bin)]
base[, `:=`(
  total_base_head = sum(base_head),
  total_base_endorsements = sum(base_endorsements)
), by = .(fips, cattle_type)]
base[, `:=`(
  base_head_share = fifelse(total_base_head > 0, base_head / total_base_head, 0),
  base_endorsement_share = fifelse(total_base_endorsements > 0, base_endorsements / total_base_endorsements, 0)
)]
rate_month <- unique(d[, .(month, coverage_bin, statutory_rate)])
rate_month[, pre2019_rate := .13]
rate_month[, rate_delta_from_pre2019_10pp := (statutory_rate - pre2019_rate) / .10]
pred <- merge(base, rate_month, by = "coverage_bin", allow.cartesian = TRUE)
pred[, `:=`(
  pred_head_exposure_10pp = sum(base_head_share * rate_delta_from_pre2019_10pp),
  pred_endorsement_exposure_10pp = sum(base_endorsement_share * rate_delta_from_pre2019_10pp)
), by = .(fips, cattle_type, month)]
pred <- unique(pred[, .(fips, cattle_type, month, pred_head_exposure_10pp, pred_endorsement_exposure_10pp)])

cm <- d[, .(
  insured_head = sum(insured_head),
  endorsements = sum(endorsements),
  liability = sum(liability)
), by = .(fips, state_fips, state_abbr, cattle_type, month)]
cm[pred, on = .(fips, cattle_type, month),
   `:=`(pred_head_exposure_10pp = i.pred_head_exposure_10pp,
        pred_endorsement_exposure_10pp = i.pred_endorsement_exposure_10pp)]
cm <- cm[!is.na(pred_head_exposure_10pp)]
cm[, `:=`(
  post_2019 = as.integer(month >= as.IDate("2019-07-01")),
  year_month = factor(month),
  county_id = interaction(fips, cattle_type, drop = TRUE)
)]

for (ct in c("feeder", "fed")) {
  x <- cm[cattle_type == ct]
  support[[length(support) + 1L]] <- x[, .(
    design = "predetermined_exposure_scale",
    event_name = "full_schedule",
    window_months = NA_integer_,
    observations = .N,
    counties = uniqueN(fips),
    county_months = .N,
    zero_head_share = mean(insured_head == 0),
    zero_endorsement_share = mean(endorsements == 0)
  ), by = cattle_type]

  scale_outcomes <- c(insured_head = "pred_head_exposure_10pp",
                      endorsements = "pred_endorsement_exposure_10pp")
  for (outcome_name in names(scale_outcomes)) {
    term <- scale_outcomes[[outcome_name]]
    y <- if (outcome_name == "insured_head") "insured_head" else "endorsements"
    metadata <- data.table(cattle_type = ct, outcome = outcome_name,
                           design = "predetermined_exposure_scale", event_name = "full_schedule",
                           window_months = NA_integer_, weighting = "unweighted")
    f <- as.formula(paste0(y, " ~ ", term, " | county_id + year_month"))
    fit <- run_fepois(f, x, term, metadata)
    results[[length(results) + 1L]] <- fit$result
    if (!is.null(fit$model)) {
      key <- paste(ct, outcome_name, "predetermined_exposure_scale", sep = "__")
      models[[key]] <- fit$model
    }
  }
}

result_table <- rbindlist(results, fill = TRUE)
support_table <- rbindlist(support, fill = TRUE)
pretrend_table <- rbindlist(pretrends, fill = TRUE)

fwrite(result_table, file.path(PATHS$results, "06_identification_grid_results.csv"))
fwrite(support_table, file.path(PATHS$results, "06_identification_grid_support.csv"))
fwrite(pretrend_table, file.path(PATHS$results, "06_identification_grid_pretrends.csv"))
saveRDS(models, file.path(PATHS$results, "06_identification_grid_models.rds"))

summary_lines <- c(
  "# Identification grid",
  "",
  "These are diagnostic model runs, not paper tables.",
  "",
  "Designs included:",
  "- Local reform windows around the July 2019 and July 2020 statutory schedule changes.",
  "- A stacked two-reform design that pools both schedule changes.",
  "- A predetermined-exposure county-month scale design using pre-2019 county-tier shares.",
  "",
  "All tier-allocation designs use the official statutory subsidy schedule as treatment.",
  "The current panel contains tier cells inside active county-months, so these diagnostics speak to allocation and scale conditional on active LRP purchase months.",
  "They do not identify producer entry."
)
writeLines(summary_lines, file.path(PATHS$results, "06_IDENTIFICATION_GRID_SUMMARY.md"))

print(support_table)
print(result_table)
print(pretrend_table)
cat("Saved identification-grid results.\n")
