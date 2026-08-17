source("config.R")
source("R/00_helpers.R")

cat("Running full county-month risk-set diagnostics...\n")

cm_path <- file.path(PATHS$processed, "full_county_month_risk_panel_2005_2024.csv")
tier_path <- file.path(PATHS$processed, "full_county_month_tier_panel_2005_2024.csv")
if (!file.exists(cm_path) || !file.exists(tier_path)) {
  stop("Run R/07_build_full_county_month_panel.R before this script.")
}

d <- fread(cm_path)
tier_panel <- fread(tier_path)
d[, month := as.IDate(month)]
tier_panel[, month := as.IDate(month)]

d <- d[month >= as.IDate("2017-07-01") & month <= as.IDate("2021-06-01")]
tier_panel <- tier_panel[month >= as.IDate("2017-07-01") & month <= as.IDate("2021-06-01")]

d[, `:=`(
  county_id = interaction(fips, cattle_type, drop = TRUE),
  month_fe = factor(month),
  state_month = interaction(state_fips, cattle_type, month, drop = TRUE)
)]

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

run_feols <- function(formula, data, term, metadata) {
  m <- tryCatch(feols(formula, data = data, cluster = ~fips), error = identity)
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

joint_pretrend <- function(model) {
  b <- coef(model)
  event_terms <- names(b)[grepl("^event_time::", names(b))]
  if (!length(event_terms)) {
    return(data.table(pre_terms = 0L, chi_square = NA_real_,
                      degrees_freedom = NA_integer_, pretrend_p_value = NA_real_))
  }
  event_times <- suppressWarnings(as.integer(sub("^event_time::(-?[0-9]+):.*$", "\\1", event_terms)))
  pre_terms <- event_terms[!is.na(event_times) & event_times <= -2L]
  if (!length(pre_terms)) {
    return(data.table(pre_terms = 0L, chi_square = NA_real_,
                      degrees_freedom = NA_integer_, pretrend_p_value = NA_real_))
  }
  v <- vcov(model)[pre_terms, pre_terms, drop = FALSE]
  bb <- b[pre_terms]
  eig <- eigen((v + t(v)) / 2, symmetric = TRUE)
  keep <- eig$values > max(eig$values) * 1e-8
  if (!any(keep)) {
    return(data.table(pre_terms = length(pre_terms), chi_square = NA_real_,
                      degrees_freedom = NA_integer_, pretrend_p_value = NA_real_))
  }
  projected_b <- crossprod(eig$vectors[, keep, drop = FALSE], bb)
  stat <- sum(projected_b^2 / eig$values[keep])
  data.table(pre_terms = length(pre_terms), chi_square = stat,
             degrees_freedom = sum(keep),
             pretrend_p_value = pchisq(stat, sum(keep), lower.tail = FALSE))
}

descriptive <- d[, .(
  observations = .N,
  counties = uniqueN(fips),
  months = uniqueN(month),
  active_county_months = sum(any_lrp),
  active_share = mean(any_lrp),
  mean_active_head = mean(insured_head[any_lrp == 1]),
  mean_active_endorsements = mean(endorsements[any_lrp == 1]),
  total_head = sum(insured_head),
  total_endorsements = sum(endorsements)
), by = cattle_type]

base_tier <- tier_panel[month >= as.IDate("2017-07-01") & month <= as.IDate("2019-06-01"),
                        .(base_head = sum(insured_head), base_endorsements = sum(endorsements)),
                        by = .(fips, cattle_type, coverage_bin)]
base_tier[, `:=`(
  total_base_head = sum(base_head),
  total_base_endorsements = sum(base_endorsements)
), by = .(fips, cattle_type)]
base_tier[, `:=`(
  base_head_share = fifelse(total_base_head > 0, base_head / total_base_head, 0),
  base_endorsement_share = fifelse(total_base_endorsements > 0, base_endorsements / total_base_endorsements, 0)
)]

event_rate_change <- function(event_date) {
  pre_date <- as.IDate(seq(as.Date(event_date), by = "-1 month", length.out = 2L)[2L])
  pre <- unique(tier_panel[month == pre_date, .(coverage_bin, pre_rate = statutory_rate)])
  post <- unique(tier_panel[month == event_date, .(coverage_bin, post_rate = statutory_rate)])
  z <- merge(pre, post, by = "coverage_bin", all = FALSE)
  z[, event_delta_rate_10pp := (post_rate - pre_rate) / .10]
  z
}

event_exposure <- function(event_name, event_date) {
  z <- merge(base_tier, event_rate_change(event_date), by = "coverage_bin", all = FALSE)
  z[, .(
    event_name = event_name,
    event_delta_head_10pp = sum(base_head_share * event_delta_rate_10pp),
    event_delta_endorsement_10pp = sum(base_endorsement_share * event_delta_rate_10pp)
  ), by = .(fips, cattle_type)]
}

events <- list(
  list(event_name = "pm19_coverage_dependent", event_date = as.IDate("2019-07-01")),
  list(event_name = "pm20_coverage_expansion", event_date = as.IDate("2020-07-01"))
)
exposure_table <- rbindlist(lapply(
  events,
  function(ev) event_exposure(ev$event_name, ev$event_date)
), fill = TRUE)
fwrite(exposure_table, file.path(PATHS$results, "08_full_county_month_event_exposure.csv"))

build_stack <- function(window) {
  pieces <- list()
  for (ev in events) {
    x <- copy(d[month_index(month, ev$event_date) >= -window &
                  month_index(month, ev$event_date) <= window - 1L])
    if (!nrow(x)) next
    x[, `:=`(
      event_name = ev$event_name,
      event_date = ev$event_date,
      event_time = month_index(month, ev$event_date),
      post_event = as.integer(month >= ev$event_date),
      stack_county = interaction(ev$event_name, fips, cattle_type, drop = TRUE),
      stack_month = interaction(ev$event_name, month, drop = TRUE),
      stack_state_month = interaction(ev$event_name, state_fips, cattle_type, month, drop = TRUE)
    )]
    x[exposure_table[event_name == ev$event_name],
      on = .(fips, cattle_type),
      `:=`(event_delta_head_10pp = i.event_delta_head_10pp,
           event_delta_endorsement_10pp = i.event_delta_endorsement_10pp)]
    x[is.na(event_delta_head_10pp), `:=`(
      event_delta_head_10pp = 0,
      event_delta_endorsement_10pp = 0
    )]
    x[, `:=`(
      post_delta_head_10pp = post_event * event_delta_head_10pp,
      post_delta_endorsement_10pp = post_event * event_delta_endorsement_10pp
    )]
    pieces[[length(pieces) + 1L]] <- x
  }
  rbindlist(pieces, fill = TRUE)
}

outcome_specs <- list(
  any_lrp = list(y = "any_lrp", family = "ols", term = "post_delta_head_10pp",
                 event_exposure = "event_delta_head_10pp"),
  insured_head = list(y = "insured_head", family = "ppml", term = "post_delta_head_10pp",
                      event_exposure = "event_delta_head_10pp"),
  endorsements = list(y = "endorsements", family = "ppml", term = "post_delta_endorsement_10pp",
                      event_exposure = "event_delta_endorsement_10pp")
)

models <- list()
local_results <- list()
event_coefficients <- list()
pretrends <- list()
support <- list()

for (window in c(6L, 12L)) {
  stacked <- build_stack(window)
  if (!nrow(stacked)) next

  support[[length(support) + 1L]] <- stacked[, .(
    design = "stacked_local_full_county_month",
    window_months = window,
    observations = .N,
    counties = uniqueN(fips),
    active_county_months = sum(any_lrp),
    active_share = mean(any_lrp),
    total_head = sum(insured_head),
    total_endorsements = sum(endorsements)
  ), by = cattle_type]

  for (ct in c("feeder", "fed")) {
    x <- stacked[cattle_type == ct]
    if (!nrow(x)) next

    for (outcome_name in names(outcome_specs)) {
      spec <- outcome_specs[[outcome_name]]

      for (fe_setup in c("stack_county_plus_stack_month", "stack_county_plus_stack_state_month")) {
        fe_rhs <- if (fe_setup == "stack_county_plus_stack_month") {
          "stack_county + stack_month"
        } else {
          "stack_county + stack_state_month"
        }
        metadata <- data.table(cattle_type = ct, outcome = outcome_name,
                               design = "stacked_local_full_county_month",
                               window_months = window, fe_setup = fe_setup)
        f <- as.formula(paste0(spec$y, " ~ ", spec$term, " | ", fe_rhs))
        fit <- if (spec$family == "ppml") {
          run_fepois(f, x, spec$term, metadata)
        } else {
          run_feols(f, x, spec$term, metadata)
        }
        local_results[[length(local_results) + 1L]] <- fit$result
        if (!is.null(fit$model)) {
          key <- paste(ct, outcome_name, "local", window, fe_setup, sep = "__")
          models[[key]] <- fit$model
        }
      }

      metadata <- data.table(cattle_type = ct, outcome = outcome_name,
                             design = "stacked_local_full_county_month_eventstudy",
                             window_months = window,
                             fe_setup = "stack_county_plus_stack_month")
      f_event <- as.formula(paste0(
        spec$y, " ~ i(event_time, ", spec$event_exposure, ", ref = -1) | ",
        "stack_county + stack_month"
      ))
      m_event <- tryCatch(
        if (spec$family == "ppml") fepois(f_event, data = x, cluster = ~fips) else feols(f_event, data = x, cluster = ~fips),
        error = identity
      )
      if (inherits(m_event, "error")) {
        pretrends[[length(pretrends) + 1L]] <- cbind(metadata, data.table(
          pre_terms = NA_integer_, chi_square = NA_real_,
          degrees_freedom = NA_integer_, pretrend_p_value = NA_real_,
          status = paste0("not_estimated: ", conditionMessage(m_event))
        ))
      } else {
        key <- paste(ct, outcome_name, "event", window, sep = "__")
        models[[key]] <- m_event
        event_tab <- tidy_coeftable(m_event)
        event_tab <- event_tab[grepl("^event_time::", term)]
        event_tab[, event_time := as.integer(sub("^event_time::(-?[0-9]+):.*$", "\\1", term))]
        event_coefficients[[length(event_coefficients) + 1L]] <- cbind(
          metadata,
          event_tab[, .(event_time, coefficient, std_error, p_value)]
        )
        pretrends[[length(pretrends) + 1L]] <- cbind(
          metadata,
          joint_pretrend(m_event),
          data.table(status = "estimated")
        )
      }
    }
  }
}

# Sensitivity only: full-period exposure slope. These estimates are not the main
# risk-set design because they combine persistent county composition with reform
# exposure over the whole post-reform expansion.
sensitivity <- list()
sensitivity_specs <- list(
  any_lrp = list(y = "any_lrp", family = "ols", term = "pred_head_exposure_10pp"),
  insured_head = list(y = "insured_head", family = "ppml", term = "pred_head_exposure_10pp"),
  endorsements = list(y = "endorsements", family = "ppml", term = "pred_endorsement_exposure_10pp")
)
for (ct in c("feeder", "fed")) {
  x <- d[cattle_type == ct]
  if (!nrow(x)) next
  for (outcome_name in names(sensitivity_specs)) {
    spec <- sensitivity_specs[[outcome_name]]
    metadata <- data.table(cattle_type = ct, outcome = outcome_name,
                           design = "sensitivity_full_period_exposure",
                           window_months = NA_integer_,
                           fe_setup = "county_plus_month")
    f <- as.formula(paste0(spec$y, " ~ ", spec$term, " | county_id + month_fe"))
    fit <- if (spec$family == "ppml") {
      run_fepois(f, x, spec$term, metadata)
    } else {
      run_feols(f, x, spec$term, metadata)
    }
    sensitivity[[length(sensitivity) + 1L]] <- fit$result
  }
}

fwrite(descriptive, file.path(PATHS$results, "08_full_county_month_descriptive.csv"))
fwrite(rbindlist(support, fill = TRUE), file.path(PATHS$results, "08_full_county_month_support.csv"))
fwrite(rbindlist(local_results, fill = TRUE), file.path(PATHS$results, "08_full_county_month_local_results.csv"))
fwrite(rbindlist(event_coefficients, fill = TRUE), file.path(PATHS$results, "08_full_county_month_event_coefficients.csv"))
fwrite(rbindlist(pretrends, fill = TRUE), file.path(PATHS$results, "08_full_county_month_pretrends.csv"))
fwrite(rbindlist(sensitivity, fill = TRUE), file.path(PATHS$results, "08_full_county_month_sensitivity_full_period_exposure.csv"))
saveRDS(models, file.path(PATHS$results, "08_full_county_month_models.rds"))

summary_lines <- c(
  "# Full county-month risk-set diagnostics",
  "",
  "This script separates descriptive participation from reform-local identification diagnostics.",
  "",
  "Descriptive panel:",
  "- All ever-valid-LRP counties by cattle type are expanded monthly.",
  "- Inactive county-months are retained with zero endorsements, zero insured head, and any_lrp equal to zero.",
  "",
  "Main diagnostics:",
  "- Stacked local windows around the July 2019 and July 2020 statutory schedule changes.",
  "- Treatment is post-reform times the incremental statutory rate change, weighted by pre-2019 county-tier shares.",
  "- any_lrp is estimated by LPM; insured_head and endorsements are estimated by PPML.",
  "- Main fixed effects are reform-stack county and reform-stack month.",
  "- A state-by-month version is reported as a robustness check.",
  "",
  "Full-period exposure slopes are written only as sensitivity diagnostics."
)
writeLines(summary_lines, file.path(PATHS$results, "08_FULL_COUNTY_MONTH_SUMMARY.md"))

cat("Saved full county-month risk-set diagnostics.\n")
