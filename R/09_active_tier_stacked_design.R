source("config.R")
source("R/00_helpers.R")

cat("Running active county-month-tier stacked statutory design...\n")

panel_path <- file.path(PATHS$processed, "county_month_fixed_tier_panel_2005_2024.csv")
if (!file.exists(panel_path)) {
  stop("Run R/02_build_county_month_tier_panel.R before this script.")
}

d <- fread(panel_path)
d[, month := as.IDate(month)]
d <- d[month >= as.IDate("2017-07-01") & month <= as.IDate("2021-06-01")]
d[, `:=`(
  cal_month = format(month, "%m"),
  total_head_cm = sum(insured_head),
  total_endorsements_cm = sum(endorsements)
), by = .(fips, cattle_type, month)]

events <- list(
  list(event_name = "pm19_coverage_dependent", event_date = as.IDate("2019-07-01")),
  list(event_name = "pm20_coverage_expansion", event_date = as.IDate("2020-07-01"))
)

event_rate_change <- function(event_date) {
  pre_date <- as.IDate(seq(as.Date(event_date), by = "-1 month", length.out = 2L)[2L])
  pre <- unique(d[month == pre_date, .(coverage_bin, pre_rate = statutory_rate)])
  post <- unique(d[month == event_date, .(coverage_bin, post_rate = statutory_rate)])
  z <- merge(pre, post, by = "coverage_bin", all = FALSE)
  z[, delta_rate_10pp := (post_rate - pre_rate) / .10]
  z
}

build_stack <- function(window = 12L) {
  pieces <- list()
  for (ev in events) {
    x <- copy(d[month_index(month, ev$event_date) >= -window &
                  month_index(month, ev$event_date) <= window - 1L])
    if (!nrow(x)) next
    x[event_rate_change(ev$event_date), on = "coverage_bin",
      delta_rate_10pp := i.delta_rate_10pp]
    x[, `:=`(
      event_name = ev$event_name,
      event_date = ev$event_date,
      event_time = month_index(month, ev$event_date),
      post_event = as.integer(month >= ev$event_date),
      post_delta_10pp = as.integer(month >= ev$event_date) * delta_rate_10pp,
      stack_county_month = interaction(ev$event_name, fips, cattle_type, month, drop = TRUE),
      stack_county_tier = interaction(ev$event_name, fips, cattle_type, coverage_bin, drop = TRUE),
      stack_tier_calmonth = interaction(ev$event_name, coverage_bin, cal_month, drop = TRUE)
    )]
    pieces[[length(pieces) + 1L]] <- x
  }
  rbindlist(pieces, fill = TRUE)
}

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
event_coefficients <- list()
pretrends <- list()
support <- list()

outcome_specs <- list(
  head_share = list(y = "within_month_head_share", family = "ols", weight = NULL),
  endorsement_share = list(y = "within_month_endorsement_share", family = "ols", weight = NULL),
  head_share_weighted = list(y = "within_month_head_share", family = "ols", weight = "total_head_cm"),
  endorsement_share_weighted = list(y = "within_month_endorsement_share", family = "ols", weight = "total_endorsements_cm"),
  insured_head = list(y = "insured_head", family = "ppml", weight = NULL),
  endorsements = list(y = "endorsements", family = "ppml", weight = NULL)
)

for (window in c(12L, 18L)) {
  stacked <- build_stack(window)
  if (!nrow(stacked)) next

  support[[length(support) + 1L]] <- stacked[, .(
    design = "active_tier_stacked_two_reforms",
    window_months = window,
    observations = .N,
    counties = uniqueN(fips),
    county_months = uniqueN(stack_county_month),
    zero_head_share = mean(insured_head == 0),
    zero_endorsement_share = mean(endorsements == 0),
    total_head = sum(insured_head),
    total_endorsements = sum(endorsements)
  ), by = cattle_type]

  for (ct in c("feeder", "fed")) {
    x <- stacked[cattle_type == ct]
    if (!nrow(x)) next

    for (outcome_name in names(outcome_specs)) {
      spec <- outcome_specs[[outcome_name]]
      metadata <- data.table(cattle_type = ct, outcome = outcome_name,
                             design = "active_tier_stacked_two_reforms",
                             window_months = window,
                             fe_setup = "county_month_plus_county_tier_plus_tier_seasonality")
      f <- as.formula(paste0(
        spec$y, " ~ post_delta_10pp | ",
        "stack_county_month + stack_county_tier + stack_tier_calmonth"
      ))
      weights <- if (is.null(spec$weight)) NULL else x[[spec$weight]]
      fit <- if (spec$family == "ppml") {
        run_fepois(f, x, "post_delta_10pp", metadata)
      } else {
        run_feols(f, x, "post_delta_10pp", metadata, weights = weights)
      }
      results[[length(results) + 1L]] <- fit$result
      if (!is.null(fit$model)) {
        key <- paste(ct, outcome_name, window, "static", sep = "__")
        models[[key]] <- fit$model
      }

      f_event <- as.formula(paste0(
        spec$y, " ~ i(event_time, delta_rate_10pp, ref = -1) | ",
        "stack_county_month + stack_county_tier + stack_tier_calmonth"
      ))
      m_event <- tryCatch(
        if (spec$family == "ppml") {
          fepois(f_event, data = x, cluster = ~fips)
        } else {
          feols(f_event, data = x, weights = weights, cluster = ~fips)
        },
        error = identity
      )
      event_metadata <- data.table(cattle_type = ct, outcome = outcome_name,
                                   design = "active_tier_stacked_two_reforms_eventstudy",
                                   window_months = window,
                                   fe_setup = "county_month_plus_county_tier_plus_tier_seasonality")
      if (inherits(m_event, "error")) {
        pretrends[[length(pretrends) + 1L]] <- cbind(event_metadata, data.table(
          pre_terms = NA_integer_, chi_square = NA_real_,
          degrees_freedom = NA_integer_, pretrend_p_value = NA_real_,
          status = paste0("not_estimated: ", conditionMessage(m_event))
        ))
      } else {
        key <- paste(ct, outcome_name, window, "event", sep = "__")
        models[[key]] <- m_event
        event_tab <- tidy_coeftable(m_event)
        event_tab <- event_tab[grepl("^event_time::", term)]
        event_tab[, event_time := as.integer(sub("^event_time::(-?[0-9]+):.*$", "\\1", term))]
        event_coefficients[[length(event_coefficients) + 1L]] <- cbind(
          event_metadata,
          event_tab[, .(event_time, coefficient, std_error, p_value)]
        )
        pretrends[[length(pretrends) + 1L]] <- cbind(
          event_metadata,
          joint_pretrend(m_event),
          data.table(status = "estimated")
        )
      }
    }
  }
}

fwrite(rbindlist(support, fill = TRUE), file.path(PATHS$results, "09_active_tier_stacked_support.csv"))
fwrite(rbindlist(results, fill = TRUE), file.path(PATHS$results, "09_active_tier_stacked_results.csv"))
fwrite(rbindlist(event_coefficients, fill = TRUE), file.path(PATHS$results, "09_active_tier_stacked_event_coefficients.csv"))
fwrite(rbindlist(pretrends, fill = TRUE), file.path(PATHS$results, "09_active_tier_stacked_pretrends.csv"))
saveRDS(models, file.path(PATHS$results, "09_active_tier_stacked_models.rds"))

summary_lines <- c(
  "# Active county-month-tier stacked statutory design",
  "",
  "This is the candidate advisor-aligned tier-choice design.",
  "",
  "Sample:",
  "- County-months with positive LRP activity are expanded to all fixed coverage tiers.",
  "- Feeder and fed cattle are estimated separately.",
  "",
  "Design:",
  "- Stacked local windows around July 2019 and July 2020 statutory subsidy reforms.",
  "- Treatment is post-reform times the tier-specific incremental statutory subsidy-rate change.",
  "- Fixed effects are reform-stack county-month, reform-stack county-tier, and reform-stack tier by calendar month.",
  "- Outcomes include active-purchase tier shares and tier-level counts.",
  "",
  "Interpretation:",
  "- This design identifies changes in tier allocation and tier-level scale conditional on active LRP purchasing.",
  "- It does not identify new producer entry."
)
writeLines(summary_lines, file.path(PATHS$results, "09_ACTIVE_TIER_STACKED_SUMMARY.md"))

cat("Saved active tier stacked design outputs.\n")
