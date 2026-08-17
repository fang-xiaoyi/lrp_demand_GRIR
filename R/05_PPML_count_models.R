source("config.R")
source("R/00_helpers.R")

cat("Running PPML county-month-tier count models...\n")

d <- fread(file.path(PATHS$processed, "county_month_fixed_tier_panel_2005_2024.csv"))
d[, month := as.IDate(month)]
d <- d[month >= as.IDate("2017-07-01") & month <= as.IDate("2020-06-01")]

d[, `:=`(
  event_month = month_index(month, as.IDate("2019-07-01")),
  cal_month = format(month, "%m"),
  subsidy_rate_10pp = statutory_rate / .10,
  tier_month_of_year = interaction(coverage_bin, format(month, "%m"), drop = TRUE)
)]

rate_lookup <- unique(d[month == as.IDate("2019-07-01"),
                        .(coverage_bin, post_rate_2019 = statutory_rate)])
if (nrow(rate_lookup) != 5L) stop("The July 2019 schedule must contain all five fixed tiers.")
d[rate_lookup, on = "coverage_bin", post_rate_2019 := i.post_rate_2019]
d[, delta_rate_10pp := (post_rate_2019 - .13) / .10]

outcomes <- c(insured_head = "insured_head", endorsements = "endorsements")
setups <- list(
  advisor_base = "subsidy_rate_10pp | county_month + county_tier",
  tier_seasonality = "subsidy_rate_10pp | county_month + county_tier + tier_month_of_year"
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

models <- list()
results <- list()
events <- list()
pretrend_tests <- list()

support <- d[, .(
  observations = .N,
  county_months = uniqueN(county_month),
  counties = uniqueN(fips),
  zero_head_share = mean(insured_head == 0),
  zero_endorsement_share = mean(endorsements == 0),
  total_head = sum(insured_head),
  total_endorsements = sum(endorsements)
), by = cattle_type]

for (ct in c("feeder", "fed")) {
  x <- d[cattle_type == ct]
  if (!nrow(x)) next

  available_pre_months <- sort(unique(x[event_month < 0, event_month]))
  if (!length(available_pre_months)) {
    warning("Skipping event study for ", ct, ": no active pre-reform month is available.")
    reference_month <- NA_integer_
  } else {
    reference_month <- max(available_pre_months)
  }

  for (outcome_name in names(outcomes)) {
    y <- outcomes[[outcome_name]]

    for (setup in names(setups)) {
      formula_text <- paste0(y, " ~ ", setups[[setup]])
      m <- tryCatch(
        fepois(as.formula(formula_text), data = x, cluster = ~fips),
        error = identity
      )
      metadata <- data.table(
        cattle_type = ct,
        outcome = outcome_name,
        setup = setup,
        counties = uniqueN(x$fips)
      )
      if (inherits(m, "error")) {
        results[[length(results) + 1L]] <- cbind(metadata, data.table(
          term = "subsidy_rate_10pp", coefficient = NA_real_, std_error = NA_real_,
          p_value = NA_real_, nobs = NA_integer_,
          status = paste0("not_estimated: ", conditionMessage(m))
        ))
      } else {
        key <- paste(ct, outcome_name, setup, sep = "__")
        models[[key]] <- m
        results[[length(results) + 1L]] <- extract_term(m, "subsidy_rate_10pp", metadata)
      }
    }

    if (!is.na(reference_month)) {
      event_formula <- as.formula(paste0(
        y, " ~ i(event_month, delta_rate_10pp, ref = ", reference_month, ") | ",
        "county_month + county_tier"
      ))
      m_event <- tryCatch(fepois(event_formula, data = x, cluster = ~fips), error = identity)
      metadata <- data.table(
        cattle_type = ct,
        outcome = outcome_name,
        setup = "event_advisor_base",
        counties = uniqueN(x$fips),
        event_reference_month = reference_month
      )
      if (inherits(m_event, "error")) {
        pretrend_tests[[length(pretrend_tests) + 1L]] <- cbind(metadata, data.table(
          pre_months_tested = NA_integer_, chi_square = NA_real_,
          degrees_freedom = NA_integer_, p_value = NA_real_,
          status = paste0("not_estimated: ", conditionMessage(m_event))
        ))
      } else {
        key <- paste(ct, outcome_name, "event_advisor_base", sep = "__")
        models[[key]] <- m_event

        et <- as.data.table(coeftable(m_event), keep.rownames = "term")
        setnames(et, c("Estimate", "Std. Error", "z value", "Pr(>|z|)"),
                 c("coefficient", "std_error", "statistic", "p_value"), skip_absent = TRUE)
        et <- et[grepl("^event_month::", term)]
        et[, event_month := as.integer(sub("^event_month::(-?[0-9]+):.*$", "\\1", term))]
        et[, `:=`(cattle_type = ct, outcome = outcome_name,
                  event_reference_month = reference_month)]
        events[[length(events) + 1L]] <- et[, .(
          cattle_type, outcome, event_month, event_reference_month,
          coefficient, std_error, p_value
        )]

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
        pretrend_tests[[length(pretrend_tests) + 1L]] <- cbind(metadata, data.table(
          pre_months_tested = length(pre_terms),
          chi_square = joint_stat,
          degrees_freedom = joint_df,
          p_value = joint_p,
          status = "estimated"
        ))
      }
    }
  }
}

result_table <- rbindlist(results, fill = TRUE)
event_table <- rbindlist(events, fill = TRUE)
pretrend_table <- rbindlist(pretrend_tests, fill = TRUE)

fwrite(result_table, file.path(PATHS$results, "05_PPML_count_results.csv"))
fwrite(event_table, file.path(PATHS$results, "05_PPML_event_coefficients.csv"))
fwrite(pretrend_table, file.path(PATHS$results, "05_PPML_pretrend_tests.csv"))
fwrite(support, file.path(PATHS$results, "05_PPML_support.csv"))
saveRDS(models, file.path(PATHS$results, "05_PPML_count_models.rds"))

summary_lines <- c(
  "# PPML county-month-tier count models",
  "",
  "The outcomes are insured head and endorsement counts in each county-month-tier cell.",
  "The treatment is the official statutory subsidy rate, scaled in 10-percentage-point units.",
  "",
  "Base specification:",
  "",
  "`E[Y_itb | X] = exp(alpha_it + gamma_ib + beta s_bt)`",
  "",
  "where alpha_it is a county-by-month fixed effect and gamma_ib is a county-by-coverage-tier fixed effect.",
  "The tier-seasonality robustness adds coverage-tier by calendar-month fixed effects.",
  "The event-study version interacts event month with the tier-specific July 2019 statutory subsidy-rate change.",
  "Predetermined CME downside-risk controls are not included yet."
)
writeLines(summary_lines, file.path(PATHS$results, "05_PPML_COUNT_SUMMARY.md"))

print(support)
print(result_table)
print(pretrend_table)
cat("Saved PPML count results.\n")
