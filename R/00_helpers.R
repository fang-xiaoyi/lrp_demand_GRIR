suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(fixest)
})

required_packages <- c("data.table", "ggplot2", "fixest")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install these packages before running: ", paste(missing_packages, collapse = ", "))
}

coverage_bins <- data.table(
  coverage_bin = c("b70_79", "b80_84", "b85_89", "b90_94", "b95_100"),
  coverage_label = c("70--79.99%", "80--84.99%", "85--89.99%", "90--94.99%", "95--100%"),
  bin_order = 1:5
)

assign_bin <- function(x) {
  fcase(
    x >= .70 & x < .80, "b70_79",
    x >= .80 & x < .85, "b80_84",
    x >= .85 & x < .90, "b85_89",
    x >= .90 & x < .95, "b90_94",
    x >= .95 & x <= 1.000001, "b95_100",
    default = NA_character_
  )
}

month_index <- function(date, origin) {
  12L * (as.integer(format(date, "%Y")) - as.integer(format(origin, "%Y"))) +
    as.integer(format(date, "%m")) - as.integer(format(origin, "%m"))
}

read_schedule <- function() {
  s <- fread(PATHS$schedule)
  required <- c("commodity_code", "period_name", "effective_start", "effective_end",
                "coverage_min", "coverage_max", "subsidy_old", "subsidy_new")
  if (!all(required %in% names(s))) {
    stop("The policy schedule must contain: ", paste(required, collapse = ", "))
  }
  s[, coverage_bin := fcase(
    coverage_min == .70 & coverage_max == .80, "b70_79",
    coverage_min == .80 & coverage_max == .85, "b80_84",
    coverage_min == .85 & coverage_max == .90, "b85_89",
    coverage_min == .90 & coverage_max == .95, "b90_94",
    coverage_min == .95 & coverage_max > 1, "b95_100",
    default = NA_character_
  )]
  if (anyNA(s$coverage_bin)) stop("At least one policy-schedule coverage interval does not map to a fixed bin.")
  s[, `:=`(start_date = as.IDate(effective_start), end_date = as.IDate(effective_end),
           statutory_rate = subsidy_new,
           source_note = paste0(period_name, "; commodity ", commodity_code))]
  schedule_check <- s[, .(n_rates = uniqueN(statutory_rate)),
                      by = .(start_date, end_date, coverage_bin)]
  if (schedule_check[n_rates != 1, .N]) {
    stop("Commodity-specific schedules differ; add commodity code to the endorsement panel before proceeding.")
  }
  s <- unique(s[, .(start_date, end_date, coverage_bin, coverage_min, coverage_max,
                    statutory_rate, source_note = period_name)])
  stopifnot(nrow(s) > 0L, !anyDuplicated(s[, .(start_date, end_date, coverage_bin)]))
  s
}

attach_schedule <- function(d, date_col = "month") {
  s <- read_schedule()
  x <- copy(d)
  x[, join_date := as.IDate(get(date_col))]
  x[, row_id___ := .I]
  z <- s[x, on = .(coverage_bin, start_date <= join_date, end_date >= join_date), nomatch = 0L,
         .(row_id___ = i.row_id___, statutory_rate, schedule_start = x.start_date,
           schedule_end = x.end_date, source_note)]
  if (nrow(z) != nrow(x) || anyDuplicated(z$row_id___)) {
    stop("The statutory schedule does not map one-to-one to all rows.")
  }
  x[z, on = "row_id___", `:=`(statutory_rate = i.statutory_rate,
                                schedule_start = i.schedule_start,
                                schedule_end = i.schedule_end,
                                schedule_note = i.source_note)]
  x[, c("join_date", "row_id___") := NULL]
  x
}

save_plot <- function(p, stem, width = 9, height = 5.5) {
  ggsave(file.path(PATHS$figures, paste0(stem, ".png")), p, width = width, height = height, dpi = 300)
  ggsave(file.path(PATHS$figures, paste0(stem, ".pdf")), p, width = width, height = height)
}

tidy_coeftable <- function(model) {
  x <- as.data.table(coeftable(model), keep.rownames = "term")
  estimate_col <- intersect(c("Estimate"), names(x))[1]
  se_col <- intersect(c("Std. Error"), names(x))[1]
  stat_col <- intersect(c("t value", "z value"), names(x))[1]
  p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), names(x))[1]
  if (is.na(estimate_col) || is.na(se_col) || is.na(p_col)) {
    stop("Unexpected coefficient table columns: ", paste(names(x), collapse = ", "))
  }
  out <- data.table(
    term = x$term,
    coefficient = x[[estimate_col]],
    std_error = x[[se_col]],
    statistic = if (is.na(stat_col)) NA_real_ else x[[stat_col]],
    p_value = x[[p_col]]
  )
  out
}
