# ============================================================
# 03_build_us_lp_aggregate
#
# Purpose:
# Build a U.S. aggregate labor productivity growth series
# (TOT_IND-style, excluding government) from the industry panel.
#
# Method:
# - industry labor productivity growth = annual log difference of lp_index
# - weights = value-added shares (va_level)
# - aggregation = Törnqvist weighting
#
# Output:
# data/clean/us_TOTIND_exgov_growth.csv
# Unit of observation: year
# Coverage: U.S. industries (excluding government)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(rlang)
})

# ------------------------------------------------
# Paths
# ------------------------------------------------

library(here)

DATA_CLEAN <- here("data", "clean")

IN_FILE  <- file.path(DATA_CLEAN, "us_industry_productivity_panel.csv")
OUT_FILE <- file.path(DATA_CLEAN, "us_TOTIND_exgov_growth.csv")

# Ensure output directory exists
dir.create(DATA_CLEAN, recursive = TRUE, showWarnings = FALSE)

# Check required input
if (!file.exists(IN_FILE)) {
  stop("Missing input file from Script 01: ", IN_FILE)
}

# ------------------------------------------------
# Settings
# ------------------------------------------------

DROP_GOV_EXACT <- c("Federal", "State and local")

# ------------------------------------------------
# Helper functions
# ------------------------------------------------

logdiff <- function(x) {
  dplyr::if_else(
    !is.na(x) & x > 0 & !is.na(dplyr::lag(x)) & dplyr::lag(x) > 0,
    log(x) - log(dplyr::lag(x)),
    NA_real_
  )
}

tornqvist_aggregate <- function(df, g_col) {
  g_sym <- rlang::ensym(g_col)
  
  df %>%
    filter(!is.na(va_share), !is.na(va_share_lag), !is.na(!!g_sym)) %>%
    mutate(w = 0.5 * (va_share + va_share_lag)) %>%
    group_by(year) %>%
    summarise(
      g_agg = sum((w / sum(w, na.rm = TRUE)) * (!!g_sym), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year)
}

# ------------------------------------------------
# 1) Load industry panel
# ------------------------------------------------

us <- read_csv(IN_FILE, show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    industry = str_squish(as.character(industry)),
    industry_key = str_squish(as.character(industry_key))
  )

if (!("lp_index" %in% names(us))) {
  stop("Column 'lp_index' missing from input file.")
}

if (!("va_level" %in% names(us))) {
  stop("Column 'va_level' missing from input file.")
}

if (any(us$industry %in% DROP_GOV_EXACT, na.rm = TRUE)) {
  stop("Government industries found in input file. Re-run Script 01 with government excluded.")
}

# ------------------------------------------------
# 2) Compute industry LP growth
# ------------------------------------------------

us_lp <- us %>%
  arrange(industry_key, year) %>%
  group_by(industry_key) %>%
  mutate(
    lp_g_i = logdiff(lp_index)
  ) %>%
  ungroup()

# ------------------------------------------------
# 3) Compute value-added shares
# ------------------------------------------------

us_lp <- us_lp %>%
  group_by(year) %>%
  mutate(
    va_share = va_level / sum(va_level, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(industry_key, year) %>%
  group_by(industry_key) %>%
  mutate(
    va_share_lag = lag(va_share)
  ) %>%
  ungroup()

# ------------------------------------------------
# 4) Aggregate LP growth using Törnqvist weights
# ------------------------------------------------

us_totind_lp <- tornqvist_aggregate(us_lp, lp_g_i) %>%
  rename(LP_g_TOTIND_exgov = g_agg)

# ------------------------------------------------
# 5) Lightweight sanity checks
# ------------------------------------------------

weight_check <- us_lp %>%
  group_by(year) %>%
  summarise(
    va_share_sum = sum(va_share, na.rm = TRUE),
    .groups = "drop"
  )

if (any(abs(weight_check$va_share_sum - 1) > 1e-8, na.rm = TRUE)) {
  print(weight_check)
  stop("VA shares do not sum to 1 in all years.")
}

if (any(is.na(us_totind_lp$LP_g_TOTIND_exgov))) {
  print(us_totind_lp)
  stop("Aggregate LP series contains missing values.")
}

# ------------------------------------------------
# 6) Save output
# ------------------------------------------------

write_csv(us_totind_lp, OUT_FILE)





diag_agg <- us_totind_lp %>%
  summarise(
    n_years = n(),
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE)
  )

print(diag_agg)

message("✓ US aggregate LP growth series built successfully.")
message("Saved to: ", normalizePath(OUT_FILE, winslash = "/"))