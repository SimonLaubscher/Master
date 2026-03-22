# NOTE:
# This script produces descriptive diagnostics for the CompNet hybrid dataset.
# It is NOT required to reproduce the final empirical results and can be skipped
# in the replication pipeline. It is kept for transparency and data validation.
# ============================================================
# 9b_compnet_hybrid_diagnostics.R
#
# Purpose:
#   Describe the final CompNet hybrid dataset used in the thesis.
#
# Inputs:
#   compnet_hybrid_annual.csv
#
# Outputs:
#   appendix_compnet_hybrid_coverage_country.csv
#   appendix_compnet_hybrid_coverage_country_year.csv
#   appendix_compnet_hybrid_missingness_overall.csv
#   appendix_compnet_hybrid_missingness_by_country.csv
#   appendix_compnet_hybrid_weight_diagnostics.csv
#   appendix_compnet_truncation_by_country_industry.csv
#   appendix_compnet_internal_gaps_summary.csv
#   fig_compnet_hybrid_avg_HHI_rev.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

# -------------------------
# Paths
# -------------------------

ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_CLEAN <- file.path(ROOT, "dataclean")

HY_PATH <- file.path(DATA_CLEAN, "compnet_hybrid_annual.csv")

if (!file.exists(HY_PATH)) stop("Hybrid dataset not found")

# -------------------------
# Load hybrid dataset
# -------------------------

hy <- read_csv(HY_PATH, show_col_types = FALSE)

# ============================================================
# 1) Country coverage
# ============================================================

cov_country <- hy %>%
  group_by(country) %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    n_years = n_distinct(year),
    n_industries = n_distinct(NACE2),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(country)

write_csv(
  cov_country,
  file.path(DATA_CLEAN, "appendix_compnet_hybrid_coverage_country.csv")
)

print(cov_country)

# ============================================================
# 2) Country-year coverage
# ============================================================

cov_country_year <- hy %>%
  count(country, year, name = "n_industries") %>%
  arrange(country, year)

write_csv(
  cov_country_year,
  file.path(DATA_CLEAN, "appendix_compnet_hybrid_coverage_country_year.csv")
)

# ============================================================
# 3) Missingness diagnostics
# ============================================================

vars_main <- c("HHI_rev","FV08_nrev_mn","FV08_nrev_sw","rev_tot_proxy")

missing_overall <- hy %>%
  summarise(
    across(
      all_of(vars_main),
      list(
        nonmiss = ~sum(!is.na(.)),
        share_missing = ~mean(is.na(.))
      ),
      .names = "{.col}_{.fn}"
    )
  )

write_csv(
  missing_overall,
  file.path(DATA_CLEAN, "appendix_compnet_hybrid_missingness_overall.csv")
)

missing_by_country <- hy %>%
  group_by(country) %>%
  summarise(
    across(
      all_of(vars_main),
      ~mean(is.na(.)),
      .names = "{.col}_missing"
    ),
    .groups = "drop"
  )

write_csv(
  missing_by_country,
  file.path(DATA_CLEAN, "appendix_compnet_hybrid_missingness_by_country.csv")
)

# ============================================================
# 4) Weight diagnostics
# ============================================================

weight_diag <- hy %>%
  summarise(
    n = n(),
    share_nonmissing = mean(!is.na(rev_tot_proxy)),
    min = min(rev_tot_proxy, na.rm = TRUE),
    median = median(rev_tot_proxy, na.rm = TRUE),
    mean = mean(rev_tot_proxy, na.rm = TRUE),
    max = max(rev_tot_proxy, na.rm = TRUE)
  )

write_csv(
  weight_diag,
  file.path(DATA_CLEAN, "appendix_compnet_hybrid_weight_diagnostics.csv")
)

# ============================================================
# 5) Average HHI over time
# ============================================================

avg_hhi <- hy %>%
  group_by(country, year) %>%
  summarise(
    HHI_rev_avg = mean(HHI_rev, na.rm = TRUE),
    .groups = "drop"
  )

p_avg <- ggplot(avg_hhi, aes(year, HHI_rev_avg, color = country)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Average revenue-based HHI by country",
    x = "Year",
    y = "HHI_rev"
  )

ggsave(
  file.path(DATA_CLEAN, "fig_compnet_hybrid_avg_HHI_rev.png"),
  p_avg,
  width = 10,
  height = 6
)

# ============================================================
# 6) Industry truncation diagnostics
# ============================================================

country_end <- hy %>%
  group_by(country) %>%
  summarise(end_year = max(year), .groups = "drop")

trunc_tbl <- hy %>%
  group_by(country, NACE2) %>%
  summarise(
    last_year = max(year),
    .groups = "drop"
  ) %>%
  left_join(country_end, by = "country") %>%
  mutate(
    truncated = last_year < end_year
  )

write_csv(
  trunc_tbl,
  file.path(DATA_CLEAN, "appendix_compnet_truncation_by_country_industry.csv")
)

# ============================================================
# 7) Internal gaps diagnostics
# ============================================================

gap_summary <- hy %>%
  group_by(country, NACE2) %>%
  summarise(
    n_years = n_distinct(year),
    span = max(year) - min(year) + 1,
    has_gap = n_years < span,
    .groups = "drop"
  ) %>%
  group_by(country) %>%
  summarise(
    share_with_gaps = mean(has_gap),
    .groups = "drop"
  )

write_csv(
  gap_summary,
  file.path(DATA_CLEAN, "appendix_compnet_internal_gaps_summary.csv")
)

message("✓ Script 9b complete: hybrid diagnostics saved.")