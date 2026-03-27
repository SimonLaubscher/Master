# ============================================================
# 09_compnet_appendix_tables.R
#
# Purpose:
#   Create appendix-ready LaTeX tables for the CompNet data used
#   in the thesis:
#     1) Coverage of the final hybrid dataset by country
#     2) Validation of the hybrid construction using V9/V10 overlap
#
# Note:
#   This script is not required to reproduce the main empirical
#   results. It generates appendix tables only.
#
# Inputs:
#   - data/clean/compnet_hybrid_annual.csv
#   - data/clean/compnet_v10_annual.csv
#   - data/clean/compnet_v9_overlap_DE_FR_DK.csv
#
# Outputs:
#   - output/appendix/tables/appendix_compnet_hybrid_coverage_tabular.tex
#   - output/appendix/tables/appendix_compnet_vintage_validation_tabular.tex
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(knitr)
  library(here)
})

# -------------------------
# Paths
# -------------------------
DATA_CLEAN <- here("data", "clean")
OUTPUT_DIR <- here("output", "appendix", "tables")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

HY_PATH  <- file.path(DATA_CLEAN, "compnet_hybrid_annual.csv")
V10_PATH <- file.path(DATA_CLEAN, "compnet_v10_annual.csv")
V9_PATH  <- file.path(DATA_CLEAN, "compnet_v9_overlap_DE_FR_DK.csv")

if (!file.exists(HY_PATH))  stop("Input file not found: ", HY_PATH)
if (!file.exists(V10_PATH)) stop("Input file not found: ", V10_PATH)
if (!file.exists(V9_PATH))  stop("Input file not found: ", V9_PATH)

# -------------------------
# Helpers
# -------------------------
corr_safe <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  if (sum(ok) < 3) return(NA_real_)
  cor(a[ok], b[ok])
}

harmonise_types <- function(df) {
  df %>%
    mutate(
      country = as.character(country),
      year = as.integer(year),
      NACE2 = sprintf("%02d", as.integer(NACE2))
    )
}

# ============================================================
# 1) Hybrid coverage table
# ============================================================

hy <- read_csv(HY_PATH, show_col_types = FALSE) %>%
  harmonise_types()

industry_year_counts <- hy %>%
  count(country, year, name = "n_industries_year")

industry_summary <- industry_year_counts %>%
  group_by(country) %>%
  summarise(
    `Min. industries` = min(n_industries_year),
    `Max. industries` = max(n_industries_year),
    .groups = "drop"
  )

missing_years_tbl <- hy %>%
  distinct(country, year) %>%
  group_by(country) %>%
  summarise(
    start_year = min(year),
    end_year = max(year),
    missing_years = {
      yrs <- sort(unique(year))
      full <- seq(start_year, end_year)
      miss <- setdiff(full, yrs)
      if (length(miss) == 0) "--" else paste(miss, collapse = ", ")
    },
    .groups = "drop"
  )

appendix_cov_tbl <- hy %>%
  group_by(country) %>%
  summarise(
    `Start year` = min(year),
    `End year` = max(year),
    Observations = n(),
    .groups = "drop"
  ) %>%
  left_join(industry_summary, by = "country") %>%
  left_join(
    missing_years_tbl %>% select(country, missing_years),
    by = "country"
  ) %>%
  mutate(
    Country = recode(
      country,
      "DE" = "Germany",
      "FR" = "France",
      "DK" = "Denmark",
      "SE" = "Sweden"
    ),
    `Missing years` = missing_years
  ) %>%
  select(
    Country,
    `Start year`,
    `End year`,
    `Missing years`,
    `Min. industries`,
    `Max. industries`,
    Observations
  ) %>%
  mutate(
    Country = factor(
      Country,
      levels = c("Germany", "France", "Denmark", "Sweden")
    )
  ) %>%
  arrange(Country) %>%
  mutate(Country = as.character(Country))

latex_cov <- knitr::kable(
  appendix_cov_tbl,
  format = "latex",
  booktabs = TRUE,
  longtable = FALSE
)

writeLines(
  latex_cov,
  file.path(OUTPUT_DIR, "appendix_compnet_hybrid_coverage_tabular.tex")
)

# ============================================================
# 2) Vintage validation table (V9 vs V10)
# ============================================================

v10 <- read_csv(V10_PATH, show_col_types = FALSE) %>%
  harmonise_types()

v9 <- read_csv(V9_PATH, show_col_types = FALSE) %>%
  harmonise_types()

ov <- inner_join(
  v9 %>%
    select(country, year, NACE2, HHI_rev),
  v10 %>%
    select(country, year, NACE2, HHI_rev),
  by = c("country", "year", "NACE2"),
  suffix = c("_v9", "_v10")
)

if (nrow(ov) == 0) {
  stop("No overlapping country-year-industry cells found between V9 and V10.")
}

validation_tbl <- ov %>%
  group_by(country) %>%
  summarise(
    `Matched cells` = n(),
    `Correlation of HHI` = corr_safe(HHI_rev_v9, HHI_rev_v10),
    `Mean absolute difference in HHI` = mean(abs(HHI_rev_v10 - HHI_rev_v9), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Country = recode(
      country,
      "DE" = "Germany",
      "FR" = "France",
      "DK" = "Denmark"
    )
  ) %>%
  select(
    Country,
    `Matched cells`,
    `Correlation of HHI`,
    `Mean absolute difference in HHI`
  ) %>%
  mutate(
    Country = factor(
      Country,
      levels = c("Germany", "France", "Denmark")
    )
  ) %>%
  arrange(Country) %>%
  mutate(
    Country = as.character(Country),
    `Correlation of HHI` = round(`Correlation of HHI`, 3),
    `Mean absolute difference in HHI` = round(`Mean absolute difference in HHI`, 4)
  )

latex_val <- knitr::kable(
  validation_tbl,
  format = "latex",
  booktabs = TRUE,
  longtable = FALSE
)

writeLines(
  latex_val,
  file.path(OUTPUT_DIR, "appendix_compnet_vintage_validation_tabular.tex")
)

message("✓ CompNet appendix LaTeX tables saved.")