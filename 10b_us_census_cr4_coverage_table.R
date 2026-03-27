# ============================================================
# 10b_us_census_cr4_coverage_table.R
#
# Purpose:
#   Create an appendix-ready table summarising U.S. Economic
#   Census industry coverage by NAICS digit level using only
#   cells with non-missing CR4.
#
# Input:
#   data/clean/census_concentration_panel_2002_2022.csv
#
# Outputs:
#   output/appendix/tables/appendix_us_census_cr4_coverage_table.csv
#   output/appendix/tables/appendix_us_census_cr4_coverage_tabular.tex
#
# Notes:
#   - This script is NOT required to reproduce the main results.
#     It generates an appendix table only.
#   - Coverage is based on non-missing CR4 observations.
#   - Uses harmonised Census panel constructed in Step 10.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(knitr)
})

# -------------------------
# Paths
# -------------------------
library(here)

DATA_CLEAN <- here("data", "clean")
OUTPUT_DIR <- here("output", "appendix", "tables")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

PANEL_PATH <- file.path(DATA_CLEAN, "census_concentration_panel_2002_2022.csv")
# -------------------------
# Load panel
# -------------------------
census <- read_csv(PANEL_PATH, show_col_types = FALSE)

# -------------------------
# Keep only cells with CR4 coverage
# -------------------------
census_cr4 <- census %>%
  filter(!is.na(CR4))

# -------------------------
# Collapse NAICS levels for table
#   2, 3, 4, and 5-8 digits
# -------------------------
coverage_tbl <- census_cr4 %>%
  mutate(
    NAICS_group = case_when(
      NAICS_len == 2 ~ "2-digit",
      NAICS_len == 3 ~ "3-digit",
      NAICS_len == 4 ~ "4-digit",
      NAICS_len >= 5 ~ "5--8-digit",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(NAICS_group)) %>%
  group_by(year, NAICS_group) %>%
  summarise(
    Industries = n_distinct(NAICS_key),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = NAICS_group,
    values_from = Industries,
    values_fill = 0
  ) %>%
  rename(
    `Census year` = year,
    `2-digit` = `2-digit`,
    `3-digit` = `3-digit`,
    `4-digit` = `4-digit`,
    `5--8-digit` = `5--8-digit`
  ) %>%
  arrange(`Census year`)

# -------------------------
# Save CSV
# -------------------------
write_csv(
  coverage_tbl,
  file.path(OUTPUT_DIR, "appendix_us_census_cr4_coverage_table.csv")
)

# -------------------------
# Save LaTeX tabular
# -------------------------
latex_tab <- knitr::kable(
  coverage_tbl,
  format = "latex",
  booktabs = TRUE,
  longtable = FALSE
)

writeLines(
  latex_tab,
  file.path(OUTPUT_DIR, "appendix_us_census_cr4_coverage_tabular.tex")
)

message("✓ U.S. Census CR4 coverage table saved.")