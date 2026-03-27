# ============================================================
# 07b_klems_ict_classification_table.R
#
# Purpose:
#   Create an appendix-ready table reporting the distribution of
#   European industries across ICT-producing, ICT-using, Other,
#   and ICT baseline missing groups.
#
# Input (dataclean/):
#   ICT_classification_check_C26_ONLY_1997baseline.csv
#
# Outputs (output/tables/):
#   appendix_ict_classification_table.csv
#   appendix_ict_classification_tabular.tex
#
# Notes:
#   - This script is not required to reproduce the main empirical
#     results. It generates an appendix table only.
#   - The table reports European countries only.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(knitr)
})

library(here)

DATA_CLEAN <- here("data", "clean")
OUTPUT_DIR <- here("output", "appendix", "tables")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

IN_PATH <- file.path(DATA_CLEAN, "ICT_classification_check_C26_ONLY_1997baseline.csv")

if (!file.exists(IN_PATH)) {
  stop("Input file not found: ", IN_PATH)
}

# -------------------------
# Load classification check file
# -------------------------
ict <- read_csv(IN_PATH, show_col_types = FALSE)

# -------------------------
# Build appendix table
# -------------------------
appendix_tbl <- ict %>%
  count(country, ICT_group_baseline, name = "n") %>%
  mutate(
    ICT_group_baseline = as.character(ICT_group_baseline)
  ) %>%
  pivot_wider(
    names_from = ICT_group_baseline,
    values_from = n,
    values_fill = 0
  ) %>%
  # 🔧 Rename column to clearer label
  rename(
    `ICT intensity missing (1997)` = `ICT baseline missing`
  ) %>%
  mutate(
    Total = `ICT-producing` + `ICT-using` + Other + `ICT intensity missing (1997)`,
    Country = recode(
      country,
      "DE" = "Germany",
      "FR" = "France",
      "DK" = "Denmark",
      "SE" = "Sweden"
    )
  ) %>%
  select(
    Country,
    `ICT-producing`,
    `ICT-using`,
    Other,
    `ICT intensity missing (1997)`,
    Total
  ) %>%
  mutate(
    Country = factor(
      Country,
      levels = c("Germany", "France", "Denmark", "Sweden")
    )
  ) %>%
  arrange(Country) %>%
  mutate(Country = as.character(Country))

# -------------------------
# Save CSV
# -------------------------
write_csv(
  appendix_tbl,
  file.path(OUTPUT_DIR, "appendix_ict_classification_table.csv")
)

# -------------------------
# Save LaTeX tabular
# -------------------------
latex_tab <- knitr::kable(
  appendix_tbl,
  format = "latex",
  booktabs = TRUE,
  longtable = FALSE
)

writeLines(
  latex_tab,
  file.path(OUTPUT_DIR, "appendix_ict_classification_tabular.tex")
)

message("✓ ICT classification appendix table saved.")