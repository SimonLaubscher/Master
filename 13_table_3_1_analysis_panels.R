# ============================================================
# 13_table_3_1_analysis_panels.R
# ------------------------------------------------------------
# Purpose:
#   Construct Table 3.1 summarizing the final analysis panels
#   used in the thesis for the Europe and United States samples.
#
# Description:
#   - Loads the final Europe and US analysis panels produced in
#     the replication pipeline
#   - Computes simple panel statistics:
#         * number of industries
#         * sample years
#         * number of observations
#   - Produces a descriptive table by country
#   - Exports the table in CSV and LaTeX format
#
# Inputs:
#   - dataclean/panel_eu/panel_common_2003_2020_analysis_strict.rds
#   - dataclean/panel_us/panel_US_main_cr4_balanced_2002_2022.csv
#
# Outputs:
#   - outputs/tables/Table_3_1_analysis_panels.csv
#   - outputs/tables/Table_3_1_analysis_panels.tex
#
# Notes:
#   - Europe panel reports statistics separately for Germany,
#     France, Denmark, and Sweden.
#   - US panel reports statistics for the national industry
#     panel constructed from ILPA industries mapped to NAICS
#     concentration measures.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(cli)
  library(knitr)
})

# ============================================================
# 0) PATHS
# ============================================================

ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_CLEAN   <- file.path(ROOT, "dataclean")
PANEL_EU_DIR <- file.path(DATA_CLEAN, "panel_eu")
PANEL_US_DIR <- file.path(DATA_CLEAN, "panel_us")

OUT_DIR <- file.path(ROOT, "outputs", "tables")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

PATH_EU_MAIN <- file.path(
  PANEL_EU_DIR,
  "panel_common_2003_2020_analysis_strict.rds"
)

PATH_US_MAIN <- file.path(
  PANEL_US_DIR,
  "panel_US_main_cr4_balanced_2002_2022.csv"
)

stopifnot(
  file.exists(PATH_EU_MAIN),
  file.exists(PATH_US_MAIN)
)

# ============================================================
# 1) LOAD DATA
# ============================================================

eu_main <- readRDS(PATH_EU_MAIN)

us_main <- read_csv(
  PATH_US_MAIN,
  show_col_types = FALSE
)

# ============================================================
# 2) EUROPE PANEL SUMMARY (country level)
# ============================================================

tab_europe <- eu_main %>%
  transmute(
    country = as.character(country),
    year = as.integer(year),
    industry_clean = as.character(industry_clean)
  ) %>%
  group_by(country) %>%
  summarise(
    Panel = "Europe",
    Industries = paste0(n_distinct(industry_clean), " NACE 2-digit industries"),
    Years = paste0(min(year, na.rm = TRUE), "--", max(year, na.rm = TRUE)),
    Observations = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Country = case_when(
      country == "DE" ~ "Germany",
      country == "DK" ~ "Denmark",
      country == "FR" ~ "France",
      country == "SE" ~ "Sweden",
      TRUE ~ country
    )
  ) %>%
  select(Country, Panel, Industries, Years, Observations)

# ============================================================
# 3) UNITED STATES PANEL SUMMARY
# ============================================================

tab_us <- us_main %>%
  transmute(
    year = as.integer(year),
    industry_key = as.character(industry_key)
  ) %>%
  summarise(
    Country = "United States",
    Panel = "United States",
    Industries = paste0(n_distinct(industry_key), " NAICS 2--4 digit industries"),
    Years = paste0(min(year, na.rm = TRUE), "--", max(year, na.rm = TRUE)),
    Observations = n()
  )

# ============================================================
# 4) COMBINE + ORDER TABLE
# ============================================================

table_3_1 <- bind_rows(tab_europe, tab_us) %>%
  mutate(
    order_id = case_when(
      Country == "Germany" ~ 1,
      Country == "France" ~ 2,
      Country == "Denmark" ~ 3,
      Country == "Sweden" ~ 4,
      Country == "United States" ~ 5,
      TRUE ~ 99
    )
  ) %>%
  arrange(order_id) %>%
  select(-order_id)

print(table_3_1, n = Inf)

# ============================================================
# 5) EXPORT CSV
# ============================================================

write_csv(
  table_3_1,
  file.path(OUT_DIR, "Table_3_1_analysis_panels.csv")
)

# ============================================================
# 6) EXPORT LATEX
# ------------------------------------------------------------
# Export only the tabular environment so it can be wrapped
# inside a table environment in Overleaf.
# ============================================================

latex_table <- knitr::kable(
  table_3_1,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Country", "Panel", "Industries", "Years", "Observations")
)

cat(
  latex_table,
  file = file.path(OUT_DIR, "Table_3_1_analysis_panels.tex")
)

cli::cli_alert_success("✓ Table 3.1 exported to outputs/tables/")