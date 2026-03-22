# ================================================================
# 06_klems_coverage_diagnostics
# Coverage + Missingness Diagnostics for EU KLEMS thesis sample
#
# Purpose:
#   - Check panel coverage after Script 02
#   - Summarize missingness in key yearly variables
#   - Check baseline ICT coverage
#   - Create heatmaps and problem lists for appendix / diagnostics
#
# Input:
#   - dataclean/klems_panel_full_master_EUonly.rds
#
# Outputs:
#   - output/diagnostics/table_A1_country_row_counts.csv
#   - output/diagnostics/table_A1_industry_counts.csv
#   - output/diagnostics/coverage_years_by_industry_country.csv
#   - output/diagnostics/heatmap_valid_LP1G.png
#   - output/diagnostics/heatmap_valid_ICTcap.png
#   - output/diagnostics/table_A2_missing_shares_by_industry.csv
#   - output/diagnostics/table_A3_missing_shares_by_country.csv
#   - output/diagnostics/table_A4_missing_shares_by_industry_country.csv
#   - output/diagnostics/table_A3b_missing_shares_ICT_baselines_by_country.csv
#   - output/diagnostics/table_A4b_missing_shares_ICT_baselines_by_industry_country.csv
#   - output/diagnostics/problem_pairs_LP1G_all_missing.csv
#   - output/diagnostics/problem_industries_by_country.csv
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_CLEAN <- file.path(ROOT, "dataclean")
OUT_DIR    <- file.path(ROOT, "diagnostics")

IN_FILE <- file.path(DATA_CLEAN, "klems_panel_full_master_EUonly.rds")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(IN_FILE)) {
  stop("Input file not found: ", IN_FILE)
}

# ------------------------------------------------------------
# 0) Load data
# ------------------------------------------------------------
df <- readRDS(IN_FILE)

min_y <- min(df$year, na.rm = TRUE)
max_y <- max(df$year, na.rm = TRUE)
expected_years_fullwindow <- (max_y - min_y + 1)

keep_industries <- c(
  "A", "B",
  "C10-C12", "C13-C15", "C16-C18", "C19", "C20-C21", "C22-C23", "C24-C25",
  "C26", "C27", "C28", "C29-C30", "C31-C33",
  "D", "E", "F",
  "G45", "G46", "G47",
  "H49", "H50", "H51", "H52", "H53",
  "I",
  "J58-J60", "J61", "J62-J63",
  "K", "L", "M", "N", "O", "P",
  "Q86", "Q87-Q88", "R", "S", "T", "U"
)

df <- df %>%
  mutate(industry_clean = factor(industry_clean, levels = keep_industries))

# ------------------------------------------------------------
# 0b) Must-have variables check
# ------------------------------------------------------------
vars_yearly <- c(
  "LP1_G",
  "LP1ConLC",
  "LP1ConTangNICT",
  "LP1ConTangICT",
  "LP1ConIntang",
  "LP1ConTFP",
  "CAP_QI",
  "CAPICT_QI",
  "ICT_intensity_index"
)

vars_yearly_pp <- c(
  "LP1_G_pp",
  "LP1ConLC_pp",
  "LP1ConTangNICT_pp",
  "LP1ConTangICT_pp",
  "LP1ConIntang_pp",
  "LP1ConTFP_pp"
)

vars_baseline <- c(
  "ICT_intensity_1997",
  "ICT_intensity_base"
)

must_have <- c(vars_yearly, vars_yearly_pp, vars_baseline)

missing_vars <- setdiff(must_have, names(df))
if (length(missing_vars) > 0) {
  stop(
    "Missing expected variables in input RDS: ",
    paste(missing_vars, collapse = ", "),
    "\nRun 02_clean_klems_prepare.R again, or update variable names."
  )
}

# ------------------------------------------------------------
# 0c) Unit sanity checks
# ------------------------------------------------------------
unit_check <- df %>%
  summarise(
    med_abs_LP1_G    = median(abs(LP1_G), na.rm = TRUE),
    p99_abs_LP1_G    = quantile(abs(LP1_G), 0.99, na.rm = TRUE),
    med_abs_LP1_G_pp = median(abs(LP1_G_pp), na.rm = TRUE),
    p99_abs_LP1_G_pp = quantile(abs(LP1_G_pp), 0.99, na.rm = TRUE)
  )

cat("\n=== Unit sanity (log vs pp) ===\n")
print(unit_check)

if (is.finite(unit_check$p99_abs_LP1_G) && unit_check$p99_abs_LP1_G > 0.5) {
  warning("LP1_G p99(abs) > 0.5. Unusually large for Δln; check Script 02 conversion / outliers.")
}

# One-row-per-country-industry table for baseline variables
df_ci <- df %>%
  distinct(country, industry_clean, industry_name,
           ICT_intensity_1997, ICT_intensity_base)

# ------------------------------------------------------------
# 1) Basic coverage
# ------------------------------------------------------------
country_counts <- df %>%
  count(country, name = "n_rows")

industry_counts <- df %>%
  group_by(country) %>%
  summarise(
    n_industries = n_distinct(industry_clean),
    .groups = "drop"
  )

write_csv(country_counts,  file.path(OUT_DIR, "table_A1_country_row_counts.csv"))
write_csv(industry_counts, file.path(OUT_DIR, "table_A1_industry_counts.csv"))

coverage_years <- df %>%
  group_by(industry_clean, country) %>%
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_years_expected_fullwindow = expected_years_fullwindow,
    n_years_anyrow = n_distinct(year),
    n_years_LP1G   = sum(!is.na(LP1_G)),
    n_years_ICTcap = sum(!is.na(CAPICT_QI)),
    n_years_CAP    = sum(!is.na(CAP_QI)),
    n_years_ICTint = sum(!is.na(ICT_intensity_index)),
    n_years_missing_LP1G   = expected_years_fullwindow - n_years_LP1G,
    n_years_missing_ICTcap = expected_years_fullwindow - n_years_ICTcap,
    n_years_missing_CAP    = expected_years_fullwindow - n_years_CAP,
    n_years_missing_ICTint = expected_years_fullwindow - n_years_ICTint,
    .groups = "drop"
  )

write_csv(coverage_years, file.path(OUT_DIR, "coverage_years_by_industry_country.csv"))

# ------------------------------------------------------------
# 2) Valid-year heatmaps
# ------------------------------------------------------------
valid_LP1G <- df %>%
  group_by(industry_clean, country) %>%
  summarise(
    n_valid_LP1G = sum(!is.na(LP1_G)),
    .groups = "drop"
  )

p_LP1G <- ggplot(valid_LP1G, aes(x = country, y = industry_clean, fill = n_valid_LP1G)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", na.value = "grey80") +
  theme_minimal() +
  labs(
    title = paste0("Valid Years: LP1_G (Δln, ", min_y, "–", max_y, ")"),
    x = "Country",
    y = "Industry (grouped codes)",
    fill = "# Valid years"
  )

ggsave(
  filename = file.path(OUT_DIR, "heatmap_valid_LP1G.png"),
  plot = p_LP1G,
  width = 10,
  height = 8
)

valid_ICT <- df %>%
  group_by(industry_clean, country) %>%
  summarise(
    n_valid_ICTcap = sum(!is.na(CAPICT_QI)),
    .groups = "drop"
  )

p_ICT <- ggplot(valid_ICT, aes(x = country, y = industry_clean, fill = n_valid_ICTcap)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", na.value = "grey80") +
  theme_minimal() +
  labs(
    title = paste0("Valid Years: CAPICT_QI (", min_y, "–", max_y, ")"),
    x = "Country",
    y = "Industry (grouped codes)",
    fill = "# Valid years"
  )

ggsave(
  filename = file.path(OUT_DIR, "heatmap_valid_ICTcap.png"),
  plot = p_ICT,
  width = 10,
  height = 8
)

# ------------------------------------------------------------
# 3) Missingness diagnostics
# ------------------------------------------------------------
missing_shares_by_industry <- df %>%
  group_by(industry_clean) %>%
  summarise(
    across(all_of(vars_yearly), ~ mean(is.na(.)), .names = "share_missing_{.col}"),
    .groups = "drop"
  ) %>%
  arrange(desc(share_missing_CAPICT_QI), desc(share_missing_CAP_QI), desc(share_missing_LP1_G))

write_csv(
  missing_shares_by_industry,
  file.path(OUT_DIR, "table_A2_missing_shares_by_industry.csv")
)

missing_shares_by_country <- df %>%
  group_by(country) %>%
  summarise(
    across(all_of(vars_yearly), ~ mean(is.na(.)), .names = "share_missing_{.col}"),
    .groups = "drop"
  ) %>%
  arrange(country)

write_csv(
  missing_shares_by_country,
  file.path(OUT_DIR, "table_A3_missing_shares_by_country.csv")
)

missing_shares_by_industry_country <- df %>%
  group_by(country, industry_clean) %>%
  summarise(
    across(all_of(vars_yearly), ~ mean(is.na(.)), .names = "share_missing_{.col}"),
    .groups = "drop"
  ) %>%
  arrange(country, desc(share_missing_CAPICT_QI), desc(share_missing_CAP_QI), desc(share_missing_LP1_G))

write_csv(
  missing_shares_by_industry_country,
  file.path(OUT_DIR, "table_A4_missing_shares_by_industry_country.csv")
)

baseline_missing_by_country <- df_ci %>%
  group_by(country) %>%
  summarise(
    share_missing_ICT_intensity_1997 = mean(is.na(ICT_intensity_1997)),
    share_missing_ICT_intensity_base = mean(is.na(ICT_intensity_base)),
    .groups = "drop"
  ) %>%
  arrange(country)

write_csv(
  baseline_missing_by_country,
  file.path(OUT_DIR, "table_A3b_missing_shares_ICT_baselines_by_country.csv")
)

baseline_missing_by_industry_country <- df_ci %>%
  group_by(country, industry_clean) %>%
  summarise(
    share_missing_ICT_intensity_1997 = mean(is.na(ICT_intensity_1997)),
    share_missing_ICT_intensity_base = mean(is.na(ICT_intensity_base)),
    .groups = "drop"
  ) %>%
  arrange(country, desc(share_missing_ICT_intensity_base))

write_csv(
  baseline_missing_by_industry_country,
  file.path(OUT_DIR, "table_A4b_missing_shares_ICT_baselines_by_industry_country.csv")
)

# ------------------------------------------------------------
# 4) Problem lists
# ------------------------------------------------------------
lp1g_all_missing <- df %>%
  group_by(country, industry_clean) %>%
  summarise(
    any_LP1G = any(!is.na(LP1_G)),
    .groups = "drop"
  ) %>%
  filter(!any_LP1G) %>%
  arrange(country, industry_clean)

write_csv(
  lp1g_all_missing,
  file.path(OUT_DIR, "problem_pairs_LP1G_all_missing.csv")
)

threshold <- 0.50

problem_panel <- missing_shares_by_industry_country %>%
  transmute(
    country, industry_clean,
    share_missing_LP1_G,
    share_missing_CAPICT_QI,
    share_missing_CAP_QI,
    share_missing_ICT_intensity_index,
    problem_panel = share_missing_LP1_G > threshold |
      share_missing_CAPICT_QI > threshold |
      share_missing_CAP_QI > threshold
  )

problem_baseline <- baseline_missing_by_industry_country %>%
  transmute(
    country, industry_clean,
    share_missing_ICT_intensity_base,
    problem_baseline = share_missing_ICT_intensity_base > threshold
  )

problem_industries <- problem_panel %>%
  left_join(problem_baseline, by = c("country", "industry_clean")) %>%
  mutate(
    problem_baseline = if_else(is.na(problem_baseline), FALSE, problem_baseline),
    problem_flag = problem_panel | problem_baseline
  ) %>%
  filter(problem_flag) %>%
  arrange(
    country,
    desc(problem_baseline),
    desc(problem_panel),
    desc(share_missing_LP1_G),
    desc(share_missing_CAPICT_QI)
  )

write_csv(
  problem_industries,
  file.path(OUT_DIR, "problem_industries_by_country.csv")
)

cat(
  "\n✔ EU KLEMS coverage + missingness diagnostics created.\n",
  "Year range detected: ", min_y, "–", max_y, "\n",
  "Saved outputs to: ", normalizePath(OUT_DIR, winslash = "/"), "\n",
  sep = ""
)