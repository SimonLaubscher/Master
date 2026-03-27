# ================================================================
# 07_classify_klems_ict_industries
#
# Purpose:
# Classify EU KLEMS industries into ICT-producing, ICT-using, and Other
# using a 1997 baseline ICT intensity measure.
#
# Classification:
# - ICT-producing: industry C26
# - ICT-using vs Other: country-specific median split of ICT_intensity_1997
# - industries without a valid 1997 baseline are flagged as
#   "ICT baseline missing"
#
# Creates:
# - ICT_group_baseline
# - ICT_using_dummy
# - ICT_group_usable
# - baseline_status
#
# Input:
# - data/clean/klems_panel_full_master_EUonly.rds
#
# Outputs:
# - data/clean/klems_classified_ICT_C26_ONLY_1997baseline.rds
# - data/clean/klems_classified_ICT_C26_ONLY_1997baseline.csv
# - data/clean/ICT_classification_check_C26_ONLY_1997baseline.csv
# - data/clean/ICT_group_shares_by_country.csv
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

library(here)

DATA_CLEAN <- here("data", "clean")

IN_FILE <- file.path(DATA_CLEAN, "klems_panel_full_master_EUonly.rds")

OUT_RDS   <- file.path(DATA_CLEAN, "klems_classified_ICT_C26_ONLY_1997baseline.rds")
OUT_CSV   <- file.path(DATA_CLEAN, "klems_classified_ICT_C26_ONLY_1997baseline.csv")
OUT_CHECK <- file.path(DATA_CLEAN, "ICT_classification_check_C26_ONLY_1997baseline.csv")
OUT_GROUP <- file.path(DATA_CLEAN, "ICT_group_shares_by_country.csv")

# Ensure output directory exists
dir.create(DATA_CLEAN, recursive = TRUE, showWarnings = FALSE)
# Check required input
if (!file.exists(IN_FILE)) {
  stop("Input file not found: ", IN_FILE)
}
# ------------------------------------------------------------
# 0) Load cleaned EU KLEMS panel
# ------------------------------------------------------------
df <- readRDS(IN_FILE)

# ------------------------------------------------------------
# 1) Define ICT-producing industries
# ------------------------------------------------------------
ICT_PRODUCING <- c("C26")

# ------------------------------------------------------------
# 2) Required inputs check
# ------------------------------------------------------------
needed <- c(
  "country", "year", "industry_clean", "industry_name",
  "ICT_intensity_1997"
)

miss <- setdiff(needed, names(df))
if (length(miss) > 0) {
  stop(
    "Missing required variables in input RDS: ",
    paste(miss, collapse = ", "),
    "\nRun 02_clean_klems_prepare.R first."
  )
}

df <- df %>%
  mutate(industry_clean = as.character(industry_clean))

# Helper: baseline must be constant within country-industry
unique_or_na <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 1) ux else NA_real_
}

# ------------------------------------------------------------
# 3) Build one-row-per-country-industry baseline table
# ------------------------------------------------------------
df_ci <- df %>%
  group_by(country, industry_clean) %>%
  summarise(
    industry_name      = first(industry_name),
    ICT_intensity_1997 = unique_or_na(ICT_intensity_1997),
    .groups = "drop"
  ) %>%
  mutate(
    ICT_producing_flag    = industry_clean %in% ICT_PRODUCING,
    baseline_reliable_1997 = is.finite(ICT_intensity_1997),
    baseline_status = case_when(
      ICT_producing_flag ~ "ICT-producing (forced)",
      baseline_reliable_1997 ~ "Reliable (1997)",
      TRUE ~ "1997 missing"
    )
  )

# ------------------------------------------------------------
# 4) Country medians
#    Median based on all non-ICT-producing industries with valid 1997 baseline
# ------------------------------------------------------------
country_medians_1997 <- df_ci %>%
  filter(!ICT_producing_flag, baseline_reliable_1997) %>%
  group_by(country) %>%
  summarise(
    med_ICT_1997 = median(ICT_intensity_1997, na.rm = TRUE),
    n_ind_for_median = n(),
    .groups = "drop"
  )

print(country_medians_1997)

if (any(country_medians_1997$n_ind_for_median < 8)) {
  warning("Some countries have < 8 industries with reliable 1997 baseline. Median split may be unstable.")
}

expected_countries <- sort(unique(df_ci$country))
median_countries <- sort(unique(country_medians_1997$country))

if (!identical(expected_countries, median_countries)) {
  print(median_countries)
  warning("Not all countries received a valid ICT median.")
}
# ------------------------------------------------------------
# 5) Assign ICT-using vs Other
# ------------------------------------------------------------
df_ci <- df_ci %>%
  left_join(country_medians_1997 %>% select(country, med_ICT_1997), by = "country") %>%
  mutate(
    # ties at the median are assigned to "Other"
    ICT_using_base_flag = case_when(
      ICT_producing_flag ~ FALSE,
      baseline_reliable_1997 ~ ICT_intensity_1997 > med_ICT_1997,
      TRUE ~ NA
    ),
    ICT_group_baseline = case_when(
      ICT_producing_flag ~ "ICT-producing",
      ICT_using_base_flag == TRUE  ~ "ICT-using",
      ICT_using_base_flag == FALSE ~ "Other",
      TRUE ~ "ICT baseline missing"
    ),
    ICT_group_baseline = factor(
      ICT_group_baseline,
      levels = c("ICT-producing", "ICT-using", "Other", "ICT baseline missing")
    ),
    ICT_group_usable = ICT_group_baseline %in% c("ICT-producing", "ICT-using", "Other"),
    ICT_using_dummy = case_when(
      ICT_group_baseline == "ICT-using" ~ 1L,
      ICT_group_baseline == "Other"     ~ 0L,
      TRUE                              ~ NA_integer_
    )
  )

dup_class <- df_ci %>%
  count(country, industry_clean) %>%
  filter(n > 1)

if (nrow(dup_class) > 0) {
  print(dup_class)
  stop("Duplicate country-industry classifications detected.")
}
# ------------------------------------------------------------
# 6) Join classification back to full panel
# ------------------------------------------------------------
df_out <- df %>%
  left_join(
    df_ci %>%
      select(
        country, industry_clean,
        ICT_producing_flag, baseline_reliable_1997, baseline_status,
        med_ICT_1997, ICT_using_base_flag, ICT_group_baseline,
        ICT_group_usable, ICT_using_dummy
      ),
    by = c("country", "industry_clean")
  )

missing_class <- df_out %>%
  filter(is.na(ICT_group_baseline)) %>%
  distinct(country, industry_clean)

if (nrow(missing_class) > 0) {
  print(missing_class)
  stop("Some panel observations did not receive an ICT classification.")
}

# ------------------------------------------------------------
# 7) Save outputs
# ------------------------------------------------------------
saveRDS(df_out, OUT_RDS)
write_csv(df_out, OUT_CSV)

check_tbl <- df_ci %>%
  arrange(country, ICT_group_baseline, desc(ICT_intensity_1997)) %>%
  select(
    country, industry_clean, industry_name,
    ICT_intensity_1997, med_ICT_1997,
    ICT_producing_flag, ICT_using_base_flag, ICT_group_baseline,
    ICT_group_usable, ICT_using_dummy,
    baseline_status
  )

write_csv(check_tbl, OUT_CHECK)

group_shares <- df_ci %>%
  count(country, ICT_group_baseline) %>%
  group_by(country) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

write_csv(group_shares, OUT_GROUP)

cat(
  "✓ ICT baseline classification completed (C26-only; median split uses 1997 baseline).\n",
  "Saved:\n",
  " - ", normalizePath(OUT_RDS, winslash = "/"), "\n",
  " - ", normalizePath(OUT_CSV, winslash = "/"), "\n",
  " - ", normalizePath(OUT_CHECK, winslash = "/"), "\n",
  " - ", normalizePath(OUT_GROUP, winslash = "/"), "\n",
  sep = ""
)

print(group_shares, n = 200)