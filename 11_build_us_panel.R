# ============================================================
# 11_build_us_panel.R
# ------------------------------------------------------------
# Purpose:
#   Merge the US ILPA industry panel with US Economic Census
#   concentration data.
#
# Main steps:
#   - load the cleaned ILPA panel with ICT classification
#   - load the cleaned Census concentration panel
#   - map ILPA industries to Census NAICS4 industries
#   - aggregate Census CR4 and CR8 to ILPA industry-year level
#     using Census receipts (RCPTOT) as weights
#   - construct annual concentration series by carrying forward
#     Census values between Census years
#   - save the full merged panel
#   - save the main analysis sample balanced on CR4
#
# Inputs:
#   data/clean/us_ilpa_ICT_industry_panel_NARROW.csv
#   data/clean/us_ilpa_naics_map_long.csv
#   data/clean/census_concentration_panel_2002_2022.csv
#
# Outputs:
#   data/clean/panel_us/panel_US_full_ILPAxCensus_2002_2022.csv
#   data/clean/panel_us/panel_US_main_cr4_balanced_2002_2022.csv
#
# Notes:
#   - CR4 is the main concentration measure used in the thesis.
#   - CR8 is retained for robustness analysis.
#   - Census concentration is observed in 5-year waves and
#     carried forward between Census years.
#   - Some ILPA industries cannot be mapped to NAICS4 and will
#     have missing concentration measures in the full panel.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(cli)
  library(here)
})

# ----------------------------
# Paths
# ----------------------------


DATA_CLEAN <- here("data", "clean")
OUT_DIR    <- here("data", "clean", "panel_us")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

PATH_ILPA_PANEL    <- file.path(DATA_CLEAN, "us_ilpa_ICT_industry_panel_NARROW.csv")
PATH_ILPA_MAP_LONG <- file.path(DATA_CLEAN, "us_ilpa_naics_map_long.csv")
PATH_CENSUS        <- file.path(DATA_CLEAN, "census_concentration_panel_2002_2022.csv")

stopifnot(
  file.exists(PATH_ILPA_PANEL),
  file.exists(PATH_ILPA_MAP_LONG),
  file.exists(PATH_CENSUS)
)
# ----------------------------
# Helpers
# ----------------------------

# Clean NAICS codes to digits only
norm_naics <- function(x) {
  y <- gsub("[^0-9]", "", as.character(x))
  ifelse(nchar(y) == 0, NA_character_, y)
}

# Weighted mean using Census receipts as weights
wmean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}


# ============================================================
# 1) Load ILPA base panel
# ============================================================

us_ilpa <- read_csv(PATH_ILPA_PANEL, show_col_types = FALSE) %>%
  mutate(
    country      = "US",
    year         = as.integer(year),
    industry     = str_squish(as.character(industry)),
    industry_key = str_squish(as.character(industry_key))
  )

cli::cli_alert_success(
  "Loaded ILPA panel: {nrow(us_ilpa)} rows, {n_distinct(us_ilpa$industry_key)} industries, years {min(us_ilpa$year, na.rm = TRUE)}–{max(us_ilpa$year, na.rm = TRUE)}"
)

# Check that excluded aggregate/government industries are not present
bad_gov <- us_ilpa %>%
  distinct(industry) %>%
  filter(industry %in% c("Federal", "State and local", "All industries"))

if (nrow(bad_gov) > 0) {
  print(bad_gov)
  stop("Excluded aggregate/government industries found in ILPA panel.")
}

# Check that ILPA panel is unique at industry-year level
dup_ilpa <- us_ilpa %>%
  count(industry_key, year, name = "n") %>%
  filter(n > 1)

if (nrow(dup_ilpa) > 0) {
  print(dup_ilpa)
  stop("Duplicate industry_key-year rows found in ILPA panel.")
}

YEAR_MIN <- 2002L
YEAR_MAX <- 2022L
YEAR_SEQ <- YEAR_MIN:YEAR_MAX

us_ilpa <- us_ilpa %>%
  filter(year >= YEAR_MIN, year <= YEAR_MAX)


# ============================================================
# 2) Load Census panel (NAICS 4-digit, Census years only)
# ============================================================
cen4_base <- read_csv(PATH_CENSUS, show_col_types = FALSE) %>%
  mutate(
    year      = as.integer(year),
    NAICS_key = as.character(NAICS_key),
    NAICS_len = as.integer(NAICS_len),
    NAICS4    = vapply(NAICS_key, norm_naics, character(1)),
    CR4       = suppressWarnings(as.numeric(CR4)),
    CR8       = suppressWarnings(as.numeric(CR8)),
    RCPTOT    = suppressWarnings(as.numeric(RCPTOT))
  ) %>%
  filter(year %in% c(2002L, 2007L, 2012L, 2017L, 2022L)) %>%
  filter(NAICS_len == 4L) %>%
  filter(!is.na(NAICS4), nchar(NAICS4) == 4)

# Expand Census NAICS4 data to annual frequency by carrying values forward
cen4_step <- tidyr::crossing(
  NAICS4 = sort(unique(cen4_base$NAICS4)),
  year   = YEAR_SEQ
) %>%
  left_join(
    cen4_base %>% select(NAICS4, year, CR4, CR8, RCPTOT),
    by = c("NAICS4", "year")
  ) %>%
  arrange(NAICS4, year) %>%
  group_by(NAICS4) %>%
  tidyr::fill(CR4, CR8, RCPTOT, .direction = "down") %>%
  ungroup() %>%
  mutate(
    naics3 = substr(NAICS4, 1, 3),
    naics2 = substr(NAICS4, 1, 2)
  )

# --- diagnostics: CR4 and RCPTOT coverage after step-fill ---
cr4_cov <- cen4_step %>%
  summarise(
    n_naics4 = n_distinct(NAICS4),
    share_CR4_nonmiss = mean(is.finite(CR4)),
    share_CR8_nonmiss = mean(is.finite(CR8)),
    share_RCPTOT_nonmiss = mean(is.finite(RCPTOT) & RCPTOT > 0)
  )

print(cr4_cov)


# ============================================================
# 3) Build ILPA -> Census NAICS4 links
# ============================================================
ilpa_map_long <- read_csv(PATH_ILPA_MAP_LONG, show_col_types = FALSE) %>%
  mutate(
    industry_key = as.character(industry_key),
    industry     = as.character(industry),
    NAICS_code   = vapply(NAICS_code, norm_naics, character(1)),
    NAICS_len    = as.integer(NAICS_len)
  ) %>%
  filter(!is.na(NAICS_code), NAICS_len %in% 2:4) %>%
  distinct(industry_key, NAICS_code, .keep_all = TRUE) %>%
  mutate(
    naics2 = if_else(nchar(NAICS_code) >= 2, substr(NAICS_code, 1, 2), NA_character_),
    naics3 = if_else(nchar(NAICS_code) >= 3, substr(NAICS_code, 1, 3), NA_character_),
    naics4 = if_else(nchar(NAICS_code) >= 4, substr(NAICS_code, 1, 4), NA_character_)
  )

best_depth <- ilpa_map_long %>%
  group_by(industry_key) %>%
  summarise(
    has4 = any(!is.na(naics4) & nchar(naics4) == 4),
    has3 = any(!is.na(naics3) & nchar(naics3) == 3),
    has2 = any(!is.na(naics2) & nchar(naics2) == 2),
    .groups = "drop"
  ) %>%
  mutate(
    map_type = case_when(
      has4 ~ "naics4",
      has3 ~ "naics3",
      has2 ~ "naics2",
      TRUE ~ "unmapped"
    )
  )

ilpa_inds <- us_ilpa %>%
  distinct(industry_key, industry)

ilpa_best <- ilpa_inds %>%
  left_join(best_depth %>% select(industry_key, map_type), by = "industry_key")

cen_naics4_by3 <- cen4_step %>%
  distinct(naics3, NAICS4)

cen_naics4_by2 <- cen4_step %>%
  distinct(naics2, NAICS4)

ilpa_best_join <- ilpa_best %>%
  left_join(
    ilpa_map_long %>% distinct(industry_key, naics2, naics3, naics4),
    by = "industry_key"
  )

ilpa_to_naics4 <- bind_rows(
  ilpa_best_join %>%
    filter(map_type == "naics4", !is.na(naics4), nchar(naics4) == 4) %>%
    transmute(industry_key, industry, NAICS4 = naics4, map_type),
  
  ilpa_best_join %>%
    filter(map_type == "naics3", !is.na(naics3), nchar(naics3) == 3) %>%
    left_join(cen_naics4_by3, by = "naics3", relationship = "many-to-many") %>%
    transmute(industry_key, industry, NAICS4, map_type),
  
  ilpa_best_join %>%
    filter(map_type == "naics2", !is.na(naics2), nchar(naics2) == 2) %>%
    left_join(cen_naics4_by2, by = "naics2", relationship = "many-to-many") %>%
    transmute(industry_key, industry, NAICS4, map_type)
) %>%
  filter(!is.na(NAICS4), nchar(NAICS4) == 4) %>%
  distinct(industry_key, NAICS4, .keep_all = TRUE)

missing_map <- ilpa_inds %>%
  anti_join(ilpa_to_naics4 %>% distinct(industry_key), by = "industry_key")

if (nrow(missing_map) > 0) {
  print(missing_map)
}
# ------------------------------------------------------------
# Clarify consequence of unmapped industries
# ------------------------------------------------------------
if (nrow(missing_map) > 0) {
  cli::cli_alert_info(
    "Unmapped ILPA industries will have CR4/CR8 = NA in panel_US_full (no NAICS4 links)."
  )
}
# ============================================================
# 4) Aggregate Census concentration to ILPA industry-year
# ============================================================
ilpa_conc_year <- ilpa_to_naics4 %>%
  left_join(
    cen4_step %>% select(NAICS4, year, CR4, CR8, RCPTOT),
    by = "NAICS4",
    relationship = "many-to-many"
  ) %>%
  group_by(industry_key, industry, year) %>%
  summarise(
    map_type = first(map_type),
    n_naics4_links = n_distinct(NAICS4),
    share_have_weight = mean(is.finite(RCPTOT) & RCPTOT > 0),
    CR4 = wmean_safe(CR4, RCPTOT),
    CR8 = wmean_safe(CR8, RCPTOT),
    RCPTOT_sum = if (any(is.finite(RCPTOT) & RCPTOT > 0)) {
      sum(RCPTOT[is.finite(RCPTOT) & RCPTOT > 0])
    } else {
      NA_real_
    },
    .groups = "drop"
  )

expected_max <- n_distinct(us_ilpa$industry_key) * length(YEAR_SEQ)
if (nrow(ilpa_conc_year) > expected_max) {
  stop("Too many rows in ilpa_conc_year (possible join explosion).")
}


weight_cov_year <- ilpa_conc_year %>%
  group_by(year) %>%
  summarise(
    n_industries = n_distinct(industry_key),
    share_have_weight_mean = mean(share_have_weight, na.rm = TRUE),
    share_CR4_nonmiss = mean(is.finite(CR4)),
    share_CR8_nonmiss = mean(is.finite(CR8)),
    .groups = "drop"
  )

print(weight_cov_year)


# ============================================================
# 5) Merge Census concentration into ILPA panel
# ============================================================
panel_US_full <- us_ilpa %>%
  left_join(ilpa_conc_year, by = c("industry_key", "industry", "year")) %>%
  arrange(year, industry)

dup_full <- panel_US_full %>%
  count(industry_key, year, name = "n") %>%
  filter(n > 1)

if (nrow(dup_full) > 0) {
  print(dup_full)
  stop("Duplicate industry_key-year rows found after merge.")
}

write_csv(
  panel_US_full,
  file.path(OUT_DIR, "panel_US_full_ILPAxCensus_2002_2022.csv")
)

cli::cli_h2("CHECK: CR4 coverage in merged panel")

print(
  panel_US_full %>%
    summarise(
      share_CR4_nonmiss = mean(is.finite(CR4)),
      share_CR8_nonmiss = mean(is.finite(CR8)),
      n_industries = n_distinct(industry_key)
    ),
  n = Inf
)
# ============================================================
# 6) Build main analysis sample (balanced on CR4)
# ============================================================
main_industries <- panel_US_full %>%
  filter(year >= YEAR_MIN, year <= YEAR_MAX) %>%
  group_by(industry_key, industry) %>%
  summarise(
    ok_balanced_cr4 = all(is.finite(CR4)),
    .groups = "drop"
  ) %>%
  filter(ok_balanced_cr4) %>%
  pull(industry_key)

panel_US_main_cr4_balanced <- panel_US_full %>%
  filter(industry_key %in% main_industries) %>%
  arrange(year, industry)

write_csv(
  panel_US_main_cr4_balanced,
  file.path(OUT_DIR, "panel_US_main_cr4_balanced_2002_2022.csv")
)
cli::cli_h2("CHECK: balanced sample structure")

print(
  panel_US_main_cr4_balanced %>%
    summarise(
      n_rows = n(),
      n_industries = n_distinct(industry_key),
      n_years = n_distinct(year),
      min_year = min(year),
      max_year = max(year)
    ),
  n = Inf
)
bad_balance <- panel_US_main_cr4_balanced %>%
  count(industry_key) %>%
  filter(n != length(YEAR_SEQ))

if (nrow(bad_balance) > 0) {
  print(bad_balance)
  stop("Balanced panel is NOT fully balanced.")
}
cli::cli_h2("CHECK: CR4 range")

print(
  panel_US_full %>%
    summarise(
      min_CR4 = min(CR4, na.rm = TRUE),
      p01 = quantile(CR4, 0.01, na.rm = TRUE),
      p50 = median(CR4, na.rm = TRUE),
      p99 = quantile(CR4, 0.99, na.rm = TRUE),
      max_CR4 = max(CR4, na.rm = TRUE)
    ),
  n = Inf
)

cli::cli_alert_success("✓ Script 11 complete: U.S. full and balanced panels saved.")