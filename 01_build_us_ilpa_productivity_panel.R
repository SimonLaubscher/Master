# ============================================================
# 01_build_us_ilpa_productivity_panel
#
# Purpose:
# Construct the U.S. industry-level productivity panel used in the thesis,
# based on BEA BLS ILPA data.
#
# Description:
# This script:
# - reads raw ILPA Excel files (productivity and contributions)
# - excludes total and government industries
# - computes labor productivity growth (log differences)
# - retains value added levels 
# - reshapes value-added growth contributions into wide format
# - merges all components into a final industry-year panel
# - exports a clean dataset for subsequent analysis
#
# Output:
# data/clean/us_industry_productivity_panel.csv
# Unit of observation: industry-year
# Coverage: U.S. industries (excluding government), 1997–2023
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})


library(here)

DATA_RAW   <- here("data", "raw")
DATA_CLEAN <- here("data", "clean")

IPA_FILE     <- file.path(DATA_RAW, "industry-production-account-capital.xlsx")
CONTRIB_FILE <- file.path(DATA_RAW, "industry-contributions-to-growth.xlsx")
OUT_FILE     <- file.path(DATA_CLEAN, "us_industry_productivity_panel.csv")
dir.create(DATA_CLEAN, recursive = TRUE, showWarnings = FALSE)


required_files <- c(IPA_FILE, CONTRIB_FILE)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing required input files:\n", paste(missing_files, collapse = "\n"))
}




COUNTRY <- "US"

# ----------------------------
# Rows to drop
# ----------------------------
DROP_TOTALS <- c("All industries")
DROP_GOV_EXACT <- c("Federal", "State and local")

# ----------------------------
# Helper functions
# ----------------------------
logdiff <- function(x) {
  dplyr::if_else(
    !is.na(x) & x > 0 & !is.na(dplyr::lag(x)) & dplyr::lag(x) > 0,
    log(x) - log(dplyr::lag(x)),
    NA_real_
  )
}

rename_if_present <- function(df, old, new) {
  if (old %in% names(df)) {
    dplyr::rename(df, !!new := all_of(old))
  } else {
    df
  }
}

make_industry_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

read_ilpa_index_sheet <- function(file, sheet, skip = 1) {
  raw <- suppressMessages(
    read_excel(file, sheet = sheet, skip = skip, .name_repair = "minimal")
  )
  
  if (ncol(raw) < 2) stop("Sheet too small or misread: ", sheet)
  names(raw)[1] <- "industry"
  
  year_cols <- names(raw)[grepl("^[0-9]{4}$", names(raw))]
  if (length(year_cols) == 0) stop("No year columns detected in sheet: ", sheet)
  
  out <- raw %>%
    filter(!is.na(industry)) %>%
    filter(!(industry %in% c("Industry Description", "Note:"))) %>%
    filter(!grepl("^\\*", as.character(industry))) %>%
    filter(!(industry %in% DROP_TOTALS)) %>%
    filter(!(industry %in% DROP_GOV_EXACT)) %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "year",
      values_to = "value"
    ) %>%
    mutate(
      industry     = str_squish(as.character(industry)),
      industry_key = make_industry_key(industry),
      year         = as.integer(year),
      value        = as.numeric(value)
    ) %>%
    arrange(industry_key, year)
  
  dup <- out %>%
    count(industry_key, year) %>%
    filter(n > 1)
  
  if (nrow(dup) > 0) {
    print(dup)
    stop("Duplicate industry_key-year rows in sheet: ", sheet)
  }
  
  out
}

# ----------------------------
# Productivity data
# ----------------------------
lp_long <- read_ilpa_index_sheet(
  file = IPA_FILE,
  sheet = "Integrated Labor Productivity",
  skip = 1
) %>%
  rename(lp_index = value, industry_lp = industry)

va_level_long <- read_ilpa_index_sheet(
  file = IPA_FILE,
  sheet = "Value Added",
  skip = 1
) %>%
  rename(va_level = value, industry_va = industry)

us_prod_core <- lp_long %>%
  full_join(
    va_level_long %>% select(industry_key, year, va_level, industry_va),
    by = c("industry_key", "year")
  ) %>%
  mutate(industry = coalesce(industry_lp, industry_va)) %>%
  select(-industry_lp, -industry_va) %>%
  arrange(industry_key, year) %>%
  group_by(industry_key) %>%
  mutate(
    country = COUNTRY,
    lp_g = logdiff(lp_index)
  ) %>%
  ungroup()

if (any(is.na(us_prod_core$industry))) {
  stop("Some rows have missing industry labels after joins.")
}

bad_check <- us_prod_core %>%
  distinct(industry) %>%
  filter(industry %in% c(DROP_TOTALS, DROP_GOV_EXACT))

if (nrow(bad_check) > 0) {
  print(bad_check)
  stop("Dropped totals/government industries still present after filtering.")
} else {
  message("✓ Totals and government industries excluded.")
}

# ----------------------------
# Contributions to VA growth
# ----------------------------
VA_SHEET <- "Contributions to VA growth"

raw_va <- suppressMessages(
  read_excel(CONTRIB_FILE, sheet = VA_SHEET, skip = 2, .name_repair = "minimal")
)

if (ncol(raw_va) < 3) {
  stop("VA contributions sheet read too few columns. Check skip value.")
}

names(raw_va)[1] <- "industry"
names(raw_va)[2] <- "component"

year_cols <- names(raw_va)[grepl("^[0-9]{4}$", names(raw_va))]
if (length(year_cols) == 0) {
  stop("No year columns detected in VA sheet. Try skip=1 or skip=3.")
}

contrib_va_long <- raw_va %>%
  mutate(
    industry     = str_squish(as.character(industry)),
    industry_key = make_industry_key(industry),
    component    = str_squish(as.character(component))
  ) %>%
  filter(!is.na(industry), !is.na(component)) %>%
  filter(!(industry %in% DROP_TOTALS)) %>%
  filter(!(industry %in% DROP_GOV_EXACT)) %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "contribution"
  ) %>%
  mutate(
    year = as.integer(year),
    contribution = as.numeric(contribution)
  ) %>%
  arrange(industry_key, component, year)

va_growth_contrib <- contrib_va_long %>%
  filter(component == "Value Added") %>%
  select(industry_key, year, va_growth_contrib = contribution)

contrib_va_components <- contrib_va_long %>%
  filter(component != "Value Added")

tmp_names <- contrib_va_components %>%
  distinct(component) %>%
  mutate(component_clean = make.names(component))

if (any(duplicated(tmp_names$component_clean))) {
  print(tmp_names)
  stop("Component name collision after make.names(). Use manual renaming.")
}

contrib_va_wide <- contrib_va_components %>%
  mutate(component_clean = make.names(component)) %>%
  select(industry_key, year, component_clean, contribution) %>%
  pivot_wider(names_from = component_clean, values_from = contribution) %>%
  arrange(industry_key, year)

# ----------------------------
# Final panel
# ----------------------------
us_final <- us_prod_core %>%
  left_join(contrib_va_wide, by = c("industry_key", "year")) %>%
  left_join(va_growth_contrib, by = c("industry_key", "year")) %>%
  arrange(industry_key, year) %>%
  rename_if_present("Capital", "va_contrib_capital_total") %>%
  rename_if_present("Labor", "va_contrib_labor_total") %>%
  rename_if_present("Integrated.TFP.Growth", "va_contrib_tfp") %>%
  rename_if_present("IT.Capital", "va_contrib_it_capital") %>%
  rename_if_present("Software.Capital", "va_contrib_software_capital") %>%
  rename_if_present("R.D.Capital", "va_contrib_rd_capital") %>%
  rename_if_present("Other.Capital", "va_contrib_other_capital") %>%
  rename_if_present("Entertainment.Originals.Capital", "va_contrib_entertainment_capital") %>%
  rename_if_present("College.Labor", "va_contrib_college_labor") %>%
  rename_if_present("Non.College.Labor", "va_contrib_noncollege_labor")

write_csv(us_final, OUT_FILE)

# ----------------------------
# Diagnostics
# ----------------------------
diag <- us_final %>%
  summarise(
    n = n(),
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE),
    n_industry = n_distinct(industry_key)
  )

print(diag)

# ----------------------------
# Sanity checks
# ----------------------------
stopifnot(
  nrow(us_final) > 0,
  all(!is.na(us_final$lp_index)),
  all(!is.na(us_final$va_level))
)

dup_check <- us_final %>%
  count(industry_key, year) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  stop("Duplicates detected in final dataset.")
}

message("✓ US industry productivity panel build complete.")
message("Dataset written to: ", normalizePath(OUT_FILE, winslash = "/"))


# ----------------------------
# Sanity checks
# ----------------------------
stopifnot(
  nrow(us_final) > 0,
  all(!is.na(us_final$lp_index)),
  all(!is.na(us_final$va_level))
)

dup_check <- us_final %>%
  count(industry_key, year) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  stop("Duplicates detected in final dataset.")
}