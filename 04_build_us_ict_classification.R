# ============================================================
# 04_build_us_ict_classification
#
# Purpose:
# Construct ICT intensity measures and classify U.S. ILPA industries
# into ICT-producing, ICT-using, and Other industries.
#
# Classification:
# - ICT-producing: industries mapped to NAICS 334
# - ICT-using vs Other: median split of baseline ICT intensity
#
# ICT intensity definition:
# ICT intensity is proxied by the share of IT capital compensation
# in total capital compensation in ILPA.
#
# Baseline year: 1997
# Industries without ICT intensity in 1997 are excluded from the
# final classified panel.
#
# Output:
# data/clean/us_ilpa_ICT_industry_panel_NARROW.csv
# data/clean/US_ILPA_ICT_classification_check_NARROW.csv
# Unit of observation:
# - industry-year (panel file)
# - industry (classification check file)
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

# ------------------------------------------------
# Paths
# ------------------------------------------------

library(here)

DATA_RAW   <- here("data", "raw")
DATA_CLEAN <- here("data", "clean")

IPA_FILE   <- file.path(DATA_RAW, "industry-production-account-capital.xlsx")
PANEL_FILE <- file.path(DATA_CLEAN, "us_industry_productivity_panel.csv")
NAICS_FILE <- file.path(DATA_CLEAN, "us_ilpa_naics_map_long.csv")

OUT_DIR <- DATA_CLEAN
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Check required inputs
required_files <- c(IPA_FILE, PANEL_FILE, NAICS_FILE)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing required input files:\n", paste(missing_files, collapse = "\n"))
}

# ------------------------------------------------
# Settings
# ------------------------------------------------

COUNTRY <- "US"
BASE_YEAR_CLASS <- 1997L

# ------------------------------------------------
# Helpers
# ------------------------------------------------

make_industry_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

read_ilpa_wide_year_sheet <- function(file, sheet, skip = 1) {
  
  raw <- suppressMessages(
    read_excel(file, sheet = sheet, skip = skip, .name_repair = "minimal")
  )
  
  names(raw)[1] <- "industry"
  
  year_cols <- names(raw)[grepl("^[0-9]{4}$", names(raw))]
  
  raw %>%
    filter(!is.na(industry)) %>%
    mutate(industry = str_squish(as.character(industry))) %>%
    pivot_longer(cols = all_of(year_cols),
                 names_to = "year",
                 values_to = "value") %>%
    mutate(
      industry_key = make_industry_key(industry),
      year = as.integer(year),
      value = as.numeric(value)
    ) %>%
    arrange(industry_key, year)
  
}

apply_median_split <- function(ind_tbl) {
  
  med_val <- ind_tbl %>%
    filter(!ICT_producing) %>%
    summarise(med = median(ICT_intensity_base, na.rm = TRUE)) %>%
    pull(med)
  
  ind_tbl %>%
    mutate(
      ICT_group_baseline = case_when(
        ICT_producing ~ "ICT-producing",
        ICT_intensity_base > med_val ~ "ICT-using",
        TRUE ~ "Other"
      )
    )
  
}

# ------------------------------------------------
# 1) Load industry panel
# ------------------------------------------------

us_panel <- read_csv(PANEL_FILE, show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    industry = str_squish(industry),
    industry_key = make_industry_key(industry_key)
  )

# ------------------------------------------------
# 2) Load NAICS mapping and identify ICT producers
# ------------------------------------------------

naics_long <- read_csv(NAICS_FILE, show_col_types = FALSE) %>%
  mutate(
    industry_key = make_industry_key(industry_key),
    NAICS_code = as.character(NAICS_code),
    naics3 = substr(NAICS_code, 1, 3)
  )

producer_keys <- naics_long %>%
  filter(naics3 == "334") %>%
  distinct(industry_key)

# ------------------------------------------------
# 3) Read capital compensation sheets
# ------------------------------------------------

cap_it    <- read_ilpa_wide_year_sheet(IPA_FILE, "Capital_IT Compensation")       %>% rename(cap_it = value)
cap_soft  <- read_ilpa_wide_year_sheet(IPA_FILE, "Capital_Software Compensation") %>% rename(cap_soft = value)
cap_other <- read_ilpa_wide_year_sheet(IPA_FILE, "Capital_Other Compensation")    %>% rename(cap_other = value)
cap_rd    <- read_ilpa_wide_year_sheet(IPA_FILE, "Capital_R&D Compensation")      %>% rename(cap_rd = value)
cap_art   <- read_ilpa_wide_year_sheet(IPA_FILE, "Capital_Art Compensation")      %>% rename(cap_art = value)

cap_panel <- cap_it %>%
  full_join(cap_soft,  by = c("industry_key","year")) %>%
  full_join(cap_other, by = c("industry_key","year")) %>%
  full_join(cap_rd,    by = c("industry_key","year")) %>%
  full_join(cap_art,   by = c("industry_key","year")) %>%
  mutate(
    cap_total = coalesce(cap_it,0)+coalesce(cap_soft,0)+
      coalesce(cap_other,0)+coalesce(cap_rd,0)+
      coalesce(cap_art,0),
    
    ICT_share_US = cap_it / cap_total
  )

# ------------------------------------------------
# 4) Merge ICT intensity into panel
# ------------------------------------------------

us_panel_ict <- us_panel %>%
  left_join(
    cap_panel %>% select(industry_key,year,ICT_share_US),
    by = c("industry_key","year")
  ) %>%
  mutate(
    ICT_producing = industry_key %in% producer_keys$industry_key
  )
missing_ict <- us_panel_ict %>%
  filter(is.na(ICT_share_US)) %>%
  distinct(industry_key, industry)

if (nrow(missing_ict) > 0) {
  warning("Some industry-year observations have missing ICT intensity.")
}
# ------------------------------------------------
# 5) Build 1997 baseline intensity
# ------------------------------------------------

baseline_tbl <- us_panel_ict %>%
  filter(year == BASE_YEAR_CLASS,
         is.finite(ICT_share_US)) %>%
  distinct(industry_key, .keep_all = TRUE) %>%
  transmute(
    industry_key,
    ICT_intensity_base = ICT_share_US
  )

ind_tbl <- us_panel_ict %>%
  distinct(industry_key, industry, ICT_producing) %>%
  left_join(baseline_tbl, by = "industry_key") %>%
  filter(is.finite(ICT_intensity_base))

ind_class <- apply_median_split(ind_tbl)

dup_class <- ind_class %>%
  count(industry_key) %>%
  filter(n > 1)

if (nrow(dup_class) > 0) {
  print(dup_class)
  stop("Duplicate industry classifications detected.")
}

# ------------------------------------------------
# 6) Merge classification back into panel
# ------------------------------------------------

us_panel_ict <- us_panel_ict %>%
  inner_join(
    ind_class %>%
      select(industry_key,ICT_intensity_base,ICT_group_baseline),
    by="industry_key"
  )

# ------------------------------------------------
# 7) Save final datasets
# ------------------------------------------------

write_csv(
  us_panel_ict,
  file.path(OUT_DIR,"us_ilpa_ICT_industry_panel_NARROW.csv")
)

industry_classification <- us_panel_ict %>%
  group_by(industry_key) %>%
  summarise(
    industry = first(industry),
    ICT_producing = first(ICT_producing),
    ICT_intensity_base = first(ICT_intensity_base),
    ICT_group_baseline = first(ICT_group_baseline),
    .groups="drop"
  ) %>%
  arrange(desc(ICT_producing),
          desc(ICT_intensity_base))

write_csv(
  industry_classification,
  file.path(OUT_DIR,"US_ILPA_ICT_classification_check_NARROW.csv")
)

# ------------------------------------------------
# Diagnostics (printed only)
# ------------------------------------------------

cat("\nIndustry classification summary:\n")

print(
  industry_classification %>%
    count(ICT_group_baseline)
)

cat("\nNumber of industries:", nrow(industry_classification),"\n")

cat("\n✓ Script 04 finished\n")