# ============================================================
# 02_build_us_naics_crosswalk
#
# Purpose:
# Attach NAICS codes to the U.S. ILPA productivity panel and
# construct a clean NAICS crosswalk for later merges.
#
# Description:
# This script:
# - reads the U.S. ILPA productivity panel from Script 01
# - reads the NAICS codes sheet from the ILPA workbook
# - cleans industry labels and NAICS mappings
# - applies manual patches for industries missing NAICS codes
# - verifies NAICS mapping coverage by merging with the ILPA panel
# - expands NAICS mappings into long format (2–4 digit codes)
# - exports a clean crosswalk dataset for subsequent merges
#
# Output:
# data/clean/us_ilpa_naics_map_long.csv
# Unit of observation: industry–NAICS mapping
# Coverage: U.S. ILPA industries
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

IPA_FILE <- file.path(DATA_RAW, "industry-production-account-capital.xlsx")
IN_FILE  <- file.path(DATA_CLEAN, "us_industry_productivity_panel.csv")

OUT_DIR <- DATA_CLEAN
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

required_files <- c(IPA_FILE, IN_FILE)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing required input files:\n", paste(missing_files, collapse = "\n"))
}


# ------------------------------------------------
# Helper functions
# ------------------------------------------------

make_industry_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

norm_naics <- function(x) {
  if (is.na(x)) return(NA_character_)
  y <- gsub("[^0-9]", "", as.character(x))
  if (!nzchar(y)) NA_character_ else y
}

extract_all_naics_codes <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  
  x <- str_squish(as.character(x))
  parts <- unlist(str_split(x, ","))
  parts <- str_squish(parts)
  
  out <- character(0)
  
  for (part in parts) {
    part <- gsub("[–—]", "-", part)
    
    if (grepl("^\\d{2,4}\\s*-\\s*\\d{2,4}$", part)) {
      bounds <- unlist(str_split(part, "\\s*-\\s*"))
      start <- as.integer(bounds[1])
      end   <- as.integer(bounds[2])
      
      if (!is.na(start) && !is.na(end) && start <= end) {
        out <- c(out, as.character(seq(start, end)))
      }
      
    } else if (grepl("^\\d{2,4}$", part)) {
      out <- c(out, part)
      
    } else {
      extracted <- str_extract_all(part, "\\d{2,4}")[[1]]
      if (length(extracted) > 0) {
        out <- c(out, extracted)
      }
    }
  }
  
  out <- vapply(out, norm_naics, character(1))
  out <- out[!is.na(out)]
  unique(out)
}

# ------------------------------------------------
# 1) Load ILPA panel
# ------------------------------------------------

us <- read_csv(IN_FILE, show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    industry = str_squish(industry),
    industry_key = make_industry_key(industry)
  )

if (!("va_level" %in% names(us))) {
  stop("Expected column 'va_level' not found. Check Script 01 outputs.")
}

# ------------------------------------------------
# 2) Read NAICS sheet
# ------------------------------------------------

naics_raw <- suppressMessages(
  read_excel(IPA_FILE, sheet = "NAICS codes", skip = 2, .name_repair = "minimal")
)

names(naics_raw)[1:3] <- c("industry", "unused", "naics_codes_raw")

# ------------------------------------------------
# 3) Clean NAICS mapping
# ------------------------------------------------

naics_map <- naics_raw %>%
  filter(!is.na(industry)) %>%
  mutate(
    industry = str_squish(as.character(industry)),
    industry_key = make_industry_key(industry),
    naics_codes_raw = str_squish(as.character(naics_codes_raw))
  ) %>%
  select(industry, industry_key, naics_codes_raw) %>%
  distinct()

# ------------------------------------------------
# 4) Manual patch for missing industries
# ------------------------------------------------

naics_patch <- tibble::tribble(
  ~industry, ~naics_codes_raw,
  "Data processing, internet publishing, and other information services", "518, 519",
  "Publishing industries, except internet (includes software)", "511"
) %>%
  mutate(industry_key = make_industry_key(industry))

naics_map <- naics_map %>%
  bind_rows(
    naics_patch %>%
      anti_join(naics_map, by = "industry_key")
  ) %>%
  distinct(industry_key, .keep_all = TRUE)

# ------------------------------------------------
# 5) Check duplicates before join
# ------------------------------------------------

dup_map <- naics_map %>%
  count(industry_key) %>%
  filter(n > 1)

if (nrow(dup_map) > 0) {
  print(dup_map)
  stop("naics_map has duplicate industry_key values; join would duplicate rows.")
}

# ------------------------------------------------
# 6) Merge NAICS mapping onto panel
# ------------------------------------------------

us_naics <- us %>%
  left_join(
    naics_map %>% select(industry_key, naics_codes_raw),
    by = "industry_key"
  )


missing_naics <- us_naics %>%
  distinct(industry_key, industry, naics_codes_raw) %>%
  filter(is.na(naics_codes_raw))

if (nrow(missing_naics) > 0) {
  print(missing_naics)
  warning("Some ILPA industries do not have a NAICS mapping.")
} else {
  message("✓ All ILPA industries matched to a NAICS mapping.")
}
# ------------------------------------------------
# 7) Detailed NAICS long map (2–4 digits)
# ------------------------------------------------

naics_long <- naics_map %>%
  mutate(NAICS_list = lapply(naics_codes_raw, extract_all_naics_codes)) %>%
  select(industry, industry_key, naics_codes_raw, NAICS_list) %>%
  unnest_longer(NAICS_list, values_to = "NAICS_code") %>%
  mutate(
    NAICS_code = vapply(NAICS_code, norm_naics, character(1)),
    NAICS_len  = nchar(NAICS_code)
  ) %>%
  filter(!is.na(NAICS_code), NAICS_len %in% 2:4) %>%
  distinct(industry_key, NAICS_code, .keep_all = TRUE) %>%
  arrange(industry, NAICS_len, NAICS_code)

write_csv(
  naics_long,
  file.path(OUT_DIR, "us_ilpa_naics_map_long.csv")
)



diag_naics <- tibble::tibble(
  mapped_industries = n_distinct(naics_map$industry_key),
  naics_long_rows = nrow(naics_long)
)

print(diag_naics)

message("✓ US ILPA NAICS mapping build complete.")
message("Files written to: ", normalizePath(OUT_DIR, winslash = "/"))
