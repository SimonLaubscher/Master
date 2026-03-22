# ============================================================
# 02_build_us_naics_crosswalk
# Purpose:
# Attach NAICS codes to the US ILPA panel and build clean
# NAICS crosswalk files for later merges.
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

ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_RAW   <- file.path(ROOT, "dataraw")
DATA_CLEAN <- file.path(ROOT, "dataclean")

IPA_FILE <- file.path(DATA_RAW, "industry-production-account-capital.xlsx")
IN_FILE  <- file.path(DATA_CLEAN, "us_industry_productivity_panel.csv")

OUT_DIR <- DATA_CLEAN
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

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

# ------------------------------------------------
# 8) NAICS4 mapping
# ------------------------------------------------

naics4_long <- naics_long %>%
  mutate(
    naics4 = if_else(
      nchar(NAICS_code) == 4,
      NAICS_code,
      NA_character_
    )
  ) %>%
  filter(!is.na(naics4)) %>%
  select(industry, industry_key, naics4) %>%
  distinct() %>%
  arrange(industry, naics4)

write_csv(
  naics4_long,
  file.path(OUT_DIR, "us_ilpa_naics4_map_long.csv")
)

message("✓ US ILPA NAICS mapping build complete.")
message("Files written to: ", normalizePath(OUT_DIR, winslash = "/"))










