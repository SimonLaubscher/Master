# ================================================================
# 12_build_EU_panel.R
#
# Purpose:
#   Construct the industry–year panel used in the thesis by merging
#   EU KLEMS productivity data with CompNet firm-level concentration
#   measures aggregated to the NACE Rev.2 2-digit level.
#
# Key steps:
#   1. Load cleaned EU KLEMS and CompNet datasets
#   2. Restrict to the thesis countries (DE, DK, FR, SE)
#   3. Expand KLEMS grouped industries (e.g. C10-C12) to NACE2 parts
#   4. Merge CompNet concentration measures at the NACE2 level
#   5. Aggregate concentration to KLEMS industries using revenue weights
#   6. Construct the merged industry panel for 2003–2020
#   7. Apply strict completeness requirements for grouped industries
#
# Notes:
#   - HHI_rev from CompNet is used on the 0–10000 scale.
#   - The final analysis sample is not fully balanced due to data availability
#     (e.g. missing years or industries in some countries).
#
# Final outputs:
#   panel_common_2003_2020_analysis_strict.rds
#       -> Main estimation sample used in the thesis
#
#   panel_common_2003_2020_baseline.rds
#       -> Merged panel before analysis restrictions
#
#   panel_base_full.rds
#       -> Full merged dataset before restricting the time window
#
#   panel_merge_diagnostics.rds
#       -> Diagnostics on merge coverage and missing concentration data
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

# ----------------------------
# Paths
# ----------------------------
library(here)

DATA_CLEAN <- here("data", "clean")
OUT_DIR    <- file.path(DATA_CLEAN, "panel_eu")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

PATH_KLEMS   <- file.path(DATA_CLEAN, "klems_classified_ICT_C26_ONLY_1997baseline.rds")
PATH_COMPNET <- file.path(DATA_CLEAN, "compnet_hybrid_annual.csv")

# ----------------------------
# Sample definition
# ----------------------------
countries_keep <- c("DE", "DK", "FR", "SE")
AGG_CODES      <- c("TOT", "TOT_IND", "MARKT", "MARKTxAG")

COMMON_START <- 2003L
COMMON_END   <- 2020L

# Industries with no usable CompNet HHI observations in the thesis sample
DROP_ALWAYS <- c("C19", "Q86", "Q87-Q88")

# Require full NACE2 coverage for grouped KLEMS industries
MIN_PARTS_STRICT <- 1.00

# ----------------------------
# Input checks
# ----------------------------
stopifnot(
  file.exists(PATH_KLEMS),
  file.exists(PATH_COMPNET)
)

# ----------------------------
# Helpers
# ----------------------------
wmean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & (w > 0)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

expand_klems_to_nace2 <- function(industry_clean) {
  ic <- as.character(industry_clean)
  ic <- str_replace_all(ic, "–", "-")
  ic <- str_replace_all(ic, "\\s+", "")
  
  # Single NACE2 code, e.g. "C26" -> "26"
  if (str_detect(ic, "^[A-Z][0-9]{2}$")) {
    return(str_extract(ic, "[0-9]{2}"))
  }
  
  # Contiguous range, e.g. "C10-C12" -> c("10", "11", "12")
  if (str_detect(ic, "^[A-Z][0-9]{2}-[A-Z]?[0-9]{2}$")) {
    nums <- str_extract_all(ic, "[0-9]{2}")[[1]]
    if (length(nums) == 2) {
      a <- as.integer(nums[1])
      b <- as.integer(nums[2])
      if (!is.na(a) && !is.na(b) && a <= b) {
        return(sprintf("%02d", seq(a, b)))
      }
    }
  }
  
  character(0)
}

# ----------------------------
# 1) Load inputs
# ----------------------------
klems_raw <- readRDS(PATH_KLEMS) %>%
  mutate(
    country = as.character(country),
    year = as.integer(year),
    industry_clean = as.character(industry_clean)
  ) %>%
  filter(country %in% countries_keep) %>%
  filter(!industry_clean %in% AGG_CODES)

comp_raw <- read_csv(PATH_COMPNET, show_col_types = FALSE) %>%
  mutate(
    country = as.character(country),
    year = as.integer(year),
    NACE2 = sprintf("%02d", as.integer(NACE2))
  ) %>%
  filter(country %in% countries_keep)

# ----------------------------
# Required variables
# ----------------------------
req_klems <- c(
  "country", "year", "industry_clean",
  "LP1_G_pp",
  "ICT_group_baseline",
  "ICT_intensity_1997"
)

miss_k <- setdiff(req_klems, names(klems_raw))
if (length(miss_k) > 0) {
  stop("KLEMS input missing: ", paste(miss_k, collapse = ", "))
}

req_comp <- c("country", "year", "NACE2", "HHI_rev", "rev_tot_proxy")

miss_c <- setdiff(req_comp, names(comp_raw))
if (length(miss_c) > 0) {
  stop("CompNet input missing: ", paste(miss_c, collapse = ", "))
}

# ----------------------------
# 2) Prepare analysis-ready KLEMS variables
# ----------------------------
klems <- klems_raw %>%
  filter(
    str_detect(industry_clean, "^[A-Z][0-9]{2}$") |
      str_detect(industry_clean, "^[A-Z][0-9]{2}-[A-Z]?[0-9]{2}$")
  ) %>%
  mutate(
    # Use already prepared percentage-point productivity growth
    lp_pp = as.numeric(LP1_G_pp),
    ICT_intensity_1997 = as.numeric(ICT_intensity_1997),
    ICT_group_use = factor(
      ICT_group_baseline,
      levels = c("ICT-producing", "ICT-using", "Other", "ICT baseline missing")
    )
  )

# ----------------------------
# 3) Expand KLEMS industries to NACE2 parts
# ----------------------------
klems_expanded <- klems %>%
  mutate(nace2 = lapply(industry_clean, expand_klems_to_nace2)) %>%
  unnest(nace2, keep_empty = FALSE) %>%
  filter(!is.na(nace2), nace2 != "")

# ----------------------------
# 4) Merge KLEMS with CompNet at NACE2 level
# ----------------------------
comp_keep <- comp_raw %>%
  select(country, year, NACE2, HHI_rev, rev_tot_proxy)

dup_comp <- comp_keep %>%
  count(country, year, NACE2) %>%
  filter(n > 1)

if (nrow(dup_comp) > 0) {
  print(dup_comp)
  stop("CompNet keys are not unique: country-year-NACE2")
}

klems_nace2 <- klems_expanded %>%
  left_join(comp_keep, by = c("country", "year", "nace2" = "NACE2"))

# ----------------------------
# 5) Coverage diagnostics for grouped industries
# ----------------------------
parts_cov <- klems_nace2 %>%
  group_by(country, year, industry_clean) %>%
  summarise(
    expected_parts = n_distinct(nace2),
    realized_parts = n_distinct(nace2[!is.na(HHI_rev)]),
    share_parts_realized = realized_parts / expected_parts,
    .groups = "drop"
  )

# ----------------------------
# 6) Aggregate NACE2 concentration to KLEMS industries
# ----------------------------
comp_agg <- klems_nace2 %>%
  group_by(country, year, industry_clean) %>%
  summarise(
    HHI_rev_agg = wmean_safe(HHI_rev, rev_tot_proxy),
    .groups = "drop"
  )

# ----------------------------
# 7) Build merged panel at KLEMS industry-year level
#    Keep all original KLEMS variables
# ----------------------------
klems_keys <- klems %>%
  distinct(country, year, industry_clean, .keep_all = TRUE)

dup_k <- klems_keys %>%
  count(country, year, industry_clean) %>%
  filter(n > 1)

if (nrow(dup_k) > 0) {
  print(dup_k)
  stop("Duplicates in KLEMS keys after distinct().")
}

panel_base <- klems_keys %>%
  left_join(parts_cov, by = c("country", "year", "industry_clean")) %>%
  left_join(comp_agg,  by = c("country", "year", "industry_clean")) %>%
  filter(!industry_clean %in% DROP_ALWAYS)

panel_diagnostics <- panel_base %>%
  transmute(
    country,
    year,
    industry_clean,
    expected_parts,
    realized_parts,
    share_parts_realized,
    has_hhi = !is.na(HHI_rev_agg)
  )

# ----------------------------
# 8) Main thesis sample: balanced common window
# ----------------------------
panel_common_2003_2020_baseline <- panel_base %>%
  filter(year >= COMMON_START, year <= COMMON_END)

# ----------------------------
# 9) Final analysis sample: common window + strict completeness
#    Exclude industries with missing ICT baseline classification
# ----------------------------
panel_common_2003_2020_analysis_strict <- panel_common_2003_2020_baseline %>%
  filter(
    ICT_group_use %in% c("ICT-producing", "ICT-using", "Other"),
    is.finite(lp_pp),
    !is.na(HHI_rev_agg),
    !is.na(share_parts_realized) & share_parts_realized >= MIN_PARTS_STRICT
  )

# ----------------------------
# End-of-script checks
# ----------------------------
cat("\n=== CHECK: HHI_rev_agg range (full merged panel) ===\n")
print(
  panel_base %>%
    summarise(
      min = min(HHI_rev_agg, na.rm = TRUE),
      p01 = quantile(HHI_rev_agg, 0.01, na.rm = TRUE),
      p50 = median(HHI_rev_agg, na.rm = TRUE),
      p99 = quantile(HHI_rev_agg, 0.99, na.rm = TRUE),
      max = max(HHI_rev_agg, na.rm = TRUE)
    ),
  n = Inf
)

cat("\n=== CHECK: FR missing years in common strict analysis sample ===\n")
fr_missing <- panel_common_2003_2020_analysis_strict %>%
  filter(country == "FR") %>%
  distinct(year) %>%
  { setdiff(seq(COMMON_START, COMMON_END), .$year) }

cat("FR missing years:", paste(fr_missing, collapse = ", "), "\n")

cat("\n=== CHECK: strict completeness in analysis sample ===\n")
print(
  panel_common_2003_2020_analysis_strict %>%
    summarise(
      min_share = min(share_parts_realized, na.rm = TRUE),
      max_share = max(share_parts_realized, na.rm = TRUE)
    ),
  n = Inf
)

# ----------------------------
# 10) Print key sample summaries
# ----------------------------
cat("\n=== PANEL SUMMARY: common 2003-2020 baseline ===\n")
print(
  panel_common_2003_2020_baseline %>%
    summarise(
      n_rows = n(),
      n_countries = n_distinct(country),
      n_industries = n_distinct(interaction(country, industry_clean)),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE)
    ),
  n = Inf
)

cat("\n=== PANEL SUMMARY: common 2003-2020 strict analysis sample ===\n")
print(
  panel_common_2003_2020_analysis_strict %>%
    group_by(country) %>%
    summarise(
      n_rows = n(),
      n_industries = n_distinct(industry_clean),
      n_years = n_distinct(year),
      .groups = "drop"
    ),
  n = Inf
)

# ----------------------------
# 11) Exports
# ----------------------------
write_out <- function(df, stem) {
  saveRDS(df, file.path(OUT_DIR, paste0(stem, ".rds")))
  invisible(TRUE)
}

write_out(
  panel_common_2003_2020_analysis_strict,
  "panel_common_2003_2020_analysis_strict"
)

write_out(
  panel_common_2003_2020_baseline,
  "panel_common_2003_2020_baseline"
)

write_out(
  panel_base,
  "panel_base_full"
)

saveRDS(
  panel_diagnostics,
  file.path(OUT_DIR, "panel_merge_diagnostics.rds")
)

cat(
  "\n✓ EU KLEMS–CompNet merge complete.\nExports in:\n",
  normalizePath(OUT_DIR, winslash = "/"),
  "\n",
  sep = ""
)





