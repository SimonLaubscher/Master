# ============================================================
# 08_build_compnet_concentration.R
#
# Purpose:
#   Build annual CompNet concentration panels and construct the
#   final hybrid dataset used in the thesis:
#     - V10: DE / FR / DK
#     - V9 : SE
#
# Outputs (dataclean/):
#   compnet_v10_annual.csv
#   compnet_v9_overlap_DE_FR_DK.csv
#   compnet_v9_se_annual.csv
#   compnet_hybrid_annual.csv
#   compnet_year_availability.csv
#
# Main variable used in the thesis:
#   HHI_rev = CV07_hhi_rev_pop_2D_tot
#
# Added for aggregation weighting later:
#   FV08_nrev_mn  = mean nominal revenue
#   FV08_nrev_sw  = summed weights (= population # firms / survey weights)
#   rev_tot_proxy = FV08_nrev_mn * FV08_nrev_sw (proxy for total nominal revenue)
#
# Notes:
#   (A) HHI scaling:
#       - detects whether raw HHI is in [0,1] or [0,10000]
#       - normalizes to [0,1] internally if needed
#       - final output is stored on the [0,10000] scale
#   (B) Check key uniqueness BEFORE any de-duplication
#   (C) Add diagnostics for rev_tot_proxy
#   (D) Add missing-year listing per country
# ============================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(readr)
  library(here)
})

# -------------------------
# Paths
# -------------------------


DATA_RAW   <- here("data", "raw")
DATA_CLEAN <- here("data", "clean")

FILE_V10 <- file.path(
  DATA_RAW,
  "unconditional_industry2d_20e_weightedV10.dta"
)

FILE_V9 <- file.path(
  DATA_RAW,
  "unconditional_industry2d_20e_weightedV9.dta"
)

# Ensure output directory exists
dir.create(DATA_CLEAN, recursive = TRUE, showWarnings = FALSE)

# Check required inputs
if (!file.exists(FILE_V10)) {
  stop("Input file not found: ", FILE_V10)
}

if (!file.exists(FILE_V9)) {
  stop("Input file not found: ", FILE_V9)
}

# -------------------------
# Helpers
# -------------------------
key_unique_or_stop <- function(df, keys) {
  dups <- df %>% count(across(all_of(keys))) %>% filter(n > 1)
  if (nrow(dups) > 0) {
    print(head(dups, 20))
    stop("Duplicate keys detected for: ", paste(keys, collapse = ", "))
  }
  invisible(TRUE)
}

to_iso2 <- function(x) {
  v <- toupper(trimws(as.character(x)))
  is_iso2 <- str_detect(v, "^[A-Z]{2}$")
  out <- v
  out[!is_iso2] <- recode(
    out[!is_iso2],
    "GERMANY" = "DE", "DEUTSCHLAND" = "DE",
    "FRANCE"  = "FR", "FRANKREICH" = "FR",
    "DENMARK" = "DK", "DANMARK"    = "DK",
    "SWEDEN"  = "SE", "SVERIGE"    = "SE",
    .default  = out[!is_iso2]
  )
  out
}

# Ensure HHI is on [0,1] scale
check_and_scale_hhi <- function(x, varname) {
  r <- range(x, na.rm = TRUE)
  if (!is.finite(r[1]) || !is.finite(r[2])) return(x)
  
  if (r[1] >= -1e-8 && r[2] <= 1 + 1e-8) return(x)
  
  if (r[1] >= -1e-8 && r[2] <= 10000 + 1e-6) {
    message(
      varname,
      ": detected HHI scaling ~0..10000; converting to 0..1 (divide by 10000). Range was [",
      round(r[1], 2), ", ", round(r[2], 2), "]"
    )
    return(x / 10000)
  }
  
  stop(sprintf(
    "%s has unexpected range [%.4f, %.4f]. Check scaling/definition.",
    varname, r[1], r[2]
  ))
}

# Missing-year listing helper
missing_years_by_country <- function(df, country_var = "country", year_var = "year") {
  out <- df %>%
    distinct(.data[[country_var]], .data[[year_var]]) %>%
    group_by(.data[[country_var]]) %>%
    summarise(
      min_year = min(.data[[year_var]], na.rm = TRUE),
      max_year = max(.data[[year_var]], na.rm = TRUE),
      missing_years = {
        yrs <- sort(unique(.data[[year_var]]))
        full <- seq(min_year, max_year)
        miss <- setdiff(full, yrs)
        if (length(miss) == 0) "" else paste(miss, collapse = ", ")
      },
      .groups = "drop"
    ) %>%
    mutate(missing_years = as.character(missing_years))
  
  out
}

# -------------------------
# Core builder
# -------------------------
build_annual <- function(path, keep_countries, source_tag) {
  
  x <- read_dta(path) %>% zap_labels()
  
  x <- x %>%
    mutate(
      country = if ("country_iso2" %in% names(.)) country_iso2 else to_iso2(country),
      year    = as.integer(year),
      NACE2_num = as.integer(industry2d),
      NACE2 = ifelse(is.na(NACE2_num), NA_character_, sprintf("%02d", NACE2_num))
    ) %>%
    filter(country %in% keep_countries, !is.na(year), !is.na(NACE2))
  
  # Main concentration variable used in the thesis
  v_hhi_rev <- "CV07_hhi_rev_pop_2D_tot"
  
  # Variables used to construct revenue-based aggregation weights
  v_nrev_mn <- "FV08_nrev_mn"
  v_nrev_sw <- "FV08_nrev_sw"
  
  out <- x %>%
    transmute(
      country,
      year,
      NACE2,
      HHI_rev = as.numeric(.data[[v_hhi_rev]]),
      FV08_nrev_mn = as.numeric(.data[[v_nrev_mn]]),
      FV08_nrev_sw = as.numeric(.data[[v_nrev_sw]]),
      rev_tot_proxy = as.numeric(.data[[v_nrev_mn]]) * as.numeric(.data[[v_nrev_sw]]),
      source_vintage = source_tag
    )
  
  # key uniqueness
  key_unique_or_stop(out, c("country", "year", "NACE2"))
  
  # ensure HHI is standardized and returned in 0–10000 scale
  out <- out %>%
    mutate(
      HHI_rev = check_and_scale_hhi(HHI_rev, "HHI_rev"),  # normalize to 0–1 if needed
      HHI_rev = HHI_rev * 10000                           # convert to 0–10000 (final scale)
    )
  if (any(out$HHI_rev < 0 | out$HHI_rev > 10000, na.rm = TRUE)) {
    stop("HHI_rev outside [0,10000] after scaling.")
  }
  
  # sanity checks for weights
  if (any(out$FV08_nrev_sw < 0, na.rm = TRUE)) stop("FV08_nrev_sw has negatives (unexpected).")
  if (any(out$rev_tot_proxy < 0, na.rm = TRUE)) stop("rev_tot_proxy has negatives (unexpected).")
  
  # diagnostics
  cat("\n================ BUILD SUMMARY:", source_tag, "================\n")
  cat("Rows:", nrow(out), "\n")
  cat("Countries:", paste(sort(unique(out$country)), collapse = ", "), "\n")
  cat("Years:", min(out$year, na.rm = TRUE), "-", max(out$year, na.rm = TRUE), "\n")
  cat("NACE2 count:", n_distinct(out$NACE2), "\n")
  
  cat("\nMissingness:\n")
  print(
    out %>%
      summarise(
        n = n(),
        miss_HHI_rev = mean(is.na(HHI_rev)),
        miss_nrev_mn = mean(is.na(FV08_nrev_mn)),
        miss_nrev_sw = mean(is.na(FV08_nrev_sw)),
        miss_rev_tot = mean(is.na(rev_tot_proxy))
      ),
    n = Inf
  )
  
  cat("\nWeight diagnostics (rev_tot_proxy):\n")
  print(
    out %>%
      summarise(
        n_nonmiss = sum(!is.na(rev_tot_proxy)),
        share_nonmiss = mean(!is.na(rev_tot_proxy)),
        min = suppressWarnings(min(rev_tot_proxy, na.rm = TRUE)),
        p50 = suppressWarnings(median(rev_tot_proxy, na.rm = TRUE)),
        mean = suppressWarnings(mean(rev_tot_proxy, na.rm = TRUE)),
        max = suppressWarnings(max(rev_tot_proxy, na.rm = TRUE))
      ),
    n = Inf
  )
  
  cat("\nCorrelation diagnostics (proxy vs ingredients):\n")
  print(
    out %>%
      summarise(
        cor_proxy_vs_sw = cor(rev_tot_proxy, FV08_nrev_sw, use = "pairwise.complete.obs"),
        cor_proxy_vs_mn = cor(rev_tot_proxy, FV08_nrev_mn, use = "pairwise.complete.obs")
      ),
    n = Inf
  )
  
  cat("\nMissing years by country (within this build):\n")
  print(missing_years_by_country(out, "country", "year"), n = Inf)
  
  out
}

# -------------------------
# Build datasets
# -------------------------

# Main sample from CompNet V10: DE / FR / DK
comp_v10 <- build_annual(FILE_V10, c("DE", "FR", "DK"), source_tag = "compnet_v10")
write_csv(comp_v10, file.path(DATA_CLEAN, "compnet_v10_annual.csv"))

# Validation sample: V9 overlap for DE / FR / DK
# Used to compare with V10 and validate the hybrid construction.
comp_v9_overlap <- build_annual(FILE_V9, c("DE", "FR", "DK"), source_tag = "compnet_v9")
write_csv(comp_v9_overlap, file.path(DATA_CLEAN, "compnet_v9_overlap_DE_FR_DK.csv"))

# Sweden comes from CompNet V9
comp_v9_se <- build_annual(FILE_V9, c("SE"), source_tag = "compnet_v9")
write_csv(comp_v9_se, file.path(DATA_CLEAN, "compnet_v9_se_annual.csv"))

# Final hybrid dataset used in the thesis:
# V10 for DE / FR / DK and V9 for SE
comp_hybrid <- bind_rows(comp_v10, comp_v9_se) %>%
  arrange(country, year, NACE2)

key_unique_or_stop(comp_hybrid, c("country", "year", "NACE2"))

# Final sanity on hybrid
cat("\n================ HYBRID SUMMARY ================\n")
cat("Rows:", nrow(comp_hybrid), "\n")
cat("Countries:", paste(sort(unique(comp_hybrid$country)), collapse = ", "), "\n")
cat("Years:", min(comp_hybrid$year, na.rm = TRUE), "-", max(comp_hybrid$year, na.rm = TRUE), "\n")
cat("NACE2 count:", n_distinct(comp_hybrid$NACE2), "\n")

cat("\nMissingness in hybrid:\n")
print(
  comp_hybrid %>%
    summarise(
      n = n(),
      miss_HHI_rev = mean(is.na(HHI_rev)),
      miss_nrev_mn = mean(is.na(FV08_nrev_mn)),
      miss_nrev_sw = mean(is.na(FV08_nrev_sw)),
      miss_rev_tot = mean(is.na(rev_tot_proxy))
    ),
  n = Inf
)

cat("\nMissing years by country (hybrid):\n")
print(missing_years_by_country(comp_hybrid, "country", "year"), n = Inf)

avail <- missing_years_by_country(comp_hybrid, "country", "year")
write_csv(avail, file.path(DATA_CLEAN, "compnet_year_availability.csv"))

write_csv(comp_hybrid, file.path(DATA_CLEAN, "compnet_hybrid_annual.csv"))

message("✓ Script 08 complete: CompNet hybrid dataset and year-availability table saved.")