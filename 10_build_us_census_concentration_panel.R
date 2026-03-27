# ============================================================
# 10_build_us_census_concentration_panel.R
# ------------------------------------------------------------
# US ECONOMIC CENSUS CONCENTRATION PANEL (2002–2022)
#
# Purpose:
#   Build the US Economic Census concentration panel and
#   carry totals used later as aggregation weights.
#
# Variables constructed:
#     - CR4    : share of receipts of 4 largest firms
#     - CR8    : share of receipts of 8 largest firms
#     - RCPTOT : industry receipts (used as weight)
#
# Notes:
#   - Panel remains at native NAICS_len (2–8); no aggregation here.
#   - Keeps US total + all establishments/all firms only.
#   - Concentration measures are receipts-based.
#   - Source files include both Excel (2002–2012) and pipe-delimited
#     Census extracts (2017, 2022).
#   - Some non-numeric flags in raw data (e.g. "D", "k") are handled
#     via numeric coercion and do not affect the final measures.
#
# Output:
#   data/clean/census_concentration_panel_2002_2022.csv
# ============================================================

suppressPackageStartupMessages({
  library(cli)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readxl)
  library(tibble)
  library(here)
})

# ---- Paths ----
DATA_RAW   <- here("data", "raw")
DATA_CLEAN <- here("data", "clean")

dir.create(DATA_CLEAN, showWarnings = FALSE, recursive = TRUE)

# ---- File paths ----
file02_xlsx <- file.path(DATA_RAW, "Daten Census 2002.xlsx")
file07_xlsx <- file.path(DATA_RAW, "Daten Census 2007.xlsx")
file12_xlsx <- file.path(DATA_RAW, "Daten Census 2012.xlsx")
file17_dat  <- file.path(DATA_RAW, "EC1700SIZECONCEN.dat")
file22_dat  <- file.path(DATA_RAW, "EC2200SIZECONCEN.dat")

if (!file.exists(file02_xlsx)) stop("Input file not found: ", file02_xlsx)
if (!file.exists(file07_xlsx)) stop("Input file not found: ", file07_xlsx)
if (!file.exists(file12_xlsx)) stop("Input file not found: ", file12_xlsx)
if (!file.exists(file17_dat))  stop("Input file not found: ", file17_dat)
if (!file.exists(file22_dat))  stop("Input file not found: ", file22_dat)

# ============================================================
# GENERIC HELPERS
# ============================================================

# ------------------------------------------------------------
# Remove metadata rows accidentally imported from Excel/CSV
# (e.g. rows containing "Geographic identifier code", "Year")
# ------------------------------------------------------------
drop_meta_any <- function(df) {
  
  geo_vals  <- if ("GEO_ID" %in% names(df)) as.character(df$GEO_ID) else rep("", nrow(df))
  year_vals <- if ("YEAR"   %in% names(df)) as.character(df$YEAR)   else rep("", nrow(df))
  
  df %>%
    mutate(across(where(is.character), stringr::str_squish)) %>%
    filter(
      !grepl("^Geographic identifier code$", geo_vals, fixed = TRUE),
      !grepl("^Year$", year_vals, fixed = TRUE)
    )
}


# ------------------------------------------------------------
# Identify rows corresponding to the United States total
# ------------------------------------------------------------
is_us_row <- function(df) {
  
  flag <- rep(TRUE, nrow(df))
  
  if ("GEOTYPE" %in% names(df))
    flag <- flag & df$GEOTYPE %in% c(1, "1", "01", 1L)
  
  if ("#GEOTYPE" %in% names(df))
    flag <- flag & df$`#GEOTYPE` %in% c("01","1")
  
  if ("GEO_ID" %in% names(df))
    flag <- flag & grepl("US$", as.character(df$GEO_ID), ignore.case = TRUE)
  
  if ("GEO_TTL" %in% names(df)) {
    flag <- flag & grepl("United States", df$GEO_TTL, ignore.case = TRUE)
    
  } else if ("GEO_LABEL" %in% names(df)) {
    flag <- flag & grepl("United States", df$GEO_LABEL, ignore.case = TRUE)
  }
  
  flag
}


# ------------------------------------------------------------
# Identify rows representing all establishments / all firms
# (filter out subsets of firms where possible)
# ------------------------------------------------------------
is_all_est_row <- function(df) {
  
  ok <- rep(TRUE, nrow(df))
  
  if ("OPTAX" %in% names(df)) {
    ok <- ok & (is.na(df$OPTAX) | grepl("^(A|0|T)", as.character(df$OPTAX)))
    
  } else if ("OPTAX_MEANING" %in% names(df)) {
    ok <- ok & (
      is.na(df$OPTAX_MEANING) |
        grepl("All (establishments|firms)|federal income tax",
              df$OPTAX_MEANING,
              ignore.case = TRUE)
    )
  }
  
  if ("TYPOP" %in% names(df))
    ok <- ok & (is.na(df$TYPOP) | df$TYPOP %in% c("00","0",""))
  
  ok
}


# ------------------------------------------------------------
# Prefer "All establishments" rows over taxable-only rows
# when duplicates exist
# ------------------------------------------------------------
prefer_all_over_taxable <- function(df) {
  
  rank_code <- function(x) {
    
    x <- as.character(x)
    
    dplyr::case_when(
      is.na(x)       ~ 0L,   # best: missing or all establishments
      grepl("^A", x) ~ 0L,   # "All establishments"
      grepl("^0", x) ~ 1L,
      grepl("^T", x) ~ 2L,   # taxable-only
      TRUE           ~ 3L
    )
  }
  
  key_cols <- intersect(
    c("year","NAICS_key","NAICS_len","CONCENFI","GEO_ID","GEO_TTL","GEO_LABEL"),
    names(df)
  )
  
  if (!length(key_cols))
    key_cols <- intersect(c("year","NAICS_key","NAICS_len"), names(df))
  
  df %>%
    mutate(
      .optax_rank = if ("OPTAX" %in% names(df)) rank_code(OPTAX) else 0L
    ) %>%
    group_by(across(all_of(key_cols))) %>%
    slice_min(.optax_rank, with_ties = FALSE) %>%
    ungroup() %>%
    select(-.optax_rank)
}




# ----------------- NAICS handling -----------------
prep_naics <- function(df, naics_col, keep_levels = 2:8,
                       require_us = TRUE, require_all_est = TRUE) {
  
  df1 <- df
  
  # Keep only United States rows if requested
  if (require_us)
    df1 <- df1[is_us_row(df1), , drop = FALSE]
  
  # Keep only all-establishment rows if requested
  if (require_all_est)
    df1 <- df1[is_all_est_row(df1), , drop = FALSE]
  
  df1 %>%
    mutate(
      
      # Ensure NAICS column is character
      !!naics_col := as.character(.data[[naics_col]]),
      
      # Clean raw NAICS code
      NAICS_raw = .data[[naics_col]] %>%
        as.character() %>%
        str_squish(),
      
      # Identify sector-level NAICS codes (e.g. 31 or 31-33)
      is_2hy = str_detect(NAICS_raw, "^(\\d{2}|\\d{2}-\\d{2})$"),
      
      # Remove all non-digit characters
      NAICS_digits = str_remove_all(NAICS_raw, "[^0-9]"),
      
      # Count number of digits
      ndigits = nchar(NAICS_digits),
      
      # Handle special grouped sectors used in NAICS
      sector_key = case_when(
        str_detect(NAICS_raw, "^31-33") ~ "31-33",
        str_detect(NAICS_raw, "^44-45") ~ "44-45",
        str_detect(NAICS_raw, "^48-49") ~ "48-49",
        TRUE                            ~ str_sub(NAICS_digits, 1, 2)
      ),
      
      # Construct the final industry key
      NAICS_key = case_when(
        is_2hy        ~ sector_key,
        ndigits >= 8  ~ str_sub(NAICS_digits, 1, 8),
        TRUE          ~ str_sub(NAICS_digits, 1, ndigits)
      ),
      
      # Store NAICS detail level
      NAICS_len = case_when(
        is_2hy ~ 2L,
        TRUE   ~ ndigits
      )
    ) %>%
    filter(!is.na(NAICS_len), NAICS_len %in% keep_levels)
}


# ---- Pick receipts column used later as aggregation weight ----

choose_totals_cols <- function(df) {
  list(RCPTOT_col = "RCPTOT")
}

# Map Census concentration codes to CR4 / CR8
map_con_metric <- function(df){
  
  con_lbl_col <- intersect(
    c("CONCENFI_TTL","CONCENFI_LABEL","CONCENFI_MEANING"),
    names(df)
  )[1]
  
  has_label <- !is.na(con_lbl_col)
  
  df %>%
    mutate(
      CONCENFI_chr = as.character(CONCENFI),
      
      # Use text labels if available
      CONCENFI_TXT = if (has_label)
        str_squish(tolower(.data[[con_lbl_col]]))
      else
        NA_character_,
      
      metric = case_when(
        
        has_label & str_detect(CONCENFI_TXT, "\\b4\\s*largest\\b") ~ "CR4",
        has_label & str_detect(CONCENFI_TXT, "\\b8\\s*largest\\b") ~ "CR8",
        
        # Fallback to Census codes
        CONCENFI_chr %in% c("604","804","4") ~ "CR4",
        CONCENFI_chr %in% c("608","808","8") ~ "CR8",
        
        TRUE ~ NA_character_
      )
    )
}
# Enforce CR4 <= CR8
enforce_monotone_cr <- function(cr4, cr8) {
  
  c4 <- cr4
  c8 <- pmax(c4, cr8, na.rm = TRUE)
  
  list(
    c4 = c4,
    c8 = c8
  )
}
build_label_map <- function(df, naics_col) {
  cand <- grep(
    paste0(
      "(?i)^", naics_col, "\\s*[_ ]?(meaning|label|title|ttl|desc|description)$|",
      "(?i)^(meaning|label|title|ttl|desc|description)$"
    ),
    names(df),
    value = TRUE,
    perl = TRUE
  )
  
  if (!length(cand))
    return(tibble(NAICS_key = character(), LABEL = character()))
  
  nm <- cand[1]
  
  df %>%
    filter(NAICS_len == 2) %>%
    transmute(NAICS_key, LABEL = as.character(.data[[nm]])) %>%
    distinct()
}

# Add fallback labels for grouped 2-digit sectors
add_hyphen_labels <- function(labmap) {
  fallback <- tibble(
    NAICS_key = c("31-33", "44-45", "48-49"),
    LABEL = c("Manufacturing", "Retail trade", "Transportation and warehousing")
  )
  
  bind_rows(labmap, fallback) %>%
    distinct(NAICS_key, .keep_all = TRUE)
}

validate_panel <- function(df) {
  
  dup_bad <- df %>%
    count(year, NAICS_key, NAICS_len) %>%
    filter(n > 1)
  
  if (nrow(dup_bad))
    cli::cli_alert_warning("Duplicate (year, key, len) rows: {nrow(dup_bad)}")
  
  mono_bad <- df %>%
    filter(!is.na(CR4) & !is.na(CR8) & CR8 < CR4)
  
  if (nrow(mono_bad))
    cli::cli_alert_warning("Found CR8 < CR4 in {nrow(mono_bad)} rows.")
  
  invisible(list(
    dups = dup_bad,
    mono = mono_bad
  ))
}

print_naics_len <- function(df, year) {
  tab <- df %>%
    count(NAICS_len, name = "rows") %>%
    arrange(NAICS_len)
  
  cli::cli_alert_info("NAICS lengths present in {year}: {paste(tab$NAICS_len, collapse = ', ')}")
  print(tab)
}
# ============================================================
# READERS
# ============================================================

read_all_sheets <- function(path) {
  
  sh <- excel_sheets(path)
  
  map_dfr(sh, function(s) {
    
    read_excel(path, sheet = s, .name_repair = "unique") %>%
      mutate(
        across(everything(), as.character),   # avoid type clashes across sheets
        across(where(is.character), str_squish),
        source_sheet = s
      )
  })
}
# ============================================================
# METRIC EXTRACTOR  (CR4 / CR8 + RCPTOT)
# ============================================================
# ---- Identify column containing concentration shares ----
choose_share_col <- function(df) {
  
  cand <- unique(c(
    "VAL_PCT", "VALPCT", "CCORCPPCT", "VSHERFI",
    grep("(PCT|PERCENT)$|_(PCT|PERCENT)$",
         names(df), ignore.case = TRUE, value = TRUE)
  ))
  
  cand <- cand[cand %in% names(df)]
  
  if (!length(cand)) return(NA_character_)
  
  cand[which.max(
    sapply(cand, function(cn)
      sum(is.finite(suppressWarnings(as.numeric(df[[cn]])))))
  )]
}

extract_metrics <- function(df) {
  
  tmp <- map_con_metric(df)
  share_col <- choose_share_col(df)
  
  # Receipts column used later as aggregation weight
  tot_cols <- choose_totals_cols(df)
  rcpt_col <- tot_cols$RCPTOT_col
  cli::cli_alert_info("Receipts column used: {rcpt_col}") 
  
  # Extract CR4 / CR8
  crs <- tmp %>%
    filter(!is.na(metric)) %>%
    group_by(NAICS_key, NAICS_len, metric) %>%
    summarise(
      value = {
        if (is.na(share_col)) {
          NA_real_
        } else {
          v <- suppressWarnings(as.numeric(.data[[share_col]]))
          v <- v[!is.na(v)]
          if (length(v) == 0) NA_real_ else v[1]
        }
      },
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = metric, values_from = value)
  
  out <- df %>%
    distinct(NAICS_key, NAICS_len) %>%
    left_join(crs, by = c("NAICS_key", "NAICS_len"))
  
  # Extract receipts total by NAICS cell
  totals <- df %>%
    group_by(NAICS_key, NAICS_len) %>%
    summarise(
      RCPTOT = if (!is.na(rcpt_col)) {
        x <- suppressWarnings(as.numeric(.data[[rcpt_col]]))
        if (all(!is.finite(x))) NA_real_ else max(x, na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
  
  out %>%
    left_join(totals, by = c("NAICS_key", "NAICS_len"))
}
# ============================================================
# YEAR RUNNERS
# ============================================================

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

run_excel_year <- function(path, naics_col, year_tag) {
  
  cli::cli_h2("Processing {year_tag} from {basename(path)}")
  
  raw <- read_all_sheets(path)
  
  df <- raw %>%
    drop_meta_any() %>%
    prep_naics(naics_col = naics_col, require_us = TRUE, require_all_est = TRUE) %>%
    prefer_all_over_taxable()
  
  print_naics_len(df, year_tag)
  
  m_raw <- extract_metrics(df)
  
  m <- m_raw %>%
    group_by(NAICS_key, NAICS_len) %>%
    summarise(
      CR4    = first_nonmissing(CR4),
      CR8    = first_nonmissing(CR8),
      RCPTOT = first_nonmissing(RCPTOT),
      .groups = "drop"
    ) %>%
    mutate(year = year_tag)
  
  labs <- build_label_map(df, naics_col) %>%
    add_hyphen_labels()
  
  list(metrics = m, labels = labs)
}

run_csv_year <- function(path, naics_col, year) {
  
  cli::cli_h2("Processing {year} from {basename(path)}")
  
  df <- read_delim(
    path,
    delim = "|",
    col_types = cols(.default = col_character()),
    trim_ws = TRUE
  ) %>%
    drop_meta_any()
  
  if ("GEO_LABEL" %in% names(df) && !("GEO_TTL" %in% names(df))) {
    df$GEO_TTL <- df$GEO_LABEL
  }
  
  dfp <- df %>%
    prep_naics(naics_col = naics_col, require_us = TRUE, require_all_est = TRUE) %>%
    prefer_all_over_taxable()
  
  print_naics_len(dfp, year)
  
  m_raw <- extract_metrics(dfp)
  
  m <- m_raw %>%
    group_by(NAICS_key, NAICS_len) %>%
    summarise(
      CR4    = first_nonmissing(CR4),
      CR8    = first_nonmissing(CR8),
      RCPTOT = first_nonmissing(RCPTOT),
      .groups = "drop"
    ) %>%
    mutate(year = year)
  
  labs <- build_label_map(dfp, naics_col) %>%
    add_hyphen_labels()
  
  list(metrics = m, labels = labs)
}
# ============================================================
# EXECUTION PIPELINE
# ============================================================

res02 <- if (file.exists(file02_xlsx))
  run_excel_year(file02_xlsx, "NAICS2002", 2002) else list(metrics = tibble(), labels = tibble())

res07 <- if (file.exists(file07_xlsx))
  run_excel_year(file07_xlsx, "NAICS2007", 2007) else list(metrics = tibble(), labels = tibble())

res12 <- if (file.exists(file12_xlsx))
  run_excel_year(file12_xlsx, "NAICS2012", 2012) else list(metrics = tibble(), labels = tibble())

res17 <- run_csv_year(file17_dat, "NAICS2017", 2017)
res22 <- run_csv_year(file22_dat, "NAICS2022", 2022)


# Attach labels for CSV years
m17 <- res17$metrics %>% left_join(res17$labels, by = "NAICS_key")
m22 <- res22$metrics %>% left_join(res22$labels, by = "NAICS_key")


# Combine all years
panel_all <- bind_rows(
  res02$metrics,
  res07$metrics,
  res12$metrics,
  m17,
  m22
) %>%
  mutate(across(
    c(CR4, CR8, RCPTOT),
    ~ suppressWarnings(as.numeric(.))
  )) %>%
  arrange(year, NAICS_len, NAICS_key) %>%
  distinct(year, NAICS_key, NAICS_len, .keep_all = TRUE)
cli::cli_h2("Final Census panel summary")

panel_all %>%
  summarise(
    rows = n(),
    industries = n_distinct(NAICS_key),
    years = n_distinct(year),
    min_year = min(year),
    max_year = max(year)
  ) %>%
  print()
# ---- CLEAN: treat 0 as missing if the other CR is positive ----
panel_all <- panel_all %>%
  mutate(
    CR4 = ifelse(CR4 == 0 & coalesce(CR8, 0) > 0, NA_real_, CR4),
    CR8 = ifelse(CR8 == 0 & coalesce(CR4, 0) > 0, NA_real_, CR8)
  )

# ---- ENFORCE MONOTONICITY ----
panel_all <- panel_all %>%
  mutate(
    CR8 = ifelse(!is.na(CR4) & !is.na(CR8) & CR8 < CR4, CR4, CR8)
  )

# convenience shares
panel_all <- panel_all %>%
  mutate(
    CR4_sh = CR4 / 100,
    CR8_sh = CR8 / 100
  )

# ---- VALIDATE + EXPORT ----
validate_panel(panel_all)

out_file <- file.path(DATA_CLEAN, "census_concentration_panel_2002_2022.csv")
write_csv(panel_all, out_file)
cli::cli_alert_success("Wrote panel -> {out_file}")




cli::cli_h1("Sanity checks – Census concentration panel")

# 1) Uniqueness check
dup <- panel_all %>%
  count(year, NAICS_key, NAICS_len) %>%
  filter(n > 1)

cli::cli_alert_info("Duplicate (year, NAICS_key, NAICS_len) rows: {nrow(dup)}")
if (nrow(dup) > 0) print(head(dup, 50))

# 2) CR bounds + monotonicity
bounds <- panel_all %>%
  summarise(
    n = n(),
    share_CR4_outside = mean(is.finite(CR4) & (CR4 < 0 | CR4 > 100)),
    share_CR8_outside = mean(is.finite(CR8) & (CR8 < 0 | CR8 > 100))
  )
print(bounds)

mono <- panel_all %>%
  summarise(
    n_CR8_lt_CR4 = sum(is.finite(CR4) & is.finite(CR8) & CR8 < CR4)
  )
print(mono)

# 3) RCPTOT sanity
w_sanity <- panel_all %>%
  summarise(
    share_missing_RCPTOT = mean(!is.finite(RCPTOT)),
    share_nonpos_RCPTOT  = mean(is.finite(RCPTOT) & RCPTOT <= 0)
  )
print(w_sanity)

# 4) RCPTOT consistency within cell
rcpt_var <- panel_all %>%
  group_by(year, NAICS_key, NAICS_len) %>%
  summarise(
    n_unique_rcpt = n_distinct(RCPTOT),
    .groups = "drop"
  ) %>%
  filter(n_unique_rcpt > 1)

cli::cli_alert_info(
  "Cells where RCPTOT differs within a NAICS-year cell: {nrow(rcpt_var)} (expected 0)"
)

if (nrow(rcpt_var) > 0) print(head(rcpt_var, 20))

cli::cli_alert_success("✓ Sanity checks complete.")


