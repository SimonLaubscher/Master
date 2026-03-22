# ================================================================
# 05_clean_klems_prepare
# EU KLEMS (Growth Accounts Basic) – thesis countries, grouped industries
#
# Core:
#   - Convert LP1 growth + LP1 contributions from percent/pp to log-units (Δln) ONCE.
#   - Keep original percent/pp versions with _pp suffix for traceability.
#   - Construct ICT intensity proxy (index ratio) and exact 1997 baseline.
#
# Baseline variables created:
#   - ICT_intensity_1997  : exact 1997 value
#   - ICT_intensity_base  : baseline ICT intensity, set equal to ICT_intensity_1997
#
# Note:
#   - industries without a 1997 ICT intensity remain missing here
#   - these can be dropped later at the classification stage if needed
#
# Inputs:
#   - dataraw/growth_accounts.rds
#
# Outputs:
#   - dataclean/klems_clean_total.rds
#   - dataclean/klems_panel_full_master_EUonly.rds
# ================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# ------------------------------------------------
# Paths
# ------------------------------------------------
ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_RAW   <- file.path(ROOT, "dataraw")
DATA_CLEAN <- file.path(ROOT, "dataclean")

IN_FILE     <- file.path(DATA_RAW, "growth_accounts.rds")
OUT_TOTAL   <- file.path(DATA_CLEAN, "klems_clean_total.rds")
OUT_EU_ONLY <- file.path(DATA_CLEAN, "klems_panel_full_master_EUonly.rds")

dir.create(DATA_CLEAN, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(IN_FILE)) {
  stop("Input file not found: ", IN_FILE)
}

# ------------------------------------------------
# 1) Load raw KLEMS growth accounts (long format)
# ------------------------------------------------
df <- readRDS(IN_FILE) %>%
  mutate(year = as.integer(year))

# ------------------------------------------------
# 2) Check key structure before reshape
# ------------------------------------------------
dup_check <- df %>%
  count(geo_code, nace_r2_code, nace_r2_name, year, var) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  print(dup_check)
  stop("Duplicate geo-industry-year-var rows found in raw KLEMS data.")
}

# ------------------------------------------------
# 3) Long -> wide, rename IDs
# ------------------------------------------------
df_wide <- df %>%
  pivot_wider(names_from = var, values_from = value) %>%
  rename(
    country       = geo_code,
    industry      = nace_r2_code,
    industry_name = nace_r2_name
  )

# ------------------------------------------------
# 4) Required variables check (pre-unit fix)
# ------------------------------------------------
must_have_pre <- c(
  "LP1_G", "LP1ConLC", "LP1ConTangNICT", "LP1ConTangICT",
  "LP1ConIntang", "LP1ConTFP", "CAP_QI", "CAPICT_QI"
)

missing_vars <- setdiff(must_have_pre, names(df_wide))
if (length(missing_vars) > 0) {
  stop("Missing KLEMS vars: ", paste(missing_vars, collapse = ", "))
}

# ------------------------------------------------
# 5) Unit guard + conversion (percent/pp -> Δln)
#    Raw KLEMS tables store LP growth and contributions in percent/pp.
#    We convert ONCE and keep originals with _pp suffix.
# ------------------------------------------------
p99 <- quantile(df_wide$LP1_G, 0.99, na.rm = TRUE)
mx  <- max(df_wide$LP1_G, na.rm = TRUE)

# Guardrail: if values already look like decimal log changes, stop.
if (is.finite(p99) && is.finite(mx) && p99 < 1 && mx < 5) {
  stop("LP1_G likely already in Δln units (p99 < 1 and max < 5). Do not divide by 100.")
}

vars_lp_pp <- c(
  "LP1_G", "LP1ConLC", "LP1ConTangNICT",
  "LP1ConTangICT", "LP1ConIntang", "LP1ConTFP"
)

df_wide <- df_wide %>%
  rename_with(~ paste0(.x, "_pp"), all_of(vars_lp_pp)) %>%
  mutate(across(ends_with("_pp"), ~ .x / 100, .names = "{sub('_pp$', '', .col)}"))

# ------------------------------------------------
# 6) Restrict to thesis countries + years
#    Keep US here only for aggregate total-series comparison.
# ------------------------------------------------
countries <- c("US", "DE", "FR", "SE", "DK")

df_wide <- df_wide %>%
  filter(country %in% countries, year >= 1995)

# ------------------------------------------------
# 7) Save totals (aggregate comparison series; includes US)
# ------------------------------------------------
df_total <- df_wide %>%
  filter(industry %in% c("TOT", "TOT_IND", "MARKT", "MARKTxAG")) %>%
  mutate(industry_clean = industry)

saveRDS(df_total, OUT_TOTAL)

# ------------------------------------------------
# 8) Keep grouped industries used in thesis (EU only)
# ------------------------------------------------
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

df_panel <- df_wide %>%
  filter(country %in% c("DE", "FR", "SE", "DK")) %>%
  filter(industry %in% keep_industries) %>%
  mutate(industry_clean = industry)

# ------------------------------------------------
# 9) ICT intensity proxy + exact 1997 baseline
#    NOTE: ICT_intensity_index = CAPICT_QI / CAP_QI is an index-ratio proxy
#    (not a bounded share). Suitable for ranking/median splits.
# ------------------------------------------------
df_panel <- df_panel %>%
  mutate(
    ICT_intensity_index = if_else(
      !is.na(CAP_QI) & CAP_QI > 0,
      CAPICT_QI / CAP_QI,
      NA_real_
    )
  ) %>%
  group_by(country, industry_clean) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ICT_intensity_1997 = ICT_intensity_index[match(1997, year)],
    ICT_intensity_base = ICT_intensity_1997
  ) %>%
  ungroup()

# ------------------------------------------------
# 10) Sanity: decomposition identity (quick)
#     LP1_G should equal sum of contributions (up to rounding).
# ------------------------------------------------
id_check <- df_wide %>%
  transmute(
    resid = LP1_G - (LP1ConLC + LP1ConTangICT + LP1ConTangNICT + LP1ConIntang + LP1ConTFP)
  ) %>%
  summarise(
    p99_abs = quantile(abs(resid), 0.99, na.rm = TRUE)
  )

if (is.finite(id_check$p99_abs) && id_check$p99_abs > 0.005) {
  warning("Identity check: p99 abs resid > 0.005 (check variable alignment or rounding).")
}

# ------------------------------------------------
# 11) Save outputs
# ------------------------------------------------
saveRDS(df_panel, OUT_EU_ONLY)

cat(
  "✓ KLEMS cleaning complete.\n",
  "Saved:\n",
  "- klems_clean_total.rds\n",
  "- klems_panel_full_master_EUonly.rds\n",
  sep = ""
)