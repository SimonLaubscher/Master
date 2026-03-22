# ============================================================
# 15_us_eu_ict_results.R
#
# Purpose:
# Construct the main Chapter 5 comparative results for the
# United States and four European countries (Germany, France,
# Denmark, Sweden).
#
# Main outputs:
# 1. Labour productivity growth by ICT group and period
# 2. ICT capital deepening contribution by ICT group and period
# 3. Top 5 industries by average labour productivity growth
#
# Input data:
# - US ILPA ICT-classified industry panel
# - EU KLEMS ICT-classified industry panel
#
# Outputs:
# - tables and figures for Chapter 5
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
})

# ============================================================
# 0) Paths
# ============================================================

ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_CLEAN <- file.path(ROOT, "dataclean")
OUT_DIR    <- file.path(ROOT, "outputs")
OUT_FIG    <- file.path(OUT_DIR, "figures")
OUT_TAB    <- file.path(OUT_DIR, "tables")

dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)

US_FILE <- file.path(DATA_CLEAN, "us_ilpa_ICT_industry_panel_NARROW.csv")
EU_FILE <- file.path(DATA_CLEAN, "klems_classified_ICT_C26_ONLY_1997baseline.rds")

if (!file.exists(US_FILE)) {
  stop("Missing US ICT panel: ", US_FILE,
       "\nRun the US ILPA build and ICT classification scripts first.")
}

if (!file.exists(EU_FILE)) {
  stop("Missing EU ICT panel: ", EU_FILE,
       "\nRun the EU KLEMS cleaning and ICT classification scripts first.")
}

# ============================================================
# 1) Settings
# ============================================================

START_YEAR <- 1998L   # common comparison window across US and EU
END_YEAR   <- 2021L
BREAK_YEAR <- 2005L

GROUP_LEVELS <- c("ICT-producing", "ICT-using", "Other")

country_labels <- c(
  "US" = "United States",
  "DE" = "Germany",
  "FR" = "France",
  "DK" = "Denmark",
  "SE" = "Sweden"
)

# Defensive drop of aggregate industry codes
AGG_CODES <- c("TOT", "TOT_IND", "MARKT", "MARKTxAG")

period_label <- function(y) {
  ifelse(y <= BREAK_YEAR,
         paste0("Pre-", BREAK_YEAR),
         paste0("Post-", BREAK_YEAR))
}

wmean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

check_required_cols <- function(df, cols, label = "data") {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      label, " is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
}

check_no_dups <- function(df, keys, label = "data") {
  dup_rows <- df %>%
    count(across(all_of(keys)), name = "n") %>%
    filter(n > 1)
  
  if (nrow(dup_rows) > 0) {
    print(dup_rows)
    stop(
      "Duplicate rows detected in ", label,
      " for key(s): ", paste(keys, collapse = ", ")
    )
  }
}


# ============================================================
# 2) Load and harmonize US industry panel
# ============================================================

us_raw <- read_csv(US_FILE, show_col_types = FALSE)

check_required_cols(
  us_raw,
  c("year", "industry_key", "industry", "lp_g", "va_level", "ICT_group_baseline"),
  label = "US file"
)

has_us_it_contrib <- "va_contrib_it_capital" %in% names(us_raw)

us <- us_raw %>%
  mutate(
    dataset = "US_ILPA",
    country = "US",
    country_name = unname(country_labels["US"]),
    year = as.integer(year),
    
    industry_code = str_squish(as.character(industry_key)),
    industry_name = str_squish(as.character(industry)),
    
    ICT_group_baseline = factor(as.character(ICT_group_baseline), levels = GROUP_LEVELS),
    
    period = factor(
      period_label(year),
      levels = c(paste0("Pre-", BREAK_YEAR), paste0("Post-", BREAK_YEAR))
    ),
    
    lp_pp = 100 * as.numeric(lp_g),  
    
    weight_va = as.numeric(va_level),  # nominal VA (current prices)
    
    ict_contrib_pp = if (has_us_it_contrib) {
      100 * as.numeric(va_contrib_it_capital)
    } else {
      NA_real_
    }
  ) %>%
  filter(year >= START_YEAR, year <= END_YEAR) %>%
  filter(!is.na(ICT_group_baseline)) %>%
  filter(is.finite(weight_va), weight_va > 0)

check_no_dups(us, c("industry_code", "year"), label = "US harmonized")



# ============================================================
# 3) Load and harmonize EU industry panel
# ============================================================

eu_raw <- readRDS(EU_FILE)

check_required_cols(
  eu_raw,
  c("country", "year", "industry_clean", "ICT_group_baseline", "LP1_G", "VA_CP"),
  label = "EU file"
)

eu <- eu_raw %>%
  mutate(
    dataset = "EU_KLEMS",
    country = as.character(country),
    country_name = dplyr::recode(country, !!!country_labels, .default = country),
    year = as.integer(year),
    
    industry_code = str_squish(as.character(industry_clean)),
    industry_name = if ("industry_name" %in% names(.)) {
      str_squish(as.character(industry_name))
    } else {
      industry_code
    },
    
    ICT_group_baseline = factor(as.character(ICT_group_baseline), levels = GROUP_LEVELS),
    
    period = factor(
      period_label(year),
      levels = c(paste0("Pre-", BREAK_YEAR), paste0("Post-", BREAK_YEAR))
    ),
    
    lp_pp = 100 * as.numeric(LP1_G),      
    weight_va = as.numeric(VA_CP),        # nominal GVA at current prices
    ict_intensity = as.numeric(ICT_intensity_base),
    
    ict_contrib_pp = case_when(
      "LP1ConTangICT_pp" %in% names(eu_raw) ~ as.numeric(LP1ConTangICT_pp),
      "LP1ConTangICT" %in% names(eu_raw)    ~ 100 * as.numeric(LP1ConTangICT),
      TRUE                                  ~ NA_real_
    )
  ) %>%
  filter(country %in% c("DE", "FR", "DK", "SE")) %>%
  filter(year >= START_YEAR, year <= END_YEAR) %>%
  filter(!industry_code %in% AGG_CODES) %>%
  filter(!is.na(ICT_group_baseline)) %>%
  filter(is.finite(weight_va), weight_va > 0)

check_no_dups(eu, c("country", "industry_code", "year"), label = "EU harmonized")

# ============================================================
# 4) Stack harmonized US and EU panels
# ============================================================

df <- bind_rows(us, eu)

check_no_dups(
  df,
  c("dataset", "country", "industry_code", "year"),
  label = "stacked US-EU panel"
)

# Optional diagnostic: check ICT contribution scale after harmonization
ict_scale <- df %>%
  filter(is.finite(ict_contrib_pp)) %>%
  group_by(dataset, country_name) %>%
  summarise(
    n = n(),
    med_abs = median(abs(ict_contrib_pp), na.rm = TRUE),
    p99_abs = quantile(abs(ict_contrib_pp), 0.99, na.rm = TRUE),
    max_abs = max(abs(ict_contrib_pp), na.rm = TRUE),
    .groups = "drop"
  )

print(ict_scale)


# ============================================================
# 4A) Top 5 industries by average LP growth (full sample)
# ============================================================

top5_full <- df %>%
  filter(is.finite(lp_pp)) %>%
  group_by(country_name, industry_code, industry_name) %>%
  summarise(
    avg_lp_pp = mean(lp_pp, na.rm = TRUE),
    n_years = n_distinct(year),
    .groups = "drop"
  ) %>%
  group_by(country_name) %>%
  arrange(desc(avg_lp_pp), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(avg_lp_pp = round(avg_lp_pp, 2)) %>%
  arrange(
    factor(country_name, levels = c("United States", "Germany", "France", "Denmark", "Sweden")),
    desc(avg_lp_pp)
  )

write_csv(
  top5_full,
  file.path(OUT_TAB, "13_top5_industries_lp_growth_full.csv")
)

top5_full_latex <- top5_full %>%
  arrange(
    factor(country_name, levels = c("United States", "Germany", "France", "Denmark", "Sweden")),
    desc(avg_lp_pp)
  ) %>%
  group_by(country_name) %>%
  mutate(
    Country = if_else(row_number() == 1, country_name, "")
  ) %>%
  ungroup() %>%
  select(
    Country,
    Industry = industry_name,
    `Avg. LP growth` = avg_lp_pp
  )

top5_full_tex <- knitr::kable(
  top5_full_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "l", "c"),
  escape = TRUE
)

writeLines(
  top5_full_tex,
  file.path(OUT_TAB, "13_top5_industries_lp_growth_full.tex")
)


# ============================================================
# 4B) Top 5 industries by average LP growth (by period)
# ============================================================

top5_period <- df %>%
  filter(is.finite(lp_pp)) %>%
  group_by(country_name, period, industry_code, industry_name) %>%
  summarise(
    avg_lp_pp = mean(lp_pp, na.rm = TRUE),
    n_years = n_distinct(year),
    .groups = "drop"
  ) %>%
  group_by(country_name, period) %>%
  arrange(desc(avg_lp_pp), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(avg_lp_pp = round(avg_lp_pp, 2)) %>%
  arrange(
    factor(country_name, levels = c("United States", "Germany", "France", "Denmark", "Sweden")),
    factor(period, levels = c(paste0("Pre-", BREAK_YEAR), paste0("Post-", BREAK_YEAR))),
    desc(avg_lp_pp)
  )

# Save CSV
write_csv(
  top5_period,
  file.path(OUT_TAB, "13_top5_industries_lp_growth_by_period.csv")
)

# Prepare LaTeX table
top5_period_latex <- top5_period %>%
  arrange(
    factor(country_name, levels = c("United States", "Germany", "France", "Denmark", "Sweden")),
    factor(period, levels = c(paste0("Pre-", BREAK_YEAR), paste0("Post-", BREAK_YEAR))),
    desc(avg_lp_pp)
  ) %>%
  group_by(country_name, period) %>%
  mutate(
    Country = if_else(row_number() == 1, country_name, ""),
    Period  = if_else(row_number() == 1, as.character(period), "")
  ) %>%
  ungroup() %>%
  select(
    Country,
    Period,
    Industry = industry_name,
    `Avg. annual LP growth` = avg_lp_pp
  )

# Generate LaTeX table (regular tabular, not longtable)
top5_period_tex <- top5_period_latex %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    align = c("l", "l", "p{9.3cm}", "r"),
    escape = TRUE,
    col.names = c(
      "Country",
      "Period",
      "Industry",
      "Avg. annual LP growth"
    )
  ) %>%
  kable_styling(
    font_size = 8,
    latex_options = c("hold_position")
  )

# Save LaTeX
writeLines(
  as.character(top5_period_tex),
  file.path(OUT_TAB, "13_top5_industries_lp_growth_by_period.tex")
)

# ============================================================
# 4C) DIAGNOSTIC: Number of industries per group per country #maybe use for appendix table needs adjustment
# ============================================================
n_industries <- valid_lp %>%
  group_by(country_name, ICT_group_baseline, period) %>%
  summarise(
    n_industries = n_distinct(industry_code),
    n_years      = n_distinct(year),
    .groups = "drop"
  ) %>%
  mutate(n_industries = round(n_industries, 0)) %>%
  arrange(country_name, ICT_group_baseline, period)

print("Number of industries per group per country-period:")
print(n_industries, n = Inf)  # Shows ALL rows

write_csv(n_industries, file.path(OUT_TAB, "13_n_industries_per_group.csv"))





# ============================================================
# 5) Labour productivity growth by ICT group and period
# ============================================================

valid_lp <- df %>%
  filter(is.finite(lp_pp), is.finite(weight_va), weight_va > 0)

lp_group_year <- valid_lp %>%
  group_by(country_name, ICT_group_baseline, year, period) %>%
  summarise(
    lp_pp_year = wmean_safe(lp_pp, weight_va),
    .groups = "drop"
  )

lp_group_period <- lp_group_year %>%
  group_by(country_name, ICT_group_baseline, period) %>%
  summarise(
    avg_lp_pp = mean(lp_pp_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(avg_lp_pp = round(avg_lp_pp, 2)) %>%
  arrange(
    factor(country_name, levels = c("United States", "Germany", "France", "Denmark", "Sweden")),
    factor(period, levels = c(paste0("Pre-", BREAK_YEAR), paste0("Post-", BREAK_YEAR))),
    factor(ICT_group_baseline, levels = GROUP_LEVELS)
  )

write_csv(
  lp_group_period,
  file.path(OUT_TAB, "13_lp_growth_by_ict_group_period.csv")
)

tab_ict_lp_wide <- lp_group_period %>%
  mutate(
    grp = case_when(
      ICT_group_baseline == "ICT-producing" ~ "ICTproducing",
      ICT_group_baseline == "ICT-using"     ~ "ICTusing",
      ICT_group_baseline == "Other"         ~ "Other",
      TRUE ~ NA_character_
    ),
    per = case_when(
      period == paste0("Pre-", BREAK_YEAR)  ~ paste0(START_YEAR, "_", BREAK_YEAR),
      period == paste0("Post-", BREAK_YEAR) ~ paste0(BREAK_YEAR + 1, "_", END_YEAR),
      TRUE ~ NA_character_
    )
  ) %>%
  select(country_name, per, grp, avg_lp_pp) %>%
  pivot_wider(
    names_from = c(per, grp),
    names_sep = "__",
    values_from = avg_lp_pp
  ) %>%
  arrange(factor(country_name, levels = c("United States", "Germany", "France", "Denmark", "Sweden")))

write_csv(
  tab_ict_lp_wide,
  file.path(OUT_TAB, "13_lp_growth_by_ict_group_period_wide.csv")
)

tab_ict_lp_latex <- tab_ict_lp_wide %>%
  transmute(
    Country = country_name,
    `ICT-producing`  = .data[[paste0(START_YEAR, "_", BREAK_YEAR, "__ICTproducing")]],
    `ICT-using`      = .data[[paste0(START_YEAR, "_", BREAK_YEAR, "__ICTusing")]],
    `Other`          = .data[[paste0(START_YEAR, "_", BREAK_YEAR, "__Other")]],
    `ICT-producing ` = .data[[paste0(BREAK_YEAR + 1, "_", END_YEAR, "__ICTproducing")]],
    `ICT-using `     = .data[[paste0(BREAK_YEAR + 1, "_", END_YEAR, "__ICTusing")]],
    `Other `         = .data[[paste0(BREAK_YEAR + 1, "_", END_YEAR, "__Other")]]
  )

tex_ict_lp <- knitr::kable(
  tab_ict_lp_latex,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = "lcccccc",
  escape = TRUE
) %>%
  kableExtra::add_header_above(
    setNames(
      c(1, 3, 3),
      c(
        " ",
        paste0(START_YEAR, "--", BREAK_YEAR),
        paste0(BREAK_YEAR + 1, "--", END_YEAR)
      )
    )
  )

writeLines(
  as.character(tex_ict_lp),
  file.path(OUT_TAB, "13_lp_growth_by_ict_group_period.tex")
)




# ============================================================
# 6) ICT capital deepening contribution by ICT group and period
# ============================================================

valid_contrib <- df %>%
  filter(is.finite(ict_contrib_pp), is.finite(weight_va), weight_va > 0)

if (nrow(valid_contrib) == 0) {
  message("No valid ICT contribution data available; skipping ICT contribution table.")
} else {
  
  contrib_group_year <- valid_contrib %>%
    group_by(country_name, ICT_group_baseline, year, period) %>%
    summarise(
      ict_contrib_pp_year = wmean_safe(ict_contrib_pp, weight_va),
      .groups = "drop"
    )
  
  contrib_group_period <- contrib_group_year %>%
    group_by(country_name, ICT_group_baseline, period) %>%
    summarise(
      avg_ict_contrib_pp = mean(ict_contrib_pp_year, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(avg_ict_contrib_pp = round(avg_ict_contrib_pp, 2)) %>%
    arrange(
      factor(country_name, levels = c("United States", "Germany", "France", "Denmark", "Sweden")),
      factor(period, levels = c(paste0("Pre-", BREAK_YEAR), paste0("Post-", BREAK_YEAR))),
      factor(ICT_group_baseline, levels = GROUP_LEVELS)
    )
}
write_csv(
  contrib_group_period,
  file.path(OUT_TAB, "13_ict_contribution_by_group_period.csv")
)

tab_ict_contrib_wide <- contrib_group_period %>%
  mutate(
    grp = case_when(
      ICT_group_baseline == "ICT-producing" ~ "ICTproducing",
      ICT_group_baseline == "ICT-using"     ~ "ICTusing",
      ICT_group_baseline == "Other"         ~ "Other",
      TRUE ~ NA_character_
    ),
    per = case_when(
      period == paste0("Pre-", BREAK_YEAR)  ~ paste0(START_YEAR, "_", BREAK_YEAR),
      period == paste0("Post-", BREAK_YEAR) ~ paste0(BREAK_YEAR + 1, "_", END_YEAR),
      TRUE ~ NA_character_
    )
  ) %>%
  select(country_name, per, grp, avg_ict_contrib_pp) %>%
  pivot_wider(
    names_from = c(per, grp),
    names_sep = "__",
    values_from = avg_ict_contrib_pp
  ) %>%
  arrange(factor(country_name, levels = c("United States", "Germany", "France", "Denmark", "Sweden")))

write_csv(
  tab_ict_contrib_wide,
  file.path(OUT_TAB, "13_ict_contribution_by_group_period_wide.csv")
)

tab_ict_contrib_latex <- tab_ict_contrib_wide %>%
  transmute(
    Country = country_name,
    `ICT-producing`  = .data[[paste0(START_YEAR, "_", BREAK_YEAR, "__ICTproducing")]],
    `ICT-using`      = .data[[paste0(START_YEAR, "_", BREAK_YEAR, "__ICTusing")]],
    `Other`          = .data[[paste0(START_YEAR, "_", BREAK_YEAR, "__Other")]],
    `ICT-producing ` = .data[[paste0(BREAK_YEAR + 1, "_", END_YEAR, "__ICTproducing")]],
    `ICT-using `     = .data[[paste0(BREAK_YEAR + 1, "_", END_YEAR, "__ICTusing")]],
    `Other `         = .data[[paste0(BREAK_YEAR + 1, "_", END_YEAR, "__Other")]]
  )

tex_ict_contrib <- knitr::kable(
  tab_ict_contrib_latex,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = "lcccccc",
  escape = TRUE
) %>%
  kableExtra::add_header_above(
    setNames(
      c(1, 3, 3),
      c(
        " ",
        paste0(START_YEAR, "--", BREAK_YEAR),
        paste0(BREAK_YEAR + 1, "--", END_YEAR)
      )
    )
  )

writeLines(
  as.character(tex_ict_contrib),
  file.path(OUT_TAB, "13_ict_contribution_by_group_period.tex")
)
# ============================================================
# 7) Final checks and completion message
# ============================================================

cat("\n==================== FINAL CHECKS ====================\n")

cat("\nInput files used:\n")
cat("US file: ", normalizePath(US_FILE, winslash = "/"), "\n", sep = "")
cat("EU file: ", normalizePath(EU_FILE, winslash = "/"), "\n", sep = "")

cat("\nRows by dataset and country after harmonization:\n")
print(
  df %>%
    count(dataset, country_name, name = "n_obs") %>%
    arrange(dataset, country_name),
  n = 200
)

cat("\nRows used for LP-group table:\n")
print(
  valid_lp %>%
    count(country_name, name = "n_obs") %>%
    arrange(country_name),
  n = 200
)

cat("\nUS ICT contribution variable available: ", has_us_it_contrib, "\n", sep = "")

cat("\n✓ Script 13 complete. Outputs saved to:\n")
cat(normalizePath(OUT_TAB, winslash = "/"), "\n", sep = "")
cat(normalizePath(OUT_FIG, winslash = "/"), "\n", sep = "")