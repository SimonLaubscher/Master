# ============================================================
# 18_eu_lp_growth_vs_concentration_FE.R
# ------------------------------------------------------------
# Purpose:
# Estimate the relationship between market concentration and
# labor productivity growth in the EU panel using annual data.
#
# This script:
#   - loads the main EU analysis panel from Script 12
#   - prepares descriptive statistics and figures
#   - reports descriptive correlations between concentration
#     and labor productivity growth
#   - plots mean concentration by ICT group over time
#   - estimates annual fixed-effects models of labor
#     productivity growth on concentration
#   - examines heterogeneity by ICT-using status and baseline
#     ICT intensity
#   - exports figures, tables, and LaTeX regression output
#
# Main specification:
#   - Dependent variable:
#       annual labor productivity growth (lp_pp)
#   - Main regressor:
#       annual concentration (HHI_rev_agg)
#   - Fixed effects:
#       country-industry and year fixed effects
#
# Input:
#   - data/clean/panel_eu/panel_common_2003_2020_analysis_strict.rds

# Outputs:
#   - output/figures/
#   - output/tables/
#   - output/appendix/tables/
# ============================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(fixest)
  library(broom)
  library(cli)
  library(knitr)
  library(kableExtra)
  library(here)
})

# -----------------------------
# Paths
# -----------------------------

DATA_CLEAN <- here("data", "clean")
PANEL_EU   <- here("data", "clean", "panel_eu")

OUT_FIG <- here("output", "figures")
OUT_TAB <- here("output", "tables")
OUT_APP_TAB <- here("output", "appendix", "tables")


dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_APP_TAB, recursive = TRUE, showWarnings = FALSE)



PATH_IN <- file.path(PANEL_EU, "panel_common_2003_2020_analysis_strict.rds")

if (!file.exists(PATH_IN)) {
  stop("Input file not found: ", PATH_IN)
}

# -----------------------------
# Settings
# -----------------------------
YEAR_MIN <- 2003L
YEAR_MAX <- 2020L

Y_VAR         <- "lp_pp"
CONC_VAR      <- "HHI_rev_agg"
ICT_BASE_VAR  <- "ICT_intensity_base"
ICT_GROUP_VAR <- "ICT_group_baseline"
COUNTRIES     <- c("DE", "DK", "FR", "SE")

# -----------------------------
# Load data
# -----------------------------
p <- readRDS(PATH_IN)

required_vars <- c(
  "country", "year", "industry_clean",
  Y_VAR, CONC_VAR, ICT_BASE_VAR, ICT_GROUP_VAR
)

missing_vars <- setdiff(required_vars, names(p))
if (length(missing_vars) > 0) {
  stop("Missing variables: ", paste(missing_vars, collapse = ", "))
}

p <- p %>%
  mutate(
    country        = as.character(country),
    year           = as.integer(year),
    industry_clean = as.character(industry_clean),
    y              = suppressWarnings(as.numeric(.data[[Y_VAR]])),
    conc           = suppressWarnings(as.numeric(.data[[CONC_VAR]])),
    conc_100 = conc / 100,
    ICT_base       = suppressWarnings(as.numeric(.data[[ICT_BASE_VAR]])),
    ICT_group      = as.character(.data[[ICT_GROUP_VAR]])
  ) %>%
  filter(country %in% COUNTRIES) %>%
  filter(year >= YEAR_MIN, year <= YEAR_MAX) %>%
  mutate(
    ICT_group = factor(ICT_group, levels = c("ICT-producing", "ICT-using", "Other")),
    ICT_using_dummy = as.integer(ICT_group == "ICT-using"),
    unit_id = interaction(country, industry_clean, drop = TRUE)
  )

# -----------------------------
# Descriptive sample
# -----------------------------
p_desc <- p %>%
  filter(
    is.finite(y),
    is.finite(conc),
    !is.na(ICT_group)
  )

# -----------------------------
# FE sample
# -----------------------------
# FE models with continuous ICT heterogeneity require a non-missing
# baseline ICT intensity measure (ICT_base). A small number of rows
# remain in the descriptive sample but not in the FE sample because
# some industries have a valid ICT group classification but no reported
# baseline ICT intensity in 1997 (e.g. SE, C26, 2016–2020).
p_fe <- p %>%
  filter(
    is.finite(y),
    is.finite(conc),
    is.finite(ICT_base),
    !is.na(ICT_group)
  )

cli::cli_alert_info(
  "Descriptive sample: rows={nrow(p_desc)} units={n_distinct(p_desc$unit_id)} countries={n_distinct(p_desc$country)} years={min(p_desc$year)}–{max(p_desc$year)}"
)

cli::cli_alert_info(
  "FE sample: rows={nrow(p_fe)} units={n_distinct(p_fe$unit_id)} countries={n_distinct(p_fe$country)} years={min(p_fe$year)}–{max(p_fe$year)}"
)

# -----------------------------
# Diagnostic: observations in descriptives but not in FE
# -----------------------------
# Observations dropped from FE are expected here if ICT group is known
# but the continuous baseline ICT intensity is unavailable.
dropped_from_fe <- p_desc %>%
  anti_join(
    p_fe %>% select(country, year, industry_clean),
    by = c("country", "year", "industry_clean")
  ) %>%
  arrange(country, industry_clean, year)

if (nrow(dropped_from_fe) > 0) {
  cli::cli_alert_warning(
    "Observations dropped from FE because ICT_base is missing: {nrow(dropped_from_fe)}"
  )
}


# ============================================================
# DESCRIPTIVES (EU)
# ============================================================

# -----------------------------
# Table helper: LaTeX export
# -----------------------------
save_kable_latex <- function(df, file, caption, label,
                             notes = NULL,
                             digits = 3) {
  
  df_out <- df
  
  num_cols <- names(df_out)[vapply(df_out, is.numeric, logical(1))]
  for (cc in num_cols) {
    df_out[[cc]] <- round(df_out[[cc]], digits)
  }
  
  k <- knitr::kable(
    df_out,
    format = "latex",
    booktabs = TRUE,
    caption = caption,
    label = label,
    escape = FALSE
  ) |>
    kableExtra::kable_styling(
      latex_options = "hold_position",
      font_size = 10
    )
  
  if (!is.null(notes) && length(notes) > 0) {
    k <- k |>
      kableExtra::footnote(
        general = paste(notes, collapse = " "),
        threeparttable = TRUE,
        escape = FALSE
      )
  }
  
  writeLines(as.character(k), con = file)
  cli::cli_alert_success("Saved LaTeX table: {file}")
}

# ============================================================
# A) Correlations: LP growth vs concentration (EU)
# ============================================================
cli::cli_h1("A) Correlations (EU)")

corr_safe <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok]))
}

COUNTRY_ORDER <- c("DE", "FR", "DK", "SE")

# -----------------------------
# 1) Country-only correlations (for Overall rows)
# -----------------------------
tab_corr_overall <- p_desc %>%
  filter(
    !is.na(ICT_group),
    ICT_group %in% c("ICT-using", "Other")
  ) %>%
  mutate(country = factor(country, levels = COUNTRY_ORDER)) %>%
  group_by(country) %>%
  summarise(
    N_obs_overall = n(),
    N_ind_overall = n_distinct(industry_clean),
    Corr_overall  = corr_safe(y, conc),
    .groups = "drop"
  ) %>%
  arrange(country) %>%
  mutate(
    country = dplyr::recode(
      as.character(country),
      "DE" = "Germany",
      "FR" = "France",
      "DK" = "Denmark",
      "SE" = "Sweden"
    )
  )

# -----------------------------
# 2) Country × ICT group correlations (ICT-using / Other)
# -----------------------------
tab_corr_group <- p_desc %>%
  filter(
    !is.na(ICT_group),
    ICT_group %in% c("ICT-using", "Other")
  ) %>%
  mutate(
    country   = factor(country, levels = COUNTRY_ORDER),
    ICT_group = factor(ICT_group, levels = c("ICT-using", "Other"))
  ) %>%
  group_by(country, ICT_group) %>%
  summarise(
    N_obs   = n(),
    N_ind   = n_distinct(industry_clean),
    Corr    = corr_safe(y, conc),
    .groups = "drop"
  ) %>%
  arrange(country, ICT_group) %>%
  mutate(
    country = dplyr::recode(
      as.character(country),
      "DE" = "Germany",
      "FR" = "France",
      "DK" = "Denmark",
      "SE" = "Sweden"
    )
  ) %>%
  rename(
    `ICT group` = ICT_group,
    `N obs.` = N_obs,
    `N industries` = N_ind,
    `Corr(LP growth, HHI)` = Corr
  )

# -----------------------------
# 3) Overall rows
# -----------------------------
tab_corr_overall <- tab_corr_overall %>%
  mutate(
    `ICT group` = "Overall",
    `N obs.` = N_obs_overall,
    `N industries` = N_ind_overall,
    `Corr(LP growth, HHI)` = Corr_overall
  ) %>%
  select(country, `ICT group`, `N obs.`, `N industries`, `Corr(LP growth, HHI)`)

# -----------------------------
# 4) Combine group and overall rows
# -----------------------------
tab_corr_full <- bind_rows(
  tab_corr_group,
  tab_corr_overall
) %>%
  mutate(
    country = factor(
      country,
      levels = c("Germany", "France", "Denmark", "Sweden")
    )
  ) %>%
  arrange(
    country,
    factor(`ICT group`, levels = c("ICT-using", "Other", "Overall"))
  )

# -----------------------------
# 5) Round correlations and save CSV + LaTeX table
# -----------------------------
tab_corr_full <- tab_corr_full %>%
  mutate(
    `Corr(LP growth, HHI)` = round(`Corr(LP growth, HHI)`, 3)
  )

write_csv(
  tab_corr_full,
  file.path(OUT_TAB, "tab_eu_corr_by_country_ICTgroup.csv")
)

# create display version (blank repeated country names)
tab_corr_tex <- tab_corr_full %>%
  mutate(
    country = as.character(country),
    country = ifelse(duplicated(country), "", country)
  )

tabular_code <- kable(
  tab_corr_tex,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  escape = FALSE,
  col.names = c(
    "Country",
    "ICT group",
    "Observations",
    "Industries",
    "Correlation"
  ),
  align = c("l", "l", "r", "r", "r")
)

# insert subtle spacing after each country block
tabular_lines <- strsplit(tabular_code, "\n")[[1]]
row_end_idx <- grep("\\\\\\\\$", tabular_lines)

# exclude header row
body_row_idx <- row_end_idx[-1]

# after rows 3, 6, 9 insert spacing
insert_after <- body_row_idx[c(3, 6, 9)]

for (i in rev(insert_after)) {
  tabular_lines <- append(
    tabular_lines,
    "\\addlinespace[0.2em]",
    after = i
  )
}

tabular_code <- paste(tabular_lines, collapse = "\n")

writeLines(
  tabular_code,
  file.path(OUT_TAB, "tab_eu_corr_by_country_ICTgroup.tex")
)





# ============================================================
# B) Mean concentration by ICT group over time (MAIN TEXT)
# ============================================================
cli::cli_h1("B) Mean concentration by ICT group (EU)")

fig_conc_group <- p_desc %>%
  filter(ICT_group %in% c("ICT-using", "Other")) %>%
  mutate(
    country = dplyr::recode(
      country,
      "DE" = "Germany",
      "FR" = "France",
      "DK" = "Denmark",
      "SE" = "Sweden"
    ),
    country = factor(country, levels = c("Germany", "France", "Denmark", "Sweden")),
    ICT_group = factor(ICT_group, levels = c("ICT-using", "Other")),
    conc_hhi = conc
  ) %>%
  group_by(country, year, ICT_group) %>%
  summarise(
    mean_conc_hhi = mean(conc_hhi, na.rm = TRUE),
    n_industries = n_distinct(industry_clean),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year, y = mean_conc_hhi, color = ICT_group, group = ICT_group)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.9) +
  scale_color_manual(
    values = c("ICT-using" = "#e41a1c", "Other" = "#377eb8")
  ) +
  facet_wrap(~country, scales = "free_y") +
  labs(
    x = "Year",
    y = "Mean concentration (HHI)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave(
  filename = file.path(OUT_FIG, "fig_eu_mean_conc_by_ICTgroup.png"),
  plot = fig_conc_group,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# Temporary text table (scaled to HHI)
tab_long <- p_desc %>%
  filter(ICT_group %in% c("ICT-using", "Other")) %>%
  group_by(country, year, ICT_group) %>%
  summarise(
    mean_hhi = round(mean(conc, na.rm = TRUE), 0),
    n_industries = n_distinct(industry_clean),
    .groups = "drop"
  )
print(tab_long, n = Inf)  # Now shows proper HHI scale

# Note:
# Variation in mean concentration partly reflects small group sizes
# in some country-year cells, especially for ICT-using industries.






## ============================================================
# C) FIXED EFFECTS REGRESSIONS (EU)
# ============================================================
cli::cli_h1("C) Fixed Effects regressions (EU)")

COUNTRY_ORDER <- c("DE", "FR", "DK", "SE")

# -----------------------------
# FE estimation sample
# -----------------------------
# Main FE models focus on ICT-using and Other industries.
# ICT-producing industries are excluded because this category
# often contains only one industry and is not central to the
# main heterogeneity analysis.
p_fe_main <- p_fe %>%
  filter(ICT_group %in% c("ICT-using", "Other"))

cli::cli_alert_info(
  "Main FE sample: rows={nrow(p_fe_main)} units={n_distinct(p_fe_main$unit_id)} countries={n_distinct(p_fe_main$country)} years={min(p_fe_main$year)}–{max(p_fe_main$year)}"
)

fe_models <- list()

for (cc in COUNTRY_ORDER) {
  
  cli::cli_h2("Running FE for {cc}")
  
  d <- p_fe_main %>%
    filter(country == cc) %>%
    arrange(unit_id, year)
  
  # -----------------------------
  # Main models
  # -----------------------------
  
  # FE1
  m1 <- feols(
    y ~ conc_100 | unit_id + year,
    data = d,
    vcov = ~unit_id
  )
  
  # FE2
  m2 <- feols(
    y ~ conc_100 + conc_100:ICT_using_dummy | unit_id + year,
    data = d,
    vcov = ~unit_id
  )
  
  # FE3
  m3 <- feols(
    y ~ conc_100 + conc_100:ICT_base | unit_id + year,
    data = d,
    vcov = ~unit_id
  )
  
  fe_models[[cc]] <- list(
    m1 = m1,
    m2 = m2,
    m3 = m3
  )
  
  # -----------------------------
  # Appendix country table (tabular only)
  # -----------------------------
  TEX_FE_CC <- file.path(OUT_APP_TAB, paste0("tab_eu_fe_", cc, ".tex"))
  
  fixest::etable(
    m1, m2, m3,
    tex = TRUE,
    file = TEX_FE_CC,
    replace = TRUE,
    se.below = TRUE,
    digits = 3,
    depvar = FALSE,
    dict = c(
      "conc_100" = "HHI (per 100 points)",
      "conc_100:ICT_using_dummy" = "HHI (per 100 points) $\\times$ ICT-using",
      "conc_100:ICT_base" = "HHI (per 100 points) $\\times$ ICT intensity",
      "unit_id" = "Industry",
      "year" = "Year"
    ),
    fitstat = c("n", "r2", "wr2"),
    headers = c(
      "FE1" = "Baseline",
      "FE2" = "ICT-using",
      "FE3" = "ICT intensity"
    ),
    notes = ""
  )
  
  # Clean LaTeX output: remove significance legend and clustered-SE footer
  tab_lines <- readLines(TEX_FE_CC)
  
  tab_lines <- tab_lines[
    !grepl("Signif\\. Codes", tab_lines)
  ]
  tab_lines <- tab_lines[
    !grepl("Clustered .*standard-errors in parentheses", tab_lines)
  ]
  
  writeLines(tab_lines, TEX_FE_CC)
  
  cli::cli_alert_success("Saved FE appendix table for {cc}: {TEX_FE_CC}")
}

# ============================================================
# D) FE summary table (main thesis table)
# ============================================================
cli::cli_h1("D) FE summary table")

get_coef_info <- function(m, term) {
  ct <- tryCatch(fixest::coeftable(m), error = function(e) NULL)
  if (is.null(ct) || !(term %in% rownames(ct))) {
    return(c(NA_real_, NA_real_, NA_real_))
  }
  c(
    unname(ct[term, "Estimate"]),
    unname(ct[term, "Std. Error"]),
    unname(ct[term, "Pr(>|t|)"])
  )
}

fmt <- function(b, se, pval = NA_real_, digits = 3) {
  if (!is.finite(b)) return(NA_character_)
  
  stars <- ""
  if (is.finite(pval)) {
    if (pval < 0.01) {
      stars <- "***"
    } else if (pval < 0.05) {
      stars <- "**"
    } else if (pval < 0.10) {
      stars <- "*"
    }
  }
  
  if (is.finite(se)) {
    paste0(round(b, digits), stars, " (", round(se, digits), ")")
  } else {
    paste0(round(b, digits), stars)
  }
}

tab_fe_summary <- bind_rows(lapply(COUNTRY_ORDER, function(cc) {
  
  m1 <- fe_models[[cc]]$m1
  m2 <- fe_models[[cc]]$m2
  m3 <- fe_models[[cc]]$m3
  
  b1  <- get_coef_info(m1, "conc_100")
  b2m <- get_coef_info(m2, "conc_100")
  b2i <- get_coef_info(m2, "conc_100:ICT_using_dummy")
  b3m <- get_coef_info(m3, "conc_100")
  b3i <- get_coef_info(m3, "conc_100:ICT_base")
  
  tibble::tibble(
    Country = cc,
    HHI_baseline = fmt(b1[1], b1[2], b1[3]),
    HHI_ict_using = fmt(b2m[1], b2m[2], b2m[3]),
    `HHI × ICT-using` = fmt(b2i[1], b2i[2], b2i[3]),
    HHI_ict_intensity = fmt(b3m[1], b3m[2], b3m[3]),
    `HHI × ICT intensity` = fmt(b3i[1], b3i[2], b3i[3])
  )
}))

tab_fe_summary <- tab_fe_summary %>%
  mutate(
    Country = dplyr::recode(
      Country,
      "DE" = "Germany",
      "FR" = "France",
      "DK" = "Denmark",
      "SE" = "Sweden"
    )
  )

# Set final display names for LaTeX output
colnames(tab_fe_summary) <- c(
  "Country",
  "HHI (per 100 points)",
  "HHI (per 100 points)",
  "HHI (per 100 points) $\\times$ ICT-using",
  "HHI (per 100 points)",
  "HHI (per 100 points) $\\times$ ICT intensity"
)

write_csv(
  tab_fe_summary,
  file.path(OUT_TAB, "tab_eu_fe_summary.csv")
)


tabular_code <- tab_fe_summary %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c", "c"),
    escape = FALSE,
    linesep = ""
  ) %>%
  add_header_above(c(
    " " = 1,
    "Baseline" = 1,
    "ICT-using" = 2,
    "ICT intensity" = 2
  )) %>%
  row_spec(1:3, extra_latex_after = "\\addlinespace[0.2em]\n")

writeLines(
  as.character(tabular_code),
  file.path(OUT_TAB, "tab_eu_fe_summary.tex")
)

cat("\n✓ Script 18 complete. Outputs saved to:\n")
cat(normalizePath(OUT_TAB, winslash = "/"), "\n", sep = "")
cat(normalizePath(OUT_APP_TAB, winslash = "/"), "\n", sep = "")
cat(normalizePath(OUT_FIG, winslash = "/"), "\n", sep = "")
