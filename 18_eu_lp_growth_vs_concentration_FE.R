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
#   - dataclean/panel_eu/panel_common_2003_2020_analysis_strict.rds
#
# Outputs:
#   - outputs/chapter5_eu/figures/
#   - outputs/chapter5_eu/tables/
#
# Notes:
#   - EU concentration is available annually, unlike the US
#     Census-based concentration panel.
#   - Results are descriptive and do not imply causality.
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
})

# -----------------------------
# Paths
# -----------------------------
ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_CLEAN <- file.path(ROOT, "dataclean")
PANEL_EU   <- file.path(DATA_CLEAN, "panel_eu")

OUT_DIR <- file.path(ROOT, "outputs", "chapter5_eu")
OUT_FIG <- file.path(OUT_DIR, "figures")
OUT_TAB <- file.path(OUT_DIR, "tables")

dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)

PATH_IN <- file.path(PANEL_EU, "panel_common_2003_2020_analysis_strict.rds")

if (!file.exists(PATH_IN)) {
  stop(
    "Missing EU main panel: ", PATH_IN,
    "\nRun 12_build_EU_panel.R first."
  )
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

tabular_code <- kable(
  tab_corr_full,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Country", "ICT group", "N obs.", "N industries", "Corr(LP growth, HHI)"),
  align = c("l", "l", "r", "r", "r")
)

writeLines(tabular_code, file.path(OUT_TAB, "tab_eu_corr_by_country_ICTgroup.tex"))


# ============================================================
# B) Mean concentration by ICT group over time (MAIN TEXT)
# ============================================================
cli::cli_h1("B) Mean concentration by ICT group (EU)")

fig_conc_group <- p_desc %>%
  filter(ICT_group %in% c("ICT-using", "Other")) %>%
  mutate(
    country = factor(country, levels = COUNTRY_ORDER),
    ICT_group = factor(ICT_group, levels = c("ICT-using", "Other")),
    conc_hhi = conc * 10000
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
    color = "Industry type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
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
    mean_hhi = round(mean(conc, na.rm = TRUE)*10000, 0),
    n_industries = n_distinct(industry_clean),
    .groups = "drop"
  )
print(tab_long, n = Inf)  # Now shows proper HHI scale

# Note:
# Variation in mean concentration partly reflects small group sizes
# in some country-year cells, especially for ICT-using industries.






# ============================================================
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
  
  # FE1: baseline
  m1 <- feols(
    y ~ conc | unit_id + year,
    data = d,
    vcov = ~unit_id
  )
  
  # FE2: heterogeneity by ICT-using dummy
  m2 <- feols(
    y ~ conc + conc:ICT_using_dummy | unit_id + year,
    data = d,
    vcov = ~unit_id
  )
  
  # FE3: heterogeneity by baseline ICT intensity
  m3 <- feols(
    y ~ conc + conc:ICT_base | unit_id + year,
    data = d,
    vcov = ~unit_id
  )
  
  # -----------------------------
  # Robustness models
  # -----------------------------
  
  # ROB1: lagged concentration
  d_lag <- d %>%
    group_by(unit_id) %>%
    mutate(
      conc_l1 = lag(conc, 1)
    ) %>%
    ungroup() %>%
    filter(is.finite(conc_l1))
  
  m_rob1 <- feols(
    y ~ conc_l1 | unit_id + year,
    data = d_lag,
    vcov = ~unit_id
  )
  
  # ROB2: annual change in concentration
  d_diff <- d %>%
    group_by(unit_id) %>%
    mutate(
      d_conc = conc - lag(conc, 1)
    ) %>%
    ungroup() %>%
    filter(is.finite(d_conc))
  
  m_rob2 <- feols(
    y ~ d_conc | unit_id + year,
    data = d_diff,
    vcov = ~unit_id
  )
  
  fe_models[[cc]] <- list(
    m1 = m1,
    m2 = m2,
    m3 = m3,
    m_rob1 = m_rob1,
    m_rob2 = m_rob2
  )
  
  # -----------------------------
  # Appendix country table
  # -----------------------------
  TEX_FE_CC <- file.path(OUT_TAB, paste0("tab_eu_fe_", cc, ".tex"))
  
  fixest::etable(
    m1, m2, m3, m_rob1, m_rob2,
    tex = TRUE,
    file = TEX_FE_CC,
    replace = TRUE,
    se.below = TRUE,
    digits = 3,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    dict = c(
      "conc" = "HHI",
      "conc:ICT_using_dummy" = "HHI $\\times$ ICT-using",
      "conc:ICT_base" = "HHI $\\times$ ICT intensity",
      "conc_l1" = "Lagged HHI",
      "d_conc" = "$\\Delta$ HHI"
    ),
    headers = c(
      "FE1" = "Baseline",
      "FE2" = "ICT-using",
      "FE3" = "ICT intensity",
      "ROB1" = "Lagged HHI",
      "ROB2" = "$\\Delta$ HHI"
    ),
    title = paste0("Fixed effects regressions: ", cc),
    notes = c(
      "Dependent variable is annual labour productivity growth.",
      "All models include country-industry and year fixed effects.",
      "Standard errors are clustered at the country-industry level.",
      "The estimation sample includes ICT-using and Other industries only; ICT-producing industries are excluded because this category often contains only one industry.",
      "ROB1 uses one-year lagged concentration. ROB2 uses the annual change in concentration.",
      "Significance levels: *** p<0.01, ** p<0.05, * p<0.1."
    )
  )
  
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
  
  d_cc <- p_fe_main %>% filter(country == cc)
  
  b1  <- get_coef_info(m1, "conc")
  b2m <- get_coef_info(m2, "conc")
  b2i <- get_coef_info(m2, "conc:ICT_using_dummy")
  b3m <- get_coef_info(m3, "conc")
  b3i <- get_coef_info(m3, "conc:ICT_base")
  
  tibble::tibble(
    Country = cc,
    `N industries` = n_distinct(d_cc$industry_clean),
    `HHI (FE1)` = fmt(b1[1], b1[2], b1[3]),
    `HHI (FE2)` = fmt(b2m[1], b2m[2], b2m[3]),
    `HHI × ICT-using` = fmt(b2i[1], b2i[2], b2i[3]),
    `HHI (FE3)` = fmt(b3m[1], b3m[2], b3m[3]),
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

write_csv(
  tab_fe_summary,
  file.path(OUT_TAB, "tab_eu_fe_summary.csv")
)

save_kable_latex(
  df = tab_fe_summary,
  file = file.path(OUT_TAB, "tab_eu_fe_summary.tex"),
  caption = "Fixed effects estimates of labour productivity growth and market concentration (EU)",
  label = "tab:eu_fe_summary",
  notes = c(
    "All models are estimated separately by country.",
    "The estimation sample includes ICT-using and Other industries only; ICT-producing industries are excluded because this category often contains only one industry.",
    "All models include country-industry and year fixed effects.",
    "Standard errors are clustered at the country-industry level.",
    "FE2 reports heterogeneity by ICT-using status. FE3 interacts concentration with baseline ICT intensity.",
    "Entries report coefficients with clustered standard errors in parentheses.",
    "Significance levels: *** p<0.01, ** p<0.05, * p<0.1."
  ),
  digits = 3
)

# ============================================================
# E) Robustness summary table (optional appendix)
# ============================================================
cli::cli_h1("E) Robustness summary table")

tab_fe_robust <- bind_rows(lapply(COUNTRY_ORDER, function(cc) {
  
  m_rob1 <- fe_models[[cc]]$m_rob1
  m_rob2 <- fe_models[[cc]]$m_rob2
  
  b_rob1 <- get_coef_info(m_rob1, "conc_l1")
  b_rob2 <- get_coef_info(m_rob2, "d_conc")
  
  tibble::tibble(
    Country = cc,
    `Lagged HHI` = fmt(b_rob1[1], b_rob1[2], b_rob1[3]),
    `$\\Delta$ HHI` = fmt(b_rob2[1], b_rob2[2], b_rob2[3])
  )
}))

tab_fe_robust <- tab_fe_robust %>%
  mutate(
    Country = dplyr::recode(
      Country,
      "DE" = "Germany",
      "FR" = "France",
      "DK" = "Denmark",
      "SE" = "Sweden"
    )
  )

write_csv(
  tab_fe_robust,
  file.path(OUT_TAB, "tab_eu_fe_robustness_summary.csv")
)

save_kable_latex(
  df = tab_fe_robust,
  file = file.path(OUT_TAB, "tab_eu_fe_robustness_summary.tex"),
  caption = "Robustness checks for fixed effects estimates of labour productivity growth and market concentration (EU)",
  label = "tab:eu_fe_robust",
  notes = c(
    "All models are estimated separately by country.",
    "The estimation sample includes ICT-using and Other industries only.",
    "All models include country-industry and year fixed effects.",
    "Standard errors are clustered at the country-industry level.",
    "Lagged HHI uses one-year lagged concentration; $\\Delta$ HHI uses the annual change in concentration.",
    "Entries report coefficients with clustered standard errors in parentheses.",
    "Significance levels: *** p<0.01, ** p<0.05, * p<0.1."
  ),
  digits = 3
)

cli::cli_alert_success("EU FE regressions finished.")