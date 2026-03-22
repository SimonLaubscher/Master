# ============================================================
# 17_us_lp_growth_vs_concentration_interval_FE.R
# ------------------------------------------------------------
# Purpose:
# Estimate the relationship between market concentration and
# labor productivity growth in the United States using the
# main balanced industry sample and Census-to-Census intervals.
#
# This script:
#   - loads the main balanced US panel from Script 11
#   - prepares annual descriptives for Census years
#   - reports descriptive correlations between CR4 and annual
#     labor productivity growth, overall and by ICT group
#   - plots mean CR4 by industry type across Census years
#   - constructs 5-year Census intervals using lp_index
#   - estimates industry and period fixed-effects models of
#     interval productivity growth on changes in CR4
#   - examines heterogeneity by ICT-using status and baseline
#     ICT intensity
#   - estimates a robustness specification using the level of
#     CR4 at the start of the interval
#   - exports figures, tables, and LaTeX regression output
#
# Main specification:
#   - Dependent variable:
#       average annual log growth in lp_index between Census years
#   - Main regressor:
#       change in CR4 over the same Census interval
#   - Fixed effects:
#       industry and Census-interval period fixed effects
#
# Input:
#   - dataclean/panel_us/panel_US_main_cr4_balanced_2002_2022.csv
#
# Outputs:
#   - outputs/chapter5_us/figures/
#   - outputs/chapter5_us/tables/
#
# Notes:
#   - Annual descriptives use lp_g in Census years only.
#   - The main FE specification uses Census-to-Census intervals
#     because CR4 is observed only in Census years.
#   - CR4 is scaled to shares (0-1) in the estimation code.
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
})


# -----------------------------
# Paths
# -----------------------------
ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_CLEAN <- file.path(ROOT, "dataclean")
PANEL_US   <- file.path(DATA_CLEAN, "panel_us")

OUT_DIR <- file.path(ROOT, "outputs", "chapter5_us")
OUT_FIG <- file.path(OUT_DIR, "figures")
OUT_TAB <- file.path(OUT_DIR, "tables")

dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)

PATH_IN <- file.path(PANEL_US, "panel_US_main_cr4_balanced_2002_2022.csv")

if (!file.exists(PATH_IN)) {
  stop(
    "Missing US main panel: ", PATH_IN,
    "\nRun 11_build_us_panel.R first."
  )
}

# -----------------------------
# Settings
# -----------------------------
CENSUS_YEARS <- c(2002L, 2007L, 2012L, 2017L, 2022L)

# Variable names in the input panel
Y_VAR         <- "lp_g"                  # annual log growth of lp_index, used for descriptives
LP_INDEX_VAR  <- "lp_index"              # LP index used to construct 5-year interval growth
CR_VAR        <- "CR4"                   # main concentration measure
ICT_BASE_VAR  <- "ICT_intensity_base"    # continuous baseline ICT intensity
ICT_GROUP_VAR <- "ICT_group_baseline"    # categorical baseline ICT group

SCALE_CR_TO_SHARE <- TRUE                # convert CR4 from percent (0-100) to share (0-1)

# -----------------------------
# Load data
# -----------------------------
p <- read_csv(PATH_IN, show_col_types = FALSE)

required_vars <- c(
  "industry_key", "industry", "year",
  "lp_g", "lp_index", "CR4", "CR8",
  "ICT_intensity_base", "ICT_group_baseline"
)

missing_vars <- setdiff(required_vars, names(p))
if (length(missing_vars) > 0) {
  stop("Missing variables: ", paste(missing_vars, collapse = ", "))
}

p <- p %>%
  mutate(
    industry_key = as.character(industry_key),
    industry     = as.character(industry),
    year         = as.integer(year),
    lp_g         = as.numeric(lp_g),
    lp_index     = as.numeric(lp_index),
    CR4          = as.numeric(CR4),
    CR8          = as.numeric(CR8),
    ICT_base     = as.numeric(ICT_intensity_base),
    ICT_group    = as.character(ICT_group_baseline)
  )

if (SCALE_CR_TO_SHARE) {
  p <- p %>%
    mutate(
      CR4 = CR4 / 100,
      CR8 = CR8 / 100
    )
}

# -----------------------------
# ICT classification + analysis samples
# -----------------------------
bad_groups <- setdiff(unique(na.omit(p$ICT_group)), c("ICT-producing", "ICT-using", "Other"))
if (length(bad_groups) > 0) {
  cli::cli_alert_warning("Unexpected ICT group labels found: {paste(bad_groups, collapse = ', ')}")
}

p <- p %>%
  mutate(
    ICT_using_dummy = as.integer(ICT_group == "ICT-using"),
    lp_g_pct = 100 * lp_g
  )

# Annual descriptives sample
p_desc <- p %>%
  filter(
    is.finite(lp_g_pct),
    is.finite(CR4),
    !is.na(ICT_group)
  )

# Base sample for interval FE construction
p_fe <- p %>%
  filter(
    year %in% CENSUS_YEARS,
    is.finite(lp_index),
    is.finite(CR4),
    is.finite(ICT_base),
    !is.na(ICT_group)
  )

cli::cli_alert_info(
  "Descriptive sample: rows={nrow(p_desc)} industries={n_distinct(p_desc$industry_key)} years={min(p_desc$year)}–{max(p_desc$year)}"
)

cli::cli_alert_info(
  "FE base sample: rows={nrow(p_fe)} industries={n_distinct(p_fe$industry_key)} years={min(p_fe$year)}–{max(p_fe$year)}"
)

check_fe <- p_fe %>%
  count(industry_key) %>%
  filter(n != length(CENSUS_YEARS))

if (nrow(check_fe) > 0) {
  print(check_fe)
  stop("Some industries do not have all Census years in p_fe.")
}

# ============================================================
# A) Descriptive correlations (Census years only)
# Main-text figure: pooled scatter with group-specific fits
# ============================================================
cli::cli_h1("A) Descriptive correlations (Census years)")

p_cy_plot <- p_desc %>%
  filter(year %in% CENSUS_YEARS) %>%
  mutate(
    CR4_pct   = 100 * CR4,     # CR4 in percent
    lp_g_pct  = 100 * lp_g,    # labor productivity growth in percent
    ICT_group = factor(ICT_group, levels = c("ICT-using", "Other"))
  )

p_scatter <- ggplot(p_cy_plot, aes(x = CR4_pct, y = lp_g_pct, color = ICT_group)) +
  
  # Points
  geom_point(alpha = 0.7, size = 2.2) +
  
  # Group-specific linear fits
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  
  # Pooled fit (dashed, across all observations)
  geom_smooth(
    data = p_cy_plot,
    aes(x = CR4_pct, y = lp_g_pct),
    inherit.aes = FALSE,
    method = "lm",
    se = FALSE,
    color = "black",
    linetype = "dashed",
    linewidth = 1.1
  ) +
  
  # Labels
  labs(
    x = "CR4 (percent)",
    y = "Annual labor productivity growth (percent)",
    color = "Industry type"
  ) +
  
  # Optional: keep only if you want fixed colors across figures
  scale_color_manual(
    values = c("ICT-using" = "#e41a1c", "Other" = "#377eb8")
  ) +
  
  # Theme
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

ggsave(
  filename = file.path(OUT_FIG, "fig_us_scatter_lp_g_vs_cr4_ICTgroup_pooled.png"),
  plot = p_scatter,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)















library(dplyr)
library(tidyr)
library(knitr)

# ------------------------------------------------------------
# Correlations by year and ICT group
# ------------------------------------------------------------
corr_by_year_group <- p_desc %>%
  filter(
    year %in% CENSUS_YEARS,
    ICT_group %in% c("ICT-using", "Other"),
    is.finite(CR4),
    is.finite(lp_g)
  ) %>%
  group_by(year, ICT_group) %>%
  summarise(
    corr = cor(CR4, lp_g, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(corr = round(corr, 3)) %>%
  pivot_wider(
    names_from = ICT_group,
    values_from = corr
  )

# ------------------------------------------------------------
# Overall correlation by year
# ------------------------------------------------------------
corr_overall <- p_desc %>%
  filter(
    year %in% CENSUS_YEARS,
    is.finite(CR4),
    is.finite(lp_g)
  ) %>%
  group_by(year) %>%
  summarise(
    Overall = round(cor(CR4, lp_g, use = "complete.obs"), 3),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Merge yearly results
# ------------------------------------------------------------
corr_table <- corr_by_year_group %>%
  left_join(corr_overall, by = "year") %>%
  arrange(year) %>%
  mutate(year = as.character(year))

# ------------------------------------------------------------
# Pooled (all years combined)
# ------------------------------------------------------------
corr_pooled <- p_desc %>%
  filter(
    year %in% CENSUS_YEARS,
    is.finite(CR4),
    is.finite(lp_g)
  ) %>%
  summarise(
    year = "All years",
    `ICT-using` = round(
      cor(CR4[ICT_group == "ICT-using"],
          lp_g[ICT_group == "ICT-using"],
          use = "complete.obs"),
      3
    ),
    Other = round(
      cor(CR4[ICT_group == "Other"],
          lp_g[ICT_group == "Other"],
          use = "complete.obs"),
      3
    ),
    Overall = round(cor(CR4, lp_g, use = "complete.obs"), 3)
  )

# ------------------------------------------------------------
# Combine
# ------------------------------------------------------------
corr_table_final <- bind_rows(corr_table, corr_pooled)




latex_code <- kable(
  corr_table_final,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Census year", "ICT-using", "Other", "Overall"),
  align = c("l", "c", "c", "c")
)

writeLines(latex_code, file.path(OUT_TAB, "tab_A5_us_corr_by_year_ictgroup.tex"))









fig_cr4_group <- p %>%
  filter(
    year %in% CENSUS_YEARS,
    ICT_group %in% c("ICT-using", "Other"),
    is.finite(CR4)
  ) %>%
  group_by(year, ICT_group) %>%
  summarise(
    mean_CR4 = mean(CR4, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_CR4 = 100 * mean_CR4,
    ICT_group = factor(ICT_group, levels = c("ICT-using", "Other"))
  ) %>%
  ggplot(aes(x = year, y = mean_CR4, color = ICT_group, group = ICT_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("ICT-using" = "#e41a1c", "Other" = "#377eb8")
  ) +
  scale_x_continuous(breaks = CENSUS_YEARS) +
  labs(
    x = "Census year",
    y = "Mean CR4 (percent)",
    color = "Industry type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

ggsave(
  filename = file.path(OUT_FIG, "fig_us_mean_cr4_by_ICTgroup_census_years.png"),
  plot = fig_cr4_group,
  width = 6.5,
  height = 3.6,
  dpi = 300,
  bg = "white"
)





# ============================================================
# Appendix Table: Mean CR4 by industry type and Census year
# ============================================================

tab_cr4_appendix <- p %>%
  filter(year %in% CENSUS_YEARS) %>%
  group_by(year, ICT_group) %>%
  summarise(
    mean_CR4 = mean(CR4, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_CR4 = if (SCALE_CR_TO_SHARE) mean_CR4 * 100 else mean_CR4  # back to %
  ) %>%
  tidyr::pivot_wider(
    names_from = ICT_group,
    values_from = mean_CR4
  ) %>%
  arrange(year)


readr::write_csv(
  tab_cr4_appendix,
  file.path(OUT_TAB, "tab_us_mean_cr4_by_group_census_years.csv")
)


kable(tab_cr4_appendix, 
      format = "latex", 
      digits = 1, 
      booktabs = TRUE, 
      escape = FALSE) %>%
  cat(file = file.path(OUT_TAB, "tab_A5us_mean_cr4_by_group_census_years.tex"))


# ============================================================
# B) Main US FE: Census‑to‑Census interval panel (only tabular)
# ============================================================
cli::cli_h1("B) Main: Interval FE (Census‑to‑Census) - tabular only")

# Check that lp_index is positive before taking logs
min_lp <- min(p_fe$lp_index, na.rm = TRUE)
if (!is.finite(min_lp) || min_lp <= 0) {
  stop("lp_index contains non‑positive values; cannot take logs safely. min(lp_index) = ", min_lp)
}

# Construct 5‑year intervals
df_int <- p_fe %>%
  arrange(industry_key, year) %>%
  group_by(industry_key) %>%
  mutate(
    year_lead   = lead(year),
    gap         = year_lead - year,
    lp_lead     = lead(lp_index),
    CR4_start   = CR4,
    CR4_end     = lead(CR4),
    # 5‑year log growth (annualized)
    lp_g_5y     = (log(lp_lead) - log(lp_index)) / 5,
    # Change in CR4 over interval
    d_CR4_5y    = CR4_end - CR4_start,
    # Period label (e.g., "2002–2007")
    period      = paste0(year, "-", year_lead)
  ) %>%
  ungroup()

# Ensure all intervals are exactly 5 years
if (any(df_int$gap != 5, na.rm = TRUE)) {
  stop("Non‑5‑year interval detected in Census panel.")
}

# Keep only valid intervals
df_int <- df_int %>%
  filter(
    is.finite(lp_g_5y),
    is.finite(d_CR4_5y),
    !is.na(period)
  ) %>%
  mutate(
    period = factor(period)
  )

# Info
cli::cli_alert_info(
  "Interval sample: rows={nrow(df_int)} industries={n_distinct(df_int$industry_key)} periods={n_distinct(df_int$period)}"
)

# Main fixed‑effects specifications
int1 <- feols(
  lp_g_5y ~ d_CR4_5y | industry_key + period,
  data = df_int,
  vcov = ~industry_key
)

# Interaction with ICT‑using dummy
int3 <- feols(
  lp_g_5y ~ d_CR4_5y + d_CR4_5y:ICT_using_dummy | industry_key + period,
  data = df_int,
  vcov = ~industry_key
)

# Interaction with ICT baseline intensity
int2 <- feols(
  lp_g_5y ~ d_CR4_5y + d_CR4_5y:ICT_base | industry_key + period,
  data = df_int,
  vcov = ~industry_key
)

# Diagnostic table (for console / debugging)
print(
  etable(
    int1, int3, int2,
    se.below = TRUE,
    digits = 4,
    dict = c(
      "d_CR4_5y" = "Δ CR4",
      "d_CR4_5y:ICT_base" = "Δ CR4 × ICT intensity",
      "d_CR4_5y:ICT_using_dummy" = "Δ CR4 × ICT‑using"
    ),
    headers = c(
      "INT1" = "Baseline",
      "INT3" = "ICT‑using",
      "INT2" = "ICT intensity"
    )
  )
)

# Tidy regression results (for CSV)
tab_int_main <- bind_rows(
  tidy(int1) %>% mutate(model = "Baseline"),
  tidy(int3) %>% mutate(model = "ICT‑using"),
  tidy(int2) %>% mutate(model = "ICT intensity")
) %>%
  select(model, term, estimate, std.error, statistic, p.value)

write_csv(tab_int_main, file.path(OUT_TAB, "tab_us_interval_fe_main.csv"))

# Export main table to LaTeX (tabular only, no caption/notes)
TEX_TABULAR_MAIN <- file.path(OUT_TAB, "tab_us_interval_fe_main.tex")

fixest::etable(
  int1, int3, int2,
  tex = TRUE,
  file = TEX_TABULAR_MAIN,
  replace = TRUE,
  se.below = TRUE,
  digits = 4,
  dict = c(
    "d_CR4_5y" = "$\\Delta$ CR4",
    "d_CR4_5y:ICT_base" = "$\\Delta$ CR4 $\\times$ ICT intensity",
    "d_CR4_5y:ICT_using_dummy" = "$\\Delta$ CR4 $\\times$ ICT‑using"
  ),
  fitstat = c("n", "r2", "wr2"),
  headers = c(
    "INT1" = "Baseline",
    "INT3" = "ICT‑using",
    "INT2" = "ICT intensity"
  )
)


cli::cli_alert_success("Saved LaTeX tabular (main interval): {TEX_TABULAR_MAIN}")





# ============================================================
# C) Appendix robustness: CR4 level at start of interval (tabular only)
# ============================================================
cli::cli_h1("C) Appendix robustness: CR4 level at start of interval - tabular only")

# Level of CR4 at start of interval
rob1 <- feols(
  lp_g_5y ~ CR4_start | industry_key + period,
  data = df_int,
  vcov = ~industry_key
)

# Interaction with ICT baseline intensity
rob2 <- feols(
  lp_g_5y ~ CR4_start + CR4_start:ICT_base | industry_key + period,
  data = df_int,
  vcov = ~industry_key
)

# Diagnostic table (for console)
print(etable(rob1, rob2, se.below = TRUE))

# Export robustness table to LaTeX (tabular only, no caption/notes)
TEX_TABULAR_ROB_LEVEL <- file.path(OUT_TAB, "tab_us_interval_rob_level.tex")
fixest::etable(
  rob1, rob2,
  se.below = TRUE,
  digits = 3,
  dict = c(
    "CR4_start" = "CR4 at start of interval",
    "CR4_start:ICT_base" = "CR4 start $\\times$ ICT intensity (baseline)"
  ),
  fitstat = c("n", "r2", "wr2"),
  headers = c(
    "ROB1" = "Level (start)",
    "ROB2" = "Level $\\times$ ICT"
  ),
  tex = TRUE,
  file = TEX_TABULAR_ROB_LEVEL,
  replace = TRUE
)


cli::cli_alert_success("Saved LaTeX tabular (robustness level): {TEX_TABULAR_ROB_LEVEL}")
cli::cli_alert_success("US interval FE script finished (tabular only).")

