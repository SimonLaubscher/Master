# ============================================================
# 16_concentration_outputs_5_3.R
# Chapter 5.3: market concentration
#
# Inputs:
#   dataclean/compnet_hybrid_annual.csv
#   dataclean/census_concentration_panel_2002_2022.csv
#
# Outputs:
#   outputs/figures/
#   outputs/tables/
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
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

EU_FILE <- file.path(DATA_CLEAN, "compnet_hybrid_annual.csv")
US_FILE <- file.path(DATA_CLEAN, "census_concentration_panel_2002_2022.csv")

if (!file.exists(EU_FILE)) {
  stop("Missing CompNet hybrid panel: ", EU_FILE,
       "\nRun the CompNet build script first.")
}

if (!file.exists(US_FILE)) {
  stop("Missing US Census concentration panel: ", US_FILE,
       "\nRun the Census concentration build script first.")
}

# ============================================================
# 1) Settings
# ============================================================

PLOT_NAICS_LEN <- 4L
TOP_N <- 10

save_plot <- function(p, fname, w = 10, h = 6, dpi = 300) {
  ggsave(
    filename = file.path(OUT_FIG, fname),
    plot = p,
    width = w, height = h, dpi = dpi, bg = "white"
  )
}

# ============================================================
# 2) Load data
# ============================================================

# EU: common sample period for cross-country comparison
eu <- read_csv(EU_FILE, show_col_types = FALSE) %>%
  mutate(
    country = as.character(country),
    year    = as.integer(year),
    NACE2   = sprintf("%02d", as.integer(NACE2)),
    HHI_rev = as.numeric(HHI_rev)
  ) %>%
  filter(year >= 2003, year <= 2020)

# US: full Economic Census panel
us <- read_csv(US_FILE, show_col_types = FALSE) %>%
  mutate(
    year      = as.integer(year),
    NAICS_len = as.integer(NAICS_len),
    NAICS_key = as.character(NAICS_key),
    CR4       = as.numeric(CR4),
    CR8       = as.numeric(CR8),
  )

# ============================================================
# 3) Sanity checks
# ============================================================

dup_us <- us %>%
  count(year, NAICS_key, NAICS_len) %>%
  filter(n > 1)

if (nrow(dup_us) > 0) {
  stop("US data contains duplicate (year, NAICS_key, NAICS_len) rows.")
}

if (any(is.finite(us$CR4) & (us$CR4 < 0 | us$CR4 > 100))) {
  stop("US CR4 contains values outside [0, 100].")
}

if (any(is.finite(eu$HHI_rev) & (eu$HHI_rev < 0 | eu$HHI_rev > 1))) {
  stop("EU HHI_rev contains values outside [0, 1].")
}

# ============================================================
# 4) Coverage
# ============================================================

eu_n_by_year <- eu %>%
  distinct(country, year, NACE2) %>%
  count(country, year, name = "n_industries") %>%
  arrange(country, year)

write_csv(
  eu_n_by_year,
  file.path(OUT_TAB, "diag_eu_n_industries_by_country_year.csv")
)

us_L <- us %>%
  filter(NAICS_len == PLOT_NAICS_LEN, !is.na(NAICS_key))

us_n_by_year <- us_L %>%
  distinct(year, NAICS_key) %>%
  count(year, name = "n_industries") %>%
  arrange(year)

write_csv(
  us_n_by_year,
  file.path(OUT_TAB, paste0("diag_us_n_industries_by_year_naics", PLOT_NAICS_LEN, ".csv"))
)
# ============================================================
# 4.5) CR4 coverage verification
# ============================================================

us_cr4_by_year <- us_L %>%
  filter(is.finite(CR4)) %>%  # ONLY industries WITH CR4 data
  distinct(year, NAICS_key) %>%
  count(year, name = "n_cr4_industries") %>%
  arrange(year)

print(us_cr4_by_year)  # Check: ~175 pre-2017?

write_csv(
  us_cr4_by_year,
  file.path(OUT_TAB, "diag_us_cr4_industries_by_year_naics4.csv")
)


# ============================================================
# 5) Trends in market concentration
# ============================================================

# ---- US: CR4 main trend ----
us_trend <- us_L %>%
  group_by(year) %>%
  summarise(
    mean_CR4 = mean(CR4, na.rm = TRUE),
    med_CR4  = median(CR4, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  us_trend,
  file.path(OUT_TAB, paste0("tab_531_us_cr4_naics", PLOT_NAICS_LEN, ".csv"))
)



p_us_cr4_bottom <- us_trend %>%
  pivot_longer(
    cols = c(mean_CR4, med_CR4),
    names_to = "stat",
    values_to = "CR4"
  ) %>%
  mutate(
    stat = recode(stat,
                  mean_CR4 = "Mean",
                  med_CR4  = "Median"
    )
  ) %>%
  ggplot(aes(x = year, y = CR4, linetype = stat, shape = stat)) +
  geom_line(linewidth = 1.0, color = "black") +
  geom_point(size = 2.0, color = "black") +
  scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dashed")) +
  scale_shape_manual(values = c("Mean" = 16, "Median" = 17)) +
  scale_x_continuous(breaks = sort(unique(us_trend$year))) +
  labs(
    x = "Year",
    y = "CR4 (%)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(
    linetype = guide_legend(nrow = 1, keywidth = 2.2),
    shape = guide_legend(nrow = 1)
  )

save_plot(
  p_us_cr4_bottom,
  paste0("fig_531_us_cr4_mean_median_naics", PLOT_NAICS_LEN, "_legend_bottom.png"),
  w = 6.5,
  h = 3.6
)


# ---- US: CR8 robustness table ----
us_trend_cr8 <- us_L %>%
  group_by(year) %>%
  summarise(
    mean_CR8 = mean(CR8, na.rm = TRUE),
    med_CR8  = median(CR8, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  us_trend_cr8,
  file.path(OUT_TAB, paste0("tab_531_us_cr8_naics", PLOT_NAICS_LEN, ".csv"))
)

# ---- EU: HHI main trend ----
eu_trend_year <- eu %>%
  filter(is.finite(HHI_rev)) %>%
  group_by(country, year) %>%
  summarise(
    n_industries = n_distinct(NACE2),
    mean_HHI = mean(HHI_rev, na.rm = TRUE) * 10000,
    med_HHI  = median(HHI_rev, na.rm = TRUE) * 10000,
    .groups = "drop"
  ) %>%
  arrange(country, year)

write_csv(
  eu_trend_year,
  file.path(OUT_TAB, "tab_531_eu_hhi_by_year_2003_2020.csv")
)

p_eu_hhi <- eu_trend_year %>%
  pivot_longer(
    cols = c(mean_HHI, med_HHI),
    names_to = "stat",
    values_to = "HHI"
  ) %>%
  mutate(
    stat = recode(
      stat,
      mean_HHI = "Mean",
      med_HHI  = "Median"
    ),
    country = factor(country, levels = c("DE", "DK", "FR", "SE"))
  ) %>%
  ggplot(aes(x = year, y = HHI, linetype = stat)) +
  geom_line(linewidth = 0.9, color = "black") +
  geom_point(aes(shape = stat), size = 1.8, color = "black") +
  facet_wrap(~country, ncol = 2) +
  scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dashed")) +
  scale_shape_manual(values = c("Mean" = 16, "Median" = 17)) +
  scale_x_continuous(breaks = c(2003, 2005, 2010, 2015, 2020)) +
  labs(
    x = "Year",
    y = "HHI (0–10,000)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

save_plot(
  p_eu_hhi,
  "fig_531_eu_hhi_mean_median_2003_2020.png",
  w = 11,
  h = 6.5
)


# ============================================================
# 5.5) US Trends: Tables + Balanced Panel Robustness
# ============================================================

# A1) Full sample CR4/CR8 trends WITH coverage calculation
us_full_trends <- us_L %>%
  group_by(year) %>%
  summarise(
    n_total = n_distinct(NAICS_key),
    n_cr4   = n_distinct(NAICS_key[!is.na(CR4)]),
    mean_CR4 = mean(CR4, na.rm = TRUE),
    med_CR4  = median(CR4, na.rm = TRUE),
    mean_CR8 = mean(CR8, na.rm = TRUE),
    med_CR8  = median(CR8, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage = paste0(n_cr4, "/", n_total, " (", 
                      round(100 * n_cr4 / n_total, 0), "\\%)")
  ) %>%
  select(year, mean_CR4, med_CR4, mean_CR8, med_CR8, coverage)

# Table A1: CR4/CR8 full sample → TABULAR
kable(us_full_trends, 
      digits = 1,
      format = "latex",
      booktabs = TRUE,
      col.names = c("Year", "Mean CR4", "Median CR4", "Mean CR8", "Median CR8", "Cov.")) %>%
  cat(file = file.path(OUT_TAB, "tab_A1_cr4_cr8_full.tex"))

# A2) Balanced panel (144 industries, all 5 years)
us_balanced_keys <- us_L %>%
  filter(is.finite(CR4)) %>%
  distinct(year, NAICS_key) %>%
  count(NAICS_key, name = "n_years") %>%
  filter(n_years == 5) %>%
  pull(NAICS_key)

cat("Balanced panel N:", length(us_balanced_keys), "\n")  # 144

us_balanced_trend <- us_L %>%
  filter(NAICS_key %in% us_balanced_keys) %>%
  group_by(year) %>%
  summarise(
    n_ind = n_distinct(NAICS_key),
    mean_CR4 = mean(CR4, na.rm = TRUE),
    med_CR4  = median(CR4, na.rm = TRUE),
    .groups = "drop"
  )

# Table A2: Balanced panel → TABULAR  
kable(us_balanced_trend,
      digits = 1,
      format = "latex",
      booktabs = TRUE,
      col.names = c("Year", "N", "Mean CR4", "Median CR4")) %>%
  cat(file = file.path(OUT_TAB, "tab_A2_balanced.tex"))


# ============================================================
# EU HHI Table: Clean aggregate trends by country (Appendix)
# ============================================================

eu_hhi_ends <- eu_trend_year %>%
  filter(year %in% c(2003, 2020)) %>%
  mutate(
    mean_HHI = round(mean_HHI, 0),
    med_HHI  = round(med_HHI, 0),
    year = as.character(year)
  ) %>%
  select(country, year, mean_HHI, med_HHI, n_industries) %>%
  pivot_wider(
    names_from = year,
    values_from = c(mean_HHI, med_HHI, n_industries),
    names_glue = "{year}_{.value}"
  ) %>%
  select(
    country,
    `2003_mean_HHI`, `2003_med_HHI`, `2003_n_industries`,
    `2020_mean_HHI`, `2020_med_HHI`, `2020_n_industries`
  )

tab_A3_tex <- eu_hhi_ends %>%
  rename(
    Country = country,
    Mean = `2003_mean_HHI`,
    Median = `2003_med_HHI`,
    N = `2003_n_industries`,
    `Mean ` = `2020_mean_HHI`,
    `Median ` = `2020_med_HHI`,
    `N ` = `2020_n_industries`
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = "lcccccc",
    escape = TRUE
  ) %>%
  add_header_above(c(" " = 1, "2003" = 3, "2020" = 3))

writeLines(
  as.character(tab_A3_tex),
  file.path(OUT_TAB, "tab_A3_eu_hhi_ends.tex")
)

# ============================================================
# EU HHI Table: Industry Coverage (Appendix)
# ============================================================
# ============================================================
# Table A4a: 2003-2011 (Early Period)
# ============================================================
industry_a <- eu_trend_year %>%
  filter(year <= 2011) %>%
  select(country, year, n_industries) %>%
  distinct() %>%
  pivot_wider(names_from = year, values_from = n_industries, values_fill = NA) %>%
  arrange(country)

kable(industry_a, 
      format = "latex", 
      booktabs = TRUE, 
      escape = FALSE,
      col.names = c("Country", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")) %>%
  cat(file = file.path(OUT_TAB, "tab_A4a_coverage_2003_2011.tex"))

# ============================================================
# Table A4b: 2012-2020 (Late Period)  
# ============================================================
industry_b <- eu_trend_year %>%
  filter(year >= 2012) %>%
  select(country, year, n_industries) %>%
  distinct() %>%
  pivot_wider(names_from = year, values_from = n_industries, values_fill = NA) %>%
  arrange(country)

kable(industry_b, 
      format = "latex", 
      booktabs = TRUE, 
      escape = FALSE,
      col.names = c("Country", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")) %>%
  cat(file = file.path(OUT_TAB, "tab_A4b_coverage_2012_2020.tex"))



#delet from here onwards?
# ============================================================
# 6) Cross-industry concentration 
# ============================================================

# ---- EU: top 10 industries by average HHI ----
eu_top_hhi <- eu %>%
  filter(is.finite(HHI_rev)) %>%
  group_by(country, NACE2) %>%
  summarise(
    mean_HHI = mean(HHI_rev, na.rm = TRUE) * 10000,
    .groups = "drop"
  ) %>%
  group_by(country) %>%
  slice_max(mean_HHI, n = TOP_N, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(country, desc(mean_HHI))

write_csv(
  eu_top_hhi,
  file.path(OUT_TAB, "tab_532_eu_top_industries_by_mean_hhi.csv")
)

# ---- US: top 10 industries by average CR4 ----
us_top <- us_L %>%
  filter(is.finite(CR4)) %>%
  group_by(NAICS_key) %>%
  summarise(
    mean_CR4 = mean(CR4, na.rm = TRUE),
    LABEL    = dplyr::first(na.omit(LABEL)),
    .groups = "drop"
  ) %>%
  slice_max(mean_CR4, n = TOP_N, with_ties = FALSE) %>%
  arrange(desc(mean_CR4))

write_csv(
  us_top,
  file.path(OUT_TAB, paste0("tab_532_us_top_industries_by_mean_cr4_naics", PLOT_NAICS_LEN, ".csv"))
)





library(kableExtra)
library(dplyr)

# ============================================================
# Table A6: EU Top 10 Industries by HHI, 2020 ONLY
# ============================================================
eu_2020 <- eu %>%
  filter(year == 2020) %>%
  group_by(country, NACE_key) %>%
  slice_max(HHI, n = 1) %>%
  slice_head(n = 10) %>%  # Top 10 across countries
  ungroup()

kable(eu_2020[, c("country", "NACE_key", "HHI")], 
      col.names = c("Country", "Industry", "HHI"),
      format = "latex", digits = 0, booktabs = TRUE, escape = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  cat(file = file.path(OUT_TAB, "tab_A6_eu_top_hhi.tex"))

# ============================================================
# Table A7: US Top 10 Industries by CR4, 2022 ONLY
# ============================================================
us_2022 <- us %>%
  filter(year == 2022) %>%
  slice_max(CR4, n = 10)

kable(us_2022[, c("NAICS_key", "CR4")], 
      col.names = c("Industry", "CR4"),
      format = "latex", digits = 1, booktabs = TRUE, escape = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  cat(file = file.path(OUT_TAB, "tab_A7_us_top_cr4.tex"))
