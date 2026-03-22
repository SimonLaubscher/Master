# ============================================================
# 14_aggregate_lp_analysis.R
# Purpose:
# Construct and compare aggregate labor productivity across
# selected countries (US, Germany, France, Denmark, Sweden).
#
# Steps:
# 1. Load EU KLEMS aggregate productivity growth (TOT_IND)
# 2. Load US aggregate productivity growth (re-aggregated ILPA)
# 3. Restrict to common sample period across countries
# 4. Construct productivity index (base year = first overlap year)
# 5. Produce Figure 5.1 (productivity index)
# 6. Compute Table 5.1 (average growth by period)
# 7. Export figure and LaTeX table
#
# Data sources:
# - EU KLEMS (LP1_G, TOT_IND)
# - BEA–BLS ILPA (re-aggregated, excluding government)
#
# Output:
# - fig_5_1_lp_index_classic.png
# - table_5_1_LP_growth_periods.csv
# - table_5_1_LP_growth_periods.tex
#
# ============================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(knitr)
  library(kableExtra)
})

# ============================================================
# 0) Paths
# ============================================================

ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_CLEAN <- file.path(ROOT, "dataclean")
OUT_FIG    <- file.path(ROOT, "outputs", "figures")
OUT_TAB    <- file.path(ROOT, "outputs", "tables")


dir.create(OUT_FIG,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB,  recursive = TRUE, showWarnings = FALSE)


EU_TOTAL_RDS <- file.path(DATA_CLEAN, "klems_clean_total.rds")
US_REAGG_PATH <- file.path(DATA_CLEAN, "us_TOTIND_exgov_growth.csv")

stopifnot(
  file.exists(EU_TOTAL_RDS),
  file.exists(US_REAGG_PATH)
)
# ============================================================
# 1) Settings
# ============================================================

EU_COUNTRIES <- c("DE", "FR", "DK", "SE")
COUNTRIES    <- c("US", EU_COUNTRIES)

EU_AGG_CODE <- "TOT_IND"
BREAK_YEAR  <- 2005L

theme_thesis <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      legend.position = "bottom"
    )
}

# ============================================================
# 2) Load EU KLEMS aggregate labour productivity growth
# ============================================================

if (!file.exists(EU_TOTAL_RDS)) {
  stop("Missing EU KLEMS totals file: ", EU_TOTAL_RDS)
}

eu_raw <- readRDS(EU_TOTAL_RDS)

needed_eu <- c("country", "year", "industry_clean", "LP1_G")
missing_eu <- setdiff(needed_eu, names(eu_raw))

if (length(missing_eu) > 0) {
  stop("EU totals file is missing required columns: ",
       paste(missing_eu, collapse = ", "))
}

eu_total <- eu_raw %>%
  mutate(year = as.integer(year)) %>%
  filter(
    country %in% EU_COUNTRIES,
    industry_clean == EU_AGG_CODE
  ) %>%
  transmute(
    country,
    year,
    lp_g_log = as.numeric(LP1_G)
  ) %>%
  filter(is.finite(year), is.finite(lp_g_log))

if (nrow(eu_total) == 0) {
  stop("EU totals dataset is empty after filtering to ", EU_AGG_CODE, ".")
}

eu_p99_abs <- eu_total %>%
  summarise(p99_abs = quantile(abs(lp_g_log), 0.99, na.rm = TRUE)) %>%
  pull(p99_abs)

if (is.finite(eu_p99_abs) && eu_p99_abs > 0.5) {
  warning("EU LP growth values look unusually large; check whether LP1_G is in log units.")
}

# ============================================================
# 3) Load US aggregate labour productivity growth
# ============================================================

if (!file.exists(US_REAGG_PATH)) {
  stop("Missing US aggregate growth file: ", US_REAGG_PATH)
}

us_raw <- read_csv(US_REAGG_PATH, show_col_types = FALSE)

needed_us <- c("year", "LP_g_TOTIND_exgov")
missing_us <- setdiff(needed_us, names(us_raw))

if (length(missing_us) > 0) {
  stop("US aggregate file is missing required columns: ",
       paste(missing_us, collapse = ", "))
}

us_total <- us_raw %>%
  mutate(year = as.integer(year)) %>%
  transmute(
    country = "US",
    year,
    lp_g_log = as.numeric(LP_g_TOTIND_exgov)
  ) %>%
  filter(is.finite(year), is.finite(lp_g_log))

if (nrow(us_total) == 0) {
  stop("US aggregate dataset is empty after cleaning.")
}
# ============================================================
# 4) Find common overlap period
# ============================================================

range_tbl <- bind_rows(
  eu_total %>%
    group_by(country) %>%
    summarise(
      min_year = min(year),
      max_year = max(year),
      .groups = "drop"
    ),
  us_total %>%
    group_by(country) %>%
    summarise(
      min_year = min(year),
      max_year = max(year),
      .groups = "drop"
    )
) %>%
  filter(country %in% COUNTRIES)

common_start <- max(range_tbl$min_year)
common_end   <- min(range_tbl$max_year)

if (common_start >= common_end) {
  stop("No meaningful overlap across all countries. Check data coverage.")
}

cat("Common overlap period:", common_start, "-", common_end, "\n")
# ============================================================
# 5) Build aggregate labour productivity index
#    Base year = first overlap year (index = 100)
# ============================================================

# Check coverage in overlap window
expected_n_years <- common_end - common_start + 1

obs_check <- bind_rows(
  eu_total %>% select(country, year),
  us_total %>% select(country, year)
) %>%
  filter(
    country %in% COUNTRIES,
    year >= common_start,
    year <= common_end
  ) %>%
  group_by(country) %>%
  summarise(n_years = n(), .groups = "drop")

print(obs_check)

if (any(obs_check$n_years != expected_n_years)) {
  stop("Missing years detected inside overlap window.")
}



all_lp <- bind_rows(
  eu_total %>% select(country, year, lp_g_log),
  us_total %>% select(country, year, lp_g_log)
) %>%
  filter(
    country %in% COUNTRIES,
    year >= common_start,
    year <= common_end
  ) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  filter(is.finite(lp_g_log)) %>%
  mutate(
    lp_index = exp(cumsum(lp_g_log)),
    lp_index = 100 * lp_index / first(lp_index)
  ) %>%
  ungroup()

write_csv(all_lp, file.path(OUT_TAB, "country_year_lp_index_overlap.csv"))

# ============================================================
# 6) Figure 5.1: Aggregate labor productivity index
# ============================================================
plot_data <- all_lp %>%
  mutate(
    country = recode(
      country,
      "US" = "United States",
      "DE" = "Germany",
      "FR" = "France",
      "DK" = "Denmark",
      "SE" = "Sweden"
    ),
    country = factor(
      country,
      levels = c("United States", "Germany", "France", "Denmark", "Sweden")
    )
  )

p_classic <- ggplot(plot_data, aes(x = year, y = lp_index, color = country)) +
  geom_line(linewidth = 1.2,alpha = 0.9) +
  scale_color_manual(values = c(
    "United States" = "black",
    "Germany"       = "#1b9e77",
    "France"        = "#d95f02",
    "Denmark"       = "#7570b3",
    "Sweden"        = "#e7298a"
  )) +
  labs(
    x = "Year",
    y = paste0("Labor productivity index (", common_start, " = 100)"),
    color = NULL
  ) +
  theme_thesis() +
  guides(color = guide_legend(nrow = 1)) +
  theme(
    legend.position = "bottom"
  )
ggsave(
  filename = file.path(OUT_FIG, "fig_5_1_lp_index_classic.png"),
  plot = p_classic,
  width = 11,
  height = 6.2,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 7) Table 5.1: Average annual labor productivity growth by period
# ============================================================
tab_5_1 <- plot_data %>%
  mutate(
    period = ifelse(
      year <= BREAK_YEAR,
      paste0(common_start, "-", BREAK_YEAR),
      paste0(BREAK_YEAR + 1, "-", common_end)
    ),
    period = factor(
      period,
      levels = c(
        paste0(common_start, "-", BREAK_YEAR),
        paste0(BREAK_YEAR + 1, "-", common_end)
      )
    ),
    lp_growth_pp = lp_g_log * 100
  ) %>%
  group_by(country, period) %>%
  summarise(
    avg_LP_growth_pp = mean(lp_growth_pp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    avg_LP_growth_pp = round(avg_LP_growth_pp, 2)
  ) %>%
  arrange(country, period)

write_csv(
  tab_5_1,
  file.path(OUT_TAB, "table_5_1_LP_growth_periods.csv")
)

# ============================================================
# 8) Export Table 5.1 as LaTeX
# ============================================================
tab_5_1_tex <- tab_5_1 %>%
  select(Country = country, period, avg_LP_growth_pp) %>%
  mutate(
    Country = factor(
      Country,
      levels = c("United States", "Germany", "France", "Denmark", "Sweden")
    )
  ) %>%
  arrange(Country, period) %>%
  pivot_wider(
    names_from = period,
    values_from = avg_LP_growth_pp
  )

latex_code <- knitr::kable(
  tab_5_1_tex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", rep("c", ncol(tab_5_1_tex) - 1))
) %>%
  kableExtra::kable_styling(
    latex_options = "hold_position"
  )

writeLines(
  latex_code,
  file.path(OUT_TAB, "table_5_1_LP_growth_periods.tex")
)


# ============================================================
# 9) Completion message
# ============================================================
cat("Aggregate labor productivity outputs created successfully.\n")
cat("Common sample period: ", common_start, "-", common_end, "\n", sep = "")
cat("Saved:\n", sep = "")
cat("  - ", file.path(OUT_FIG, "fig_5_1_lp_index_classic.png"), "\n", sep = "")
cat("  - ", file.path(OUT_TAB, "table_5_1_LP_growth_periods.csv"), "\n", sep = "")
cat("  - ", file.path(OUT_TAB, "table_5_1_LP_growth_periods.tex"), "\n", sep = "")