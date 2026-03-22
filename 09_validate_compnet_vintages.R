# ============================================================
# 09_validate_compnet_vintages.R
#
# Purpose:
#   Validate the CompNet hybrid construction by comparing
#   V9 and V10 in overlapping countries (DE / FR / DK).
#
# Inputs (dataclean/):
#   compnet_v10_annual.csv
#   compnet_v9_overlap_DE_FR_DK.csv
#
# Outputs (dataclean/):
#   appendix_compnet_overlap_match_summary.csv
#   appendix_compnet_overlap_match_summary_by_country.csv
#   appendix_compnet_overlap_stats_by_country.csv
#   appendix_compnet_overlap_stats_overall.csv
#   appendix_compnet_overlap_outliers_HHI_rev.csv
#   fig_compnet_overlap_scatter_HHI_rev.png
#
# Main comparison variable:
#   HHI_rev
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

# -------------------------
# Paths
# -------------------------
ROOT <- "C:/Users/Simon Laubscher/OneDrive - Universität Zürich UZH/Desktop/Masterarbeit Code/Replication"

DATA_CLEAN <- file.path(ROOT, "dataclean")

V10_PATH <- file.path(DATA_CLEAN, "compnet_v10_annual.csv")
V9_PATH  <- file.path(DATA_CLEAN, "compnet_v9_overlap_DE_FR_DK.csv")

if (!file.exists(V10_PATH)) stop("Input file not found: ", V10_PATH)
if (!file.exists(V9_PATH))  stop("Input file not found: ", V9_PATH)

# -------------------------
# Helpers
# -------------------------
harmonise_types <- function(df) {
  df %>%
    mutate(
      country = as.character(country),
      year    = as.integer(year),
      NACE2   = sprintf("%02d", as.integer(NACE2))
    )
}

corr_safe <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  if (sum(ok) < 3) return(NA_real_)
  cor(a[ok], b[ok])
}

safe_write <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, path)
  message("✓ Wrote: ", path)
}

safe_save_plot <- function(p, path, w = 10, h = 6) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = path, plot = p, device = "png",
    width = w, height = h, dpi = 300, bg = "white"
  )
  message("✓ Saved figure: ", path)
}

# -------------------------
# Load data
# -------------------------
v10 <- read_csv(V10_PATH, show_col_types = FALSE) %>% harmonise_types()
v9  <- read_csv(V9_PATH,  show_col_types = FALSE) %>% harmonise_types()

stopifnot(!any(duplicated(v10[c("country", "year", "NACE2")])))
stopifnot(!any(duplicated(v9[c("country", "year", "NACE2")])))

# -------------------------
# Match V9 and V10 on common cells
# -------------------------
ov <- inner_join(
  v9 %>%
    select(country, year, NACE2, HHI_rev, rev_tot_proxy, source_vintage),
  v10 %>%
    select(country, year, NACE2, HHI_rev, rev_tot_proxy, source_vintage),
  by = c("country", "year", "NACE2"),
  suffix = c("_v9", "_v10")
)

if (nrow(ov) == 0) {
  stop("No overlapping country-year-industry cells found between V9 and V10.")
}

match_summary <- tibble(
  n_v9_rows = nrow(v9),
  n_v10_rows = nrow(v10),
  n_matched_rows = nrow(ov),
  share_v9_matched = nrow(ov) / nrow(v9),
  share_v10_matched = nrow(ov) / nrow(v10)
)

safe_write(
  match_summary,
  file.path(DATA_CLEAN, "appendix_compnet_overlap_match_summary.csv")
)

print(match_summary, n = Inf)

# Additional match summary by country
match_summary_by_country <- full_join(
  v9 %>% count(country, name = "n_v9_rows"),
  v10 %>% count(country, name = "n_v10_rows"),
  by = "country"
) %>%
  left_join(
    ov %>% count(country, name = "n_matched_rows"),
    by = "country"
  ) %>%
  mutate(
    share_v9_matched = n_matched_rows / n_v9_rows,
    share_v10_matched = n_matched_rows / n_v10_rows
  ) %>%
  arrange(country)

safe_write(
  match_summary_by_country,
  file.path(DATA_CLEAN, "appendix_compnet_overlap_match_summary_by_country.csv")
)

print(match_summary_by_country, n = Inf)

# -------------------------
# Overlap diagnostics by country
# -------------------------
overlap_stats_by_country <- ov %>%
  group_by(country) %>%
  summarise(
    n_matched = n(),
    corr_HHI_rev = corr_safe(HHI_rev_v9, HHI_rev_v10),
    mean_diff_HHI_rev = mean(HHI_rev_v10 - HHI_rev_v9, na.rm = TRUE),
    sd_diff_HHI_rev = sd(HHI_rev_v10 - HHI_rev_v9, na.rm = TRUE),
    mean_abs_diff_HHI_rev = mean(abs(HHI_rev_v10 - HHI_rev_v9), na.rm = TRUE),
    corr_rev_tot_proxy = corr_safe(rev_tot_proxy_v9, rev_tot_proxy_v10),
    .groups = "drop"
  ) %>%
  arrange(country)

safe_write(
  overlap_stats_by_country,
  file.path(DATA_CLEAN, "appendix_compnet_overlap_stats_by_country.csv")
)

print(overlap_stats_by_country, n = Inf)

# Optional overall summary across all matched cells
overlap_stats_overall <- ov %>%
  summarise(
    n_matched = n(),
    corr_HHI_rev = corr_safe(HHI_rev_v9, HHI_rev_v10),
    mean_diff_HHI_rev = mean(HHI_rev_v10 - HHI_rev_v9, na.rm = TRUE),
    sd_diff_HHI_rev = sd(HHI_rev_v10 - HHI_rev_v9, na.rm = TRUE),
    mean_abs_diff_HHI_rev = mean(abs(HHI_rev_v10 - HHI_rev_v9), na.rm = TRUE),
    corr_rev_tot_proxy = corr_safe(rev_tot_proxy_v9, rev_tot_proxy_v10)
  )

safe_write(
  overlap_stats_overall,
  file.path(DATA_CLEAN, "appendix_compnet_overlap_stats_overall.csv")
)

print(overlap_stats_overall, n = Inf)

# -------------------------
# Largest HHI differences
# -------------------------
overlap_outliers <- ov %>%
  mutate(
    diff_HHI_rev = HHI_rev_v10 - HHI_rev_v9,
    abs_diff_HHI_rev = abs(diff_HHI_rev)
  ) %>%
  arrange(desc(abs_diff_HHI_rev)) %>%
  select(
    country, year, NACE2,
    HHI_rev_v9, HHI_rev_v10,
    diff_HHI_rev, abs_diff_HHI_rev,
    rev_tot_proxy_v9, rev_tot_proxy_v10
  ) %>%
  slice_head(n = 50)

safe_write(
  overlap_outliers,
  file.path(DATA_CLEAN, "appendix_compnet_overlap_outliers_HHI_rev.csv")
)

# -------------------------
# Scatter plot: V9 vs V10
# -------------------------
plot_data <- ov %>%
  filter(is.finite(HHI_rev_v9), is.finite(HHI_rev_v10))

p_scatter <- ggplot(plot_data, aes(x = HHI_rev_v9, y = HHI_rev_v10)) +
  geom_point(alpha = 0.35) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ country, scales = "free") +
  theme_minimal(base_size = 12) +
  labs(
    title = "CompNet overlap validation: HHI_rev in V9 vs V10",
    x = "HHI_rev (V9)",
    y = "HHI_rev (V10)"
  )

safe_save_plot(
  p_scatter,
  file.path(DATA_CLEAN, "fig_compnet_overlap_scatter_HHI_rev.png"),
  w = 10, h = 6
)

message("✓ Script 09 complete: V9/V10 overlap validation finished.")