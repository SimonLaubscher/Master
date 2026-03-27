# ================================================================
# 06_klems_coverage_diagnostics
#
# Purpose:
# Provide visual diagnostics of data coverage in the EU KLEMS panel.
#
# This script:
# - checks availability of key variables
# - prints a light unit sanity check
# - produces heatmaps of valid observations by industry and country
#
# Note:
# This script is not required to reproduce the main results of the thesis.
# It is included for transparency and to document data coverage.
#
# Input:
# - data/clean/klems_panel_full_master_EUonly.rds
#
# Output:
# - output/diagnostics/heatmap_valid_LP1G.png
# - output/diagnostics/heatmap_valid_ICTcap.png
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(here)
})

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
DATA_CLEAN <- here("data", "clean")
OUT_DIR    <- here("output", "diagnostics")

IN_FILE <- file.path(DATA_CLEAN, "klems_panel_full_master_EUonly.rds")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(IN_FILE)) {
  stop("Input file not found: ", IN_FILE)
}

# ------------------------------------------------------------
# 0) Load data
# ------------------------------------------------------------
df <- readRDS(IN_FILE)

min_y <- min(df$year, na.rm = TRUE)
max_y <- max(df$year, na.rm = TRUE)

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

df <- df %>%
  mutate(
    year = as.integer(year),
    industry_clean = factor(industry_clean, levels = keep_industries)
  )

# ------------------------------------------------------------
# 1) Must-have variables check
# ------------------------------------------------------------
must_have <- c(
  "country",
  "industry_clean",
  "year",
  "LP1_G",
  "LP1_G_pp",
  "CAPICT_QI",
  "CAP_QI",
  "ICT_intensity_index"
)

missing_vars <- setdiff(must_have, names(df))

if (length(missing_vars) > 0) {
  stop(
    "Missing expected variables in input RDS: ",
    paste(missing_vars, collapse = ", "),
    "\nRun Script 05 again, or update variable names."
  )
}

# ------------------------------------------------------------
# 2) Unit sanity check
# ------------------------------------------------------------
unit_check <- df %>%
  summarise(
    med_abs_LP1_G    = median(abs(LP1_G), na.rm = TRUE),
    p99_abs_LP1_G    = quantile(abs(LP1_G), 0.99, na.rm = TRUE),
    med_abs_LP1_G_pp = median(abs(LP1_G_pp), na.rm = TRUE),
    p99_abs_LP1_G_pp = quantile(abs(LP1_G_pp), 0.99, na.rm = TRUE)
  )

cat("\n=== Unit sanity (log vs pp) ===\n")
print(unit_check)

if (is.finite(unit_check$p99_abs_LP1_G) && unit_check$p99_abs_LP1_G > 0.5) {
  warning("LP1_G p99(abs) > 0.5. Unusually large for Δln; check Script 05 conversion / outliers.")
}

# ------------------------------------------------------------
# 3) Heatmap: valid LP1_G observations
# ------------------------------------------------------------
valid_LP1G <- df %>%
  group_by(industry_clean, country) %>%
  summarise(
    n_valid_LP1G = sum(!is.na(LP1_G)),
    .groups = "drop"
  )

p_LP1G <- ggplot(valid_LP1G, aes(x = country, y = industry_clean, fill = n_valid_LP1G)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", na.value = "grey80") +
  theme_minimal() +
  labs(
    title = paste0("Valid Years: LP1_G (Δln, ", min_y, "–", max_y, ")"),
    x = "Country",
    y = "Industry (grouped codes)",
    fill = "# Valid years"
  )

ggsave(
  filename = file.path(OUT_DIR, "heatmap_valid_LP1G.png"),
  plot = p_LP1G,
  width = 10,
  height = 8
)

# ------------------------------------------------------------
# 4) Heatmap: valid ICT capital observations
# ------------------------------------------------------------
valid_ICT <- df %>%
  group_by(industry_clean, country) %>%
  summarise(
    n_valid_ICTcap = sum(!is.na(CAPICT_QI)),
    .groups = "drop"
  )

p_ICT <- ggplot(valid_ICT, aes(x = country, y = industry_clean, fill = n_valid_ICTcap)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", na.value = "grey80") +
  theme_minimal() +
  labs(
    title = paste0("Valid Years: CAPICT_QI (", min_y, "–", max_y, ")"),
    x = "Country",
    y = "Industry (grouped codes)",
    fill = "# Valid years"
  )

ggsave(
  filename = file.path(OUT_DIR, "heatmap_valid_ICTcap.png"),
  plot = p_ICT,
  width = 10,
  height = 8
)

cat(
  "\n✔ EU KLEMS coverage heatmaps created.\n",
  "Year range detected: ", min_y, "–", max_y, "\n",
  "Saved outputs to: ", normalizePath(OUT_DIR, winslash = "/"), "\n",
  sep = ""
)