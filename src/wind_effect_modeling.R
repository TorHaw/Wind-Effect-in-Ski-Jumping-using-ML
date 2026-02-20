# ==============================================================================
# Title: Evaluation of Wind Effects in Ski Jumping using Machine Learning
# Authors: MIDN Toren Hawk, MIDN Travis Hockin,  Dr. Gavin Taylor
# Description: This script implements the data-driven framework to validate 
#              longitudinal wind compensation. It compares Linear, Polynomial, 
#              GAM, and Random Forest models across male and female datasets.
# ==============================================================================

# 1. ENVIRONMENT SETUP ---------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(mgcv)
library(glue)
library(forcats)

# Global Publication Theme for Graphics
ski_palette <- c(
  "Linear"       = "#0072B2", "RandomForest" = "#D55E00",
  "Polynomial"   = "#009E73", "GAM"          = "#CC79A7",
  "head"         = "#0072B2", "tail"         = "#D55E00"
)

# Color / fill scales for ggplot2
scale_color_ski <- function(..., palette = "paper") {
  vals <- if (palette == "paper") ski_palette else poster_palette
  scale_color_manual(values = vals, ...)
}

scale_fill_ski <- function(..., palette = "paper") {
  vals <- if (palette == "paper") ski_palette else poster_palette
  scale_fill_manual(values = vals, ...)
}

theme_ski <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle     = element_text(size = base_size, hjust = 0),
      axis.title.x      = element_text(face = "bold"),
      axis.title.y      = element_text(face = "bold"),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      axis.line         = element_line(colour = "black"),
      legend.position   = "bottom",
      strip.background  = element_rect(fill = "white", colour = NA),
      strip.text        = element_text(face = "bold", size = base_size + 4)
    )
}

# 2. UTILITY & ANALYTICAL FUNCTIONS --------------------------------------------

# Categorization of hill size based on FIS standards
get_hill_bin <- function(HS) {
  case_when(
    HS <= 49   ~ "Small (≤49)",
    HS <= 84   ~ "Medium (50–84)",
    HS <= 109  ~ "Normal (85–109)",
    HS <= 184  ~ "Large (110–184)",
    HS >= 185  ~ "Ski Flying (≥185)",
    TRUE       ~ NA_character_
  )
}

# Standard Statistical Metrics
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
r2   <- function(y, yhat) 1 - sum((y - yhat)^2, na.rm = TRUE) / sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
mape <- function(y, yhat) mean(abs((y - yhat) / y), na.rm = TRUE) * 100

# Finite-difference derivative to estimate RF marginal wind effect
rf_wind_slope <- function(rf_model, ref_speed, ref_hs, w0, h = 0.2) {
  d1 <- tibble(Speed_kmh = ref_speed, HillSize_m = ref_hs, WindMS = w0 - h)
  d2 <- tibble(Speed_kmh = ref_speed, HillSize_m = ref_hs, WindMS = w0 + h)
  (predict(rf_model, d2) - predict(rf_model, d1)) / (2 * h)
}

# 3. CORE PLOTTING ENGINE ------------------------------------------------------

# Predicted vs Actual Scatter Plot
plot_p_vs_a <- function(df, with_wind, title, out_path) {
  set.seed(2026)
  idx <- sample(seq_len(nrow(df)), 0.8 * nrow(df))
  train <- df[idx, ]; test <- df[-idx, ]
  
  # Model Fitting
  f <- if(with_wind) "Distance_m ~ Speed_kmh + WindMS + HillSize_m" else "Distance_m ~ Speed_kmh + HillSize_m"
  m_lm <- lm(as.formula(f), train)
  m_rf <- randomForest(as.formula(f), train)
  
  res <- tibble(
    Actual = test$Distance_m,
    Linear = predict(m_lm, test),
    RandomForest = predict(m_rf, test)
  ) %>% pivot_longer(-Actual, names_to = "Model", values_to = "Predicted")
  
  p <- ggplot(res, aes(Actual, Predicted, color = Model)) +
    geom_point(alpha = 0.35) + geom_abline(slope = 1, intercept = 0, linetype = 2) +
    facet_wrap(~Model) + labs(title = title, x = "Actual (m)", y = "Predicted (m)") +
    scale_color_ski() + theme_ski() + theme(axis.title.y = element_text(angle = 90))
  
  ggsave(out_path, plot = p, width = 16, height = 12, dpi = 600, type = "cairo-png")
}

# Wind Response Curves (holding other predictors at median)
plot_wind_curves <- function(df, title, out_path) {
  set.seed(2026)
  m_lm <- lm(Distance_m ~ Speed_kmh + WindMS + HillSize_m, df)
  m_rf <- randomForest(Distance_m ~ Speed_kmh + WindMS + HillSize_m, df)
  
  grid <- tibble(
    WindMS = seq(-4, 4, length.out = 200),
    Speed_kmh = median(df$Speed_kmh, na.rm = TRUE),
    HillSize_m = median(df$HillSize_m, na.rm = TRUE)
  )
  grid$Linear <- predict(m_lm, grid); grid$RandomForest <- predict(m_rf, grid)
  
  p <- grid %>% pivot_longer(c(Linear, RandomForest), names_to = "Model", values_to = "Dist") %>%
    ggplot(aes(WindMS, Dist, color = Model, linetype = Model)) +
    geom_vline(xintercept = 0, linetype = 3) + geom_line(linewidth = 1.2) +
    scale_color_ski() + theme_ski() + labs(title = title, x = "WindMS (m/s)", y = "Predicted Distance (m)")
  
  ggsave(out_path, plot = p, width = 16, height = 12, dpi = 600, type = "cairo-png")
}

# 4. MASTER ANALYSIS WRAPPER ---------------------------------------------------

execute_full_analysis <- function(data_file, gender_label, rank_limit) {
  message(glue("Processing {gender_label} dataset..."))
  out_dir <- glue("results/{gender_label}")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Preprocessing
  raw_data <- read_csv(data_file) %>%
    filter(!is.na(Distance_m), Rank <= rank_limit, WindMS >= -4, WindMS <= 4) %>%
    mutate(
      Location = if_else(Location == "PERTILE Sandro", "Lake Placid", Location),
      HillBin = factor(get_hill_bin(HillSize_m), levels = c("Small (≤49)", "Medium (50–84)", "Normal (85–109)", "Large (110–184)", "Ski Flying (≥185)"))
    ) %>% drop_na(HillBin, Speed_kmh, WindMS)
  
  # Figure 1: Distribution Analysis
  counts <- raw_data %>% mutate(Side = if_else(WindMS > 0, "head", "tail")) %>% count(HillBin, Side)
  p_dist <- ggplot(counts, aes(HillBin, n, fill = Side)) + geom_col(position = "dodge") +
    geom_text(aes(label = n), position = position_dodge(0.9), vjust = -0.5, size = 7) +
    scale_fill_manual(values = ski_palette) + theme_ski() + labs(title = glue("Jump Distribution: {gender_label}"))
  ggsave(glue("{out_dir}/distribution.png"), p_dist, width = 16, height = 12, dpi = 600)
  
  # Model Visualization for Normal and Large Hill categories
  for (target_bin in c("Normal (85–109)", "Large (110–184)")) {
    bin_data <- raw_data %>% filter(HillBin == target_bin)
    tag <- str_replace_all(tolower(target_bin), "[^a-z]", "_")
    
    plot_p_vs_a(bin_data, TRUE, glue("{target_bin}: Predicted vs Actual"), glue("{out_dir}/{tag}_accuracy.png"))
    plot_wind_curves(bin_data, glue("{target_bin}: Wind Response Curve"), glue("{out_dir}/{tag}_curves.png"))
  }
  
  message(glue("Analysis complete for {gender_label}. Results saved to {out_dir}."))
}

# 5. EXECUTION -----------------------------------------------------------------

# Female Study: Rank limit top 20
execute_full_analysis("data/female_WC_21-25.csv", "female", 20)

# Male Study: Rank limit top 30
execute_full_analysis("data/male_WC_21-25.csv", "male", 30)