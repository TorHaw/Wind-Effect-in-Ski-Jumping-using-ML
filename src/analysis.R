# ==============================================================================
# Title: Evaluation of Wind Effects in Ski Jumping using Machine Learning
# Authors: MIDN Toren Hawk, MIDN Travis Hockin, Dr. Gavin Taylor
# Description:
#   Dataset-agnostic analysis pipeline to:
#     (1) Compare 4 model families (Linear, Polynomial, GAM, Random Forest)
#         with and without wind across hill bins.
#     (2) Produce publication-quality figures (distribution, PVA, wind curves).
#     (3) Validate wind compensation factors (Linear vs RF) against official factors
#         when WindPts/HeadWindFactor/TailWindFactor/MeterValue columns exist.
#
# Outputs:
#   results/<label>/
#     figures/*.png
#     tables/*.csv
#
# Reproducibility: controlled via cfg$seed
# ==============================================================================
#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(randomForest)
  library(mgcv)
  library(glue)
  library(forcats)
  library(scales)
})

# ------------------------------------------------------------------------------
# 1) STYLE: Theme + palettes
# ------------------------------------------------------------------------------

ski_palette <- c(
  "Linear"       = "#0072B2",
  "RandomForest" = "#D55E00",
  "Polynomial"   = "#009E73",
  "GAM"          = "#CC79A7",
  "head"         = "#0072B2",
  "tail"         = "#D55E00",
  "calm"         = "grey60"
)

poster_palette <- c(
  "head"         = "#56B4E9",
  "tail"         = "#009E73",
  "calm"         = "grey60",
  "Linear"       = "#56B4E9",
  "RandomForest" = "#009E73",
  "Polynomial"   = "#E69F00",
  "GAM"          = "#CC79A7"
)

theme_ski <- function(base_size = 14, base_family = "sans", legend_pos = "bottom") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle = element_text(size = base_size, hjust = 0),
      plot.caption  = element_text(size = base_size - 2, hjust = 1, colour = "grey40"),
      
      axis.title.x  = element_text(face = "bold"),
      axis.title.y  = element_text(face = "bold"),
      axis.text     = element_text(colour = "grey20"),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
      
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      
      panel.border = element_blank(),
      axis.line    = element_line(colour = "black"),
      
      legend.position = legend_pos,
      legend.title    = element_text(face = "bold"),
      legend.key      = element_rect(fill = "white", colour = NA),
      
      strip.background = element_rect(fill = "white", colour = NA),
      strip.text       = element_text(face = "bold")
    )
}

scale_color_ski <- function(..., palette = c("paper", "poster")) {
  palette <- match.arg(palette)
  vals <- if (palette == "paper") ski_palette else poster_palette
  scale_color_manual(values = vals, ...)
}

scale_fill_ski <- function(..., palette = c("paper", "poster")) {
  palette <- match.arg(palette)
  vals <- if (palette == "paper") ski_palette else poster_palette
  scale_fill_manual(values = vals, ...)
}

# ------------------------------------------------------------------------------
# 2) UTILITIES
# ------------------------------------------------------------------------------

hill_bin <- function(HillSize_m) dplyr::case_when(
  !is.na(HillSize_m) & HillSize_m <= 49   ~ "Small (≤49)",
  !is.na(HillSize_m) & HillSize_m <= 84   ~ "Medium (50–84)",
  !is.na(HillSize_m) & HillSize_m <= 109  ~ "Normal (85–109)",
  !is.na(HillSize_m) & HillSize_m <= 184  ~ "Large (110–184)",
  !is.na(HillSize_m) & HillSize_m >= 185  ~ "Ski Flying (≥185)",
  TRUE ~ NA_character_
)

HILL_LEVELS <- c("Small (≤49)","Medium (50–84)","Normal (85–109)","Large (110–184)","Ski Flying (≥185)")

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
r2   <- function(y, yhat) 1 - sum((y - yhat)^2, na.rm = TRUE) / sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
mape <- function(y, yhat) mean(abs((y - yhat) / y), na.rm = TRUE) * 100

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

safe_write_csv <- function(df, path) {
  ensure_dir(dirname(path))
  readr::write_csv(df, path)
}

safe_ggsave <- function(path, plot, width, height, dpi = 600, type = "cairo-png") {
  ensure_dir(dirname(path))
  ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi, type = type)
}

has_cols <- function(df, cols) all(cols %in% names(df))

# 80/20 split helper
train_test_split <- function(df, prop = 0.8, seed = 2026) {
  set.seed(seed)
  n <- nrow(df)
  if (n < 10) return(list(train = df[0, ], test = df[0, ]))
  idx <- sample(seq_len(n), size = floor(prop * n))
  list(train = df[idx, ], test = df[-idx, ])
}

# RF local slope wrt WindMS via finite difference
rf_wind_slope_fd <- function(rf_model, ref_speed, ref_hs, w0, h = 0.2) {
  newd1 <- tibble(Speed_kmh = ref_speed, HillSize_m = ref_hs, WindMS = w0 - h)
  newd2 <- tibble(Speed_kmh = ref_speed, HillSize_m = ref_hs, WindMS = w0 + h)
  y1 <- predict(rf_model, newdata = newd1)
  y2 <- predict(rf_model, newdata = newd2)
  as.numeric((y2 - y1) / (2 * h))  # meters per (m/s)
}

# ------------------------------------------------------------------------------
# 3) DATA STANDARDIZATION (dataset-agnostic)
# ------------------------------------------------------------------------------

# Map input columns -> standardized names used by the pipeline.
# If a column is already standardized, you can set mapping entry to itself.
standardize_columns <- function(df, mapping) {
  # mapping is a named list: list(StandardName = "SourceName")
  # Only rename when SourceName exists and differs.
  for (std_name in names(mapping)) {
    src <- mapping[[std_name]]
    if (!is.null(src) && src %in% names(df) && std_name != src) {
      df <- df %>% dplyr::rename(!!std_name := all_of(src))
    }
  }
  df
}

load_and_preprocess <- function(cfg) {
  message(glue("[{cfg$label}] Loading: {cfg$data_file}"))
  df_raw <- readr::read_csv(cfg$data_file, show_col_types = FALSE)
  
  # Standardize columns first (so downstream is stable)
  df <- standardize_columns(df_raw, cfg$column_map)
  
  # Minimal required columns
  req_min <- c("Distance_m", "Speed_kmh", "HillSize_m", "WindMS", "Rank", "Location")
  if (!has_cols(df, req_min)) {
    missing <- setdiff(req_min, names(df))
    stop(glue("[{cfg$label}] Missing required columns after mapping: {paste(missing, collapse=', ')}"))
  }
  
  df <- df %>%
    filter(!is.na(Distance_m)) %>%
    filter(!is.na(Rank), Rank <= cfg$rank_limit) %>%
    filter(!is.na(WindMS), WindMS >= cfg$wind_clip[1], WindMS <= cfg$wind_clip[2]) %>%
    mutate(
      Location = if_else(Location == "PERTILE Sandro", "Lake Placid", Location),
      HillSize_m = as.numeric(HillSize_m),
      HillBin = factor(hill_bin(HillSize_m), levels = HILL_LEVELS)
    ) %>%
    drop_na(HillBin, Speed_kmh, HillSize_m, WindMS, Location)
  
  df
}

# ------------------------------------------------------------------------------
# 4) MODEL FITTING (4 model families; with/without wind)
# ------------------------------------------------------------------------------

fit_models <- function(train, with_wind = TRUE, seed = 2026, rf_ntree = 500) {
  set.seed(seed)
  
  if (with_wind) {
    train <- train %>% drop_na(Distance_m, Speed_kmh, WindMS, HillSize_m)
  } else {
    train <- train %>% drop_na(Distance_m, Speed_kmh, HillSize_m)
  }
  
  if (nrow(train) < 20) return(NULL)
  
  # Adaptive GAM k (matches legacy intent)
  k_speed <- max(3, min(6, length(unique(train$Speed_kmh)) - 1))
  k_wind  <- if (with_wind) max(3, min(6, length(unique(train$WindMS)) - 1)) else NA_integer_
  
  if (with_wind) {
    m_lm   <- lm(Distance_m ~ Speed_kmh + WindMS + HillSize_m, data = train)
    m_poly <- lm(Distance_m ~ poly(Speed_kmh, 2, raw = TRUE) +
                   poly(WindMS, 2, raw = TRUE) +
                   HillSize_m, data = train)
    m_gam  <- mgcv::gam(Distance_m ~ s(Speed_kmh, k = k_speed) +
                          s(WindMS, k = k_wind) +
                          HillSize_m,
                        data = train, method = "REML", select = TRUE)
    m_rf   <- randomForest::randomForest(
      Distance_m ~ Speed_kmh + WindMS + HillSize_m,
      data = train, importance = TRUE, ntree = rf_ntree
    )
  } else {
    m_lm   <- lm(Distance_m ~ Speed_kmh + HillSize_m, data = train)
    m_poly <- lm(Distance_m ~ poly(Speed_kmh, 2, raw = TRUE) + HillSize_m, data = train)
    m_gam  <- mgcv::gam(Distance_m ~ s(Speed_kmh, k = k_speed) + HillSize_m,
                        data = train, method = "REML", select = TRUE)
    m_rf   <- randomForest::randomForest(
      Distance_m ~ Speed_kmh + HillSize_m,
      data = train, importance = TRUE, ntree = rf_ntree
    )
  }
  
  list(
    Linear = m_lm,
    Polynomial = m_poly,
    GAM = m_gam,
    RandomForest = m_rf
  )
}

predict_models <- function(models, newdata) {
  stopifnot(!is.null(models))
  preds <- purrr::imap(models, ~ predict(.x, newdata = newdata))
  tibble(!!!preds)
}

fit_and_score <- function(df_bin, with_wind = TRUE, seed = 2026, rf_ntree = 500) {
  df_bin <- df_bin %>% drop_na(Distance_m, Speed_kmh, HillSize_m, WindMS)
  
  if (!with_wind) df_bin <- df_bin %>% select(-WindMS) %>% mutate(WindMS = NA_real_) # keep columns present
  
  # Re-drop appropriately below
  if (with_wind) {
    df_bin <- df_bin %>% drop_na(Distance_m, Speed_kmh, HillSize_m, WindMS)
  } else {
    df_bin <- df_bin %>% drop_na(Distance_m, Speed_kmh, HillSize_m)
  }
  
  if (nrow(df_bin) < 20) {
    return(tibble(
      Model = c("Linear","Polynomial","GAM","RandomForest"),
      WithWind = with_wind,
      RMSE = NA_real_,
      R2 = NA_real_,
      MAPE = NA_real_,
      n = nrow(df_bin)
    ))
  }
  
  tt <- train_test_split(df_bin, prop = 0.8, seed = seed)
  models <- fit_models(tt$train, with_wind = with_wind, seed = seed, rf_ntree = rf_ntree)
  if (is.null(models)) {
    return(tibble(
      Model = c("Linear","Polynomial","GAM","RandomForest"),
      WithWind = with_wind,
      RMSE = NA_real_, R2 = NA_real_, MAPE = NA_real_, n = nrow(df_bin)
    ))
  }
  
  test <- tt$test
  if (with_wind) test <- test %>% drop_na(Distance_m, Speed_kmh, HillSize_m, WindMS)
  else test <- test %>% drop_na(Distance_m, Speed_kmh, HillSize_m)
  
  pred_tbl <- predict_models(models, test)
  
  tibble(
    Model = names(pred_tbl),
    WithWind = with_wind,
    RMSE = purrr::map_dbl(pred_tbl, ~ rmse(test$Distance_m, .x)),
    R2   = purrr::map_dbl(pred_tbl, ~ r2(test$Distance_m, .x)),
    MAPE = purrr::map_dbl(pred_tbl, ~ mape(test$Distance_m, .x)),
    n = nrow(df_bin)
  ) %>% arrange(RMSE)
}

# ------------------------------------------------------------------------------
# 5) PLOTS: Distribution, Pred vs Actual, Wind Curves
# ------------------------------------------------------------------------------

plot_distribution_by_hillbin <- function(df, cfg) {
  # Match legacy: drop WindMS==0 for this plot
  counts <- df %>%
    mutate(WindSide = case_when(
      WindMS > 0 ~ "head",
      WindMS < 0 ~ "tail",
      TRUE       ~ NA_character_
    )) %>%
    drop_na(WindSide) %>%
    count(HillBin, WindSide, name = "n_jumps")
  
  ggplot(counts, aes(HillBin, n_jumps, fill = WindSide)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(
      aes(label = scales::comma(n_jumps)),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 3.8
    ) +
    scale_fill_manual(
      values = ski_palette[c("head", "tail")],
      labels = c(head = "Headwind", tail = "Tailwind"),
      name   = "Wind direction"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title    = cfg$dist_title,
      subtitle = "Number of headwind and tailwind jumps by hill size category",
      x        = "Hill size category",
      y        = ""
    ) +
    theme_ski(base_size = cfg$base_size, legend_pos = "bottom") +
    coord_cartesian(clip = "off")
}

plot_pred_vs_actual <- function(df_bin, cfg, with_wind = TRUE, title = NULL, subtitle = NULL) {
  if (with_wind) df_bin <- df_bin %>% drop_na(Distance_m, Speed_kmh, WindMS, HillSize_m)
  else df_bin <- df_bin %>% drop_na(Distance_m, Speed_kmh, HillSize_m)
  
  if (nrow(df_bin) < 20) return(NULL)
  
  tt <- train_test_split(df_bin, prop = 0.8, seed = cfg$seed)
  models <- fit_models(tt$train, with_wind = with_wind, seed = cfg$seed, rf_ntree = cfg$rf_ntree)
  if (is.null(models)) return(NULL)
  
  test <- tt$test
  if (with_wind) test <- test %>% drop_na(Distance_m, Speed_kmh, WindMS, HillSize_m)
  else test <- test %>% drop_na(Distance_m, Speed_kmh, HillSize_m)
  
  pred_tbl <- predict_models(models, test) %>% mutate(Actual = test$Distance_m)
  long <- pred_tbl %>%
    pivot_longer(cols = c(Linear, Polynomial, GAM, RandomForest),
                 names_to = "Model", values_to = "Predicted")
  
  ggplot(long, aes(Actual, Predicted, color = Model)) +
    geom_point(alpha = 0.35) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    facet_wrap(~Model, scales = "free") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Actual Distance (m)",
      y = "Predicted Distance (m)"
    ) +
    scale_color_ski(palette = cfg$palette) +
    theme_ski(base_size = cfg$base_size, legend_pos = "bottom")
}

plot_wind_effect <- function(df_bin, cfg, wind_range = NULL, title = NULL, subtitle = NULL) {
  df_bin <- df_bin %>% drop_na(Distance_m, Speed_kmh, WindMS, HillSize_m)
  if (nrow(df_bin) < 20) return(NULL)
  
  tt <- train_test_split(df_bin, prop = 0.8, seed = cfg$seed)
  train <- tt$train %>% drop_na(Distance_m, Speed_kmh, WindMS, HillSize_m)
  models <- fit_models(train, with_wind = TRUE, seed = cfg$seed, rf_ntree = cfg$rf_ntree)
  if (is.null(models)) return(NULL)
  
  med_speed <- median(train$Speed_kmh, na.rm = TRUE)
  med_hs    <- median(train$HillSize_m, na.rm = TRUE)
  
  # Wind curve range: default quantiles (legacy)
  if (is.null(wind_range)) {
    wmin <- quantile(train$WindMS, cfg$wind_curve_quantiles[1], na.rm = TRUE)
    wmax <- quantile(train$WindMS, cfg$wind_curve_quantiles[2], na.rm = TRUE)
  } else {
    wmin <- wind_range[1]; wmax <- wind_range[2]
  }
  
  grid <- tibble(
    Speed_kmh = med_speed,
    HillSize_m = med_hs,
    WindMS = seq(wmin, wmax, length.out = 200)
  )
  
  out <- predict_models(models, grid) %>%
    mutate(WindMS = grid$WindMS) %>%
    pivot_longer(cols = c(Linear, Polynomial, GAM, RandomForest),
                 names_to = "Model", values_to = "Predicted_Distance")
  
  ggplot(out, aes(WindMS, Predicted_Distance, color = Model, linetype = Model)) +
    geom_vline(xintercept = 0, linetype = 3) +
    geom_line(linewidth = 1) +
    scale_linetype_manual(values = c(
      Linear = "solid",
      Polynomial = "dashed",
      GAM = "dotdash",
      RandomForest = "twodash"
    )) +
    scale_color_ski(palette = cfg$palette) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "WindMS (+ headwind / − tailwind)",
      y = "Predicted Distance (m)"
    ) +
    theme_ski(base_size = cfg$base_size, legend_pos = "bottom")
}

# ------------------------------------------------------------------------------
# 6) WIND COMPENSATION VALIDATION MODULE (conditional)
# ------------------------------------------------------------------------------

estimate_side_factor <- function(df_train_side, model_type = c("lm","rf"), rf_ntree = 500, seed = 2026) {
  model_type <- match.arg(model_type)
  req <- c("Distance_m","Speed_kmh","WindMS","HillSize_m","MeterValue_pts_per_m")
  if (!has_cols(df_train_side, req)) return(NULL)
  
  df_train_side <- df_train_side %>% drop_na(all_of(req))
  if (nrow(df_train_side) < 20) return(NULL
                                       
  )
  
  mv <- median(df_train_side$MeterValue_pts_per_m, na.rm = TRUE)
  
  if (model_type == "lm") {
    m <- lm(Distance_m ~ Speed_kmh + HillSize_m + WindMS, data = df_train_side)
    slope <- unname(coef(m)["WindMS"])  # meters per (m/s)
  } else {
    set.seed(seed)
    m <- randomForest(
      Distance_m ~ Speed_kmh + HillSize_m + WindMS,
      data = df_train_side, importance = TRUE, ntree = rf_ntree
    )
    ref_speed <- median(df_train_side$Speed_kmh,  na.rm = TRUE)
    ref_hs    <- median(df_train_side$HillSize_m, na.rm = TRUE)
    w0        <- median(df_train_side$WindMS,     na.rm = TRUE)
    slope     <- rf_wind_slope_fd(m, ref_speed, ref_hs, w0, h = 0.2)
  }
  
  list(
    slope_m_per_ms = slope,
    pts_per_ms     = slope * mv,
    meter_value    = mv,
    model          = m
  )
}

evaluate_one_group <- function(df_group, group_label, cfg) {
  req <- c("Distance_m","Speed_kmh","WindMS","HillSize_m",
           "MeterValue_pts_per_m","WindPts","HeadWindFactor","TailWindFactor")
  df_group <- df_group %>% drop_na(all_of(req))
  
  tt <- train_test_split(df_group, prop = 0.8, seed = cfg$seed)
  train_all <- tt$train
  test_all  <- tt$test
  
  off_head <- suppressWarnings(median(train_all$HeadWindFactor, na.rm = TRUE))
  off_tail <- suppressWarnings(median(train_all$TailWindFactor, na.rm = TRUE))
  
  build_rows_for_side <- function(side_label, official_factor) {
    if (side_label == "head") {
      train_side <- train_all %>% filter(WindMS > 0)
      test_side  <- test_all  %>% filter(WindMS > 0)
    } else {
      train_side <- train_all %>% filter(WindMS < 0)
      test_side  <- test_all  %>% filter(WindMS < 0)
    }
    
    test_req <- c("Distance_m","Speed_kmh","WindMS","HillSize_m","MeterValue_pts_per_m","WindPts")
    test_side_cc <- test_side %>% drop_na(all_of(test_req))
    
    out_rows <- list()
    for (mt in c("Linear","RandomForest")) {
      mt_code <- if (mt == "Linear") "lm" else "rf"
      fit <- estimate_side_factor(train_side, mt_code, rf_ntree = cfg$rf_ntree, seed = cfg$seed)
      
      if (is.null(fit)) {
        out_rows[[mt]] <- tibble(
          Group = group_label, side = side_label, Model = mt,
          n_train_side = nrow(train_side), n_test_side = nrow(test_side_cc),
          model_m_per_ms = NA_real_,
          model_pts_per_ms = NA_real_,
          official_pts_per_ms = official_factor,
          gap_pts_per_ms = NA_real_,
          gap_pct = NA_real_,
          RMSE = NA_real_, R2 = NA_real_,
          windpts_bias_mean = NA_real_,
          windpts_mae = NA_real_,
          windpts_iqr = NA_real_
        )
      } else {
        if (nrow(test_side_cc) > 0) {
          yhat <- predict(fit$model, newdata = test_side_cc)
          m_rmse <- rmse(test_side_cc$Distance_m, yhat)
          m_r2   <- r2(test_side_cc$Distance_m, yhat)
        } else {
          m_rmse <- NA_real_; m_r2 <- NA_real_
        }
        
        if (nrow(test_side_cc) > 0 && !is.na(fit$slope_m_per_ms)) {
          delta_m_hat <- fit$slope_m_per_ms * test_side_cc$WindMS
          windpts_hat <- - delta_m_hat * test_side_cc$MeterValue_pts_per_m
          resids <- test_side_cc$WindPts - windpts_hat
          bias_mean <- mean(resids, na.rm = TRUE)
          mae       <- mean(abs(resids), na.rm = TRUE)
          iqrv      <- IQR(resids, na.rm = TRUE)
        } else {
          bias_mean <- mae <- iqrv <- NA_real_
        }
        
        gap_pts <- fit$pts_per_ms - official_factor
        gap_pct <- 100 * gap_pts / official_factor
        
        out_rows[[mt]] <- tibble(
          Group = group_label, side = side_label, Model = mt,
          n_train_side = nrow(train_side), n_test_side = nrow(test_side_cc),
          model_m_per_ms = fit$slope_m_per_ms,
          model_pts_per_ms = fit$pts_per_ms,
          official_pts_per_ms = official_factor,
          gap_pts_per_ms = gap_pts,
          gap_pct = gap_pct,
          RMSE = m_rmse, R2 = m_r2,
          windpts_bias_mean = bias_mean,
          windpts_mae = mae,
          windpts_iqr = iqrv
        )
      }
    }
    bind_rows(out_rows$Linear, out_rows$RandomForest)
  }
  
  bind_rows(
    build_rows_for_side("head", off_head),
    build_rows_for_side("tail", off_tail)
  )
}

evaluate_wind_compensation_tt <- function(df, level = c("HillBin","Location"), cfg) {
  level <- match.arg(level)
  needed <- c("Distance_m","Speed_kmh","WindMS","HillSize_m",
              "MeterValue_pts_per_m","WindPts","HeadWindFactor","TailWindFactor", level)
  if (!has_cols(df, needed)) stop(glue("Missing columns for wind-comp validation: {paste(setdiff(needed, names(df)), collapse=', ')}"))
  
  parts <- split(df, df[[level]], drop = TRUE)
  res <- purrr::imap(parts, ~ evaluate_one_group(.x, group_label = .y, cfg = cfg))
  bind_rows(res) %>%
    mutate(Group = as.character(Group)) %>%
    relocate(Group, side, Model)
}

plot_gap_by_hillbin <- function(results_bin, cfg, model = c("Linear","RandomForest"), y_lim = 220) {
  model <- match.arg(model)
  
  if (model == "RandomForest") {
    dat <- results_bin %>%
      filter(Model == "RandomForest") %>%
      mutate(abs_gap_pct = abs(gap_pct)) %>%
      group_by(Group, side) %>%
      summarise(mean_abs_gap_pct = mean(abs_gap_pct, na.rm = TRUE), .groups = "drop")
    
    ggplot(dat, aes(x = Group, y = mean_abs_gap_pct, fill = side)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = paste0(round(mean_abs_gap_pct, 1), "%")),
        position = position_dodge(width = 0.9),
        vjust = -0.3,
        size = 7
      ) +
      scale_fill_manual(
        name = "Wind direction",
        values = ski_palette[c("head","tail")],
        labels = c(head = "Headwind", tail = "Tailwind")
      ) +
      scale_y_continuous(limits = c(0, y_lim)) +
      labs(title = "Random Forest", x = "Hill size bin", y = "Percent error") +
      theme_ski(base_size = cfg$base_size, legend_pos = "bottom")
  } else {
    dat <- results_bin %>%
      filter(Model == "Linear") %>%
      mutate(abs_gap_pct = abs(gap_pct))
    
    ggplot(dat, aes(x = Group, y = abs_gap_pct, fill = side)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = paste0(round(abs_gap_pct, 1), "%")),
        position = position_dodge(width = 0.9),
        vjust = -0.3,
        size = 7
      ) +
      scale_fill_manual(
        name = "Wind direction",
        values = ski_palette[c("head","tail")],
        labels = c(head = "Headwind", tail = "Tailwind")
      ) +
      scale_y_continuous(limits = c(0, y_lim)) +
      labs(title = "Linear", x = "Hill size bin", y = "Percent error") +
      theme_ski(base_size = cfg$base_size, legend_pos = "bottom")
  }
}

plot_gap_top5_venues <- function(results_loc, df_loc, cfg, model = c("Linear","RandomForest"), y_lim = 220) {
  model <- match.arg(model)
  top5_locs <- df_loc %>%
    group_by(Location) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(n)) %>%
    slice(1:5) %>%
    pull(Location)
  
  dat <- results_loc %>%
    filter(Group %in% top5_locs, Model == model) %>%
    mutate(abs_gap_pct = abs(gap_pct))
  
  ggplot(dat, aes(x = Group, y = abs_gap_pct, fill = side)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = paste0(round(abs_gap_pct, 1), "%")),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 7
    ) +
    scale_fill_manual(
      name = "Wind direction",
      values = ski_palette[c("head","tail")],
      labels = c(head = "Headwind", tail = "Tailwind")
    ) +
    scale_y_continuous(limits = c(0, y_lim)) +
    labs(title = model, x = "Venue", y = "Percent error") +
    theme_ski(base_size = cfg$base_size, legend_pos = "bottom")
}

plot_gap_top10_venue_side <- function(results_loc, cfg, model = c("Linear","RandomForest"), y_lim = 125) {
  model <- match.arg(model)
  
  dat <- results_loc %>%
    filter(Model == model) %>%
    mutate(
      abs_gap_pct = abs(gap_pct),
      n_side = n_train_side + n_test_side,
      GroupSide = fct_reorder(paste(Group, side, sep = "___"), n_side, .desc = TRUE),
      GroupSide = fct_rev(GroupSide)
    ) %>%
    arrange(desc(n_side)) %>%
    slice_head(n = 10)
  
  ggplot(dat, aes(x = GroupSide, y = abs_gap_pct, fill = side)) +
    geom_col() +
    geom_text(aes(label = paste0(round(abs_gap_pct, 1), "%")), hjust = -0.2, size = 7) +
    scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
    scale_fill_manual(
      name = "Wind direction",
      values = ski_palette[c("head","tail")],
      labels = c(head = "Headwind", tail = "Tailwind")
    ) +
    scale_y_continuous(limits = c(0, y_lim), expand = expansion(mult = c(0, 0.15))) +
    coord_flip(clip = "off") +
    labs(title = glue("{model} - {cfg$label}"), x = "Venue", y = "Percent error") +
    theme_minimal(base_size = cfg$base_size) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
}

plot_compensation_compare <- function(results_tbl, title = "Model vs Official Wind Factors (points per m/s)") {
  req <- c("Group","side","model_pts_per_ms","official_pts_per_ms")
  stopifnot(all(req %in% names(results_tbl)))
  
  ggplot(results_tbl, aes(x = official_pts_per_ms, y = model_pts_per_ms)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    geom_point(alpha = 0.75) +
    facet_wrap(~ side) +
    labs(
      x = "Official factor (points per m/s)",
      y = "Model-based factor (points per m/s)",
      title = title
    ) +
    theme_minimal(base_size = 12)
}

# ------------------------------------------------------------------------------
# 7) CASE STUDY BY LOCATION (LM vs RF)
# ------------------------------------------------------------------------------

fit_distance_models_by_location <- function(df, location, hs_min, hs_max, cfg) {
  dat_loc <- df %>%
    filter(Location == location, HillSize_m >= hs_min, HillSize_m <= hs_max) %>%
    drop_na(Distance_m, Speed_kmh, WindMS, HillSize_m)
  
  n <- nrow(dat_loc)
  if (is.na(n) || n < 10) return(NULL)
  
  tt <- train_test_split(dat_loc, prop = 0.8, seed = cfg$seed)
  train <- tt$train
  test  <- tt$test
  
  rf <- randomForest(
    Distance_m ~ Speed_kmh + WindMS + HillSize_m,
    data = train, importance = TRUE, ntree = cfg$rf_ntree
  )
  lm_fit <- lm(Distance_m ~ Speed_kmh + WindMS + HillSize_m, data = train)
  
  test_lm <- test %>% mutate(Predicted = predict(lm_fit, newdata = .))
  test_rf <- test %>% mutate(Predicted = predict(rf, newdata = .))
  
  # error summaries
  add_err <- function(d) d %>% mutate(
    Error = Predicted - Distance_m,
    PercentError = if_else(Distance_m != 0, 100 * (Predicted - Distance_m) / Distance_m, NA_real_),
    AbsPercentError = abs(PercentError)
  )
  
  test_lm <- add_err(test_lm)
  test_rf <- add_err(test_rf)
  
  summary_tbl <- tibble(
    Location = location,
    HS_Band = glue("{hs_min}-{hs_max}"),
    Model = c("RandomForest", "Linear"),
    MeanPercentError = c(mean(test_rf$PercentError, na.rm = TRUE),
                         mean(test_lm$PercentError, na.rm = TRUE)),
    MeanAbsPercentError = c(mean(test_rf$AbsPercentError, na.rm = TRUE),
                            mean(test_lm$AbsPercentError, na.rm = TRUE)),
    RMSE = c(rmse(test_rf$Distance_m, test_rf$Predicted),
             rmse(test_lm$Distance_m, test_lm$Predicted))
  )
  
  title_suffix <- glue("{location} (HS {hs_min}-{hs_max})")
  
  p_faceted <- bind_rows(
    test_rf %>% mutate(Model = "RandomForest"),
    test_lm %>% mutate(Model = "Linear")
  ) %>%
    ggplot(aes(Distance_m, Predicted, color = Model)) +
    geom_point(alpha = 0.5, size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~Model) +
    labs(
      title = glue("Actual vs predicted — {title_suffix}"),
      subtitle = "Random Forest vs Linear (80/20 split)",
      x = "Actual Distance (m)",
      y = "Predicted Distance (m)"
    ) +
    scale_color_ski(palette = cfg$palette) +
    theme_ski(base_size = cfg$base_size, legend_pos = "bottom")
  
  list(
    performance = summary_tbl,
    plot_faceted = p_faceted
  )
}

# ------------------------------------------------------------------------------
# 8) MAIN DRIVER
# ------------------------------------------------------------------------------

execute_full_analysis <- function(cfg) {
  # Apply theme for this run
  theme_set(theme_ski(base_size = cfg$base_size, legend_pos = "bottom"))
  
  out_root <- file.path("results", cfg$label)
  fig_dir  <- file.path(out_root, "figures")
  tab_dir  <- file.path(out_root, "tables")
  ensure_dir(fig_dir)
  ensure_dir(tab_dir)
  
  df <- load_and_preprocess(cfg)
  
  # ---- A) Distribution figure ----
  p_dist <- plot_distribution_by_hillbin(df, cfg)
  safe_ggsave(file.path(fig_dir, "hill_bin_counts.png"), p_dist, width = 16, height = 12, dpi = cfg$dpi)
  
  # ---- B) Performance tables (with and without wind) ----
  perf_with <- df %>%
    group_by(HillBin) %>%
    group_modify(~ fit_and_score(.x, with_wind = TRUE, seed = cfg$seed, rf_ntree = cfg$rf_ntree)) %>%
    ungroup() %>%
    mutate(HillBin = as.character(HillBin)) %>%
    relocate(HillBin, Model, WithWind, RMSE, R2, MAPE, n)
  
  perf_now <- df %>%
    group_by(HillBin) %>%
    group_modify(~ fit_and_score(.x, with_wind = FALSE, seed = cfg$seed, rf_ntree = cfg$rf_ntree)) %>%
    ungroup() %>%
    mutate(HillBin = as.character(HillBin)) %>%
    relocate(HillBin, Model, WithWind, RMSE, R2, MAPE, n)
  
  safe_write_csv(perf_with, file.path(tab_dir, "model_performance_by_hillbin_withwind.csv"))
  safe_write_csv(perf_now,  file.path(tab_dir, "model_performance_by_hillbin_nowind.csv"))
  
  # ---- C) Core figures for selected bins ----
  for (bin in cfg$bins_to_plot) {
    df_bin <- df %>% filter(HillBin == bin)
    
    # PVA with wind
    p_pva_w <- plot_pred_vs_actual(
      df_bin, cfg, with_wind = TRUE,
      title = glue("Predicted vs Actual ({cfg$label}) — {bin}"),
      subtitle = "Models include WindMS"
    )
    if (!is.null(p_pva_w)) {
      safe_ggsave(file.path(fig_dir, glue("{cfg$tag_map[[bin]]}_pred_v_act.png")),
                  p_pva_w, width = 16, height = 12, dpi = cfg$dpi)
    }
    
    # Wind effect curves (4 models)
    p_wind <- plot_wind_effect(
      df_bin, cfg, wind_range = cfg$wind_curve_fixed_range,
      title = glue("Estimated Wind Effect ({cfg$label}) — {bin}"),
      subtitle = "Speed_kmh and HillSize_m held at medians (train split)"
    )
    if (!is.null(p_wind)) {
      safe_ggsave(file.path(fig_dir, glue("{cfg$tag_map[[bin]]}_wind_effect.png")),
                  p_wind, width = 16, height = 12, dpi = cfg$dpi)
    }
  }
  
  # Optional: Large no-wind PVA (legacy sensitivity)
  if ("Large (110–184)" %in% cfg$bins_to_plot) {
    df_large <- df %>% filter(HillBin == "Large (110–184)")
    p_pva_nw <- plot_pred_vs_actual(
      df_large, cfg, with_wind = FALSE,
      title = glue("Predicted vs Actual ({cfg$label}) — Large (110–184)"),
      subtitle = "Models exclude WindMS"
    )
    if (!is.null(p_pva_nw)) {
      safe_ggsave(file.path(fig_dir, "large_pred_v_act_nowind.png"),
                  p_pva_nw, width = 16, height = 12, dpi = cfg$dpi)
    }
  }
  
  # ---- D) Case studies by location (optional, configurable) ----
  if (length(cfg$case_studies) > 0) {
    case_rows <- list()
    for (cs in cfg$case_studies) {
      res <- fit_distance_models_by_location(df, cs$location, cs$hs_min, cs$hs_max, cfg)
      if (is.null(res)) next
      
      case_rows[[cs$location]] <- res$performance
      safe_ggsave(
        file.path(fig_dir, glue("case_study_{str_replace_all(tolower(cs$location), '[^a-z0-9]+', '_')}.png")),
        res$plot_faceted, width = 16, height = 12, dpi = cfg$dpi
      )
    }
    if (length(case_rows) > 0) {
      safe_write_csv(bind_rows(case_rows), file.path(tab_dir, "case_study_performance.csv"))
    }
  }
  
  # ---- E) Wind compensation validation (conditional) ----
  req_windcomp <- c("MeterValue_pts_per_m","WindPts","HeadWindFactor","TailWindFactor")
  if (has_cols(df, req_windcomp)) {
    message(glue("[{cfg$label}] Wind compensation module: running (columns present)."))
    
    # By HillBin
    results_bin <- evaluate_wind_compensation_tt(df, level = "HillBin", cfg = cfg) %>%
      mutate(abs_gap_pct = abs(gap_pct)) %>%
      arrange(abs_gap_pct)
    safe_write_csv(results_bin, file.path(tab_dir, "windcomp_by_hillbin.csv"))
    
    p_lin_bin <- plot_gap_by_hillbin(results_bin, cfg, model = "Linear")
    p_rf_bin  <- plot_gap_by_hillbin(results_bin, cfg, model = "RandomForest")
    safe_ggsave(file.path(fig_dir, "windcomp_linear_gap_pct_hillbin.png"),  p_lin_bin, width = 16, height = 12, dpi = cfg$dpi)
    safe_ggsave(file.path(fig_dir, "windcomp_rf_gap_pct_hillbin.png"),      p_rf_bin,  width = 16, height = 12, dpi = cfg$dpi)
    
    # By Location (data-rich venues)
    df_loc <- df %>%
      drop_na(Distance_m, Speed_kmh, WindMS, HillSize_m, MeterValue_pts_per_m) %>%
      add_count(Location, name = "n_loc") %>%
      filter(n_loc >= cfg$min_n_per_venue)
    
    results_loc <- evaluate_wind_compensation_tt(df_loc, level = "Location", cfg = cfg) %>%
      mutate(abs_gap_pct = abs(gap_pct)) %>%
      arrange(abs_gap_pct)
    safe_write_csv(results_loc, file.path(tab_dir, "windcomp_by_location.csv"))
    
    # Top5 venues plots
    p_lin_venue <- plot_gap_top5_venues(results_loc, df_loc, cfg, model = "Linear")
    p_rf_venue  <- plot_gap_top5_venues(results_loc, df_loc, cfg, model = "RandomForest")
    safe_ggsave(file.path(fig_dir, "windcomp_linear_gap_pct_venue_top5.png"), p_lin_venue, width = 16, height = 12, dpi = cfg$dpi)
    safe_ggsave(file.path(fig_dir, "windcomp_rf_gap_pct_venue_top5.png"),     p_rf_venue,  width = 16, height = 12, dpi = cfg$dpi)
    
    # Top10 venue-side plots
    p_top10_lin <- plot_gap_top10_venue_side(results_loc, cfg, model = "Linear")
    p_top10_rf  <- plot_gap_top10_venue_side(results_loc, cfg, model = "RandomForest")
    safe_ggsave(file.path(fig_dir, "windcomp_linear_gap_pct_venue_top10_side.png"), p_top10_lin, width = 12, height = 8, dpi = cfg$dpi)
    safe_ggsave(file.path(fig_dir, "windcomp_rf_gap_pct_venue_top10_side.png"),     p_top10_rf,  width = 12, height = 8, dpi = cfg$dpi)
    
    # Optional compare scatter
    p_cmp_bin <- plot_compensation_compare(results_bin, "Bins: Model vs Official")
    p_cmp_loc <- plot_compensation_compare(results_loc, "Venues: Model vs Official")
    safe_ggsave(file.path(fig_dir, "windcomp_compare_scatter_bins.png"),  p_cmp_bin, width = 12, height = 8, dpi = cfg$dpi)
    safe_ggsave(file.path(fig_dir, "windcomp_compare_scatter_venues.png"),p_cmp_loc, width = 12, height = 8, dpi = cfg$dpi)
    
  } else {
    message(glue("[{cfg$label}] Wind compensation module: skipped (missing one of: {paste(req_windcomp, collapse=', ')})."))
  }
  
  message(glue("[{cfg$label}] Done. Outputs in {out_root}"))
}

# ------------------------------------------------------------------------------
# 9) RUN CONFIGS (edit paths as needed)
# ------------------------------------------------------------------------------

# Note: If your datasets already use these standardized names, leave mapping as identity.
# If they differ, set column_map entries accordingly.

cfg_female <- list(
  label = "female",
  data_file = file.path("data", "processed", "female_WC_21-25.csv"),
  rank_limit = 20,
  
  # dataset-agnostic knobs
  seed = 2026,
  rf_ntree = 500,
  wind_clip = c(-4, 4),
  
  # wind curve default: quantile-based (legacy)
  wind_curve_quantiles = c(0.02, 0.98),
  # Optional override: set to c(-4,4) to force fixed range; leave NULL to use quantiles
  wind_curve_fixed_range = NULL,
  
  # styling
  palette = "paper",   # "paper" or "poster"
  base_size = 14,
  dpi = 600,
  
  # narrative distribution title
  dist_title = "Most female athlete jumps occur on normal and large hills",
  
  # bins to render figures for (legacy core)
  bins_to_plot = c("Normal (85–109)", "Large (110–184)", "Ski Flying (≥185)"),
  tag_map = list(
    "Normal (85–109)" = "norm",
    "Large (110–184)" = "large",
    "Ski Flying (≥185)" = "sf"
  ),
  
  # optional case studies (can add more)
  case_studies = list(
    list(location = "Hinzenbach", hs_min = 85, hs_max = 109)
  ),
  
  # wind-comp module (venues)
  min_n_per_venue = 150,
  
  # Column mapping: StandardName = "SourceName"
  # If a source dataset uses different names, change the values here.
  column_map = list(
    Distance_m = "Distance_m",
    Speed_kmh = "Speed_kmh",
    HillSize_m = "HillSize_m",
    WindMS = "WindMS",
    Rank = "Rank",
    Location = "Location",
    MeterValue_pts_per_m = "MeterValue_pts_per_m",
    WindPts = "WindPts",
    HeadWindFactor = "HeadWindFactor",
    TailWindFactor = "TailWindFactor"
  )
)

cfg_male <- list(
  label = "male",
  data_file = file.path("data", "processed", "male_WC_21-25.csv"),
  rank_limit = 30,
  
  seed = 2026,
  rf_ntree = 500,
  wind_clip = c(-4, 4),
  
  wind_curve_quantiles = c(0.02, 0.98),
  wind_curve_fixed_range = NULL,
  
  palette = "paper",
  base_size = 14,
  dpi = 600,
  
  dist_title = "Most male athlete jumps occur on large hills",
  
  bins_to_plot = c("Normal (85–109)", "Large (110–184)", "Ski Flying (≥185)"),
  tag_map = list(
    "Normal (85–109)" = "norm",
    "Large (110–184)" = "large",
    "Ski Flying (≥185)" = "sf"
  ),
  
  case_studies = list(
    list(location = "Engelberg", hs_min = 110, hs_max = 184)
  ),
  
  min_n_per_venue = 150,
  
  column_map = list(
    Distance_m = "Distance_m",
    Speed_kmh = "Speed_kmh",
    HillSize_m = "HillSize_m",
    WindMS = "WindMS",
    Rank = "Rank",
    Location = "Location",
    MeterValue_pts_per_m = "MeterValue_pts_per_m",
    WindPts = "WindPts",
    HeadWindFactor = "HeadWindFactor",
    TailWindFactor = "TailWindFactor"
  )
)

# ------------------------------------------------------------------------------
# 10) EXECUTE
# ------------------------------------------------------------------------------

# Female
execute_full_analysis(cfg_female)

# Male
execute_full_analysis(cfg_male)
