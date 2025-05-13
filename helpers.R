# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – helpers.R
#   * Utility functions used by both ui.R and server.R
# ---------------------------------------------------------------

# ---- lavaan_to_equations ---------------------------------------
#   * Indicator  =  intercept + loading * Latent
#   * Dependent  =  intercept + Σ( slope * Predictor )
#   * 全て Raw (非標準化) 係数で生成
lavaan_to_equations <- function(fit, digits = 3) {

  # 係数取得（標準化しない）
  pe <- parameterEstimates(fit, standardized = FALSE, remove.def = TRUE)

  # 数値フォーマッタ
  format_est <- function(x, digits = 3) {
    sapply(x, function(v) {
      if (is.na(v)) return("NA")
      if (abs(v) < 10^(-digits))
        format(v, digits = digits, scientific = TRUE)
      else
        format(round(v, digits), nsmall = digits)
    })
  }

  # データフレーム分割
  meas_df      <- pe[pe$op == "=~",  ]   # 測定方程式
  reg_df       <- pe[pe$op == "~",   ]   # 構造方程式
  intercept_df <- pe[pe$op == "~1",  ]   # 切片

  eq_lines <- character(0)

  # 1. 測定方程式
  if (nrow(meas_df)) {
    for (i in seq_len(nrow(meas_df))) {
      ind     <- meas_df$rhs[i]
      lat     <- meas_df$lhs[i]
      loading <- format_est(meas_df$est[i], digits)
      int_val <- intercept_df$est[intercept_df$lhs == ind]
      rhs     <- c(if (length(int_val)) format_est(int_val, digits) else NULL,
                   paste0(loading, "*", lat))
      eq_lines <- c(eq_lines, paste(ind, "=", paste(rhs, collapse = " + ")))
    }
  }

  # 2. 構造方程式
  if (nrow(reg_df)) {
    reg_split <- split(reg_df, reg_df$lhs)
    for (lhs in names(reg_split)) {
      df  <- reg_split[[lhs]]
      int <- intercept_df$est[intercept_df$lhs == lhs]
      rhs <- paste0(format_est(df$est, digits), "*", df$rhs)
      rhs <- c(if (length(int)) format_est(int, digits) else NULL, rhs)
      eq_lines <- c(eq_lines, paste(lhs, "=", paste(rhs, collapse = " + ")))
    }
  }

  eq_lines
}

# ---- File loader ------------------------------------------------
loadDataOnce <- function(fileInput) {
  req(fileInput)
  readflex(fileInput$datapath, stringsAsFactors = FALSE)
}
