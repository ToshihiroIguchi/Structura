# global.R
library(shiny)
library(shinyjs)
library(DT)
library(lavaan)
library(readflex)

# データ読み込み関数
loadDataOnce <- function(file) {
  df <- readflex(file$datapath, stringsAsFactors = TRUE)
  # 変数名を安全な形式に変換
  names(df) <- make.names(names(df), unique = TRUE)
  return(df)
}

