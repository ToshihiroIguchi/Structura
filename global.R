# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – global.R
#   * Load libraries
#   * Define global options / helpers
#   * Source helper functions
# ---------------------------------------------------------------

options(
  shiny.fullstacktrace = TRUE,
  shiny.reactlog       = TRUE,
  shiny.sanitize.errors = TRUE
)

# ---- Libraries --------------------------------------------------
library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)
library(readflex)
library(lavaan)
library(DiagrammeR)
library(semDiagram)
library(ggplot2)
library(reshape2)
library(markdown)

# ---- Global helpers --------------------------------------------
`%||%` <- function(x, y) if (!is.null(x)) x else y
Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8")

# ---- Load shared functions -------------------------------------
source("helpers.R", local = TRUE)
