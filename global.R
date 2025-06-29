# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura – global.R
#   * Load libraries
#   * Apply environment configuration
#   * Source helper functions
# ---------------------------------------------------------------

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

# ---- Configuration System --------------------------------------
source("config/app_config.R", local = TRUE)
app_config <- apply_shiny_config()

# ---- Global helpers --------------------------------------------
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- Load shared functions -------------------------------------
source("helpers.R", local = TRUE)
source("utils/error_handler.R", local = TRUE)
