<img src="www/logo.png" width="25%" />

# Structura: Structural Insights, Simplified.

## Overview

Structura is an interactive application built in **R** using **lavaan** for Structural Equation Modeling (SEM). With an intuitive interface and enhanced commenting, Structura enables users to:

* **Upload and inspect** datasets without worrying about file encoding.  
* **Log-transform** positive numeric variables (common logarithm, log10).  
* **One-hot encode** categorical variables for SEM compatibility.  
* **Specify measurement and structural models** through an interactive table-based UI.  
* **Choose analysis options** (standardized vs. raw, *Missing Data Handling*) in a single control pane.  
* **Fit SEM models** with support for mean structures.  
* **Visualize results** through fit indices, formatted equations, parameter tables, and interactive path diagrams.

## User Interface Guide

### 1. Data Tab
* **Upload CSV** – Browse for a CSV file and explore it in a filterable table.

### 2. Filtered Tab
* **Log-transform columns (log10)** – Select positive numeric columns to apply a common logarithm.  
* **Display columns** – Choose variables to include in downstream analyses.  
* **Filtered Data** – Preview the transformed dataset.  
* **Correlation Heatmap** – Inspect pairwise correlations for selected variables.

### 3. Model Tab
* **Analysis Options**  
  * Raw vs. Standardized estimation mode.  
  * **Missing Data Handling**: `listwise`, `ml`, `ml.x`, `two.stage`, or `robust.two.stage`.  
* **Measurement Model / Structural Model** – Build your SEM via interactive tables with check-boxes.  
* **lavaan Syntax** – Generated automatically from the tables for transparency.  
* **Path Diagram Options**  
  * **Layout & Engine** selector:  
    `Hierarchical Left → Right (dot)`,  
    `Hierarchical Top → Bottom (dot)`,  
    `Spring model (neato)`,  
    `Force-Directed (fdp)`,  
    `Circular (circo)`,  
    `Radial (twopi)`.  
  * Diagram pane supports **vertical scrolling** if the graph exceeds the viewport.  

## Fit Indices

In the **Model** tab, the **Fit Indices** table presents key metrics for model adequacy:

* **p-value** – Tests exact fit; ≥ 0.05 suggests an acceptable model.  
* **SRMR** – ≤ 0.08 indicates good fit.  
* **RMSEA** – ≤ 0.06 denotes close fit.  
* **AIC / BIC** – Lower values imply a more parsimonious model.  
* **GFI / AGFI** – Proportion of variance explained; aim ≥ 0.90.  
* **NFI / CFI** – Incremental fit indices; ≥ 0.90 considered acceptable.

Values outside recommended thresholds are highlighted in **red**. Always consider multiple indices together.

## Details Tab

* **Parameter Estimates** – Table of estimates, standard errors, z-values, and p-values.  
* **Model Summary** – Comprehensive text summary with additional fit measures.

## Tips & Best Practices

* Verify that columns chosen for log-transform contain only positive values.  
* Use clear latent variable names (e.g., no spaces; camelCase or snake_case).  
* Evaluate several fit indices for a holistic view of model performance.  
* Compare AIC/BIC across nested models to select the most parsimonious specification.  
* Experiment with different **Layout & Engine** settings to improve diagram readability.

---
