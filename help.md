<img src="www/logo.png" width="25%" />

# Structura: Structural Insights, Simplified.

## Overview

Structura is an interactive application built in **R** using **lavaan** for Structural Equation Modeling (SEM). With an intuitive interface and enhanced commenting, Structura enables users to:

* **Upload and inspect** datasets without worrying about file encoding.
* **Log-transform** positive numeric variables (common logarithm, log10).
* **One-hot encode** categorical variables for SEM compatibility.
* **Specify measurement and structural models** through an interactive table-based UI.
* **Fit SEM models** with support for mean structures.
* **Visualize results** through fit indices, formatted equations, parameter tables, and path diagrams.

## User Interface Guide

### 1. Data Tab

* **Upload CSV**: Select or browse for your CSV file and inspect it in a filterable table.

### 2. Filtered Tab

* **Log-transform columns (log10)**: Choose positive numeric columns to apply a common logarithm.
* **Display columns**: Pick which variables to include in tables and analyses.
* **Filtered Data**: Preview the transformed dataset.
* **Correlation Matrix**: View pairwise correlations for selected variables.

## Fit Indices

In the **Model** tab, the **Fit Indices** table presents multiple metrics to evaluate model adequacy:

* **p‑value**: Tests the null hypothesis that the model fits as well as the saturated model. A p‑value ≥ 0.05 suggests acceptable fit.
* **SRMR** (Standardized Root Mean Square Residual): Average discrepancy between observed and predicted correlations. Target ≤ 0.08 for good fit.
* **RMSEA** (Root Mean Square Error of Approximation): Penalizes model complexity; values ≤ 0.06 indicate close fit, with 90% CI displayed in detailed summaries.
* **AIC** (Akaike Information Criterion) & **BIC** (Bayesian Information Criterion): Information‐theoretic indices where lower values denote more parsimonious models.
* **GFI** (Goodness‑of‑Fit Index) & **AGFI** (Adjusted GFI): Proportion of variance explained by the model, adjusted for degrees of freedom. Aim for values ≥ 0.90.
* **NFI** (Normed Fit Index) & **CFI** (Comparative Fit Index): Incremental fit indices comparing your model against a null baseline; values ≥ 0.90 indicate acceptable fit.

Metrics breaching recommended thresholds (e.g., SRMR > 0.08) are highlighted in red to draw attention. Consider all fit indices jointly rather than relying on a single statistic when assessing your SEM.

## Details Tab

* **Model Summary**: Comprehensive summary with fit measures and parameter details.
* **Parameter Estimates**: Table of estimates, standard errors, z‑values, and p‑values.

## Tips & Best Practices

* Ensure variables selected for log‑transform contain only positive values.
* Use clear latent variable names (e.g., no spaces; underscores or camelCase recommended).
* Evaluate multiple fit indices to get a holistic view of model performance.
* Compare AIC/BIC across nested models to select the most parsimonious specification.

---

For further assistance, consult the `README.md` on GitHub or reach out to the Structura development team.
