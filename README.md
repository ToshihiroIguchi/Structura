
# Structura

**Structural Insights, Simplified.**
<br>
<img src="www/logo.png" width="12.5%" />

## Description

Structura is an interactive Shiny application for Structural Equation Modeling (SEM) in **R** ([r-project.org](https://www.r-project.org/?utm_source=chatgpt.com)), making it easy to upload data, specify models, and visualize results in a unified interface. It leverages the **lavaan** package for comprehensive latent variable analysis ([cran.r-project.org](https://cran.r-project.org/package%3Dlavaan?utm_source=chatgpt.com)).

## Features

* **Data Upload & Inspection**: Upload CSV files with automatic encoding handling via **readflex**.
* **Log-transform**: Apply common logarithm (log10) to positive numeric columns.
* **One-hot Encoding**: Convert categorical variables to dummy indicators for SEM compatibility.
* **Model Specification**: Define measurement (`Latent =~ Indicators`) and structural (`Dependent ~ Predictors`) models in interactive Handsontable grids powered by **rhandsontable** and **DT** ([cran.r-project.org](https://cran.r-project.org/package%3Dshiny?utm_source=chatgpt.com), [shiny.posit.co](https://shiny.posit.co/?utm_source=chatgpt.com)).
* **SEM Fitting**: Fit models using **lavaan** with support for mean structures and detailed fit measures.
* **Visualization**: Render path diagrams via **DiagrammeR**/**semDiagram**, and generate plots using **ggplot2** ([cran.r-project.org](https://cran.r-project.org/package%3Dggplot2?utm_source=chatgpt.com)).
* **Comprehensive Reporting**: View fit indices (p-value, SRMR, RMSEA, AIC, BIC, GFI, AGFI, NFI, CFI), parameter tables, and formatted equations in real time.

## Installation

Install required packages from github and CRAN:

```r
devtools::install_github("ToshihiroIguchi/semDiagram", dependencies = TRUE, upgrade = "never", build = FALSE, build_vignettes = FALSE)
devtools::install_github("ToshihiroIguchi/readflex", dependencies = TRUE, upgrade = "never", build = FALSE, build_vignettes = FALSE)
install.packages(c("shiny", "shinyjs", "DT", "rhandsontable", "lavaan", "DiagrammeR", "ggplot2", "reshape2", "markdown"))
```

## Launch Application

From your R console, run:

```r
shiny::runGitHub("Structura", "ToshihiroIguchi", ref = "main")
```

## Hosting

Host the Shiny application from GitHub in a private network.
Enter the following command in R console.

    #Port specification
    port <- 8100

    #Acquire private address information
    ipconfig.dat <- system("ipconfig", intern = TRUE)
    ipv4.dat <- ipconfig.dat[grep("IPv4", ipconfig.dat)][1]
    ip <- gsub(".*? ([[:digit:]])", "\\1", ipv4.dat)

    #Host the Shiny application from GitHub
    shiny::runGitHub("Structura", "ToshihiroIguchi", launch.browser = FALSE, port = port, host = ip, ref = "main")

If you are in the private network, you can also launch the Shiny application by entering the URL following `Listing on` to the browser.

### Docker

Build and run with Docker:

```bash
docker build -t structura .
docker run --rm -p 3838:3838 structura
```

## License

Released under the **MIT License** © 2025 Toshihiro Iguchi.

## Author

**Toshihiro Iguchi**
