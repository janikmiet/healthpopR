
<!-- README.md is generated from README.Rmd. Please edit that file -->

# heathpopR <a href="https://janikmiet.github.io/healthpopR/"><img src="man/figures/logo.png" align="right" height="138" alt="healthpopR website" /></a>

## Overview

**healthpopR** helps researchers and analysts classify populations into
exposure and response groups, visualize ICD-10-based health profiles,
and perform survival and Cox regression analysis using tidy workflows.

The package is designed for analyzing how exposure diagnoses affect
response diagnoses in population health data research. Package uses
regex code for identifying ICD-10/9/8 codes.

The goal of healthpopR is to help analyze the connection between
exposure diagnoses and response diagnoses. A real-life example would be
investigating how a diagnosis of diabetes influences the risk of
developing heart failure.

## Limitations

Currently functions are developed so that they fit our specific dataset
correctly. Which means that analyses are limited to only one gender and
follow up starts at when population is 50 years old. Theses limitations
can be removed later, when we use same functions to another datasets.

## Installation

Install the development version from GitHub:

    # Install healthpopR
    remotes::install_github("janikmiet/healthpopR")

## Key Features

- 📊 **Population classification**: `classify_population()`,
  `classify_icd10_profile()`
- 🧠 **ICD-10 diagnostics**: `plot_diagnoses_src()`,
  `plot_health_icd10_profile()`
- ⏳ **Survival analysis**: `plot_survival_km()`, `cox_create_data()`,
  `cox_plot_spline()`
- 📈 **Comparison tools**: `summary_exp_resp_order()`,
  `tbl_icd10_diff_by_exposure()`
- 📊 **Incidence modeling tools**: `pirr_results()`
