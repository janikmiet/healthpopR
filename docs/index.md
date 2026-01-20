# Welcome to healthpopR

**healthpopR** helps researchers and analysts classify populations into
exposure and response groups, visualize ICD-10-based health profiles,
and perform survival and Cox regression analysis using tidy workflows.

The package is designed for analyzing how exposure diagnoses affect
response diagnoses in population health data research. Package uses
regex code for identifying ICD-10/9/8 codes. Check Quick Example for
motivation.

## Dataset Limitations

Currently functions are developed so that they fit our specific dataset
correctly. Which means that analyses are limited to only one gender and
follow up starts at when population is 50 years old. Theses limitations
can be removed later, when we use same functions to another datasets.

## Installation

Install the development version from GitHub:

    # Install healthpopR
    remotes::install_github("janikmiet/healthpopR")

## Quick Example

Package uses regex to search diagnoses. Here’s how to classify a
population to exposure and response groups, where

- Exposure is ICD-10 code E11 (*Type 2 diabetes mellitus*)
- Response is ICD-10 codes I20-I25 (*Ischemic heart diseases*)

In diagnoses dataset we use only 2 registry sources `local` and `hilmo`.

    library(healthpopR)

    ## Load Datasets
    population <- read.csv("your_population_data.csv")
    diagnoses <- read.csv("your_diagnoses_data.csv")
    ## Classify population
    dpop <- classify_population(diagnoses,
                        exposure_icd10 = "^E11",
                        exposure_src = c("local", "hilmo"),
                        response_icd10 = "^I2[0-5]",
                        response_src = c("local", "hilmo"),
                        data_population = population,
                        data_diagnoses = diagnoses)
    head(dpop)

Check
[`data model`](https://janikmiet.github.io/healthpopR/articles/data-model.md)
-vignette to view how you should arrange your `population` and
`diagnoses` -datasets.

## Key Features

- 📊 **Population classification**:
  [`classify_population()`](https://janikmiet.github.io/healthpopR/reference/classify_population.md),
  [`classify_icd10_profile()`](https://janikmiet.github.io/healthpopR/reference/classify_icd10_profile.md)
- 🧠 **ICD-10 diagnostics**:
  [`plot_diagnoses_src()`](https://janikmiet.github.io/healthpopR/reference/plot_diagnoses_src.md),
  [`plot_health_icd10_profile()`](https://janikmiet.github.io/healthpopR/reference/plot_health_icd10_profile.md)
- ⏳ **Survival analysis**:
  [`plot_survival_km()`](https://janikmiet.github.io/healthpopR/reference/plot_survival_km.md),
  [`cox_create_data()`](https://janikmiet.github.io/healthpopR/reference/cox_create_data.md),
  [`cox_plot_spline()`](https://janikmiet.github.io/healthpopR/reference/cox_plot_spline.md)
- 📈 **Comparison tools**:
  [`summary_exp_resp_order()`](https://janikmiet.github.io/healthpopR/reference/summary_exp_resp_order.md),
  [`tbl_icd10_diff_by_exposure()`](https://janikmiet.github.io/healthpopR/reference/tbl_icd10_diff_by_exposure.md)
- 📊 **Incidence modeling tools**:
  [`pirr_results()`](https://janikmiet.github.io/healthpopR/reference/pirr_results.md)

## Learn More

- [Data
  Model](https://janikmiet.github.io/healthpopR/articles/data-model.md)
- [Data
  Handling](https://janikmiet.github.io/healthpopR/articles/data-handling.md)
- [Getting Started
  Guide](https://janikmiet.github.io/healthpopR/articles/getting-started.md)
- [Functions Reference
  Manual](https://janikmiet.github.io/healthpopR/reference/index.md)
