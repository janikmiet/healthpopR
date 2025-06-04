# Welcome to healthpopR

**healthpopR** helps researchers and analysts classify populations into exposure and response groups, visualize ICD-10-based health profiles, and perform survival and Cox regression analysis using tidy workflows.

The package is designed for analyzing how exposure diagnoses affect response diagnoses in population health data research. Package uses regex code for identifying ICD-10/9/8 codes. Check Quick Example for motivation.

<!-- <img src="man/figures/ueflogo.jpg" align="right" alt="" width="120" /> -->

## Installation

Install the development version from GitHub:

```
# Install healthpopR
remotes::install_github("janikmiet/healthpopR")
```


## Quick Example

Package uses regex to search diagnoses. Here's how to classify a population to exposure and response groups, where 

- Exposure is ICD-10 code E11 (*Type 2 diabetes mellitus*)
- Response is ICD-10 codes I20-I25 (*Ischemic heart diseases*)

In diagnoses dataset we use only 2 registry sources `local` and `hilmo`.


```
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
```

Check [`data model`](articles/data-model.html) -vignette to view how you should arrange your `population` and `diagnoses` -datasets.


## Key Features

- 📊 **Population classification**: `classify_population()`, `classify_icd10_profile()`
- 🧠 **ICD-10 diagnostics**: `plot_diagnoses_src()`, `plot_health_icd10_profile()`
- ⏳ **Survival analysis**: `plot_survival_km()`, `cox_create_data()`, `cox_plot_spline()`
- 📈 **Comparison tools**: `summary_exp_resp_order()`, `tbl_icd10_diff_by_exposure()`
- 📊 **Incidence modeling tools**: `pirr_results()`

## Learn More

- [Data Model](articles/data-model.html)
- [Data Handling](articles/data-handling.html)
- [Getting Started Guide](articles/getting-started.html)
- [Functions Reference Manual](reference/index.html)
