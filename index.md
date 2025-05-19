# Welcome to healthpopR

**healthpopR** helps researchers and analysts classify populations into exposure and response groups, visualize ICD-10-based health profiles, and perform survival and Cox regression analysis using tidy workflows.

The package is designed for analyzing how exposure diagnoses affect response diagnoses in population health data research.

<!-- <img src="man/figures/ueflogo.jpg" align="right" alt="" width="120" /> -->

## Installation

Install the development version from GitHub:

```
# Install devtools if needed
install.packages("devtools")

# Install healthpopR
devtools::install_github("janikmiet/healthpopR")
```


## Quick Example

Package uses regex to search diagnoses. Here's how to classify a population based on a sample dataset:


```
library(healthpopR)

diagnoses <- read.csv("your_diagnoses_data.csv")
dpop <- classify_population(diagnoses, 
                    exposure_icd10 = "^E11", 
                    response_icd_10 = "^I2[0-5]")
head(dpop)
```


## Key Features

- 📊 **Population classification**: `classify_population()`, `classify_icd10_profile()`
- 🧠 **ICD-10 diagnostics**: `plot_diagnoses_src()`, `plot_health_icd10_profile()`
- ⏳ **Survival analysis**: `plot_survival_km()`, `cox_create_data()`, `cox_plot_spline()`
- 📈 **Comparison tools**: `summary_exp_resp_order()`, `tbl_icd10_diff_by_exposure()`


## Learn More

- [Reference Manual](reference/index.html)
- [Getting Started Guide](articles/getting-started.html)
- [Data Model](articles/data-model.html)
