# Create Exposure and Optional Response Groups from Diagnoses

This function identifies individuals with exposure diagnoses (and
optionally response diagnoses) based on ICD-10/9/8 codes and selected
registry sources. It returns a dataset of grouped individuals with
exposure (and optionally response) classifications, diagnosis dates, and
relevant metadata. Function searches first diagnose date, which is given
for exposure/response diagnose date.

## Usage

``` r
classify_population(
  exposure_icd10 = "",
  exposure_icd9 = "",
  exposure_icd8 = "",
  exposure_src = c(""),
  response_icd10 = NULL,
  response_icd9 = NULL,
  response_icd8 = NULL,
  response_src = c(""),
  data_population = population,
  data_diagnoses = diagnoses,
  runtime_shiny = FALSE
)
```

## Arguments

- exposure_icd10:

  A character vector of ICD-10 codes (regex-supported) to identify
  exposure group.

- exposure_icd9:

  A character vector of ICD-9 codes (regex-supported) to identify
  exposure group.

- exposure_icd8:

  A character vector of ICD-8 codes (regex-supported) to identify
  exposure group.

- exposure_src:

  A character vector of registry sources used to search for exposure
  diagnoses.

- response_icd10:

  Optional character vector of ICD-10 codes for response group (default:
  NULL).

- response_icd9:

  Optional character vector of ICD-9 codes for response group (default:
  NULL).

- response_icd8:

  Optional character vector of ICD-8 codes for response group (default:
  NULL).

- response_src:

  A character vector of registry sources used to search for response
  diagnoses.

- data_population:

  A data frame of the target population (default: \`population\`).

- data_diagnoses:

  A data frame of diagnoses (default: \`diagnoses\`).

- runtime_shiny:

  Logical; if TRUE and run inside a Shiny app, shows progress bar
  (default: FALSE).

## Value

A data frame with exposure (and optionally response) group
classifications and metadata, including ID, birth/death/migration dates,
diagnosis info, and binary indicators for exposure/response.

## Examples

``` r
classify_population(exposure_icd10 = "I21", exposure_src = "hilmo")
#> Error in classify_population(exposure_icd10 = "I21", exposure_src = "hilmo"): object 'diagnoses' not found
classify_population(exposure_icd10 = "F32", response_icd10 = "I21")
#> Error in classify_population(exposure_icd10 = "F32", response_icd10 = "I21"): object 'diagnoses' not found
```
