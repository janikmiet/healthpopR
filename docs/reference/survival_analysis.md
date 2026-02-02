# Competing risks survival analysis between exposure and response diagnoses

Performs a competing risks survival analysis using registry-based
diagnosis dates. The function supports different follow-up starting
points (exposure, response, or age 50) and multiple strategies for
handling diagnoses that occur before cohort entry. Results include
cumulative incidence estimates and ready-made ggplot visualizations.

## Usage

``` r
survival_analysis(
  exposure_diagnoses,
  response_diagnoses,
  dpop,
  start = c("DATE_EXPOSURE", "DATE_RESPONSE", "DATE_50"),
  censoring_date = as.Date("2024-12-21"),
  pre_entry_handling = c("truncate", "skip", "asis")
)
```

## Arguments

- exposure_diagnoses:

  A data frame containing exposure diagnoses. Must include columns `ID`
  and `DATE`.

- response_diagnoses:

  A data frame containing response diagnoses. Must include columns `ID`
  and `DATE`.

- dpop:

  Population-level data frame with at least the columns `ID`,
  `DATE_BIRTH`, `DATE_DEATH`, and `DATE_MIGRATION`.

- start:

  Character string defining the start of follow-up. One of
  `"DATE_EXPOSURE"`, `"DATE_RESPONSE"`, or `"DATE_50"`.

- censoring_date:

  Date defining administrative censoring (default: `2024-12-21`).

- pre_entry_handling:

  Strategy for handling diagnoses occurring before cohort entry (age
  50):

  truncate

  :   Diagnosis date is set to entry date.

  skip

  :   Diagnoses before entry are ignored; first post-entry diagnosis is
      used.

  asis

  :   Diagnosis date is used as recorded.

## Value

A named list with the following elements:

- plot_days:

  `ggplot` object of cumulative incidence (days).

- plot_years:

  `ggplot` object of cumulative incidence (years).

- CR_days:

  `cuminc` object with time in days.

- CR_years:

  `cuminc` object with time in years.

- dsurv:

  Final individual-level survival data used in the analysis.

## Details

The analysis workflow consists of:

1.  Constructing individual-level date data (birth, death, migration,
    diagnoses).

2.  Defining follow-up start based on `start`.

3.  Handling diagnoses occurring before cohort entry using
    `pre_entry_handling`.

4.  Computing event times (exposure, response, death, censoring).

5.  Selecting the first occurring event per individual.

6.  Estimating cumulative incidence functions using
    [`cuminc`](https://rdrr.io/pkg/cmprsk/man/cuminc.html).

Time is internally calculated in days and additionally expressed in
years (365.25 days). Individuals who die before follow-up start are
excluded.

## See also

[`cuminc`](https://rdrr.io/pkg/cmprsk/man/cuminc.html),
[`ggcompetingrisks`](https://rdrr.io/pkg/survminer/man/ggcompetingrisks.html)
