# Construct per-ID date dataset with follow-up starting at age 50

Creates a person-level dataset containing exposure and response
diagnosis dates, demographic dates, and age-50 baseline. Exposure and
response events occurring before age 50 are set to `NA`. Follow-up is
intended to start from the 50th birthday.

## Usage

``` r
df_dates_per_id(exposure_diagnoses, response_diagnoses, dpop)
```

## Arguments

- exposure_diagnoses:

  A data frame containing exposure diagnoses with variables `ID` and
  `DATE`.

- response_diagnoses:

  A data frame containing response diagnoses with variables `ID` and
  `DATE`.

- dpop:

  A population data frame containing at least `ID`, `DATE_BIRTH`,
  `DATE_DEATH`, and `DATE_MIGRATION`.

## Value

A data frame with one row per individual, including exposure date,
response date, birth date, age-50 date, death date, and migration date.

## Details

Exposure and response diagnosis dates occurring before age 50 are
excluded by setting them to `NA`. Age 50 is calculated as
`DATE_BIRTH + 50 * 365.25`.

The function joins exposure and response diagnoses to the population
data and returns a per-ID dataset suitable for survival or competing
risks analyses.

## See also

[`df_dates_ftime`](https://janikmiet.github.io/healthpopR/reference/df_dates_ftime.md)
