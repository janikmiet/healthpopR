# Create counting-process dataset for time-dependent Cox model

Constructs a long-format dataset suitable for
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) using
counting-process notation `Surv(tstart, tstop, event)`. The function
prepares a follow-up cohort starting at baseline and models a
time-dependent exposure diagnosis affecting the hazard of a response
diagnosis.

## Usage

``` r
cox_create_data(
  dpop,
  data_dates,
  data_socioeconomic,
  reference_values = list(bmi_cat1 = "Healthy Weight", bmi_cat2 = "Healthy Weight", edu =
    "1 - Low"),
  censoring_date = as.Date("2024-12-31")
)
```

## Arguments

- dpop:

  A data frame containing population-level variables. Must include `ID`,
  `DATE_BIRTH`, `DATE_MIGRATION`, `DATE_DEATH`, and diagnosis dates
  `exp.DATE` and `resp.DATE`.

- data_dates:

  A data frame containing baseline dates. Must include `ID` and baseline
  date variable `vpvmbl`.

- data_socioeconomic:

  A data frame containing socioeconomic and baseline questionnaire
  variables. Must include `ID` and covariates such as `edu`, `bmi`,
  `bmi_cat1`, and `bmi_cat2`.

- reference_values:

  A named list defining reference levels for factor variables (e.g.,
  education or BMI categories). Passed internally to
  `healthpopR:::.relevel_by_reference()`.

- censoring_date:

  Administrative censoring date. Default is `as.Date("2024-12-31")`.

## Value

A long-format data frame with the following variables:

- ID:

  Individual identifier

- tstart:

  Start of interval (days since baseline)

- tstop:

  End of interval (days since baseline)

- event:

  Event indicator (1 = response diagnosis, 0 = censored)

- exposure_td:

  Time-dependent exposure indicator (0/1)

- age_bs:

  Age at baseline (years)

- edu:

  Education level (factor)

- bmi:

  Baseline BMI

The output is ready for use in:

    coxph(Surv(tstart, tstop, event) ~ exposure_td + ..., data = output)

## Details

Baseline covariates (e.g., age, education, BMI) are treated as fixed.
Exposure is handled as a time-dependent variable that switches from 0 to
1 at the exposure diagnosis date, if it occurs before the end of
follow-up.

The function performs the following steps:

1.  Filters individuals with a valid baseline date.

2.  Computes age at baseline.

3.  Recodes exposure and response diagnoses occurring before baseline to
    the baseline date.

4.  Defines follow-up end as the minimum of migration, death,
    administrative censoring, or response diagnosis.

5.  Computes follow-up times (in days) from baseline.

6.  Splits follow-up into one or two intervals depending on whether
    exposure occurs before the end of follow-up.

Each row in the output represents a time interval during which exposure
status is constant.

## See also

[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html),
[`Surv`](https://rdrr.io/pkg/survival/man/Surv.html)
