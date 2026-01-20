# Extract all cumulative incidence curves from a `cuminc` object

Internal helper function that converts a
[`cmprsk::cuminc`](https://rdrr.io/pkg/cmprsk/man/cuminc.html) object
into a tidy data frame containing estimates, variances, standard errors,
and confidence intervals for all event types.

## Usage

``` r
extract_ci_all(ci_obj, main_label)
```

## Arguments

- ci_obj:

  A `cuminc` object produced by
  [`cmprsk::cuminc`](https://rdrr.io/pkg/cmprsk/man/cuminc.html).

- main_label:

  Character string used to label the main event of interest.

## Value

A tibble with columns:

- time:

  Time in years since age 50

- est:

  Cumulative incidence estimate

- var:

  Variance of the estimate

- se:

  Standard error

- lower:

  Lower 95% confidence limit

- upper:

  Upper 95% confidence limit

- event:

  Event label

## Details

Time is converted from days to years since age 50. Wald-type 95%
confidence intervals are computed and truncated to the interval \[0,
1\].

This function is intended for internal package use.
