# Calculate follow-up time and event status from age 50

Computes follow-up time (`ftime`) and event status based on a specified
endpoint, death, or censoring. Time is measured from age 50 onward.

## Usage

``` r
df_dates_ftime(df, censor_date = as.Date("2023-12-31"), end = DATE_EXPOSURE)
```

## Arguments

- df:

  A data frame produced by
  [`df_dates_per_id`](https://janikmiet.github.io/healthpopR/reference/df_dates_per_id.md)
  containing demographic and event dates.

- censor_date:

  Administrative censoring date. Defaults to `as.Date("2023-12-31")`.

- end:

  A date variable indicating the endpoint of interest (e.g.
  `DATE_EXPOSURE` or `DATE_RESPONSE`). Tidy-evaluated.

## Value

A data frame with added variables:

- ftime:

  Follow-up time in days from age 50

- status:

  Event status (0 = censored, 1 = event, 2 = death)

## Details

For each individual, the end of follow-up is defined as the earliest of:

- the specified endpoint (`end`),

- date of death,

- migration or administrative censoring.

Event status is coded as:

- 1:

  Endpoint event

- 2:

  Death before endpoint

- 0:

  Censored

Follow-up times occurring before age 50 are set to zero.

## See also

[`df_dates_per_id`](https://janikmiet.github.io/healthpopR/reference/df_dates_per_id.md)
