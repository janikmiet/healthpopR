# Summary of Exposure and Response Timing Order

Summarizes the temporal relationship between exposure and response
events for individuals who have both an exposure and a response
diagnosis.

## Usage

``` r
summary_exp_resp_order(data)
```

## Arguments

- data:

  A data frame containing at least the following columns:

  - `exposure`: Binary indicator (0/1) of exposure.

  - `response`: Binary indicator (0/1) of response.

  - `exp.DATE`: Date of the exposure.

  - `resp.DATE`: Date of the response.

## Value

A tibble with columns:

- `exp_resp`:

  A factor indicating the exposure-response temporal relationship.

- `n`:

  Count of cases in each category.

- `percentage`:

  Percentage of total cases for each category.

## Details

Categorizes the relationship as:

- `"Exposure < Response"` if exposure occurred before the response,

- `"Exposure == Response"` if they occurred on the same date,

- `"Exposure > Response"` if exposure occurred after the response.

This function is intended for use in shiny applications and supports
progress indication. If used in a shiny session, progress is displayed
with \`withProgress()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
  df <- data.frame(
    exposure = sample(0:1, 100, replace = TRUE),
    response = sample(0:1, 100, replace = TRUE),
    DATE = sample(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"), 100, replace = TRUE),
    resp.DATE = sample(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"), 100, replace = TRUE)
  )
  summary_exp_resp_order(df)
} # }
```
