# Multistate survival analysis of mortality with time-dependent exposure

Performs data preparation and survival modeling to estimate cumulative
incidence of mortality with and without exposure using competing risks
and multistate models. The function constructs follow-up times from a
response date, applies administrative censoring, handles overlapping
event times, and fits survival models using counting process notation.

## Usage

``` r
analysis_mortality(dpop, censoring_date)
```

## Arguments

- dpop:

  A data.frame containing individual-level data. Must include the
  following variables:

  - `ID`: Unique individual identifier

  - `DATE_BIRTH`: Date of birth

  - `resp.DATE`: Response date (follow-up start date)

  - `exp.DATE`: Exposure date

  - `DATE_DEATH`: Date of death

  - `DATE_MIGRATION`: Date of migration (censoring event)

- censoring_date:

  A Date specifying the administrative censoring date. Events occurring
  after this date are ignored.

## Value

A list with the following elements:

- `data`: Processed dataset in counting process format suitable for
  survival analysis

- `mortality_plot`: A ggplot object showing cumulative incidence curves
  for:

  - Overall mortality

  - Death without exposure

  - Death after exposure

  - Ever exposed

- `cox_results1`: Hazard ratios (HR) and confidence intervals from
  unadjusted Cox model

- `cox_results2`: Hazard ratios (HR) and confidence intervals from Cox
  model adjusted for age (spline)

## Details

Follow-up begins at `resp.DATE` and ends at the earliest of death,
migration, or administrative censoring. Exposure is treated as a
time-dependent variable. Individuals exposed before follow-up start are
flagged as exposed at baseline.

The function uses:

- Competing risks model for death vs exposure

- Multistate model to distinguish death before and after exposure

- Cox proportional hazards models with counting process formulation

Small time offsets are introduced when events occur at identical times
to ensure model stability.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- analysis_mortality(
  dpop = my_data,
  censoring_date = as.Date("2020-12-31")
)

result$mortality_plot
result$cox_results1
} # }
```
