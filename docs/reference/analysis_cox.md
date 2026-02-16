# Time-dependent Cox Proportional Hazards Analysis with Splines and Diagnostics

Fits a Cox proportional hazards model with time-dependent exposure,
optional spline-modeled continuous covariates, and categorical
covariates. The function constructs start-stop (counting process)
survival data, applies reference level releveling, fits the model, and
returns diagnostic plots and proportional hazards tests.

## Usage

``` r
analysis_cox(
  dpop,
  data_dates,
  data_socioeconomic,
  normal_vars = c("edu"),
  spline_vars = c("age_bs", "bmi"),
  reference_values = list(bmi_cat1 = "Healthy Weight", bmi_cat2 = "Healthy Weight", edu =
    "1 - Low"),
  censoring_date = as.Date("2024-12-31")
)
```

## Arguments

- dpop:

  A data frame containing population-level variables including at
  minimum: \`ID\`, \`DATE_BIRTH\`, \`DATE_DEATH\`, \`DATE_MIGRATION\`,
  \`exp.DATE\`, and \`resp.DATE\`.

- data_dates:

  A data frame containing baseline date information. Must include \`ID\`
  and \`vpvmbl\` (baseline date).

- data_socioeconomic:

  A data frame containing socioeconomic or additional covariates (e.g.,
  education).

- normal_vars:

  Character vector of covariates to include in the model as standard
  (non-spline) terms.

- spline_vars:

  Character vector of continuous variables to be modeled using cubic
  B-splines (\`splines::bs()\`).

- reference_values:

  Named list specifying reference levels for categorical variables. Used
  internally for releveling.

- censoring_date:

  Administrative censoring date (Date).

## Value

A list containing:

- model:

  Fitted \`coxph\` model object.

- test_residual:

  Result of \`cox.zph()\` proportional hazards test.

- plot_diagnostics:

  Schoenfeld residual diagnostic plot.

- plot_splines:

  Named list of spline effect plots.

- plot_forest:

  Forest plot of model estimates.

## Details

Designed for registry-based longitudinal data with:

- Baseline date (\`vpvmbl\`)

- Exposure date (\`exp.DATE\`)

- Response/event date (\`resp.DATE\`)

- Migration and death censoring dates

The function performs the following steps:

1.  Merges baseline, population, and socioeconomic data.

2.  Calculates age at baseline and follow-up time variables.

3.  Handles exposure occurring before baseline by shifting exposure to
    baseline.

4.  Constructs counting-process format survival data (\`Surv(tstart,
    tstop, event)\`).

5.  Creates a time-dependent exposure indicator.

6.  Fits a Cox model using \`survival::coxph()\`.

7.  Produces:

    - Forest plot (\`survminer::ggforest\`)

    - Spline effect plots (\`ggeffects::ggpredict\`)

    - Proportional hazards test (\`cox.zph\`)

    - Schoenfeld residual plots (\`ggcoxzph\`)

Tied event times are handled using default \`coxph\` tie handling.

## See also

[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html),
[`ggforest`](https://rdrr.io/pkg/survminer/man/ggforest.html),
[`ggcoxzph`](https://rdrr.io/pkg/survminer/man/ggcoxzph.html)
