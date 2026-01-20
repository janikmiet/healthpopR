# Create a Cox Proportional Hazards Model with Splines and Covariates

Constructs a Cox model using a survival formula and allows for the
inclusion of both regular covariates and spline-transformed continuous
variables.

## Usage

``` r
create_cox_model(
  data,
  normal_vars = c("edu"),
  spline_vars = c("age_bs", "bmi"),
  surv_formula = "Surv(tstart, tstop, diagnose) ~ exposure",
  id_var = "ID"
)
```

## Arguments

- data:

  A data frame containing the variables required for the model,
  including survival time variables and covariates.

- normal_vars:

  A character vector of covariate names to be included as standard
  (non-spline) variables in the model (e.g., \`"edu"\`).

- spline_vars:

  A character vector of continuous variables for which natural cubic
  splines will be used (via \`splines::bs()\`).

- surv_formula:

  A character string specifying the survival outcome in \`Surv()\`
  notation. Currently not parsed dynamically; the function uses
  \`Surv(tstart, tstop, diagnose)\` internally.

- id_var:

  A string giving the column name for individual-level IDs to be used in
  the \`coxph()\` call.

## Value

An object of class \`coxph\` representing the fitted Cox proportional
hazards model.

## Details

\- The function validates variable presence and ensures spline variables
are not entirely missing (\`NA\`). - Splines are applied using
\`splines::bs()\` to the specified \`spline_vars\`. - The survival
object is internally defined as \`Surv(tstart, tstop, diagnose)\`.

## Examples

``` r
if (FALSE) { # \dontrun{
model <- create_cox_model(
  data = cox_base,
  normal_vars = c("edu"),
  spline_vars = c("age_bs", "bmi")
)
summary(model)
} # }
```
