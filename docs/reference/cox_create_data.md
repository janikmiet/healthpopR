# Create Time-to-Event Data for Cox Proportional Hazards Modeling

Prepares a dataset for survival analysis with time-dependent covariates
using \`tmerge\`. This function computes baseline age, time variables
for exposure and response, and creates a survival object with
appropriate censoring.

## Usage

``` r
cox_create_data(
  data,
  data_dates,
  data_socioeconomic,
  reference_values = list(bmi_cat1 = "Healthy Weight", bmi_cat2 = "Healthy Weight", edu =
    "1 - Low"),
  censoring_date = as.Date("2023-12-31")
)
```

## Arguments

- data:

  A data frame with baseline population information, including dates
  like \`DATE_BIRTH\`, \`DATE_DEATH\`, \`DATE_MIGRATION\`, \`exp.DATE\`,
  and \`resp.DATE\`.

- data_dates:

  A data frame containing answering dates per individual. Must include
  \`ID\`, \`spvm\` (start date), and \`vpvmbl\` (baseline date).

- data_socioeconomic:

  A data frame containing socioeconomic variables such as education
  level. Must include \`ID\` and covariates to be relevelled (e.g.,
  \`edu\`, \`bmi_cat1\`, \`bmi_cat2\`).

- reference_values:

  A named list specifying the reference level for factor variables
  (e.g., \`list("bmi_cat1" = "Healthy Weight", "edu" = "1 - Low")\`).
  Used to relevel factors for Cox regression.

- censoring_date:

  A \`Date\` object indicating the end of follow-up for censoring.
  Default is \`"2023-12-31"\`.

## Value

A data frame formatted for survival analysis, containing time-dependent
covariates, \`tstart\`, \`tstop\`, event indicators (\`diagnose\`,
\`exposure\`), and a \`Surv\` object ready for use with \`coxph\`.

## Details

\- The function internally computes time-to-event and censoring
variables. - Uses \`tmerge\` to incorporate time-dependent covariates
for exposure and outcome. - Applies reference level adjustments via
\`.relevel_by_reference()\` (must be defined elsewhere).

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
result <- cox_create_data(data = dpop,
                          data_dates = ostpre_vastpaiv,
                          data_socioeconomic = edumiage)
} # }
```
