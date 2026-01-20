# Create Long-Format Survival Data from Exposure-Response Events

Processes raw exposure-response data into a long-format dataset suitable
for survival analysis, including response, death, and censoring times.
Supports use in Shiny with progress tracking.

## Usage

``` r
create_dsurv(
  data,
  censoring_date = as.Date("2024-12-21"),
  filter_early_responses = FALSE,
  type = c("exp_to_resp", "resp_to_death")
)
```

## Arguments

- data:

  A data frame that must include columns: \`ID\`, \`exp.GROUP\`,
  \`exp.DATE\`, \`resp.DATE\`, \`DATE_DEATH\`, \`DATE_MIGRATION\`. These
  are used to calculate event and censoring times.

- censoring_date:

  A \`Date\` object specifying the administrative censoring date.
  Default is \`"2023-12-21"\`.

- filter_early_responses:

  Logical; if \`TRUE\`, responses occurring before the exposure date
  (negative time) are filtered out. If \`FALSE\`, they are included and
  recoded to 0. Default is \`FALSE\`.

- type:

  character. Options: "exp_to_resp" or "resp_to_death"

## Value

A data frame in long format with columns:

- ID:

  Subject ID

- name:

  Type of event: \`"diagnose"\`, \`"dead"\`, or \`"censoring"\`

- value:

  Time (in days) from exposure to event

## Details

This function is intended to support dynamic survival model
construction. It handles competing risks (response, death, censoring)
and can run inside a Shiny app with progress bar support via
\`.safe_inc_progress()\`. Internally, it filters and summarizes the
earliest valid event per subject.

## Examples

``` r
if (FALSE) { # \dontrun{
dsurv <- create_dsurv(
  data = exposure_response_df,
  censoring_date = as.Date("2023-12-31"),
  filter_early_responses = TRUE
)
} # }
```
