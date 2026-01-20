# Prepare Person-Time and Event Data for SIR/IRR Calculations

This function merges exposure, response, and population-level data to
produce an aggregated dataset for person-time and event rate analysis
(e.g., Standardized Incidence Ratios \[SIR\] or Incidence Rate Ratios
\[IRR\]). It compares disease occurrence across exposure time windows
using registry-style longitudinal data.

## Usage

``` r
pirr_data(
  exposure_diagnoses,
  response_diagnoses,
  pop_dates,
  all_cases = FALSE,
  censoring_age = c(50, 90),
  censoring_date = c(as.Date("1953-01-01"), as.Date("2024-12-31")),
  custom_responses = list()
)
```

## Arguments

- exposure_diagnoses:

  A \`data.frame\` containing exposure diagnoses. Must include columns
  \`ID\`, \`DATE\`, and \`DG\`. Typically created with
  \`search_diagnoses()\`.

- response_diagnoses:

  A \`data.frame\` containing response diagnoses. Must include columns
  \`ID\`, \`DATE\`, and \`DG\`. Typically created with
  \`search_diagnoses()\`.

- pop_dates:

  A \`data.frame\` with population registry information, including
  \`ID\`, \`DATE_BIRTH\`, \`DATE_DEATH\`, and \`DATE_MIGRATION\`.
  Usually from \`classify_population()\`.

- all_cases:

  Logical; if \`TRUE\`, follow-up continues after the first response
  case. If \`FALSE\`, follow-up stops at the first response diagnosis.

- censoring_age:

  Numeric vector (length 2) specifying the lower and upper ages for
  follow-up inclusion (e.g., \`c(50, 90)\`).

- censoring_date:

  A \`Date\` vector (length 2) defining the administrative start and end
  of follow-up (e.g., \`c(as.Date("1960-01-01"),
  as.Date("2022-12-31"))\`).

- custom_responses:

  Optional named list defining custom response diagnosis groupings. For
  example: \`list(Any_fracture = "ankle+forearm+hip+humerus+vertebral",
  Osteoporotic = "forearm+hip+humerus+vertebral", Hip = "hip")\`.

## Value

A \`data.frame\` summarizing:

- pyrs:

  Person-years within each exposure and age stratum.

- Death:

  Count of deaths within each stratum.

- Diagnosis counts:

  Optional columns for each response diagnosis or custom grouping
  defined in \`custom_responses\`.

- caika:

  Exposure time category (e.g., \`\<1y\`, \`1–4y\`, \`5–9y\`, etc.).

- Age:

  Age group at risk.

## Details

The function requires the \*\*heaven\*\* package for Lexis splitting
utilities. See:
\[https://github.com/tagteam/heaven\](https://github.com/tagteam/heaven)

The function performs the following steps:

- Extracts and merges first exposure and response diagnoses per
  individual.

- Computes time differences between exposure and response dates.

- Splits follow-up time using \`heaven::lexisSeq()\` and
  \`heaven::lexisTwo()\`.

- Categorizes person-time into exposure windows (\`\<1y\`, \`1–4y\`,
  \`5–9y\`, \`10–14y\`, \`15+y\`).

- Optionally aggregates diagnoses using custom groupings from
  \`custom_responses\`.

- Restricts follow-up to the age range specified in \`censoring_age\`.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- pirr_data(
  exposure_diagnoses = exposure_data,
  response_diagnoses = response_data,
  pop_dates = population_data,
  censoring_age = c(50, 90),
  censoring_date = c(as.Date("1960-01-01"), as.Date("2023-12-31")),
  custom_responses = list(
    Any_fracture = "ankle+forearm+hip+humerus+vertebral",
    Osteoporotic = "forearm+hip+humerus+vertebral",
    Hip = "hip"
  )
)
} # }
```
