# Bar Plot of ICD-10 Diagnosis Differences Between Exposure Groups

Creates a horizontal bar plot to visualize differences in ICD-10
diagnosis percentages between exposure and non-exposure groups, filtered
by a specified percentage difference threshold.

## Usage

``` r
plot_icd10_diff_by_exposure(
  data,
  limit = 10,
  colors = c("#5BC0DE66", "#D9534F66")
)
```

## Arguments

- data:

  A data frame containing ICD-10 comparison results, typically produced
  by
  [`tbl_icd10_diff_by_exposure`](https://janikmiet.github.io/healthpopR/reference/tbl_icd10_diff_by_exposure.md).
  Must include the columns: \`ICD10_3LETTERS\`, \`exposure_group_pct\`,
  \`no_exposure_group_pct\`, and \`diff_pct\`.

- limit:

  Numeric value (default = 10). Only diagnoses with a group percentage
  difference greater than this threshold will be plotted.

- colors:

  A character vector of two color values (with optional alpha), used for
  the exposure and no-exposure group bars respectively. Defaults to
  \`c("#5BC0DE66", "#D9534F66")\`.

## Value

A \`ggplot2\` object showing a grouped horizontal bar chart of the
selected ICD-10 diagnosis codes, comparing the exposure and no-exposure
group percentages.

## Details

The function filters ICD-10 codes to include only those where the
absolute difference in diagnosis prevalence (\`diff_pct\`) exceeds the
specified threshold. The plot flips the coordinate system to display
diagnoses vertically for better readability.

Colors are manually assigned for visual clarity and can be customized.
The plot uses the \`hrbrthemes::theme_ipsum_rc()\` theme.

The function supports integration with Shiny and shows progress via
\`withProgress()\` and \`.safe_inc_progress()\` if inside a Shiny app.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- tbl_icd10_diff_by_exposure(
  data = dpop,
  diagnoses = diagnoses,
  exposure_icd10 = "^E11",
  exposure_src = c("hilmo", "avohilmo")
)

plot_icd10_diff_by_exposure(tbl, limit = 5)
} # }
```
