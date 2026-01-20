# Generate Crosstabulation of Exposure and Response Variables

This function produces a cross-tabulation summary between two variables,
\`exposure\` and \`response\`, from a given dataset. It returns either a
styled HTML table for interactive viewing or a Word-compatible
\`flextable\` for document output.

## Usage

``` r
summary_exp_resp_crosstabulation(data, output = "viewer")
```

## Arguments

- data:

  A data frame containing variables \`exposure\` and \`response\`.

- output:

  Character string indicating the output type. Use \`"viewer"\` to
  return an \`sjPlot\` table for interactive viewing, or \`"docx"\` to
  return a \`flextable\` object for Word reports.

## Value

An object of class \`sjTable\` (if \`output = "viewer"\`) or a
\`flextable\` (if \`output = "docx"\`).

## Details

\- When \`output = "viewer"\`, the function returns a formatted HTML
table with row percentages using \`sjPlot::tab_xtab()\`. - When \`output
= "docx"\`, the cross-tabulation is converted to a data frame using
\`sjtable2df::xtab2df()\` and formatted using \`flextable\` for
inclusion in Word documents.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example data
  df <- data.frame(
    exposure = sample(c(0, 1), 100, replace = TRUE),
    response = sample(c(0, 1), 100, replace = TRUE)
  )
  summary_exp_resp_crosstabulation(df, output = "viewer")
  ft <- summary_exp_resp_crosstabulation(df, output = "docx")
} # }
```
