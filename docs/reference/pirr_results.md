# Run PIRR-style modeling and summary plots for multiple binary outcomes

This function runs Poisson regression models using splines for age and
compares effects over two time-related variables (\`caika\` and
\`cever\`) for each binary outcome variable found in the dataset. It
returns model-based predictions, summary tables, and plots for each
outcome.

## Usage

``` r
pirr_results(adat, colors = c("#5BC0DE", "#D9534F"), limits = c(0.3, 3))
```

## Arguments

- adat:

  A data frame containing the data. Must include columns \`Age\`,
  \`caika\`, \`pyrs\`, \`cever\`, and at least one binary outcome
  variable (e.g., disease flags). Created by function \`pirr_data()\`.

- colors:

  A character vector of two hex colors for plotting binary outcome
  levels in the age vs exposure plots. Default is
  `c("#5BC0DE", "#D9534F")`.

- limits:

  A numeric vector of two values specifying the y-axis limits (log10
  scale) for the SIR plots. Default is `c(0.3, 3)`.

## Value

A named list of results, one element per binary outcome variable. Each
element is a list containing:

- table:

  A tibble with summary statistics and model predictions per level of
  \`caika\` and \`cever\`.

- plot1:

  A ggplot object of the log-scale SIR across \`caika\` or \`cever\`.

- plot2:

  A ggplot object showing adjusted exposure by Age and response status.

## Details

For each binary outcome, two Poisson regression models are fitted:

- One using \`caika\` (calendar period) as the main time variable

- One using \`cever\` (time since event or similar)

Both models adjust for age using restricted cubic splines and use
log(pyrs) as offset. Standard errors are heteroskedasticity-consistent
(HC0). Predictions are made at Age = 70 and a base rate of 10,000
person-years.

## Examples

``` r
# Example usage with simulated data
# results <- pirr_results(adat = my_data)
```
