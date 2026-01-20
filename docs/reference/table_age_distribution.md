# Summarize Age Distribution for Exposure or Response Groups

Creates a summary table of population counts and age statistics
(minimum, median, mean, and maximum) for either the "exposure" or
"response" group. Optionally, it can also include subgroups for a more
detailed breakdown.

## Usage

``` r
table_age_distribution(data, group = "exposure", subgroups = FALSE)
```

## Arguments

- data:

  A data frame containing columns \`exp.GROUP\`, \`resp.GROUP\`,
  \`exp.AGE_DG\`, and \`resp.AGE_DG\`. These are expected to represent
  exposure/response group labels and ages at diagnosis.

- group:

  A character string, either \`"exposure"\` or \`"response"\`,
  indicating which group to summarize. Defaults to \`"exposure"\`.

- subgroups:

  Logical. If \`TRUE\`, adds subgroup statistics (cross-group
  summaries). Defaults to \`FALSE\`.

## Value

A data frame summarizing the population size (\`pop_n\`) and age
statistics:

- `pop_n` - Population count

- `age_min` - Minimum age at diagnosis

- `age_median` - Median age at diagnosis

- `age_mean` - Mean age at diagnosis

- `age_max` - Maximum age at diagnosis

When \`subgroups = TRUE\`, an additional "All" row summarizes the entire
group.

## Details

This function is intended for use in both interactive and Shiny
contexts. It utilizes \`.safe_inc_progress()\` to update progress during
computation.

## Examples

``` r
if (FALSE) { # \dontrun{
table_age_distribution(mydata, group = "response", subgroups = TRUE)
} # }
```
