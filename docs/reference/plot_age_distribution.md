# Plot Age Distribution at First Diagnosis for Exposure or Response Group

Visualizes the age distribution at the time of the first diagnosis for
either the exposure or response group, optionally including subgroup
breakdowns. Intended for use with population-level health registry data.

## Usage

``` r
plot_age_distribution(
  data,
  group = "exposure",
  subgroups = FALSE,
  colors = c("#5BC0DE", "#D9534F"),
  colors_shade = c("#5BC0DE66", "#D9534F66")
)
```

## Arguments

- data:

  A data frame containing the following columns: - \`ID\`: Unique
  identifier for individuals. - \`exp.AGE_DG\`: Age at first diagnosis
  in the exposure group. - \`exp.GROUP\`: Group label (\`"exposure"\` or
  \`"response"\`). - \`resp.AGE_DG\`: Age at first diagnosis in the
  response group. - \`resp.GROUP\`: Group label (\`"exposure"\` or
  \`"response"\`).

- group:

  Character. Specifies which group to plot: either \`"exposure"\` or
  \`"response"\`. Determines which AGE_DG and GROUP columns are used.

- subgroups:

  Logical. If \`TRUE\`, the age distribution will be split by the
  opposite group (e.g., response subgroups within exposure group).

- colors:

  Character vector of two colors used for plotting. First is for
  \`"exposure"\`, second is for \`"response"\`.

- colors_shade:

  Character vector of two hex color values (with transparency) used to
  outline the bars. First for \`"exposure"\`, second for \`"response"\`.

## Value

A \`ggplot\` object showing the age distribution as a bar chart. If used
in a Shiny application, the plot is wrapped in a progress bar using
\`withProgress()\`.

## Details

The function renames and filters the appropriate AGE and GROUP columns
based on the selected \`group\`. If \`subgroups = TRUE\`, it groups the
data by both \`GROUP\` and the opposite group (used as \`SUBGROUP\`)
before plotting.

Color selection is automatically handled based on the \`group\`
argument, using the corresponding color and shade from \`colors\` and
\`colors_shade\`.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_age_distribution(data = my_data, group = "exposure")
plot_age_distribution(data = my_data, group = "response", subgroups = TRUE)
} # }
```
