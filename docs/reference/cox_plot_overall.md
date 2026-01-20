# Plot Survival Curves from Cox Model Data

Visualizes survival information using different representations (events,
cumulative hazard, or survival percentage) for exposure groups based on
a precomputed survival object.

## Usage

``` r
cox_plot_overall(
  data,
  type = c("event", "cumhaz", "pct"),
  colors_exposure_groups = c("#5BC0DE", "#D9534F"),
  legend_labels = c("No exposure", "Exposure"),
  conf_int = TRUE,
  risk_table = FALSE
)
```

## Arguments

- data:

  A data frame containing the survival object (e.g., \`Surv\`) and
  exposure variable. The survival object must already be computed
  (typically by \`cox_create_data()\`), and the data should be ready for
  plotting.

- type:

  Character string indicating the type of plot to generate. Options are:
  \`"event"\` for number of events over time, \`"cumhaz"\` for
  cumulative hazard, \`"pct"\` for survival percentage. Default is
  \`"pct"\`.

- colors_exposure_groups:

  A character vector of color codes for the exposure groups. Default is
  Bootstrap-themed: \`c("#5BC0DE", "#D9534F")\`.

- legend_labels:

  A character vector of labels for the exposure groups shown in the
  legend. Default is \`c("No exposure", "Exposure")\`.

- conf_int:

  Logical. Whether to display confidence intervals (only applies if
  \`type = "pct"\`). Default is \`TRUE\`.

- risk_table:

  Logical. Whether to include a risk table below the survival plot (only
  applies if \`type = "pct"\`). Default is \`FALSE\`.

## Value

A \`ggsurvplot\` object, which can be printed or further modified with
\`ggplot2\` functions.

## Details

The function expects the input data to already include a survival object
named \`Surv\`, such as one created via \`Surv(tstart, tstop,
diagnose)\` within \`cox_create_data()\`. The exposure variable should
also be present and appropriately formatted.

## Examples

``` r
if (FALSE) { # \dontrun{
plot <- cox_plot_overall(
  data = cox_base,
  type = "pct",
  colors_exposure_groups = c("#5BC0DE", "#D9534F"),
  legend_labels = c("No exposure", "Exposure"),
  conf_int = TRUE,
  risk_table = TRUE
)
print(plot)
} # }
```
