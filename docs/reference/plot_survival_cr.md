# Plot Competing Risks Survival Curve (Exposure to Response or Death)

Plots a cumulative incidence function from a long-format dataset using a
competing risks model.

## Usage

``` r
plot_survival_cr(
  data,
  scale = c("days", "years"),
  colors = c("#5CB85C", "#343A40")
)
```

## Arguments

- data:

  A data frame in long format with the following required columns:

  ID

  :   Subject ID

  name

  :   Type of event: must include \`"diagnose"\` for response,
      \`"dead"\` for death, and others are treated as censoring

  value

  :   Time (in days) from exposure to event

- scale:

  years or days (timeline)

- colors:

  A character vector of color hex codes to use for the plotted event
  types. Default is \`c("#5CB85C", "#343A40")\`.

## Value

A \`ggplot2\` object showing the cumulative incidence curves for
competing risks.

## Details

Internally converts event types into status codes for competing risks
analysis:

- 1 = diagnose (response)

- 2 = dead

- 3 = censored

Uses \`cuminc()\` from the \`cmprsk\` package and visualizes the results
using \`ggcompetingrisks()\`. Designed to run in both interactive and
Shiny environments, using \`.safe_inc_progress()\` for optional progress
updates.

## Examples

``` r
if (FALSE) { # \dontrun{
cr_plot <- plot_survival_cr(data = long_surv_df)
} # }
```
