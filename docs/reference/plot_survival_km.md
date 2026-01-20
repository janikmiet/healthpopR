# Plot Kaplan-Meier Survival Curve (Exposure to Response)

Plots an overall Kaplan-Meier survival curve from a long-format survival
dataset containing response, censoring, and death events.

## Usage

``` r
plot_survival_km(
  data,
  color = "#D9534F",
  scale = "years",
  plot = c("base", "survminer")
)
```

## Arguments

- data:

  A data frame in long format with the following required columns:

  ID

  :   Subject ID

  name

  :   Type of event: must include \`"diagnose"\` for response; other
      values are treated as censored

  value

  :   Time (in days) from exposure to event

- scale:

  years or days (timeline)

## Value

A Kaplan-Meier survival curve plotted using the base R \`plot()\`
function.

## Details

Internally converts event types to binary (1 = diagnose, 0 = other),
constructs a \`Surv\` object, and fits an overall survival model using
\`survfit()\`. Designed to be used interactively in Shiny with optional
progress feedback via \`.safe_inc_progress()\` and \`withProgress()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
km_plot <- plot_survival_km(data = long_surv_df)
} # }
```
