# Plot Survival Curve for Mortality Analysis

This function generates a Kaplan–Meier survival plot comparing groups
based on survival or mortality data. It uses the \`survival\` and
\`survminer\` packages to fit the model and visualize survival
probabilities over time.

## Usage

``` r
plot_survival_death(
  data,
  scale = c("days", "years"),
  surv.median.line = "v",
  colors = c("#D9534F", "#5BC0DE")
)
```

## Arguments

- data:

  A data frame containing at least the following variables:

  value

  :   Numeric; time to event or censoring in days.

  status

  :   Binary; event indicator (1 = event occurred, 0 = censored).

  GROUP

  :   Factor or character; grouping variable for comparison.

- scale:

  years or days (timeline)

- colors:

  A character vector of color hex codes to use for plotting. Default is
  `c("#D9534F", "#5BC0DE")`.

## Value

A \`ggsurvplot\` object from the survminer package, containing the
Kaplan–Meier plot with survival probabilities, confidence intervals,
p-value, and risk table.

## Details

The function fits a survival model using
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
and visualizes it with
[`survminer::ggsurvplot()`](https://rdrr.io/pkg/survminer/man/ggsurvplot.html).
It is typically used to analyze mortality data between exposure and
non-exposure groups.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
  value = c(10, 20, 30, 40, 50, 60),
  status = c(1, 0, 1, 1, 0, 0),
  GROUP = c("Exposure", "No Exposure", "Exposure", "No Exposure", "Exposure", "No Exposure")
)

plot_survival_death(data)
} # }
```
