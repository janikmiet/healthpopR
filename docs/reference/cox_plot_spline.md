# Plot Spline Effect from Cox Proportional Hazards Model

Generates a plot showing the spline-transformed effect of a continuous
variable in a Cox model using predicted marginal effects with confidence
intervals.

## Usage

``` r
cox_plot_spline(
  data_model,
  spline_var = "age_bs",
  title = "Spline effect",
  xlab = NULL,
  ylab = "Predicted risk",
  color = "#0072B2"
)
```

## Arguments

- data_model:

  A fitted Cox proportional hazards model object created with
  \`coxph()\` and spline terms (e.g., using \`splines::bs()\`).

- spline_var:

  Character string specifying the name of the spline variable in the
  model to visualize (e.g., \`"age_bs"\` or \`"bmi"\`).

- title:

  Character string specifying the title of the plot. Default is
  \`"Spline effect"\`.

- xlab:

  Optional label for the x-axis. If \`NULL\`, the variable name will be
  used.

- ylab:

  Label for the y-axis. Default is \`"Predicted risk"\`.

- color:

  Character string with a valid color for the line and confidence
  ribbon. Default is Bootstrap blue \`"#0072B2"\`.

## Value

A \`ggplot\` object showing the effect of the spline-transformed
variable with a confidence ribbon.

## Details

The function uses \`ggeffects::ggpredict()\` to compute marginal effects
across the full range of the spline variable. It assumes the model was
created using natural splines via \`splines::bs()\` and includes the
variable named in \`spline_var\`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(survival)
library(splines)

# Example model with spline
model <- coxph(Surv(time, status) ~ splines::bs(age, df = 4), data = lung)

# Plot the spline effect
p <- cox_plot_spline(model, spline_var = "age", title = "Effect of Age")
print(p)
} # }
```
