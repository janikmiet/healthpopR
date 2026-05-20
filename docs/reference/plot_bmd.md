# Plot Bone Mineral Density T-score by Age

Creates a scatter plot and smoothed GAM curves for bone mineral density
(BMD) T-scores across age. The function optionally applies
date-dependent exposure and response classifications and visualizes
osteoporosis thresholds.

## Usage

``` r
plot_bmd(
  data_bmd,
  data_dpop,
  date_dependency = FALSE,
  reference = c("hip", "fracture", "osteo")
)
```

## Arguments

- data_bmd:

  A data frame containing bone mineral density measurements. Must
  include columns:

  ID

  :   Patient identifier

  AGE

  :   Age at measurement

  TSCORE

  :   Bone mineral density T-score

  DATE

  :   Measurement date (required if `date_dependency = TRUE`)

- data_dpop:

  A data frame containing exposure and response information. Must
  include:

  ID

  :   Patient identifier

  exp.GROUP

  :   Exposure group label (if `date_dependency = FALSE`)

  resp.GROUP

  :   Response group label (if `date_dependency = FALSE`)

  exp.DATE

  :   Exposure date (if `date_dependency = TRUE`)

  resp.DATE

  :   Response date (if `date_dependency = TRUE`)

- date_dependency:

  Logical. If \`TRUE\`, exposure and response groups are determined
  dynamically based on whether exposure or response dates occurred
  before the BMD measurement date. Defaults to \`FALSE\`.

## Value

A \`ggplot2\` object showing BMD T-score trajectories by age.

## Details

The plot includes:

- Individual BMD observations

- GAM-smoothed trends for all subjects, exposure group, and response
  group

- Reference lines at T-score thresholds -1 and -2.5

- Background shading for osteopenia and osteoporosis ranges

Osteopenia is highlighted between T-scores -1 and -2.5, while
osteoporosis is highlighted below -2.5 according to WHO criteria.

Generalized additive models (GAMs) are fitted using
`geom_smooth(method = "gam")`.

## See also

[`geom_smooth`](https://ggplot2.tidyverse.org/reference/geom_smooth.html),
[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- plot_bmd(
  data_bmd = bone_density_scores,
  data_dpop = dpop
)

print(p)

p2 <- plot_bmd(
  data_bmd = bone_density_scores,
  data_dpop = dpop,
  date_dependency = TRUE
)
} # }
```
