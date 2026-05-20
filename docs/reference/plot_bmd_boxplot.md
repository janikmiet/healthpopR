# Plot boxplots of BMD T-scores by exposure, response, fracture, and osteoporosis groups

Creates boxplots of bone mineral density (BMD) T-scores (\`TSCORE\`) for
selected population subgroups, including exposure, response, hip
fracture, any fracture, and osteoporosis groups. Groups can be
determined either statically or dynamically relative to the BMD
measurement date.

## Usage

``` r
plot_bmd_boxplot(
  data_bmd,
  data_dpop,
  reference = c("hip", "fractures", "osteo"),
  date_dependency = FALSE
)
```

## Arguments

- data_bmd:

  A data frame containing bone mineral density measurements. Must
  include at least \`ID\`, \`DATE\`, \`TSCORE\`, and
  fracture/osteoporosis date variables if \`date_dependency = TRUE\`.

- data_dpop:

  A data frame containing population-level grouping variables. Must
  include \`ID\`, and either:

  - \`exp.GROUP\`, \`resp.GROUP\` when \`date_dependency = FALSE\`

  - \`exp.DATE\`, \`resp.DATE\` when \`date_dependency = TRUE\`

- reference:

  Character vector specifying which clinical reference groups to
  include. Options are \`"hip"\`, \`"fractures"\`, and \`"osteo"\`.
  Default is all.

- date_dependency:

  Logical; if \`TRUE\`, group membership is recalculated relative to BMD
  measurement date. If \`FALSE\` (default), precomputed grouping
  variables are used.

## Value

A \`ggplot2\` boxplot object showing the distribution of \`TSCORE\`
across selected groups.

## Details

If \`date_dependency = TRUE\`, exposure/response and outcome group
membership are recalculated based on whether the event date occurred
before or after the BMD measurement date (\`DATE\`).

The plot includes boxplots for:

- Exposure group

- Response group

- Hip fracture group

- Any fracture group

- Osteoporosis group

Only observations belonging to a given group (indicator value = 1) are
included in that boxplot.

## Examples

``` r
if (FALSE) { # \dontrun{
# Static grouping
p <- plot_bmd_boxplot(data_bmd, data_dpop)
print(p)

# Date-dependent grouping
p <- plot_bmd_boxplot(data_bmd, data_dpop, date_dependency = TRUE)
print(p)
} # }
```
