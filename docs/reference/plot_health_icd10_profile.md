# Plot Health ICD-10 Profile Using Radar Chart

Creates a radar chart visualization of ICD-10 diagnosis class
distributions across exposure groups. Typically used with the output of
\`classify_icd10_profile()\`.

## Usage

``` r
plot_health_icd10_profile(
  data,
  colors_exposure_groups = c("#5BC0DE", "#D9534F"),
  colors_exposure_groups_shade = c("#5BC0DE66", "#D9534F66")
)
```

## Arguments

- data:

  A data frame in wide format as returned by
  \`classify_icd10_profile()\`, with exposure group rows (e.g.,
  "exposure", "no exposure") and ICD-10 classes as columns. The first
  two rows must be artificial "Max" and "Min" rows with values 100 and 0
  for scaling the radar chart.

- colors_exposure_groups:

  A character vector of base colors (hex codes) used for the exposure
  groups. Defaults to \`c("#5BC0DE", "#D9534F")\`.

- colors_exposure_groups_shade:

  A character vector of semi-transparent versions of the base colors,
  used for the shaded area in the radar chart. Defaults to
  \`c("#5BC0DE66", "#D9534F66")\`.

## Value

No return value. The function creates a radar chart plot in the current
graphics device.

## Details

The function: - Uses the \`fmsb::radarchart()\` function to draw the
chart - Draws one polygon per exposure group with colored borders and
shaded fills - Adds a custom legend matching group names to colors

The chart is automatically displayed with a custom layout, axis labels,
and color styling. If called within a Shiny app, a progress bar is shown
using \`withProgress()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
data_final <- classify_icd10_profile(data = dpop, diagnoses = diagnoses,
                                     exposure_icd10 = "^E11",
                                     exposure_src = c("hilmo", "avohilmo"))
plot_health_icd10_profile(data_final)
} # }
```
