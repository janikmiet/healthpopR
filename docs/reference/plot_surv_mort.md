# Plot cumulative incidence of mortality and exposure states

Estimates cumulative incidence curves for mortality before and after
exposure using competing risks and multistate survival models. The
function constructs follow-up time variables, models exposure as a
time-dependent event, and produces a publication-ready cumulative
incidence plot.

## Usage

``` r
plot_surv_mort(
  dpop,
  censoring_date,
  lines = c("Death after exposure", "Death without exposure", "Overall mortality",
    "Ever exposed", "Total mortality among exposed")
)
```

## Arguments

- dpop:

  Data frame containing the study population and follow-up information.

- censoring_date:

  Date. Administrative censoring date for follow-up.

- lines:

  Character vector specifying which curves to include in the plot.
  Default options are:

  - \`"Death after exposure"\`

  - \`"Death without exposure"\`

  - \`"Overall mortality"\`

  - \`"Ever exposed"\`

  - \`"Total mortality among exposed"\`

## Value

A list containing:

- \`data\` Data frame used for plotting cumulative incidence curves.

- \`plot\` A \`ggplot2\` object containing the cumulative incidence
  plot.

## Details

Follow-up starts at \`resp.DATE\` and ends at the earliest of: death,
migration, or the administrative censoring date.

Exposure is treated as a time-dependent event using
\`survival::tmerge\`. The function estimates:

\* Competing risk models for exposure and death before exposure \*
Multistate models for death after exposure

When events occur at identical time points (e.g., exposure and death on
the same day), small time offsets are introduced to avoid numerical
issues in survival estimation.

The resulting curves represent cumulative incidence for:

\* Death without exposure \* Death after exposure \* Overall mortality
\* Exposure incidence

## Required variables in \`dpop\`

\| Variable \| Description \| \|——–\|————-\| \| \`ID\` \| Unique
individual identifier \| \| \`resp.DATE\` \| Start of follow-up
(index/response date) \| \| \`exp.DATE\` \| Exposure date \| \|
\`DATE_DEATH\` \| Date of death \| \| \`DATE_MIGRATION\` \| Date of
emigration or loss to follow-up \|

## Examples

``` r
if (FALSE) { # \dontrun{
res <- plot_surv_mort(
  dpop = population_data,
  censoring_date = as.Date("2023-12-31")
)

# Show plot
res$plot

# Access plotting data
head(res$data)
} # }
```
