# Plot Venn Diagram of Diagnoses by Source

Creates a Venn diagram showing overlaps between diagnosis sources.
Optionally selects only the first source per ID or one per ID per
source.

## Usage

``` r
plot_diagnoses_src(data, per_source = FALSE)
```

## Arguments

- data:

  data.frame. The dataset containing diagnosis records with at least
  columns `ID`, `SRC`, and `DATE`. You can get this with function
  'search_diagnoses()'

- per_source:

  logical. If `FALSE` (default), uses only the first source per ID. If
  `TRUE`, includes one record per source per ID.

## Value

A ggplot object with a Venn diagram.
