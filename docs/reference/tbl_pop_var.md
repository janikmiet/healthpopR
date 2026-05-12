# Count Population Variable Categories by Group

Filters a dataset by exposure or response group and counts the
occurrences of a selected population variable.

## Usage

``` r
tbl_pop_var(data, group, var)
```

## Arguments

- data:

  A data frame containing at minimum:

  ID

  :   Patient identifier

  exp.GROUP

  :   Exposure group classification

  resp.GROUP

  :   Response group classification

- group:

  Character string defining which subgroup to use. Must be either:

  - \`"exposure"\`

  - \`"response"\`

- var:

  Character string giving the variable name from
  \`population_variables\` to summarize.

## Value

A tibble with:

- \<var\>:

  Levels/categories of the selected variable

- n:

  Count of observations in each category

## Details

The function joins the filtered dataset with the
\`population_variables\` table by \`ID\` and returns category counts for
the specified variable.

The function uses non-standard evaluation via
[`rlang::sym()`](https://rlang.r-lib.org/reference/sym.html) to
dynamically count the selected variable.

The object \`population_variables\` must exist in the environment and
contain the requested variable and an \`ID\` column.

## See also

[`count`](https://dplyr.tidyverse.org/reference/count.html),
[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)

## Examples

``` r
if (FALSE) { # \dontrun{
tbl_pop_var(
  data = dpop,
  group = "exposure",
  var = "SEX"
)

tbl_pop_var(
  data = dpop,
  group = "response",
  var = "BMI_CLASS"
)
} # }
```
