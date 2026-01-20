# Relevel Factor Variables by Reference

Internal helper function to relevel specified factor variables in a data
frame using user-defined reference values.

## Usage

``` r
.relevel_by_reference(df, reference_values)
```

## Arguments

- df:

  A data frame containing the variables to be releveled.

- reference_values:

  A named list specifying the desired reference level for each variable.
  Names should match column names in \`df\`; values should be valid
  levels.

## Value

The original data frame with specified factor variables releveled to the
given reference. If a variable is not found in the data frame, a warning
is issued and the variable is skipped.

## Details

This function is typically used before modeling to ensure that
categorical variables have the appropriate reference level, particularly
when computing contrasts in regression models.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(group = factor(c("A", "B", "A", "C")))
ref_vals <- list(group = "B")
df <- .relevel_by_reference(df, ref_vals)
levels(df$group)  # "B" will now be the reference level
} # }
```
