# Safely Increment Shiny Progress Bar

A helper function that safely increments a Shiny progress bar only when
the Shiny app is running and within a reactive context.

## Usage

``` r
.safe_inc_progress(amount = 1)
```

## Arguments

- amount:

  Numeric value indicating how much to increment the progress bar by.
  Default is 1.

## Value

No return value; called for its side effect of updating the progress
bar.

## Details

This function checks whether the Shiny application is running and
whether there is a valid reactive domain before calling
\`shiny::incProgress()\`. It prevents errors when the progress bar is
used outside of a reactive environment or during testing/non-interactive
use.

## See also

\[shiny::incProgress()\]

## Examples

``` r
if (FALSE) { # \dontrun{
withProgress(message = "Loading...", {
  for (i in 1:10) {
    safe_inc_progress(0.1)
    Sys.sleep(0.1)
  }
})
} # }
```
