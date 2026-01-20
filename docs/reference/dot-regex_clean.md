# Clean Diagnosis Regex Strings

Prepares diagnosis code regular expressions by removing spaces and
converting to uppercase.

## Usage

``` r
.regex_clean(dglist)
```

## Arguments

- dglist:

  character. A string or vector of diagnosis code patterns.

## Value

A character vector with all spaces removed and converted to uppercase.

## Examples

``` r
regex_clean("e11 ")
#> Error in regex_clean("e11 "): could not find function "regex_clean"
regex_clean(c(" i10", " E11 "))
#> Error in regex_clean(c(" i10", " E11 ")): could not find function "regex_clean"
```
