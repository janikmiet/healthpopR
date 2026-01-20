# Capitalize the First Letter of a String

This function takes a character string and returns the same string with
the first letter converted to uppercase and the remaining letters
converted to lowercase.

## Usage

``` r
.capitalize(s)
```

## Arguments

- s:

  A character string to be capitalized.

## Value

A character string with the first letter capitalized and the rest in
lowercase.

## Examples

``` r
.capitalize("hello")   # "Hello"
#> Error in .capitalize("hello"): could not find function ".capitalize"
.capitalize("WORLD")   # "World"
#> Error in .capitalize("WORLD"): could not find function ".capitalize"
.capitalize("rStuDio") # "Rstudio"
#> Error in .capitalize("rStuDio"): could not find function ".capitalize"
```
