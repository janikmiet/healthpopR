# Categorize BMI values into clinical weight categories

Categorizes BMI values into 4 or 6 clinical categories as ordered
factors, with "Healthy Weight" as the reference level.

## Usage

``` r
categorize_bmi(bmi, levels = 4)
```

## Arguments

- bmi:

  A numeric vector of BMI values.

- levels:

  Integer. Use 4 for: Underweight, Healthy Weight, Overweight, Obesity.
  Use 6 for detailed obesity classification.

## Value

An ordered factor with "Healthy Weight" as the reference level.

## Examples

``` r
df <- tibble::tibble(bmi = c(17, 22, 27, 32, 37, 42, NA))
df %>% dplyr::mutate(
  bmi_cat4 = categorize_bmi(bmi, levels = 4),
  bmi_cat6 = categorize_bmi(bmi, levels = 6)
)
#> Error in df %>% dplyr::mutate(bmi_cat4 = categorize_bmi(bmi, levels = 4),     bmi_cat6 = categorize_bmi(bmi, levels = 6)): could not find function "%>%"
```
