# Recode diagnosis code to ICD-10 three-letter format

Extracts the first three characters from an ICD-10 diagnosis code if the
coding system is ICD-10.

## Usage

``` r
recode_icd10_3letters(DG, DGREG = DGREG)
```

## Arguments

- DG:

  A character vector of diagnosis codes.

- DGREG:

  A character scalar or vector indicating the diagnosis coding system.
  Expected value is "ICD10".

## Value

A character vector of the first three letters of ICD-10 codes, or \`NA\`
if \`DGREG\` is not "ICD10".

## Examples

``` r
diagnoses <- diagnoses_raw |>
 dplyr::mutate(
   ICD10_3LETTERS = recode_icd10_3letters(DG, DGREG = DGREG),
 )
#> Error: object 'diagnoses_raw' not found
```
