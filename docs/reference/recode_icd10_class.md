# Recode ICD-10 3-letter codes into broad classification groups

Maps 3-letter ICD-10 codes into standard ICD-10 chapter ranges (e.g.,
"A00–B99", "C00–D48").

## Usage

``` r
recode_icd10_class(ICD10_3LETTERS, DGREG = DGREG)
```

## Arguments

- ICD10_3LETTERS:

  A character vector of 3-letter ICD-10 codes.

- DGREG:

  A character scalar or vector indicating the diagnosis coding system.
  Expected value is "ICD10".

## Value

A character vector indicating the ICD-10 classification range, or \`NA\`
if \`DGREG\` is not "ICD10".

## Examples

``` r
diagnoses <- diagnoses_raw |>
 dplyr::mutate(
   ICD10_3LETTERS = recode_icd10_3letters(DG, DGREG = DGREG),
   ICD10_CLASS = recode_icd10_class(ICD10_3LETTERS, DGREG = DGREG)
 )
#> Error: object 'diagnoses_raw' not found
```
