# Compare ICD-10 Diagnoses Between Exposure Groups

Calculates and compares the prevalence of top ICD-10 diagnosis codes
(3-letter level) between exposure and non-exposure populations. Designed
to highlight diagnostic differences across groups while excluding the
exposure-defining ICD-10 codes.

## Usage

``` r
tbl_icd10_diff_by_exposure(data, diagnoses, exposure_icd10, exposure_src)
```

## Arguments

- data:

  A data frame with at least \`ID\` and \`exp.GROUP\` columns, where
  \`exp.GROUP\` should include \`"exposure"\` and \`"no exposure"\` as
  values.

- diagnoses:

  A data frame containing patient diagnosis records. Must include at
  least the columns: \`ID\`, \`DGREG\`, \`DG\`, \`ICD10_3LETTERS\`, and
  \`SRC\`.

- exposure_icd10:

  A regular expression string defining the ICD-10 pattern used to
  classify exposure. Diagnoses matching this pattern will be excluded
  from analysis.

- exposure_src:

  A character vector of source systems (e.g., \`c("hilmo",
  "avohilmo")\`) to include in the diagnosis filtering.

## Value

A data frame summarizing:

- ICD10_3LETTERS:

  ICD-10 3-letter code

- total_patients:

  Total patients across both exposure groups with this diagnosis

- exposure_group_patients:

  Number of patients in the exposure group with this diagnosis

- exposure_group_pct:

  Percent of the exposure group with this diagnosis

- no_exposure_group_patients:

  Number of patients in the no-exposure group with this diagnosis

- no_exposure_group_pct:

  Percent of the no-exposure group with this diagnosis

- diff_pct:

  Difference in percentage points between groups (exposure minus no
  exposure)

- DESC:

  ICD-10 code description (from \`data_codes\`)

## Details

This function is useful for summarizing diagnostic differences between
exposed and unexposed groups. It excludes diagnoses matching the
exposure ICD-10 pattern to avoid circularity.

Internally uses \`.safe_inc_progress()\` for progress tracking and
integrates with Shiny's \`withProgress()\` if called from a running
Shiny session.

\*\*Note\*\*: The \`data_codes\` object must be available in the global
environment or within the package, and must contain \`CODECLASS ==
"ICD10"\`, \`DG\`, and \`DESC\` columns.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- tbl_icd10_diff_by_exposure(
  data = dpop,
  diagnoses = diagnoses,
  exposure_icd10 = "^E11",
  exposure_src = c("hilmo", "avohilmo")
)
head(tbl)
} # }
```
