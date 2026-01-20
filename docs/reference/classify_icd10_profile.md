# Classify ICD-10 Profiles by Exposure Group

This function creates a summary table of ICD-10 diagnosis class
distributions across exposure groups, excluding diagnoses matching a
specified ICD-10 pattern (e.g., the exposure-defining diagnosis). It
calculates patient percentages per diagnosis class and reshapes the
result into a wide-format matrix suitable for plotting (e.g., radar
charts or heatmaps).

## Usage

``` r
classify_icd10_profile(
  data,
  diagnoses = diagnoses,
  exposure_icd10 = "",
  exposure_src = c("")
)
```

## Arguments

- data:

  A data frame containing the study population with at least columns
  \`ID\` and \`exp.GROUP\`, where \`exp.GROUP\` distinguishes between
  exposure and non-exposure groups. Can be created by
  classify_population() -function.

- diagnoses:

  A Original data frame of diagnoses that must include columns \`ID\`,
  \`DGREG\`, \`DG\`, \`SRC\`, and \`ICD10_CLASS\`.

- exposure_icd10:

  A regular expression string that defines the ICD-10 diagnosis used to
  define the exposure group. Diagnoses matching this pattern are
  excluded from the analysis.

- exposure_src:

  A character vector of source types (e.g., \`"avohilmo"\`, \`"hilmo"\`,
  etc.) to include in the filtering.

## Value

A wide-format data frame with ICD-10 class percentages by exposure
group. Includes artificial \`"Max"\` and \`"Min"\` rows (100 and 0) for
visualization purposes.

## Details

The function internally calculates: - Population sizes for each exposure
group - Diagnosis counts and patient counts per ICD-10 class -
Percentages of patients per group with each diagnosis class

If run inside a Shiny application, it will display progress bars using
\`withProgress()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
classify_icd10_profile(
  data = dpop,
  diagnoses = diagnoses,
  exposure_icd10 = "^E11",
  exposure_src = c("avohilmo", "hilmo", "erko", "local")
)
} # }
```
