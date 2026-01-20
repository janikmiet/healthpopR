# Search Diagnoses by ICD Version and Source

Retrieves diagnosis records from a dataset based on ICD-10, ICD-9, or
ICD-8 codes and selected data sources.

## Usage

``` r
search_diagnoses(
  regex_icd10 = "",
  regex_icd9 = "",
  regex_icd8 = "",
  registry_source = c(""),
  regex_extra = "",
  src_extra = "",
  data_diagnoses = diagnoses
)
```

## Arguments

- regex_icd10:

  character. Regular expression pattern for ICD-10 diagnoses.

- regex_icd9:

  character. Regular expression pattern for ICD-9 diagnoses.

- regex_icd8:

  character. Regular expression pattern for ICD-8 diagnoses.

- registry_source:

  character vector. Data sources to include (e.g., `"avohilmo"`,
  `"hilmo"`).

- regex_extra:

  character. Optional regular expression for other diagnosis formats
  (currently not implemented).

- src_extra:

  character. Optional source for extra diagnoses (currently not
  implemented).

- data_diagnoses:

  data.frame. Diagnosis dataset to search. Must include columns: `ID`,
  `DGREG`, `SRC`, `DATE`, `DG`, `ICD10_CLASS`, `ICD10_3LETTERS`, `AGE`.

## Value

A `tibble` containing matched diagnosis records sorted by ID, registry,
and date.

## Examples

``` r
if (FALSE) { # \dontrun{
  search_diagnoses(regex_icd10 = "^I2", registry_source = c("hilmo", "avohilmo"), data_diagnoses = diagnoses)
} # }
```
