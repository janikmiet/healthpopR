# Diagnoses Code Descriptions

A dataset containing mappings of diagnosis codes to their descriptions.

## Usage

``` r
data_codes
```

## Format

A data frame with columns:

- DG:

  Diagnosis code

- CODECLASS:

  ICD Diagnose Class: ICD10, ICD9 or ICD8

- CATEGORY:

  Summary Level of Diagnose Category

- DESC:

  Description of the Diagnosis

## Source

ICD8 Codes from https://www.julkari.fi/handle/10024/135324, ICD9 Codes
from https://www.julkari.fi/handle/10024/131850, and ICD10 Codes from
https://koodistopalvelu.kanta.fi/codeserver/pages/classification-view-page.xhtml?classificationKey=23
