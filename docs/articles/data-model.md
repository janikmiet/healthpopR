# Data Model

## Description

`healthpopR` functions uses follow up datasets, which have fixed
variable names. This document describes variables and names. Here you
can get idea of the shape of the datasets needed.

## population

Population data should include only one row per ID and few DATE
variables.

| Variable Name  | Description                                   |
|:---------------|:----------------------------------------------|
| ID             | Patient identifier                            |
| DATE_BIRTH     | Date of patient birth                         |
| DATE_DEATH     | Date of when patient died or NA               |
| DATE_MIGRATION | Date of when patient migrated to abroad or NA |

## diagnoses

Diagnoses data is follow-up of the patients diagnoses over the timeline.

| Variable Name | Description |
|:---|:---|
| ID | Patient identifier |
| DGREG | Diagnose Code Source (ICD10, ICD9 or ICD8) |
| SRC | Registry source (ex. hilmo, avohilmo etc.) |
| DATE | Date of the diagnose |
| DG | Full Diagnose Code (ex. E11, H903, H0410) |
| ICD10_3LETTERS | **Only with ICD10 code.** Diagnose Code in first 3 letters |
| ICD10_CLASS | **Only with ICD10 code.** ICD-10 Diagnose Class (Ex. E00–E90, H60–H95, H00–H59) |
| DATE_BIRTH | Day of patient birth |
| AGE | **Only with murtumat** Patient age on diagnose date |

## other datasets

**These will be modified in future. Now these are used only in cox
modelling phases**

### data_sosioeconomic

Data Sosioeconomic includes few extra variables for the population data.

| Variable Name | Description                        |
|:--------------|:-----------------------------------|
| ID            | Patient identifier                 |
| edu           | education in factor leveling       |
| bmi           | Body Mass Index                    |
| bmi_cat1      | Body Mass Index in factor leveling |

### data_dates

Data dates should include more date variables for the population.

| Variable Name | Description |
|:---|:---|
| ID | Patient identifier |
| vpvmbl | Baseline answering date |
| spvm to mpvmfps3y | Multiple follow-up timepoints (exact definitions to be documented) |
