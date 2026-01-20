# Package index

## Main functions

- [`classify_population()`](https://janikmiet.github.io/healthpopR/reference/classify_population.md)
  : Create Exposure and Optional Response Groups from Diagnoses
- [`classify_icd10_profile()`](https://janikmiet.github.io/healthpopR/reference/classify_icd10_profile.md)
  : Classify ICD-10 Profiles by Exposure Group
- [`search_diagnoses()`](https://janikmiet.github.io/healthpopR/reference/search_diagnoses.md)
  : Search Diagnoses by ICD Version and Source
- [`plot_age_distribution()`](https://janikmiet.github.io/healthpopR/reference/plot_age_distribution.md)
  : Plot Age Distribution at First Diagnosis for Exposure or Response
  Group
- [`plot_diagnoses_src()`](https://janikmiet.github.io/healthpopR/reference/plot_diagnoses_src.md)
  : Plot Venn Diagram of Diagnoses by Source
- [`summary_exp_resp_crosstabulation()`](https://janikmiet.github.io/healthpopR/reference/summary_exp_resp_crosstabulation.md)
  : Generate Crosstabulation of Exposure and Response Variables
- [`summary_exp_resp_order()`](https://janikmiet.github.io/healthpopR/reference/summary_exp_resp_order.md)
  : Summary of Exposure and Response Timing Order
- [`table_age_distribution()`](https://janikmiet.github.io/healthpopR/reference/table_age_distribution.md)
  : Summarize Age Distribution for Exposure or Response Groups

## Diagnoses

- [`search_diagnoses()`](https://janikmiet.github.io/healthpopR/reference/search_diagnoses.md)
  : Search Diagnoses by ICD Version and Source
- [`plot_diagnoses_src()`](https://janikmiet.github.io/healthpopR/reference/plot_diagnoses_src.md)
  : Plot Venn Diagram of Diagnoses by Source
- [`table_summary_diagnoses()`](https://janikmiet.github.io/healthpopR/reference/table_summary_diagnoses.md)
  : Summary Table of Diagnoses by Group

## Health Analysis functions

- [`classify_icd10_profile()`](https://janikmiet.github.io/healthpopR/reference/classify_icd10_profile.md)
  : Classify ICD-10 Profiles by Exposure Group
- [`plot_health_icd10_profile()`](https://janikmiet.github.io/healthpopR/reference/plot_health_icd10_profile.md)
  : Plot Health ICD-10 Profile Using Radar Chart
- [`tbl_icd10_diff_by_exposure()`](https://janikmiet.github.io/healthpopR/reference/tbl_icd10_diff_by_exposure.md)
  : Compare ICD-10 Diagnoses Between Exposure Groups
- [`plot_icd10_diff_by_exposure()`](https://janikmiet.github.io/healthpopR/reference/plot_icd10_diff_by_exposure.md)
  : Bar Plot of ICD-10 Diagnosis Differences Between Exposure Groups

## Survival Analysis functions

- [`create_dsurv()`](https://janikmiet.github.io/healthpopR/reference/create_dsurv.md)
  : Create Long-Format Survival Data from Exposure-Response Events

- [`plot_survival_cr()`](https://janikmiet.github.io/healthpopR/reference/plot_survival_cr.md)
  : Plot Competing Risks Survival Curve (Exposure to Response or Death)

- [`plot_survival_km()`](https://janikmiet.github.io/healthpopR/reference/plot_survival_km.md)
  : Plot Kaplan-Meier Survival Curve (Exposure to Response)

- [`plot_survival_death()`](https://janikmiet.github.io/healthpopR/reference/plot_survival_death.md)
  : Plot Survival Curve for Mortality Analysis

- [`df_dates_per_id()`](https://janikmiet.github.io/healthpopR/reference/df_dates_per_id.md)
  : Construct per-ID date dataset with follow-up starting at age 50

- [`df_dates_ftime()`](https://janikmiet.github.io/healthpopR/reference/df_dates_ftime.md)
  : Calculate follow-up time and event status from age 50

- [`extract_ci_all()`](https://janikmiet.github.io/healthpopR/reference/extract_ci_all.md)
  :

  Extract all cumulative incidence curves from a `cuminc` object

## Cox Model Analysis

- [`cox_create_data()`](https://janikmiet.github.io/healthpopR/reference/cox_create_data.md)
  : Create Time-to-Event Data for Cox Proportional Hazards Modeling
- [`create_cox_model()`](https://janikmiet.github.io/healthpopR/reference/create_cox_model.md)
  : Create a Cox Proportional Hazards Model with Splines and Covariates
- [`cox_plot_overall()`](https://janikmiet.github.io/healthpopR/reference/cox_plot_overall.md)
  : Plot Survival Curves from Cox Model Data
- [`cox_plot_spline()`](https://janikmiet.github.io/healthpopR/reference/cox_plot_spline.md)
  : Plot Spline Effect from Cox Proportional Hazards Model

## Poisson Regression Model (SIR)

- [`pirr_data()`](https://janikmiet.github.io/healthpopR/reference/pirr_data.md)
  : Prepare Person-Time and Event Data for SIR/IRR Calculations
- [`pirr_results()`](https://janikmiet.github.io/healthpopR/reference/pirr_results.md)
  : Run PIRR-style modeling and summary plots for multiple binary
  outcomes

## Helper functions

- [`.regex_clean()`](https://janikmiet.github.io/healthpopR/reference/dot-regex_clean.md)
  : Clean Diagnosis Regex Strings
- [`.capitalize()`](https://janikmiet.github.io/healthpopR/reference/dot-capitalize.md)
  : Capitalize the First Letter of a String
- [`.relevel_by_reference()`](https://janikmiet.github.io/healthpopR/reference/dot-relevel_by_reference.md)
  : Relevel Factor Variables by Reference
- [`.safe_inc_progress()`](https://janikmiet.github.io/healthpopR/reference/dot-safe_inc_progress.md)
  : Safely Increment Shiny Progress Bar

## Data Handling

- [`recode_icd10_3letters()`](https://janikmiet.github.io/healthpopR/reference/recode_icd10_3letters.md)
  : Recode diagnosis code to ICD-10 three-letter format
- [`recode_icd10_class()`](https://janikmiet.github.io/healthpopR/reference/recode_icd10_class.md)
  : Recode ICD-10 3-letter codes into broad classification groups
- [`categorize_bmi()`](https://janikmiet.github.io/healthpopR/reference/categorize_bmi.md)
  : Categorize BMI values into clinical weight categories

## Included Datasets

- [`data_codes`](https://janikmiet.github.io/healthpopR/reference/data_codes.md)
  : Diagnoses Code Descriptions
