# healthpopR 0.2.5

## Breaking changes

* New Survival Analysis Function 

* Old survival Analysis Functions will be depricated in future

## New features

* Survival Analysis function creates survival analysis according start-date. Function returns a plots and data in a list.

## Minor bug fixes and improvements

* AGE-variable is not anymore used from the diagnoses-dataset.


# healthpopR 0.2.2

## Breaking changes

* Survival and Poisson regression functions updated

## New features

* Poisson function pirr_data() now works a bit differently. This is due to upcoming changes to create report. 

* Survival data creation function 'create_dsurv' creates now either exposure to response or response to death models.

## Minor bug fixes and improvements

# healthpopR 0.1.8

## Breaking changes

* First documented and working release of package.

## New features

* A comprehensive overview of the data model is now available in the [Data Model](articles/data-model.html) vignette. This section explains the required data format and how to organize datasets.

* The pre-data-wrangling process is described in the [Data Handling](articles/data-handling.html) section.

* The package now includes ICD code datasets (ICD-8, ICD-9, and ICD-10) used in Finland. These datasets provide descriptions for each code. Note that the Finnish ICD-9 codes differ slightly from international standards.

* Population classification functionality has been introduced to group populations effectively.

* New population analysis functions allow datasets to be classified into exposure and response groups.

* Health analysis functions have been added to examine follow-up diagnosis data (ICD-10) and derive general insights.

* Survival analysis functions now enable analysis of survival to response events or death.

* Cox analysis functions have been included to explore how variables like BMI, age, and smoking status influence response diagnoses.

* Poisson analysis functions have been introduced. These are still undergoing refinement, as they are currently somewhat hard-coded to a specific follow-up study and need further generalization.


## Minor bug fixes and improvements

