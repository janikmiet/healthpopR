#' Recode diagnosis code to ICD-10 three-letter format
#'
#' Extracts the first three characters from an ICD-10 diagnosis code if the coding system is ICD-10.
#'
#' @param DG A character vector of diagnosis codes.
#' @param DGREG A character scalar or vector indicating the diagnosis coding system. Expected value is "ICD10".
#'
#' @return A character vector of the first three letters of ICD-10 codes, or `NA` if `DGREG` is not "ICD10".
#'
#' @examples
#' diagnoses <- diagnoses_raw |>
#'  dplyr::mutate(
#'    ICD10_3LETTERS = recode_icd10_3letters(DG, DGREG = DGREG),
#'  )
#'
#' @export
recode_icd10_3letters <- function(DG, DGREG = DGREG){
  ifelse(DGREG == "ICD10", substr(DG, 1, 3), NA)
}
