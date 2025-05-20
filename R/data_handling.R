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

#' Recode ICD-10 3-letter codes into broad classification groups
#'
#' Maps 3-letter ICD-10 codes into standard ICD-10 chapter ranges (e.g., "A00–B99", "C00–D48").
#'
#' @param ICD10_3LETTERS A character vector of 3-letter ICD-10 codes.
#' @param DGREG A character scalar or vector indicating the diagnosis coding system. Expected value is "ICD10".
#'
#' @return A character vector indicating the ICD-10 classification range, or `NA` if `DGREG` is not "ICD10".
#'
#' @examples
#' diagnoses <- diagnoses_raw |>
#'  dplyr::mutate(
#'    ICD10_3LETTERS = recode_icd10_3letters(DG, DGREG = DGREG),
#'    ICD10_CLASS = recode_icd10_class(ICD10_3LETTERS, DGREG = DGREG)
#'  )
#'
#' @export
recode_icd10_class <- function(ICD10_3LETTERS, DGREG = DGREG){
  dplyr::case_when(
    DGREG == "ICD10" & substr(ICD10_3LETTERS, 1,1 ) %in% c("A", "B")  ~ "A00–B99 ",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "C" | (substr(ICD10_3LETTERS, 1,1 ) == "D" & substr(ICD10_3LETTERS, 2,3 ) < 49 )) ~ "C00–D48",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "D" & substr(ICD10_3LETTERS, 2,3 ) > 49) ~ "D50–D89",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "E") ~ "E00–E90",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "F")  ~ "F00–F99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "G")  ~ "G00–G99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "H" & substr(ICD10_3LETTERS, 2,3 ) < 60) ~ "H00–H59",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "H" & substr(ICD10_3LETTERS, 2,3 ) > 59) ~ "H60–H95",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "I")  ~ "I00–I99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "J")  ~ "J00–J99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "K")  ~ "K00–K93",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "L")  ~ "L00–L99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "M")  ~ "M00–M99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "N")  ~ "N00–N99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "O")  ~ "O00–O99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "P")  ~ "P00–P96",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "Q")  ~ "Q00–Q99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "R")  ~ "R00–R99",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) %in% c("S", "T")) ~ "S00–T98",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) %in% c("V", "Y"))   ~ "V01–Y98",
    DGREG == "ICD10" & (substr(ICD10_3LETTERS, 1,1 ) == "Z")  ~ "Z00–ZZB",
    DGREG != "ICD10" ~ NA,
  )
}
