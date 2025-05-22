#' Diagnoses Code Descriptions
#'
#' A dataset containing mappings of diagnosis codes to their descriptions.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{DG}{Diagnosis code}
#'   \item{CODECLASS}{ICD Diagnose Class: ICD10, ICD9 or ICD8}
#'   \item{CATEGORY}{Summary Level of Diagnose Category}
#'   \item{DESC}{Description of the Diagnosis}
#' }
#' @source Internal project curation / THL registry / etc.
"data_codes"
