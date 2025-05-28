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
#' @source ICD8 Codes from https://www.julkari.fi/handle/10024/135324, ICD9 Codes from https://www.julkari.fi/handle/10024/131850, and ICD10 Codes from https://koodistopalvelu.kanta.fi/codeserver/pages/classification-view-page.xhtml?classificationKey=23
"data_codes"
