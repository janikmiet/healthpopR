#' Compare ICD-10 Diagnoses Between Exposure Groups
#'
#' Calculates and compares the prevalence of top ICD-10 diagnosis codes (3-letter level)
#' between exposure and non-exposure populations. Designed to highlight diagnostic differences
#' across groups while excluding the exposure-defining ICD-10 codes.
#'
#' @param data A data frame with at least `ID` and `exp.GROUP` columns, where `exp.GROUP`
#'   should include `"exposure"` and `"no exposure"` as values.
#' @param diagnoses A data frame containing patient diagnosis records. Must include at least
#'   the columns: `ID`, `DGREG`, `DG`, `ICD10_3LETTERS`, and `SRC`.
#' @param exposure_icd10 A regular expression string defining the ICD-10 pattern used to classify
#'   exposure. Diagnoses matching this pattern will be excluded from analysis.
#' @param exposure_src A character vector of source systems (e.g., `c("hilmo", "avohilmo")`)
#'   to include in the diagnosis filtering.
#'
#' @return A data frame summarizing:
#' \describe{
#'   \item{ICD10_3LETTERS}{ICD-10 3-letter code}
#'   \item{total_patients}{Total patients across both exposure groups with this diagnosis}
#'   \item{exposure_group_patients}{Number of patients in the exposure group with this diagnosis}
#'   \item{exposure_group_pct}{Percent of the exposure group with this diagnosis}
#'   \item{no_exposure_group_patients}{Number of patients in the no-exposure group with this diagnosis}
#'   \item{no_exposure_group_pct}{Percent of the no-exposure group with this diagnosis}
#'   \item{diff_pct}{Difference in percentage points between groups (exposure minus no exposure)}
#'   \item{DESC}{ICD-10 code description (from `data_codes`)}
#' }
#'
#' @details
#' This function is useful for summarizing diagnostic differences between exposed and unexposed groups.
#' It excludes diagnoses matching the exposure ICD-10 pattern to avoid circularity.
#'
#' Internally uses `.safe_inc_progress()` for progress tracking and integrates with Shiny's
#' `withProgress()` if called from a running Shiny session.
#'
#' **Note**: The `data_codes` object must be available in the global environment or within the package,
#' and must contain `CODECLASS == "ICD10"`, `DG`, and `DESC` columns.
#'
#' @examples
#' \dontrun{
#' tbl <- tbl_icd10_diff_by_exposure(
#'   data = dpop,
#'   diagnoses = diagnoses,
#'   exposure_icd10 = "^E11",
#'   exposure_src = c("hilmo", "avohilmo")
#' )
#' head(tbl)
#' }
#'
#' @importFrom dplyr filter group_by summarise mutate left_join select
#' @importFrom shiny isRunning withProgress
#' @export
tbl_icd10_diff_by_exposure <- function(data, diagnoses, exposure_icd10, exposure_src){

  internal_function <- function(){
    # data <- dpop
    # data_diagnoses <- diagnoses
    # exposure_icd10 <- "^E11"
    # exposure_src <- c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")

    healthpopR::.safe_inc_progress(1/4)

    # data_diagnoses <- diagnoses

    # Tarkastellaan TOP diagnoosit exposure populaatiolla
    exposure_ids <- unique(data$ID[data$exp.GROUP == "exposure"])
    n_exposure    <- length(exposure_ids)
    regex_icd10   <- .regex_clean(exposure_icd10)
    d1 <- diagnoses |>
      dplyr::filter(
        DGREG == "ICD10" & SRC  %in% exposure_src,
        ID %in% exposure_ids,
        !grepl(regex_icd10, DG)
      ) |>
      dplyr::count(ICD10_3LETTERS, name = "exposure_group_patients") |>
      dplyr::mutate(
        exposure_group_pct =
          round(100 * exposure_group_patients / n_exposure, 1)
      )

    healthpopR::.safe_inc_progress(2/4)

    # Tarkastellaan TOP diagnoosit no-exposure populaatiolla
    no_exposure_ids <- unique(data$ID[data$exp.GROUP == "no exposure"])
    n_no_exposure   <- length(no_exposure_ids)

    d2 <- diagnoses |>
      dplyr::filter(
        DGREG == "ICD10",
        SRC %in% exposure_src,
        ID %in% no_exposure_ids
      ) |>
      dplyr::count(
        ICD10_3LETTERS,
        name = "no_exposure_group_patients"
      ) |>
      dplyr::mutate(
        no_exposure_group_pct =
          round(100 * no_exposure_group_patients / n_no_exposure, 1)
      )

    healthpopR::.safe_inc_progress(3/4)

    ## Joind
    d <- dplyr::left_join(d1,d2, by = "ICD10_3LETTERS") |>
      dplyr::mutate(diff_pct = exposure_group_pct - no_exposure_group_pct,
                    total_patients = exposure_group_patients + no_exposure_group_patients) |>
      dplyr::left_join(
        healthpopR::data_codes |> dplyr::filter(CODECLASS == "ICD10") |>
          dplyr::select(DG, DESC), by = c("ICD10_3LETTERS" = "DG")) |>
      dplyr::select(ICD10_3LETTERS, total_patients, exposure_group_patients, exposure_group_pct, no_exposure_group_patients, no_exposure_group_pct, diff_pct, DESC )

    healthpopR::.safe_inc_progress(4/4)

    return(d)
  }
  if(shiny::isRunning()){
    withProgress(message = "Health ICD-10 Comparison", value = 0, {
      return(internal_function())
    })
  }else{
    return(internal_function())
  }
}
