#' Summary Table of Diagnoses by Group
#'
#' Generates a summary table of diagnoses within a specified group (`"exposure"` or `"response"`),
#' including counts of unique patients, total diagnosis cases, percentage of the group, and diagnosis descriptions.
#' Diagnoses with fewer than a threshold number of patients are combined into a single "Rest of the diagnoses" row
#' to comply with data protection principles.
#'
#' @param data A data frame containing at least the columns: `ID`, `DG`, `DGREG`.
#' @param group Character string. Either `"exposure"` or `"response"`. Determines the diagnosis group to summarize. Defaults to `"exposure"`.
#' @param sum_small_groups Integer. Diagnoses with fewer patients than this threshold are grouped into a single summary row. Must be between 2 and 9999. Default is 6.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{DG}{Diagnosis group code}
#'   \item{DGREG}{Diagnosis registry group}
#'   \item{patients}{Number of unique patients in the group with the diagnosis}
#'   \item{cases}{Total number of diagnosis entries}
#'   \item{group_pct}{Percentage of the group with the diagnosis}
#'   \item{DESC}{Diagnosis description}
#' }
#'
#' @details
#' If called within a Shiny app, progress indicators will be displayed using `withProgress()`.
#' The function uses `.safe_inc_progress()` and `.capitalize()`, which must be defined elsewhere in the package.
#' The lookup table `data_codes` must be available in the environment and must include columns `DG` and `DESC`.
#'
#' @import dplyr
#' @importFrom shiny isRunning withProgress
#' @export
#'
#' @examples
#' \dontrun{
#' table_summary_diagnoses(data = exposure_diagnoses, group = "exposure", sum_small_groups = 6)
#' }
table_summary_diagnoses <- function(data, group = "exposure", sum_small_groups=6){
  ## Creates a summary of selected diagnoses on the group (exposure/response)
  ## and gives summary information (Cases, Patients, PCT, Desc)

  if(FALSE){
    data = exposure_diagnoses
    group = "exposure"
    sum_small_groups=6
  }

  ## Checking arguments
  if (!group %in% c("exposure", "response")) {
    stop(paste("Variable 'group' should be 'exposure' or 'response'."))
  }
  if (!dplyr::between(sum_small_groups, 2, 9999)) {
    stop(paste("Variable 'sum_small_groups' should be between 2 to 9999."))
  }

  all <- function(){
    .safe_inc_progress(1/3)

    koehenkiloita <- length(unique(data$ID))

    if (koehenkiloita == 0) {
      return(data.frame(DG = "No data", DGREG = "No data", patients = 0, cases = 0, group_pct = NA, DESC = "No data"))
    }

    ## Sums of DG DGREG
    d <- data |>
      dplyr::group_by(DG, DGREG) |>
      dplyr::summarise(
        patients = length(unique(ID)),
        cases = dplyr::n()
      ) |>
      dplyr::mutate(
        group_pct = round(100 * patients / koehenkiloita, 1)
      ) |>
      dplyr::left_join(data_codes, by = "DG") |>
      dplyr::select(DG, DGREG, patients, cases, group_pct, DESC)

    .safe_inc_progress(2/3)

    ## Combine small diagnose groups
    ## Tietosuoja alle 6 tapaukset
    d <- d |>
      dplyr::filter(patients >= sum_small_groups) |>
      rbind(
        d |>
          dplyr::filter(patients < sum_small_groups) |>
          dplyr::summarise(
            DG = "XX",
            DGREG = "XX",
            patients = sum(patients),
            cases = sum(cases),
            group_pct = NA,
            DESC = "Rest of the diagnoses"
          ) |>
          dplyr::group_by(DG, DGREG) |>
          dplyr::summarise(
            patients = sum(patients),
            cases = sum(cases)
          ) |>
          dplyr::mutate(
            group_pct = round(100 * patients / koehenkiloita, 1)
          )
      )

    .safe_inc_progress(3/3)

    return(d)
  }

  if(shiny::isRunning()){
    withProgress(message = paste0("Table of ", .capitalize(group), " Diagnoses"), value = 0, {
      return(all())
    })
  }else{
    return(all())
  }
}
