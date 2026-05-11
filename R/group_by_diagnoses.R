#' @title Group Population by Diagnosis
#' @description
#' Internal helper function to classify individuals in the population as having or not having a diagnosis
#' based on ICD codes and registry source. Returns a dataset with the earliest diagnosis date (if any) and group labels.
#'
#' @param regex_icd10 Character vector of ICD-10 diagnosis code patterns (regular expressions).
#' @param regex_icd9 Character vector of ICD-9 diagnosis code patterns (regular expressions).
#' @param regex_icd8 Character vector of ICD-8 diagnosis code patterns (regular expressions).
#' @param registry_source Character vector indicating which registries to use for diagnosis search.
#' @param groups Character vector of length two: names for group with diagnosis and without diagnosis (default: c("exposure", "no exposure")).
#' @param data_diagnoses Data frame of diagnoses with at least columns `ID`, `DATE`, `SRC`, and `DGREG`.
#' @param data_population Data frame of population data with at least columns `ID`, `DATE_BIRTH`, `DATE_DEATH`, and `DATE_MIGRATION`.
#' @param runtime_shiny Logical indicating whether the function is run inside a Shiny session (used to enable progress tracking).
#'
#' @return A data frame combining individuals with and without the diagnosis. Contains variables for diagnosis date, source, registry, group, and age at diagnosis.
#' Returns `NULL` if fewer than 6 individuals meet the diagnosis criteria.
#'
#' @keywords internal
#' @noRd
.group_by_diagnosis <- function(regex_icd10 = "",
                                regex_icd9 = "",
                                regex_icd8 = "",
                                registry_source = c(""),
                                age_range = c(0,120),
                                censoring_date="",
                                groups = c("exposure", "no exposure"),
                                data_diagnoses = diagnoses,
                                data_population = population,
                                runtime_shiny = FALSE) {
  # all <- function() {

  ## Phase 1: Find matching diagnoses
  dat <- search_diagnoses(
    regex_icd10 = .regex_clean(regex_icd10),
    regex_icd9 = .regex_clean(regex_icd9),
    regex_icd8 = .regex_clean(regex_icd8),
    age_range = age_range,
    censoring_date = censoring_date,
    registry_source = registry_source,
    data_diagnoses = data_diagnoses
  )
  # safe_inc_progress(1 / 4)

  ## Phase 2: Individuals with diagnosis
  d1 <- dat |>
    dplyr::arrange(ID, DATE) |>
    dplyr::group_by(ID) |>
    dplyr::summarise(
      DATE = dplyr::first(DATE),
      SRC = dplyr::first(SRC),
      DGREG = dplyr::first(DGREG),
      .groups = "drop"
    ) |>
    dplyr::left_join(population, by = "ID") |>
    dplyr::mutate(
      GROUP = groups[1],
      AGE_DG = trunc((DATE_BIRTH %--% DATE) / lubridate::years(1))
    )
  # safe_inc_progress(2 / 4)

  ## Phase 3: Individuals without diagnosis
  d2 <- data_population |>
    dplyr::filter(!ID %in% d1$ID) |>
    dplyr::mutate(
      GROUP = groups[2],
      AGE_DG = NA,
      DATE = NA,
      SRC = NA,
      DGREG = NA
    )
  # safe_inc_progress(3 / 4)

  ## Phase 4: Combine and return
  d <- rbind(d1, d2)
  # safe_inc_progress(4 / 4)

  if (nrow(d |> filter(GROUP == groups[1])) > 5) {
    return(d)
  } else {
    return(NULL)
  }
  # }

  # if (shiny::isRunning()) {
  #   withProgress(message = "Creating population data", value = 0, {
  #     return(all())
  #   })
  # } else {
  #   return(all())
  # }
}
