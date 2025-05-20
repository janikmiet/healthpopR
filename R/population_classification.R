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
                                registry_source = c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
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
    registry_source = registry_source,
    data_diagnoses = data_diagnoses
  )
  # safe_inc_progress(1 / 4)

  ## Phase 2: Individuals with diagnosis
  d1 <- dat |>
    dplyr::arrange(ID, DATE) |>
    dplyr::group_by(ID) |>
    dplyr::summarise(
      DATE = first(DATE),
      SRC = first(SRC),
      DGREG = first(DGREG),
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


#' Create Exposure and Optional Response Groups from Diagnoses
#'
#' This function identifies individuals with exposure diagnoses (and optionally response diagnoses)
#' based on ICD-10/9/8 codes and selected registry sources. It returns a dataset of grouped individuals
#' with exposure (and optionally response) classifications, diagnosis dates, and relevant metadata.
#' Function searches first diagnose date, which is given for exposure/response diagnose date.
#'
#' @param exposure_icd10 A character vector of ICD-10 codes (regex-supported) to identify exposure group.
#' @param exposure_icd9 A character vector of ICD-9 codes (regex-supported) to identify exposure group.
#' @param exposure_icd8 A character vector of ICD-8 codes (regex-supported) to identify exposure group.
#' @param exposure_src A character vector of registry sources used to search for exposure diagnoses.
#'                     Defaults to all available sources.
#' @param response_icd10 Optional character vector of ICD-10 codes for response group (default: NULL).
#' @param response_icd9 Optional character vector of ICD-9 codes for response group (default: NULL).
#' @param response_icd8 Optional character vector of ICD-8 codes for response group (default: NULL).
#' @param response_src A character vector of registry sources used to search for response diagnoses.
#'                     Defaults to all available sources.
#' @param data_population A data frame of the target population (default: `population`).
#' @param data_diagnoses A data frame of diagnoses (default: `diagnoses`).
#' @param runtime_shiny Logical; if TRUE and run inside a Shiny app, shows progress bar (default: FALSE).
#'
#' @return A data frame with exposure (and optionally response) group classifications and metadata,
#'         including ID, birth/death/migration dates, diagnosis info, and binary indicators for exposure/response.
#'
#' @examples
#' classify_population(exposure_icd10 = "I21", exposure_src = "hilmo")
#' classify_population(exposure_icd10 = "F32", response_icd10 = "I21")
#'
#' @export


## Population exposure and response
classify_population <- function(exposure_icd10 = "",
                                exposure_icd9 = "",
                                exposure_icd8 = "",
                                exposure_src = c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
                                response_icd10 = NULL,
                                response_icd9 = NULL,
                                response_icd8 = NULL,
                                response_src = c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
                                data_population = population,
                                data_diagnoses = diagnoses,
                                runtime_shiny = FALSE) {

  all <- function() {
    ## Clean input
    exposure_icd10 <- .regex_clean(exposure_icd10)
    exposure_icd9 <- .regex_clean(exposure_icd9)
    exposure_icd8 <- .regex_clean(exposure_icd8)

    has_response <- !is.null(response_icd10) || !is.null(response_icd9) || !is.null(response_icd8)

    if (has_response) {
      response_icd10 <- .regex_clean(response_icd10)
      response_icd9 <- .regex_clean(response_icd9)
      response_icd8 <- .regex_clean(response_icd8)
    }

    .safe_inc_progress(1/4)

    ## Exposure group
    exp <- .group_by_diagnosis(regex_icd10 = exposure_icd10,
                               regex_icd9 = exposure_icd9,
                               regex_icd8 = exposure_icd8,
                               registry_source = exposure_src,
                               groups = c("exposure", "no exposure"),
                               data_population = data_population,
                               data_diagnoses = data_diagnoses) |>
      dplyr::rename_with(~ paste0("exp.", .)) |>
      dplyr::rename(ID = exp.ID,
             DATE_BIRTH = exp.DATE_BIRTH,
             DATE_DEATH = exp.DATE_DEATH,
             DATE_MIGRATION = exp.DATE_MIGRATION) |>
      dplyr::select(ID, DATE_BIRTH, DATE_DEATH, DATE_MIGRATION, exp.DATE, exp.SRC, exp.DGREG, exp.GROUP, exp.AGE_DG)

    .safe_inc_progress(2/4)

    if (has_response) {
      ## Response group
      resp <- .group_by_diagnosis(regex_icd10 = response_icd10,
                                  regex_icd9 = response_icd9,
                                  regex_icd8 = response_icd8,
                                  registry_source = response_src,
                                  groups = c("response", "no response"),
                                  data_population = data_population,
                                  data_diagnoses = data_diagnoses) |>
        dplyr::select(-DATE_BIRTH, -DATE_DEATH, -DATE_MIGRATION) |>
        dplyr::rename_with(~ paste0("resp.", .)) |>
        dplyr::rename(ID = resp.ID) |>
        dplyr::select(ID, resp.DATE, resp.SRC, resp.DGREG, resp.GROUP, resp.AGE_DG)

      .safe_inc_progress(3/4)

      ## Join
      d <- exp |>
        dplyr::left_join(resp, by = "ID") |>
        dplyr::mutate(
          exposure = ifelse(!is.na(exp.DATE), 1, 0),
          response = ifelse(!is.na(resp.DATE), 1, 0)
        )

    } else {
      d <- exp |>
        dplyr::mutate(
          exposure = ifelse(!is.na(exp.DATE), 1, 0)
        )
    }

    .safe_inc_progress(4/4)
    return(d)
  }

  if (runtime_shiny && shiny::isRunning()) {
    withProgress(message = "Creating Grouped Population", value = 0, {
      all()
    })
  } else {
    all()
  }
}

