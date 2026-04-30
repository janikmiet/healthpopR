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
#' @param response_icd10 Optional character vector of ICD-10 codes for response group (default: NULL).
#' @param response_icd9 Optional character vector of ICD-9 codes for response group (default: NULL).
#' @param response_icd8 Optional character vector of ICD-8 codes for response group (default: NULL).
#' @param response_src A character vector of registry sources used to search for response diagnoses.
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
classify_population <- function(exposure_icd10 = "", # TODO oisko tähän funktioon attribuutti: response_other = list(DGREG = "FRACTURES", DG = "hip")
                                exposure_icd9 = "", ## Population exposure and response
                                exposure_icd8 = "",
                                exposure_src = c(""),
                                response_icd10 = NULL,
                                response_icd9 = NULL,
                                response_icd8 = NULL,
                                response_src = c(""),
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
