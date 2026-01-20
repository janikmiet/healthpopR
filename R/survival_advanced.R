#' Construct per-ID date dataset with follow-up starting at age 50
#'
#' Creates a person-level dataset containing exposure and response diagnosis
#' dates, demographic dates, and age-50 baseline. Exposure and response events
#' occurring before age 50 are set to \code{NA}. Follow-up is intended to start
#' from the 50th birthday.
#'
#' @param exposure_diagnoses A data frame containing exposure diagnoses with
#'   variables \code{ID} and \code{DATE}.
#' @param response_diagnoses A data frame containing response diagnoses with
#'   variables \code{ID} and \code{DATE}.
#' @param dpop A population data frame containing at least \code{ID},
#'   \code{DATE_BIRTH}, \code{DATE_DEATH}, and \code{DATE_MIGRATION}.
#'
#' @details
#' Exposure and response diagnosis dates occurring before age 50 are excluded
#' by setting them to \code{NA}. Age 50 is calculated as
#' \code{DATE_BIRTH + 50 * 365.25}.
#'
#' The function joins exposure and response diagnoses to the population data
#' and returns a per-ID dataset suitable for survival or competing risks analyses.
#'
#' @return A data frame with one row per individual, including exposure date,
#'   response date, birth date, age-50 date, death date, and migration date.
#'
#' @seealso \code{\link{df_dates_ftime}}
#' @export
df_dates_per_id <- function(exposure_diagnoses, response_diagnoses, dpop, admin_censor = as.Date("2023-12-31")){
  ## df per id aineisto, jossa päivämäärät ja seuranta alkaa kun 50v
  ### Aineisto pvm per ID
  d1 <- exposure_diagnoses |>
    dplyr::select(ID, DATE) |>
    dplyr::rename(DATE_EXPOSURE = DATE)
  d2 <- response_diagnoses |>
    dplyr::select(ID, DATE) |>
    dplyr::rename(DATE_RESPONSE = DATE)
  df <- dpop |>
    dplyr::select(ID, DATE_BIRTH, DATE_DEATH, DATE_MIGRATION) |>
    dplyr::left_join(d1) |>
    dplyr::left_join(d2) |>
    dplyr::mutate(
      DATE_EXPOSURE = as.Date(DATE_EXPOSURE),
      DATE_RESPONSE = as.Date(DATE_RESPONSE),
      DATE_BIRTH = as.Date(DATE_BIRTH),
      DATE_50 = DATE_BIRTH + 50 * 365.25
    ) |>
    ## Lisätäänkö käsittelysääntö?
    ## Esim. jos DG ennen 50v => time=0
    dplyr::mutate(
      DATE_EXPOSURE = ifelse(DATE_EXPOSURE>= DATE_50, DATE_EXPOSURE, ifelse(is.na(DATE_EXPOSURE), NA, NA)),
      DATE_RESPONSE = ifelse(DATE_RESPONSE>= DATE_50, DATE_RESPONSE, ifelse(is.na(DATE_RESPONSE), NA, NA))
    )
  df$DATE_EXPOSURE <- as.Date(df$DATE_EXPOSURE, origin = "1970-01-01")
  df$DATE_RESPONSE <- as.Date(df$DATE_RESPONSE, origin = "1970-01-01")

  df <- df |>
    dplyr::mutate(
      censor_date = pmin(DATE_MIGRATION, admin_censor, na.rm = TRUE)
    )
  return(df)
}



#' Calculate follow-up time and event status from age 50
#'
#' Computes follow-up time (\code{ftime}) and event status based on a specified
#' endpoint, death, or censoring. Time is measured from age 50 onward.
#'
#' @param df A data frame produced by \code{\link{df_dates_per_id}} containing
#'   demographic and event dates.
#' @param censor_date Administrative censoring date. Defaults to
#'   \code{as.Date("2023-12-31")}.
#' @param end A date variable indicating the endpoint of interest (e.g.
#'   \code{DATE_EXPOSURE} or \code{DATE_RESPONSE}). Tidy-evaluated.
#'
#' @details
#' For each individual, the end of follow-up is defined as the earliest of:
#' \itemize{
#'   \item the specified endpoint (\code{end}),
#'   \item date of death,
#'   \item migration or administrative censoring.
#' }
#'
#' Event status is coded as:
#' \describe{
#'   \item{1}{Endpoint event}
#'   \item{2}{Death before endpoint}
#'   \item{0}{Censored}
#' }
#'
#' Follow-up times occurring before age 50 are set to zero.
#'
#' @return A data frame with added variables:
#'   \describe{
#'     \item{ftime}{Follow-up time in days from age 50}
#'     \item{status}{Event status (0 = censored, 1 = event, 2 = death)}
#'   }
#'
#' @seealso \code{\link{df_dates_per_id}}
#'
#' @export
df_dates_ftime <- function(df,
                           censor_date = as.Date("2023-12-31"),
                           end = DATE_EXPOSURE){
  ## df jossa lasketaan aika kuluneeksi tapahtumasta toiseen
  ## Exposure/Response and Mortality
  df_ftime <- df |>
    dplyr::mutate(
      censor_date = pmin(DATE_MIGRATION, censor_date, na.rm = TRUE),
      censor_date = if_else(is.na(censor_date), censor_date, censor_date)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      end_date = min({{ end }}, DATE_DEATH, censor_date, na.rm = TRUE),
      # end_date = as.Date(if_else(is.infinite(end_date), censor_date, end_date)),
      status = dplyr::case_when(
        !is.na({{ end }}) & {{ end }} <= end_date ~ 1L,           # Event
        !is.na(DATE_DEATH)    & DATE_DEATH    <= end_date ~ 2L,   # Death
        TRUE ~ 0L
      ),
      ftime = as.numeric(end_date - DATE_50)
    ) |>
    dplyr::ungroup() |>
    ## Tapaukset joissa tapahtuma (dg, death, censor) on tapahtunut ennen kuin 50v
    ## joko filter out tai muutetaan suoraan ftime=0
    dplyr::mutate(ftime = ifelse(ftime < 0 , 0, ftime)) |> # tai filter(ftime >= 0) |>
    dplyr::mutate(
      status = as.integer(status),
      ftime  = as.numeric(ftime)
    )
  # length(unique(df_exp$ID)) ## koska koko pop, pitää olla 14 220
  # length(unique(dpop$ID))
  ## kuolleet poistuu!
  return(df_ftime)
}


#' Extract all cumulative incidence curves from a \code{cuminc} object
#'
#' Internal helper function that converts a \code{cmprsk::cuminc} object
#' into a tidy data frame containing estimates, variances, standard errors,
#' and confidence intervals for all event types.
#'
#' @param ci_obj A \code{cuminc} object produced by \code{cmprsk::cuminc}.
#' @param main_label Character string used to label the main event of interest.
#'
#' @details
#' Time is converted from days to years since age 50. Wald-type 95\%
#' confidence intervals are computed and truncated to the interval [0, 1].
#'
#' This function is intended for internal package use.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{time}{Time in years since age 50}
#'     \item{est}{Cumulative incidence estimate}
#'     \item{var}{Variance of the estimate}
#'     \item{se}{Standard error}
#'     \item{lower}{Lower 95\% confidence limit}
#'     \item{upper}{Upper 95\% confidence limit}
#'     \item{event}{Event label}
#'   }
#'
#' @keywords internal
.extract_ci_all <- function(ci_obj, main_label) {
# Generic function to extract all curves from cuminc object
  dplyr::bind_rows(lapply(names(ci_obj), function(nm) {
    comp <- ci_obj[[nm]]

    tibble::tibble(
      time  = comp$time / 365.25,    # years since age 50
      est   = comp$est,
      var   = comp$var,
      curve = nm                     # will rename below
    ) |>
      dplyr::mutate(
        se    = sqrt(var),
        lower = pmax(0, est - 1.96*se),
        upper = pmin(1, est + 1.96*se),
        # Rename for clarity
        event = ifelse(curve == "1 1", main_label, "Death")
      )
  }))
}
