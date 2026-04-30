#' Summary of Exposure and Response Timing Order
#'
#' Summarizes the temporal relationship between exposure and response events
#' for individuals who have both an exposure and a response diagnosis.
#'
#' Categorizes the relationship as:
#' \itemize{
#'   \item \code{"Exposure < Response"} if exposure occurred before the response,
#'   \item \code{"Exposure == Response"} if they occurred on the same date,
#'   \item \code{"Exposure > Response"} if exposure occurred after the response.
#' }
#'
#' @param data A data frame containing at least the following columns:
#'   \itemize{
#'     \item \code{exposure}: Binary indicator (0/1) of exposure.
#'     \item \code{response}: Binary indicator (0/1) of response.
#'     \item \code{exp.DATE}: Date of the exposure.
#'     \item \code{resp.DATE}: Date of the response.
#'   }
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{\code{exp_resp}}{A factor indicating the exposure-response temporal relationship.}
#'     \item{\code{n}}{Count of cases in each category.}
#'     \item{\code{percentage}}{Percentage of total cases for each category.}
#'   }
#'
#' @details
#' This function is intended for use in shiny applications and supports progress indication.
#' If used in a shiny session, progress is displayed with `withProgress()`.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     exposure = sample(0:1, 100, replace = TRUE),
#'     response = sample(0:1, 100, replace = TRUE),
#'     DATE = sample(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"), 100, replace = TRUE),
#'     resp.DATE = sample(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"), 100, replace = TRUE)
#'   )
#'   summary_exp_resp_order(df)
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise case_when
#' @importFrom shiny isRunning withProgress
#' @export
summary_exp_resp_order <- function(data){
  #OBS! Previously named: tab_exp_resp
  all <- function(){
    .safe_inc_progress(amount = 1/4)
    d <- data |>
      dplyr::filter(exposure == 1 & response == 1) |>
      dplyr::mutate(
        exp_resp = ifelse(exp.DATE < resp.DATE, 1, ifelse(exp.DATE == resp.DATE, 0, -1))
        # exp_resp = ifelse(exposure_date < response_date, 1, 0)
      ) |>
      dplyr::group_by(exp_resp) |>
      dplyr::summarise(
        n = dplyr::n()
      ) |>
      dplyr::mutate(
        percentage = round(100 * n / nrow(data |> filter(exposure == 1 & response == 1)), 1),
        exp_resp = factor(case_when(
          exp_resp == 1 ~ "Exposure < Response",
          exp_resp == 0 ~ "Exposure == Response",
          exp_resp == -1 ~ "Exposure > Response"
        ), levels = c("Exposure < Response", "Exposure == Response", "Exposure > Response"))
      )
    .safe_inc_progress(amount = 4/4)
    return(d)
  }
  if(shiny::isRunning()){
    withProgress(message = "Summary of Exposure-Response Order", value = 0, {
      return(all())
    })
  }else{
    return(all())
  }
}
