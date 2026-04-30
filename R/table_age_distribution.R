#' Summarize Age Distribution for Exposure or Response Groups
#'
#' Creates a summary table of population counts and age statistics (minimum, median, mean, and maximum)
#' for either the "exposure" or "response" group. Optionally, it can also include subgroups for a more
#' detailed breakdown.
#'
#' @param data A data frame containing columns `exp.GROUP`, `resp.GROUP`, `exp.AGE_DG`, and `resp.AGE_DG`.
#'             These are expected to represent exposure/response group labels and ages at diagnosis.
#' @param group A character string, either `"exposure"` or `"response"`, indicating which group to summarize.
#'              Defaults to `"exposure"`.
#' @param subgroups Logical. If `TRUE`, adds subgroup statistics (cross-group summaries). Defaults to `FALSE`.
#'
#' @return A data frame summarizing the population size (`pop_n`) and age statistics:
#' \itemize{
#'   \item \code{pop_n} - Population count
#'   \item \code{age_min} - Minimum age at diagnosis
#'   \item \code{age_median} - Median age at diagnosis
#'   \item \code{age_mean} - Mean age at diagnosis
#'   \item \code{age_max} - Maximum age at diagnosis
#' }
#' When `subgroups = TRUE`, an additional "All" row summarizes the entire group.
#'
#' @details This function is intended for use in both interactive and Shiny contexts.
#' It utilizes `.safe_inc_progress()` to update progress during computation.
#'
#' @examples
#' \dontrun{
#' table_age_distribution(mydata, group = "response", subgroups = TRUE)
#' }
#'
#' @export
table_age_distribution <- function(
    data,
    group = "exposure",
    subgroups = FALSE
) {

  ## Checking
  if (!group %in% c("exposure", "response")) {
    stop("Argument 'group' must be either 'exposure' or 'response'.")
  }

  summarize_data <- function() {

    .safe_inc_progress(1/3)

    ## Simple statistics, output is one row
    if (group == "exposure") {
      d1 <- data |>
        dplyr::filter(exp.GROUP == "exposure") |>
        dplyr::rename(GROUP = exp.GROUP,
                      AGE = exp.AGE_DG) |>
        dplyr::group_by(GROUP) |>
        dplyr::summarise(
          pop_n = dplyr::n(),
          age_min = min(AGE, na.rm = TRUE),
          age_median = median(AGE, na.rm = TRUE),
          age_mean = mean(AGE, na.rm = TRUE),
          age_max = max(AGE, na.rm = TRUE)
        )
    } else if (group == "response") {
      # Response by group
      d1 <- data |>
        dplyr::filter(resp.GROUP == "response") |>
        dplyr::rename(GROUP = resp.GROUP,
                      AGE = resp.AGE_DG) |>
        dplyr::group_by(GROUP) |>
        dplyr::summarise(
          pop_n = dplyr::n(),
          age_min = min(AGE, na.rm = TRUE),
          age_median = median(AGE, na.rm = TRUE),
          age_mean = mean(AGE, na.rm = TRUE),
          age_max = max(AGE, na.rm = TRUE)
        )
    }

    .safe_inc_progress(2/3)

    # Stats for subgroups
    if(subgroups){
      if(group == "response"){
        d2 <- data |>
          dplyr::filter(resp.GROUP == "response") |>
          dplyr::rename(GROUP = exp.GROUP,
                        AGE = resp.AGE_DG) |>
          dplyr::group_by(GROUP) |>
          dplyr::summarise(
            pop_n = dplyr::n(),
            age_min = min(AGE, na.rm = TRUE),
            age_median = median(AGE, na.rm = TRUE),
            age_mean = mean(AGE, na.rm = TRUE),
            age_max = max(AGE, na.rm = TRUE)
          )
      }else{
        d2 <- data |>
          dplyr::filter(exp.GROUP == "exposure") |>
          dplyr::rename(GROUP = resp.GROUP,
                        AGE = exp.AGE_DG) |>
          dplyr::group_by(GROUP) |>
          dplyr::summarise(
            pop_n = dplyr::n(),
            age_min = min(AGE, na.rm = TRUE),
            age_median = median(AGE, na.rm = TRUE),
            age_mean = mean(AGE, na.rm = TRUE),
            age_max = max(AGE, na.rm = TRUE)
          )
      }
      ## Combine datas d1 and d2
      d1$GROUP <-  "All"
      d <- d2 |>
        rbind(d1)
    }else{
      ## No subgroups
      d <- d1
    }

    .safe_inc_progress(3/3)

    return(d)
  }

  if (shiny::isRunning()) {
    withProgress(message = paste("Table", .capitalize(group), "Age Distribution"), value = 0, {
      summarize_data()
    })
  } else {
    summarize_data()
  }
}
