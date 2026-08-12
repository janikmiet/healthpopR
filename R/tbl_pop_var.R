#' Count Population Variable Categories by Group
#'
#' Filters a dataset by exposure or response group and counts the
#' occurrences of a selected population variable.
#'
#' The function joins the filtered dataset with the
#' `population_variables` table by `ID` and returns category counts
#' for the specified variable.
#'
#' @param data A data frame containing at minimum:
#'   \describe{
#'     \item{ID}{Patient identifier}
#'     \item{exp.GROUP}{Exposure group classification}
#'     \item{resp.GROUP}{Response group classification}
#'   }
#'
#' @param group Character string defining which subgroup to use.
#'   Must be either:
#'   \itemize{
#'     \item `"exposure"`
#'     \item `"response"`
#'   }
#'
#' @param var Character string giving the variable name from
#'   `population_variables` to summarize.
#'
#' @return A tibble with:
#'   \describe{
#'     \item{<var>}{Levels/categories of the selected variable}
#'     \item{n}{Count of observations in each category}
#'   }
#'
#' @details
#' The function uses non-standard evaluation via
#' \code{rlang::sym()} to dynamically count the selected variable.
#'
#' The object `population_variables` must exist in the environment
#' and contain the requested variable and an `ID` column.
#'
#' @examples
#' \dontrun{
#' tbl_pop_var(
#'   data = dpop,
#'   group = "exposure",
#'   var = "SEX"
#' )
#'
#' tbl_pop_var(
#'   data = dpop,
#'   group = "response",
#'   var = "BMI_CLASS"
#' )
#' }
#'
#' @seealso
#' \code{\link[dplyr]{count}},
#' \code{\link[dplyr]{left_join}}
#'
#' @export
tbl_pop_var <- function(data, group, var){

  if(FALSE){
    data = dpop
    group = "response"
    var="bmi_cat2"
  }

  ## Check that group is exposure or response
  if (!group %in% c("exposure", "response")) {
    stop("group must be either 'exposure' or 'response'")
  }
  ## Data
  if(group == "exposure"){
    tbl <- data |> dplyr::filter(exp.GROUP == "exposure")
  }
  if(group == "response"){
    tbl <- data |> dplyr::filter(resp.GROUP == "response")
  }

  ## Count pop freqs
  tbl_1 <- dpop |>
    dplyr::left_join(population_variables, by = "ID") |>
    dplyr::count(!!dplyr::sym(var), name = "pop_n") |>
    dplyr::mutate(
      pop_pct = 100 * pop_n / sum(pop_n[!is.na(!!dplyr::sym(var))])
    )
  ## Count group freqs
  n_col <- rlang::sym(paste0(group, "_n"))

  tbl_2 <- tbl |>
    dplyr::left_join(population_variables, by = "ID") |>
    dplyr::count(!!dplyr::sym(var), name = as.character(n_col)) |>
    dplyr::mutate(
      pct = 100 * !!n_col /
        sum((!!n_col)[!is.na(!!dplyr::sym(var))])
    )
  ## Combine and percentages
  tbl_final <- tbl_2 |>
    dplyr::left_join(tbl_1, by =var)

  return(tbl_final)
}
