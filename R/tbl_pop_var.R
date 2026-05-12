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
  ## Check that group is exposure or response
  if (!group %in% c("exposure", "response")) {
    stop("group must be either 'exposure' or 'response'")
  }
  ## Data
  if(group == "exposure"){
    tbl <- data %>% filter(exp.GROUP == "exposure")
  }
  if(group == "response"){
    tbl <- data %>% filter(resp.GROUP == "response")
  }
  ## Count
  tbl <- tbl %>%
    dplyr::left_join(population_variables, by = "ID") %>%
    count(!!sym(var), name = "n")
  return(tbl)
}
