#' Plot boxplots of BMD T-scores by exposure, response, fracture, and osteoporosis groups
#'
#' Creates boxplots of bone mineral density (BMD) T-scores (`TSCORE`) for selected
#' population subgroups, including exposure, response, hip fracture, any fracture,
#' and osteoporosis groups. Groups can be determined either statically or
#' dynamically relative to the BMD measurement date.
#'
#' If `date_dependency = TRUE`, exposure/response and outcome group membership
#' are recalculated based on whether the event date occurred before or after the
#' BMD measurement date (`DATE`).
#'
#' @param data_bmd A data frame containing bone mineral density measurements.
#'   Must include at least `ID`, `DATE`, `TSCORE`, and fracture/osteoporosis
#'   date variables if `date_dependency = TRUE`.
#' @param data_dpop A data frame containing population-level grouping variables.
#'   Must include `ID`, and either:
#'   \itemize{
#'     \item `exp.GROUP`, `resp.GROUP` when `date_dependency = FALSE`
#'     \item `exp.DATE`, `resp.DATE` when `date_dependency = TRUE`
#'   }
#' @param reference Character vector specifying which clinical reference groups
#'   to include. Options are `"hip"`, `"fractures"`, and `"osteo"`.
#'   Default is all.
#' @param date_dependency Logical; if `TRUE`, group membership is recalculated
#'   relative to BMD measurement date. If `FALSE` (default), precomputed grouping
#'   variables are used.
#'
#' @return A `ggplot2` boxplot object showing the distribution of `TSCORE`
#'   across selected groups.
#'
#' @details
#' The plot includes boxplots for:
#' \itemize{
#'   \item Exposure group
#'   \item Response group
#'   \item Hip fracture group
#'   \item Any fracture group
#'   \item Osteoporosis group
#' }
#'
#' Only observations belonging to a given group (indicator value = 1) are
#' included in that boxplot.
#'
#' @examples
#' \dontrun{
#' # Static grouping
#' p <- plot_bmd_boxplot(data_bmd, data_dpop)
#' print(p)
#'
#' # Date-dependent grouping
#' p <- plot_bmd_boxplot(data_bmd, data_dpop, date_dependency = TRUE)
#' print(p)
#' }
#'
#' @export
plot_bmd_boxplot <- function(data_bmd, data_dpop, reference = c("hip", "fractures", "osteo"), date_dependency = FALSE){

  if(date_dependency){
    BMD <- bone_density_scores |>
      left_join(
        data_dpop |> select(ID, exp.DATE, resp.DATE),
        by = "ID"
      ) |>
      mutate(
        exp.GROUP  = ifelse(!is.na(exp.DATE)  & exp.DATE  <= DATE,
                            "exposure", "no exposure"),
        resp.GROUP = ifelse(!is.na(resp.DATE) & resp.DATE <= DATE,
                            "response", "no response"),
        Exposure = ifelse(exp.GROUP == "exposure", 1, 0),
        Response = ifelse(resp.GROUP == "response", 1, 0),
        OSTEO = ifelse(!is.na(DATE_OSTEO) & DATE_OSTEO >= DATE, 1, 0),
        HIP_FRACTURE = ifelse(!is.na(DATE_HIPFRACTURE) & DATE_HIPFRACTURE >= DATE, 1, 0),
        ANY_FRACTURE = ifelse(!is.na(DATE_ANYFRACTURE) & DATE_ANYFRACTURE >= DATE, 1, 0),
      )
  } else {
    BMD <- bone_density_scores |>
      left_join(
        dpop |> select(ID, exp.GROUP, resp.GROUP),
        by = "ID"
      ) |>
      dplyr::mutate(
        Exposure = ifelse(exp.GROUP == "exposure", 1, 0),
        Response = ifelse(resp.GROUP == "response", 1, 0)
      )
  }

  plot_data <- BMD %>%
    tidyr::pivot_longer(
      cols = c(Exposure, Response, HIP_FRACTURE, ANY_FRACTURE, OSTEO),
      names_to = "Group",
      values_to = "Value"
    ) %>%
    dplyr::filter(Value == 1)

  # Boxplot
  p <- ggplot(plot_data, aes(x = Group, y = TSCORE)) +
    geom_boxplot() +
    labs(
      x = "Group",
      y = "TSCORE",
      title = "TSCORE distribution by groups"
    ) +
    theme_minimal()

  return(p)
}
