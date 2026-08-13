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

  if(FALSE){
    data_bmd = bone_density_scores
    data_dpop = dpop
    reference = c("hip", "fractures")
    date_dependency = FALSE
  }

  all <- function(){

    .safe_inc_progress(1/3)

    if(date_dependency){
      BMD <- data_bmd |>
        dplyr::left_join(
          data_dpop |> dplyr::select(ID, exp.DATE, resp.DATE),
          by = "ID"
        ) |>
        dplyr::mutate(
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
      BMD <- data_bmd |>
        dplyr::left_join(
          data_dpop |> dplyr::select(ID, exp.GROUP, resp.GROUP),
          by = "ID"
        ) |>
        dplyr::mutate(
          Exposure = ifelse(exp.GROUP == "exposure", 1, 0),
          Response = ifelse(resp.GROUP == "response", 1, 0)
        )
    }

    plot_data <- BMD |>
      tidyr::pivot_longer(
        cols = c(Exposure, Response, HIP_FRACTURE, ANY_FRACTURE, OSTEO),
        names_to = "Group",
        values_to = "Value"
      ) |>
      dplyr::filter(Value == 1)

    plot_data <- plot_data |>
      dplyr::mutate(
        Group = dplyr::recode(
          Group,
          "ANY_FRACTURE" = "Any Fracture",
          "HIP_FRACTURE" = "Hip Fracture",
          "OSTEO" = "Osteoporosis"
        )
      ) |>
      dplyr::mutate(
        Group = factor(
          Group,
          levels = c(
            "Exposure",
            "Response",
            "Hip Fracture",
            "Any Fracture",
            "Osteoporosis"
          )
        )
      )

    .safe_inc_progress(2/3)

    # Boxplot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Group, y = TSCORE, color = Group)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(
        x = "Group",
        y = "TSCORE",
        title = "TSCORE distribution by groups"
      ) +
      ggplot2::scale_color_manual(values = c(
        # "All" = "yellow",
        "Exposure" = "#D9534F",
        "Response"= "#5CB85C",
        "Any Fracture" = "#5BC0DE",
        "Hip Fracture" = "#9370DB",
        "Osteoporosis" = "#F0AD4E"
      )) +
      ggplot2::theme_minimal()
    # p
    .safe_inc_progress(3/3)

    return(p)
  }

  if (shiny::isRunning()) {
    withProgress(message = "Plot BMD Boxplot", value = 0, {
      return(all())
    })
  } else {
    return(all())
  }
}
