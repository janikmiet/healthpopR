#' Plot Bone Mineral Density T-score by Age
#'
#' Creates a scatter plot and smoothed GAM curves for bone mineral density
#' (BMD) T-scores across age. The function optionally applies
#' date-dependent exposure and response classifications and visualizes
#' osteoporosis thresholds.
#'
#' The plot includes:
#' \itemize{
#'   \item Individual BMD observations
#'   \item GAM-smoothed trends for all subjects, exposure group,
#'         and response group
#'   \item Reference lines at T-score thresholds -1 and -2.5
#'   \item Background shading for osteopenia and osteoporosis ranges
#' }
#'
#' @param data_bmd A data frame containing bone mineral density
#'   measurements. Must include columns:
#'   \describe{
#'     \item{ID}{Patient identifier}
#'     \item{AGE}{Age at measurement}
#'     \item{TSCORE}{Bone mineral density T-score}
#'     \item{DATE}{Measurement date (required if
#'       \code{date_dependency = TRUE})}
#'   }
#'
#' @param data_dpop A data frame containing exposure and response
#'   information. Must include:
#'   \describe{
#'     \item{ID}{Patient identifier}
#'     \item{exp.GROUP}{Exposure group label (if
#'       \code{date_dependency = FALSE})}
#'     \item{resp.GROUP}{Response group label (if
#'       \code{date_dependency = FALSE})}
#'     \item{exp.DATE}{Exposure date (if
#'       \code{date_dependency = TRUE})}
#'     \item{resp.DATE}{Response date (if
#'       \code{date_dependency = TRUE})}
#'   }
#'
#' @param date_dependency Logical. If `TRUE`, exposure and response
#'   groups are determined dynamically based on whether exposure or
#'   response dates occurred before the BMD measurement date.
#'   Defaults to `FALSE`.
#'
#' @return A `ggplot2` object showing BMD T-score trajectories by age.
#'
#' @details
#' Osteopenia is highlighted between T-scores -1 and -2.5, while
#' osteoporosis is highlighted below -2.5 according to WHO criteria.
#'
#' Generalized additive models (GAMs) are fitted using
#' \code{geom_smooth(method = "gam")}.
#'
#' @examples
#' \dontrun{
#' p <- plot_bmd(
#'   data_bmd = bone_density_scores,
#'   data_dpop = dpop
#' )
#'
#' print(p)
#'
#' p2 <- plot_bmd(
#'   data_bmd = bone_density_scores,
#'   data_dpop = dpop,
#'   date_dependency = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link[ggplot2]{geom_smooth}},
#' \code{\link[dplyr]{left_join}}
#'
#' @export
plot_bmd <- function(data_bmd, data_dpop, date_dependency = FALSE){

  ## TODO add reference lines to fractures like hip fractures for comparison
  ## TODO options different ostheoporotic reference scales

  # colors_groups <- c(
  #   "non-exposure" = "#5BC0DE",
  #   "exposure"     = "#D9534F",
  #   "non-response" = "#F0AD4E",
  #   "response"     = "#5CB85C",
  #   "dead"         = "#292B2C"
  # )

  if(FALSE){
    data_bmd = bone_density_scores
    data_dpop = dpop
    date_dependency = FALSE
  }

  if(date_dependency){
    BMD <- data_bmd |>
      left_join(
        data_dpop |> select(ID, exp.DATE, resp.DATE),
        by = "ID"
      ) |>
      mutate(
        exp.GROUP  = ifelse(!is.na(exp.DATE)  & exp.DATE  <= DATE,
                            "exposure", "no exposure"),
        resp.GROUP = ifelse(!is.na(resp.DATE) & resp.DATE <= DATE,
                            "response", "no response")
      )
  } else {
    BMD <- data_bmd |>
      left_join(
        data_dpop |> select(ID, exp.GROUP, resp.GROUP),
        by = "ID"
      )
  }

  BMD_exp  <- filter(BMD, exp.GROUP == "exposure")
  BMD_resp <- filter(BMD, resp.GROUP == "response")

  # ggplot(BMD, aes(x = AGE, y = TSCORE)) +
  #   geom_point(
  #     color = "gray60",
  #     size = 0.5,
  #     alpha = 0.3
  #   ) +
  #   geom_smooth(
  #     method = "gam",
  #     formula = y ~ s(x, bs = "cs"),
  #     se = FALSE,
  #     color = "gray30",
  #     linewidth = 1
  #   ) +
  #   geom_smooth(
  #     data = BMD_exp,
  #     method = "gam",
  #     formula = y ~ s(x, bs = "cs"),
  #     se = FALSE,
  #     color = "green",
  #     linewidth = 1
  #   ) +
  #   geom_smooth(
  #     data = BMD_resp,
  #     method = "gam",
  #     formula = y ~ s(x, bs = "cs"),
  #     se = FALSE,
  #     color = "red",
  #     linewidth = 1
  #   )

  ggplot(BMD, aes(AGE, TSCORE)) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -2.5,
      ymax = -1,
      alpha = 0.08,
      fill = "orange"
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = -2.5,
      alpha = 0.08,
      fill = "red"
    ) +
    geom_point(alpha = 0.15, size = 0.4) +
    geom_hline(
      yintercept = -1,
      linetype = "dotted"
    ) +
    geom_hline(
      yintercept = -2.5,
      linetype = "dotted"
    ) +
    geom_smooth(
      aes(color = "All"),
      method = "gam",
      formula = y ~ s(x),
      se = TRUE
    ) +
    geom_smooth(
      data = BMD_exp,
      aes(color = "Exposure"),
      method = "gam",
      formula = y ~ s(x),
      se = TRUE
    ) +
    geom_smooth(
      data = BMD_resp,
      aes(color = "Response"),
      method = "gam",
      formula = y ~ s(x),
      se = TRUE
    ) +
    scale_color_manual(values = c(
      "All" = "yellow",
      "Exposure" = "#D9534F",
      "Response"= "#5CB85C"
    )) +
    labs(x= "Age at measure",
         color = "Group")
  # +facet_wrap(~DENS)

}
