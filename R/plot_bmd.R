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
plot_bmd <- function(data_bmd, data_dpop, date_dependency = FALSE, reference = c("hip", "fracture", "osteo")){

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
    reference = c("hip", "fracture", "osteo")
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
                              "response", "no response")
        )
    } else {
      BMD <- data_bmd |>
        dplyr::left_join(
          data_dpop |> dplyr::select(ID, exp.GROUP, resp.GROUP),
          by = "ID"
        )
    }

    BMD_exp  <- dplyr::filter(BMD, exp.GROUP == "exposure")
    BMD_resp <- dplyr::filter(BMD, resp.GROUP == "response")

    .safe_inc_progress(2/3)

    p <- ggplot2::ggplot(BMD, ggplot2::aes(AGE, TSCORE)) +
      ggplot2::annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = -2.5,
        ymax = -1,
        alpha = 0.08,
        fill = "orange"
      ) +
      ggplot2::annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = -2.5,
        alpha = 0.08,
        fill = "red"
      ) +
      ggplot2::geom_point(alpha = 0.15, size = 0.4) +
      ggplot2::geom_hline(
        yintercept = -1,
        linetype = "dotted"
      ) +
      ggplot2::geom_hline(
        yintercept = -2.5,
        linetype = "dotted"
      ) +
      ggplot2::geom_smooth(
        ggplot2::aes(color = "Full Population"),
        method = "gam",
        formula = y ~ s(x),
        se = TRUE
      ) +
      ggplot2::geom_smooth(
        data = BMD_exp,
        ggplot2::aes(color = "Exposure"),
        method = "gam",
        formula = y ~ s(x),
        se = TRUE
      ) +
      ggplot2::geom_smooth(
        data = BMD_resp,
        ggplot2::aes(color = "Response"),
        method = "gam",
        formula = y ~ s(x),
        se = TRUE
      )
    ## References
    if("hip" %in% reference){
      BMD_hip <- dplyr::filter(BMD, HIP_FRACTURE == 1)
      p <- p +
        ggplot2::geom_smooth(
          data = BMD_hip,
          ggplot2::aes(color = "Hip Fracture"),
          method = "gam",
          formula = y ~ s(x),
          se = TRUE
        )
    }
    if("osteo" %in% reference){
      BMD_osteo <- dplyr::filter(BMD, OSTEO == 1)
      p <- p +
        ggplot2::geom_smooth(
          data = BMD_osteo,
          ggplot2::aes(color = "Osteoporosis"),
          method = "gam",
          formula = y ~ s(x),
          se = TRUE
        )
    }
    if("fracture" %in% reference){
      BMD_fract <- dplyr::filter(BMD, ANY_FRACTURE == 1)
      p <- p +
        ggplot2::geom_smooth(
          data = BMD_fract,
          ggplot2::aes(color = "Any Fracture"),
          method = "gam",
          formula = y ~ s(x),
          se = TRUE
        )
    }

    p <- p +
      ggplot2::scale_color_manual(values = c(
        "Full Population" = "yellow",
        "Exposure" = "#D9534F",
        "Response"= "#5CB85C",
        "Any Fracture" = "#5BC0DE",
        "Hip Fracture" = "#9370DB",
        "Osteoporosis" = "#F0AD4E"
      )) +
      ggplot2::labs(x= "Age at measure",
                    color = "Group")
    # +facet_wrap(~DENS)
    # p
    .safe_inc_progress(3/3)

    return(p)
  }

  if (shiny::isRunning()) {
    withProgress(message = "Plot BMD", value = 0, {
      return(all())
    })
  } else {
    return(all())
  }

}
