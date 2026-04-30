#' Plot Age Distribution at First Diagnosis for Exposure or Response Group
#'
#' Visualizes the age distribution at the time of the first diagnosis for either the exposure
#' or response group, optionally including subgroup breakdowns. Intended for use with
#' population-level health registry data.
#'
#' @param data A data frame containing the following columns:
#'   - `ID`: Unique identifier for individuals.
#'   - `exp.AGE_DG`: Age at first diagnosis in the exposure group.
#'   - `exp.GROUP`: Group label (`"exposure"` or `"response"`).
#'   - `resp.AGE_DG`: Age at first diagnosis in the response group.
#'   - `resp.GROUP`: Group label (`"exposure"` or `"response"`).
#' @param group Character. Specifies which group to plot: either `"exposure"` or `"response"`.
#'   Determines which AGE_DG and GROUP columns are used.
#' @param subgroups Logical. If `TRUE`, the age distribution will be split by the opposite group
#'   (e.g., response subgroups within exposure group).
#' @param colors Character vector of two colors used for plotting. First is for `"exposure"`,
#'   second is for `"response"`.
#' @param colors_shade Character vector of two hex color values (with transparency) used to
#'   outline the bars. First for `"exposure"`, second for `"response"`.
#'
#' @return A `ggplot` object showing the age distribution as a bar chart. If used in a Shiny
#' application, the plot is wrapped in a progress bar using `withProgress()`.
#'
#' @details
#' The function renames and filters the appropriate AGE and GROUP columns based on the
#' selected `group`. If `subgroups = TRUE`, it groups the data by both `GROUP` and
#' the opposite group (used as `SUBGROUP`) before plotting.
#'
#' Color selection is automatically handled based on the `group` argument, using the
#' corresponding color and shade from `colors` and `colors_shade`.
#'
#' @examples
#' \dontrun{
#' plot_age_distribution(data = my_data, group = "exposure")
#' plot_age_distribution(data = my_data, group = "response", subgroups = TRUE)
#' }
#'
#' @export
plot_age_distribution <- function(
    data,
    group = "exposure",
    subgroups = FALSE,
    colors = c("#5BC0DE", "#D9534F"),  # ORDER: exposure, response
    colors_shade = c("#5BC0DE66", "#D9534F66")
) {

  ## Checking
  if (!group %in% c("exposure", "response")) {
    stop("Argument 'group' must be either 'exposure' or 'response'.")
  }
  # group_type <- match.arg(group)
  # subgroup_type <- match.arg(subgroup)

  plot_internal <- function() {
    .safe_inc_progress(1/4)

    d1 <- data |>
      dplyr::select(ID, exp.AGE_DG, exp.GROUP, resp.AGE_DG, resp.GROUP)

    # Filter and group exposure data
    if(group == "exposure"){
      d1 <- d1 |>
        dplyr::filter(exp.GROUP == "exposure") |>
        dplyr::rename(AGE = exp.AGE_DG,
                      GROUP = exp.GROUP,
                      SUBGROUP = resp.GROUP)
    }else if(group == "response"){
      d1 <- d1 |>
        dplyr::filter(resp.GROUP == "response") |>
        dplyr::rename(AGE = resp.AGE_DG,
                      GROUP = resp.GROUP,
                      SUBGROUP = exp.GROUP)
    }
    # Group & Subgroup Aggregate
    if(subgroups){
      d <- d1 |>
        dplyr::group_by(GROUP, SUBGROUP, AGE) |>
        dplyr::summarise(freq = dplyr::n())
    }else{
      d <- d1 |>
        dplyr::group_by(GROUP, AGE) |>
        dplyr::summarise(freq = dplyr::n())
    }

    .safe_inc_progress(2/4)

    ## Plotting Title
    title <- if (sum(d$freq) == 0) {
      paste0("No ", .capitalize(group)," Data")
    } else {
      paste(.capitalize(group), "Population size", sum(d$freq))
    }

    # Plotting with SUBGROUP or WITHOUT
    if(subgroups){
      # Plot with subgroup
      plt <- ggplot2::ggplot(d, ggplot2::aes(x = AGE, y = freq, fill = SUBGROUP, group = SUBGROUP)) +
        ggplot2::geom_bar(
          stat = "identity"
        ) +
        ggplot2::scale_fill_manual(values = rev(colors))
    }else{
      ## Pick colors
      col_fill <- colors[ifelse(group == "exposure", 2, 1)]
      col_shade <- colors_shade[ifelse(group == "exposure", 2, 1)]
      # Plot only group
      plt <- ggplot2::ggplot(d, aes(x = AGE, y = freq)) +
        ggplot2::geom_bar(
          stat = "identity",
          fill = col_fill,  # response color for consistency
          color = col_shade
        )
    }

    .safe_inc_progress(3/4)

    plt <- plt +
      hrbrthemes::theme_ipsum_rc() +
      ggplot2::theme(plot.title = element_text(size = 14, face = "bold")) +
      ggplot2::labs(
        title = title,
        subtitle = paste("Age at First", .capitalize(group),"Diagnosis"),
        x="Age")

    .safe_inc_progress(4/4)

    return(plt)
  }

  if (shiny::isRunning()) {
    withProgress(message = paste("Plot", group, "Age Distribution"), value = 0, {
      plot_internal()
    })
  } else {
    plot_internal()
  }
}
