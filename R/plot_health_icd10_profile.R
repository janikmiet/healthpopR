#' Plot Health ICD-10 Profile Using Radar Chart
#'
#' Creates a radar chart visualization of ICD-10 diagnosis class distributions across exposure groups.
#' Typically used with the output of `classify_icd10_profile()`.
#'
#' @param data A data frame in wide format as returned by `classify_icd10_profile()`, with exposure group
#'   rows (e.g., "exposure", "no exposure") and ICD-10 classes as columns. The first two rows must be artificial
#'   "Max" and "Min" rows with values 100 and 0 for scaling the radar chart.
#' @param colors_exposure_groups A character vector of base colors (hex codes) used for the exposure groups.
#'   Defaults to `c("#5BC0DE", "#D9534F")`.
#' @param colors_exposure_groups_shade A character vector of semi-transparent versions of the base colors,
#'   used for the shaded area in the radar chart. Defaults to `c("#5BC0DE66", "#D9534F66")`.
#'
#' @return No return value. The function creates a radar chart plot in the current graphics device.
#'
#' @details
#' The function:
#' - Uses the `fmsb::radarchart()` function to draw the chart
#' - Draws one polygon per exposure group with colored borders and shaded fills
#' - Adds a custom legend matching group names to colors
#'
#' The chart is automatically displayed with a custom layout, axis labels, and color styling.
#' If called within a Shiny app, a progress bar is shown using `withProgress()`.
#'
#' @examples
#' \dontrun{
#' data_final <- classify_icd10_profile(data = dpop, diagnoses = diagnoses,
#'                                      exposure_icd10 = "^E11",
#'                                      exposure_src = c("hilmo", "avohilmo"))
#' plot_health_icd10_profile(data_final)
#' }
#'
#' @importFrom fmsb radarchart
#' @importFrom scales alpha
#' @export
plot_health_icd10_profile <- function(data,
                                      colors_exposure_groups = c("#5BC0DE", "#D9534F"),
                                      colors_exposure_groups_shade = c("#5BC0DE66", "#D9534F66")
){
  # data = data_final
  internal_function <- function(){
    ## TODO colors needs to added to function. Now reversing order.
    colors_border <- c(colors_exposure_groups_shade[2], colors_exposure_groups_shade[1])
    colors_in <- c(colors_exposure_groups[2], colors_exposure_groups[1])
    # Custom the radarChart !
    fmsb::radarchart(data,  axistype=1,
                     ## Custom polygon
                     pcol=colors_border,pfcol=alpha(colors_in, .5), plwd=4,
                     plty=1,
                     ## custom grid
                     cglcol="grey", cglty=1, axislabcol="grey",
                     cglwd=0.9,
                     # custom labels
                     vlcex=0.8
    )
    # Add a legend
    legend(x=-1.4,
           y=1.35,
           legend = rownames(data[-c(1,2),]),
           bty = "n",
           pch=20,
           col=colors_in,
           text.col = "grey",
           cex=1,
           pt.cex=3)
  }

  if (shiny::isRunning()) {
    withProgress(message = "Plotting Health Profile", value = 0, {
      internal_function()
    })
  } else {
    internal_function()
  }

}
