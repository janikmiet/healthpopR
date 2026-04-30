#' Bar Plot of ICD-10 Diagnosis Differences Between Exposure Groups
#'
#' Creates a horizontal bar plot to visualize differences in ICD-10 diagnosis percentages
#' between exposure and non-exposure groups, filtered by a specified percentage difference threshold.
#'
#' @param data A data frame containing ICD-10 comparison results, typically produced by
#'   \code{\link{tbl_icd10_diff_by_exposure}}. Must include the columns: `ICD10_3LETTERS`,
#'   `exposure_group_pct`, `no_exposure_group_pct`, and `diff_pct`.
#' @param limit Numeric value (default = 10). Only diagnoses with a group percentage
#'   difference greater than this threshold will be plotted.
#' @param colors A character vector of two color values (with optional alpha), used for the
#'   exposure and no-exposure group bars respectively. Defaults to `c("#5BC0DE66", "#D9534F66")`.
#'
#' @return A `ggplot2` object showing a grouped horizontal bar chart of the selected ICD-10
#' diagnosis codes, comparing the exposure and no-exposure group percentages.
#'
#' @details
#' The function filters ICD-10 codes to include only those where the absolute difference in
#' diagnosis prevalence (`diff_pct`) exceeds the specified threshold. The plot flips the
#' coordinate system to display diagnoses vertically for better readability.
#'
#' Colors are manually assigned for visual clarity and can be customized. The plot uses
#' the `hrbrthemes::theme_ipsum_rc()` theme.
#'
#' The function supports integration with Shiny and shows progress via `withProgress()`
#' and `.safe_inc_progress()` if inside a Shiny app.
#'
#' @examples
#' \dontrun{
#' tbl <- tbl_icd10_diff_by_exposure(
#'   data = dpop,
#'   diagnoses = diagnoses,
#'   exposure_icd10 = "^E11",
#'   exposure_src = c("hilmo", "avohilmo")
#' )
#'
#' plot_icd10_diff_by_exposure(tbl, limit = 5)
#' }
#'
#' @importFrom ggplot2 ggplot geom_bar aes coord_flip scale_fill_manual labs
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom hrbrthemes theme_ipsum_rc
#' @importFrom shiny isRunning withProgress
#' @export
plot_icd10_diff_by_exposure <- function(data,
                                        limit=10,
                                        colors = c("#5BC0DE66", "#D9534F66")
){
  internal_function <- function(){
    dplot <- data |>
      dplyr::filter(diff_pct > limit ) |>
      tidyr::pivot_longer(cols = c(exposure_group_pct, no_exposure_group_pct))

    .safe_inc_progress(1/2)

    colors_in <- c(colors[2], colors[1])

    plt <- ggplot2::ggplot(dplot ) +
      ggplot2::geom_bar(ggplot2::aes(x=reorder(ICD10_3LETTERS, -value), y=value, fill=name, group=name), stat = "identity", position = "dodge") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = colors_in) +
      ggplot2::labs(x="diagnose", y="percentage (%)", title = "Exposure group top diagnoses") +
      hrbrthemes::theme_ipsum_rc()

    .safe_inc_progress(2/2)

    return(plt)
  }
  if(shiny::isRunning()){
    withProgress(message = "Health ICD-10 Comparison Plot", value = 0, {
      return(internal_function())
    })
  }else{
    return(internal_function())
  }
}
