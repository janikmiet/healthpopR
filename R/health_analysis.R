#' Classify ICD-10 Profiles by Exposure Group
#'
#' This function creates a summary table of ICD-10 diagnosis class distributions across exposure groups,
#' excluding diagnoses matching a specified ICD-10 pattern (e.g., the exposure-defining diagnosis).
#' It calculates patient percentages per diagnosis class and reshapes the result into a wide-format matrix
#' suitable for plotting (e.g., radar charts or heatmaps).
#'
#' @param data A data frame containing the study population with at least columns `ID` and `exp.GROUP`,
#'   where `exp.GROUP` distinguishes between exposure and non-exposure groups. Can be created by classify_population() -function.
#' @param diagnoses A Original data frame of diagnoses that must include columns `ID`, `DGREG`, `DG`, `SRC`, and `ICD10_CLASS`.
#' @param exposure_icd10 A regular expression string that defines the ICD-10 diagnosis used to define the exposure group.
#'   Diagnoses matching this pattern are excluded from the analysis.
#' @param exposure_src A character vector of source types (e.g., `"avohilmo"`, `"hilmo"`, etc.) to include in the filtering.
#'
#' @return A wide-format data frame with ICD-10 class percentages by exposure group. Includes artificial `"Max"` and `"Min"`
#'   rows (100 and 0) for visualization purposes.
#'
#' @details
#' The function internally calculates:
#' - Population sizes for each exposure group
#' - Diagnosis counts and patient counts per ICD-10 class
#' - Percentages of patients per group with each diagnosis class
#'
#' If run inside a Shiny application, it will display progress bars using `withProgress()`.
#'
#' @examples
#' \dontrun{
#' classify_icd10_profile(
#'   data = dpop,
#'   diagnoses = diagnoses,
#'   exposure_icd10 = "^E11",
#'   exposure_src = c("avohilmo", "hilmo", "erko", "local")
#' )
#' }
#'
#' @export
classify_icd10_profile <- function(data,
                                   diagnoses = diagnoses,
                                   exposure_icd10="",
                                   exposure_src=c("")
){
  internal_function <- function(){
    # data_population <- dpop
    # data_diagnoses <- diagnoses
    # exposure_icd10 <- "^E11"
    # exposure_src <- c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")


    ## Summat ja exposure patient prossat
    d <- data
    # pop sizes
    d_exp_popn <- d |>
      select(ID, exp.GROUP) |>
      group_by(exp.GROUP) |>
      summarise(n_group=length(unique(ID)))
    ## add total group pop sizes
    d <- d |>
      left_join(d_exp_popn, by = "exp.GROUP")

    .safe_inc_progress(1/4)

    ## Diagnoses full data
    data_diagnoses <- diagnoses
    # Groups / group & no exposure
    icd10_recoded_summary <- data_diagnoses |>
      filter(DGREG == "ICD10") |>
      filter(SRC %in% exposure_src) |>
      filter(!grepl(pattern = .regex_clean(exposure_icd10), x = DG)) |> ## Ei oteta exposure diagnooseja analyysiin
      left_join(d |> select(ID, exp.GROUP, n_group), by = "ID") |>
      group_by(exp.GROUP, ICD10_CLASS) |>
      summarise(
        cases=n(),
        patients=length(unique(ID)),
        n_group = dplyr::first(n_group)
      ) |>
      mutate(
        per100=cases/100 * n_group,
        pct = 100 * patients / n_group,
      )

    .safe_inc_progress(2/4)

    # create 'data'
    data_final <- icd10_recoded_summary |>
      tidyr::pivot_wider(id_cols = exp.GROUP, values_from = pct, names_from = ICD10_CLASS) |>
      dplyr::arrange(exp.GROUP)
    data_final <- as.data.frame(data_final)
    custom_row_names <- c("Max", "Min", data_final$exp.GROUP)  # adding these later step, but next we need to remove col
    data_final <- data_final[, 2:ncol(data_final)]
    data_final <- rbind(rep(100,ncol(data_final)) , rep(0,ncol(data_final)) , data_final) # new
    rownames(data_final) <- custom_row_names  # adding names for rows

    .safe_inc_progress(3/4)

    return(data_final)
  }

  if (shiny::isRunning()) {
    withProgress(message = "Plotting Health Profile", value = 0, {
      internal_function()
    })
  } else {
    internal_function()
  }
}


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



#' Compare ICD-10 Diagnoses Between Exposure Groups
#'
#' Calculates and compares the prevalence of top ICD-10 diagnosis codes (3-letter level)
#' between exposure and non-exposure populations. Designed to highlight diagnostic differences
#' across groups while excluding the exposure-defining ICD-10 codes.
#'
#' @param data A data frame with at least `ID` and `exp.GROUP` columns, where `exp.GROUP`
#'   should include `"exposure"` and `"no exposure"` as values.
#' @param diagnoses A data frame containing patient diagnosis records. Must include at least
#'   the columns: `ID`, `DGREG`, `DG`, `ICD10_3LETTERS`, and `SRC`.
#' @param exposure_icd10 A regular expression string defining the ICD-10 pattern used to classify
#'   exposure. Diagnoses matching this pattern will be excluded from analysis.
#' @param exposure_src A character vector of source systems (e.g., `c("hilmo", "avohilmo")`)
#'   to include in the diagnosis filtering.
#'
#' @return A data frame summarizing:
#' \describe{
#'   \item{ICD10_3LETTERS}{ICD-10 3-letter code}
#'   \item{total_patients}{Total patients across both exposure groups with this diagnosis}
#'   \item{exposure_group_patients}{Number of patients in the exposure group with this diagnosis}
#'   \item{exposure_group_pct}{Percent of the exposure group with this diagnosis}
#'   \item{no_exposure_group_patients}{Number of patients in the no-exposure group with this diagnosis}
#'   \item{no_exposure_group_pct}{Percent of the no-exposure group with this diagnosis}
#'   \item{diff_pct}{Difference in percentage points between groups (exposure minus no exposure)}
#'   \item{DESC}{ICD-10 code description (from `data_codes`)}
#' }
#'
#' @details
#' This function is useful for summarizing diagnostic differences between exposed and unexposed groups.
#' It excludes diagnoses matching the exposure ICD-10 pattern to avoid circularity.
#'
#' Internally uses `.safe_inc_progress()` for progress tracking and integrates with Shiny's
#' `withProgress()` if called from a running Shiny session.
#'
#' **Note**: The `data_codes` object must be available in the global environment or within the package,
#' and must contain `CODECLASS == "ICD10"`, `DG`, and `DESC` columns.
#'
#' @examples
#' \dontrun{
#' tbl <- tbl_icd10_diff_by_exposure(
#'   data = dpop,
#'   diagnoses = diagnoses,
#'   exposure_icd10 = "^E11",
#'   exposure_src = c("hilmo", "avohilmo")
#' )
#' head(tbl)
#' }
#'
#' @importFrom dplyr filter group_by summarise mutate left_join select
#' @importFrom shiny isRunning withProgress
#' @export
tbl_icd10_diff_by_exposure <- function(data, diagnoses, exposure_icd10, exposure_src){

  internal_function <- function(){
    # data <- dpop
    # data_diagnoses <- diagnoses
    # exposure_icd10 <- "^E11"
    # exposure_src <- c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")

    .safe_inc_progress(1/4)

    data_diagnoses <- diagnoses

    # Tarkastellaan TOP diagnoosit exposure populaatiolla
    d1 <- data_diagnoses |>
      filter(DGREG == "ICD10") |>
      filter(SRC  %in% exposure_src) |>
      filter(ID %in% data$ID[data$exp.GROUP == "exposure"] & !(grepl(pattern = .regex_clean(exposure_icd10), DG))) |>
      group_by(ICD10_3LETTERS) |>
      summarise(
        exposure_group_patients = length(unique(ID))
      ) |>
      mutate(
        exposure_group_pct = round(100 * exposure_group_patients / nrow(data[data$exp.GROUP == "exposure",]), 1)
      )

    .safe_inc_progress(2/4)

    # Tarkastellaan TOP diagnoosit no-exposure populaatiolla
    d2 <- data_diagnoses |>
      filter(DGREG == "ICD10") |>
      filter(SRC  %in% exposure_src) |>
      filter(ID %in% data$ID[data$exp.GROUP == "no exposure"]) |>
      group_by(ICD10_3LETTERS) |>
      summarise(
        no_exposure_group_patients = length(unique(ID))
      ) |>
      mutate(
        no_exposure_group_pct = round(100 * no_exposure_group_patients / nrow(data[data$exp.GROUP == "no exposure",]), 1)
      )

    .safe_inc_progress(3/4)

    d <- left_join(d1,d2, by = "ICD10_3LETTERS") |>
      mutate(diff_pct = exposure_group_pct - no_exposure_group_pct,
             total_patients = exposure_group_patients + no_exposure_group_patients) |>
      left_join(
        data_codes |> filter(CODECLASS == "ICD10") |> select(DG, DESC), ## TODO data_codes needs to go inside the function package
        by = c("ICD10_3LETTERS" = "DG")) |>
      select(ICD10_3LETTERS, total_patients, exposure_group_patients, exposure_group_pct, no_exposure_group_patients, no_exposure_group_pct, diff_pct, DESC )

    .safe_inc_progress(4/4)

    return(d)
  }
  if(shiny::isRunning()){
    withProgress(message = "Health ICD-10 Comparison", value = 0, {
      return(internal_function())
    })
  }else{
    return(internal_function())
  }
}

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
