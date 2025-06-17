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
#' @import ggplot2 dplyr hrbrthemes
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


#' Summarize Age Distribution for Exposure or Response Groups
#'
#' Creates a summary table of population counts and age statistics (minimum, median, mean, and maximum)
#' for either the "exposure" or "response" group. Optionally, it can also include subgroups for a more
#' detailed breakdown.
#'
#' @param data A data frame containing columns `exp.GROUP`, `resp.GROUP`, `exp.AGE_DG`, and `resp.AGE_DG`.
#'             These are expected to represent exposure/response group labels and ages at diagnosis.
#' @param group A character string, either `"exposure"` or `"response"`, indicating which group to summarize.
#'              Defaults to `"exposure"`.
#' @param subgroups Logical. If `TRUE`, adds subgroup statistics (cross-group summaries). Defaults to `FALSE`.
#'
#' @return A data frame summarizing the population size (`pop_n`) and age statistics:
#' \itemize{
#'   \item \code{pop_n} - Population count
#'   \item \code{age_min} - Minimum age at diagnosis
#'   \item \code{age_median} - Median age at diagnosis
#'   \item \code{age_mean} - Mean age at diagnosis
#'   \item \code{age_max} - Maximum age at diagnosis
#' }
#' When `subgroups = TRUE`, an additional "All" row summarizes the entire group.
#'
#' @details This function is intended for use in both interactive and Shiny contexts.
#' It utilizes `.safe_inc_progress()` to update progress during computation.
#'
#' @examples
#' \dontrun{
#' table_age_distribution(mydata, group = "response", subgroups = TRUE)
#' }
#'
#' @export
table_age_distribution <- function(
    data,
    group = "exposure",
    subgroups = FALSE
) {

  ## Checking
  if (!group %in% c("exposure", "response")) {
    stop("Argument 'group' must be either 'exposure' or 'response'.")
  }

  summarize_data <- function() {

    .safe_inc_progress(1/3)

    ## Simple statistics, output is one row
    if (group == "exposure") {
      d1 <- data |>
        dplyr::filter(exp.GROUP == "exposure") |>
        dplyr::rename(GROUP = exp.GROUP,
                      AGE = exp.AGE_DG) |>
        dplyr::group_by(GROUP) |>
        dplyr::summarise(
          pop_n = dplyr::n(),
          age_min = min(AGE, na.rm = TRUE),
          age_median = median(AGE, na.rm = TRUE),
          age_mean = mean(AGE, na.rm = TRUE),
          age_max = max(AGE, na.rm = TRUE)
        )
    } else if (group == "response") {
      # Response by group
      d1 <- data |>
        dplyr::filter(resp.GROUP == "response") |>
        dplyr::rename(GROUP = resp.GROUP,
                      AGE = resp.AGE_DG) |>
        dplyr::group_by(GROUP) |>
        dplyr::summarise(
          pop_n = dplyr::n(),
          age_min = min(AGE, na.rm = TRUE),
          age_median = median(AGE, na.rm = TRUE),
          age_mean = mean(AGE, na.rm = TRUE),
          age_max = max(AGE, na.rm = TRUE)
        )
    }

    .safe_inc_progress(2/3)

    # Stats for subgroups
    if(subgroups){
      if(group == "response"){
        d2 <- data |>
          dplyr::filter(resp.GROUP == "response") |>
          dplyr::rename(GROUP = exp.GROUP,
                        AGE = resp.AGE_DG) |>
          dplyr::group_by(GROUP) |>
          dplyr::summarise(
            pop_n = dplyr::n(),
            age_min = min(AGE, na.rm = TRUE),
            age_median = median(AGE, na.rm = TRUE),
            age_mean = mean(AGE, na.rm = TRUE),
            age_max = max(AGE, na.rm = TRUE)
          )
      }else{
        d2 <- data |>
          dplyr::filter(exp.GROUP == "exposure") |>
          dplyr::rename(GROUP = resp.GROUP,
                        AGE = exp.AGE_DG) |>
          dplyr::group_by(GROUP) |>
          dplyr::summarise(
            pop_n = dplyr::n(),
            age_min = min(AGE, na.rm = TRUE),
            age_median = median(AGE, na.rm = TRUE),
            age_mean = mean(AGE, na.rm = TRUE),
            age_max = max(AGE, na.rm = TRUE)
          )
      }
      ## Combine datas d1 and d2
      d1$GROUP <-  "All"
      d <- d2 |>
        rbind(d1)
    }else{
      ## No subgroups
      d <- d1
    }

    .safe_inc_progress(3/3)

    return(d)
  }

  if (shiny::isRunning()) {
    withProgress(message = paste("Table", .capitalize(group), "Age Distribution"), value = 0, {
      summarize_data()
    })
  } else {
    summarize_data()
  }
}





#' Generate Crosstabulation of Exposure and Response Variables
#'
#' This function produces a cross-tabulation summary between two variables, `exposure` and `response`,
#' from a given dataset. It returns either a styled HTML table for interactive viewing or a Word-compatible
#' `flextable` for document output.
#'
#' @param data A data frame containing variables `exposure` and `response`.
#' @param output Character string indicating the output type.
#'        Use `"viewer"` to return an `sjPlot` table for interactive viewing, or `"docx"` to return a
#'        `flextable` object for Word reports.
#'
#' @return An object of class `sjTable` (if `output = "viewer"`) or a `flextable` (if `output = "docx"`).
#'
#' @details
#' - When `output = "viewer"`, the function returns a formatted HTML table with row percentages using `sjPlot::tab_xtab()`.
#' - When `output = "docx"`, the cross-tabulation is converted to a data frame using `sjtable2df::xtab2df()` and formatted using `flextable` for inclusion in Word documents.
#'
#' @examples
#' \dontrun{
#'   # Example data
#'   df <- data.frame(
#'     exposure = sample(c(0, 1), 100, replace = TRUE),
#'     response = sample(c(0, 1), 100, replace = TRUE)
#'   )
#'   summary_exp_resp_crosstabulation(df, output = "viewer")
#'   ft <- summary_exp_resp_crosstabulation(df, output = "docx")
#' }
#'
#' @importFrom sjPlot tab_xtab
#' @importFrom sjtable2df xtab2df
#' @importFrom flextable regulartable set_caption
#' @export
summary_exp_resp_crosstabulation <- function(data, output = "viewer") {
  # output <- match.arg(output)
  # data <- dpop

  ## Checking
  if (!output %in% c("viewer", "docx")) {
    stop("Argument 'output' must be either 'viewer' or 'docx'.")
  }

  # Crosstabulation data
  tab <- sjPlot::tab_xtab(
    var.row = data$exposure,
    var.col = data$response,
    title = "Population exposure and response diagnoses",
    show.row.prc = TRUE,
    # print.summary = FALSE,   # Prevent automatic printing
    # return = "html"          # Return a kable-style HTML table
  )

  if (output == "viewer") {
    return(tab)
  } else if (output == "docx") {

    xtab_df <- sjtable2df::xtab2df(xtab = tab, output = "data.frame")

    # Create a flextable for Word
    ft <- flextable::flextable(xtab_df)
    ft <- flextable::set_caption(ft, caption = "Exposure vs Response Diagnoses")
    ft <- flextable::theme_vanilla(ft)

    # Optional styling
    ft <- flextable::fontsize(ft, size = 10, part = "all")
    ft <- flextable::align(ft, align = "center", part = "all")

    return(ft)  # Can be passed to officer::body_add_flextable()
  }
}

#' Summary of Exposure and Response Timing Order
#'
#' Summarizes the temporal relationship between exposure and response events
#' for individuals who have both an exposure and a response diagnosis.
#'
#' Categorizes the relationship as:
#' \itemize{
#'   \item \code{"Exposure < Response"} if exposure occurred before the response,
#'   \item \code{"Exposure == Response"} if they occurred on the same date,
#'   \item \code{"Exposure > Response"} if exposure occurred after the response.
#' }
#'
#' @param data A data frame containing at least the following columns:
#'   \itemize{
#'     \item \code{exposure}: Binary indicator (0/1) of exposure.
#'     \item \code{response}: Binary indicator (0/1) of response.
#'     \item \code{exp.DATE}: Date of the exposure.
#'     \item \code{resp.DATE}: Date of the response.
#'   }
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{\code{exp_resp}}{A factor indicating the exposure-response temporal relationship.}
#'     \item{\code{n}}{Count of cases in each category.}
#'     \item{\code{percentage}}{Percentage of total cases for each category.}
#'   }
#'
#' @details
#' This function is intended for use in shiny applications and supports progress indication.
#' If used in a shiny session, progress is displayed with `withProgress()`.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     exposure = sample(0:1, 100, replace = TRUE),
#'     response = sample(0:1, 100, replace = TRUE),
#'     DATE = sample(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"), 100, replace = TRUE),
#'     resp.DATE = sample(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"), 100, replace = TRUE)
#'   )
#'   summary_exp_resp_order(df)
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise case_when
#' @importFrom shiny isRunning withProgress
#' @export
summary_exp_resp_order <- function(data){
  #OBS! Previously named: tab_exp_resp
  all <- function(){
    .safe_inc_progress(amount = 1/4)
    d <- data |>
      dplyr::filter(exposure == 1 & response == 1) |>
      dplyr::mutate(
        exp_resp = ifelse(exp.DATE < resp.DATE, 1, ifelse(exp.DATE == resp.DATE, 0, -1))
        # exp_resp = ifelse(exposure_date < response_date, 1, 0)
      ) |>
      dplyr::group_by(exp_resp) |>
      dplyr::summarise(
        n = dplyr::n()
      ) |>
      dplyr::mutate(
        percentage = round(100 * n / nrow(data |> filter(exposure == 1 & response == 1)), 1),
        exp_resp = factor(case_when(
          exp_resp == 1 ~ "Exposure < Response",
          exp_resp == 0 ~ "Exposure == Response",
          exp_resp == -1 ~ "Exposure > Response"
        ), levels = c("Exposure < Response", "Exposure == Response", "Exposure > Response"))
      )
    .safe_inc_progress(amount = 4/4)
    return(d)
  }
  if(shiny::isRunning()){
    withProgress(message = "Summary of Exposure-Response Order", value = 0, {
      return(all())
    })
  }else{
    return(all())
  }
}
