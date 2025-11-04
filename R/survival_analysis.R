#' @title Create Long-Format Survival Data from Exposure-Response Events
#' @description Processes raw exposure-response data into a long-format dataset suitable for survival analysis,
#'              including response, death, and censoring times. Supports use in Shiny with progress tracking.
#'
#' @param data A data frame that must include columns: `ID`, `exp.GROUP`, `exp.DATE`, `resp.DATE`, `DATE_DEATH`,
#'             `DATE_MIGRATION`. These are used to calculate event and censoring times.
#' @param censoring_date A `Date` object specifying the administrative censoring date. Default is `"2023-12-21"`.
#' @param filter_early_responses Logical; if `TRUE`, responses occurring before the exposure date (negative time)
#'        are filtered out. If `FALSE`, they are included and recoded to 0. Default is `FALSE`.
#' @param type character. Options: "exp_to_resp" or "resp_to_death"
#'
#' @return A data frame in long format with columns:
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{name}{Type of event: `"diagnose"`, `"dead"`, or `"censoring"`}
#'   \item{value}{Time (in days) from exposure to event}
#' }
#'
#' @details This function is intended to support dynamic survival model construction. It handles competing risks
#' (response, death, censoring) and can run inside a Shiny app with progress bar support via `.safe_inc_progress()`.
#' Internally, it filters and summarizes the earliest valid event per subject.
#'
#' @examples
#' \dontrun{
#' dsurv <- create_dsurv(
#'   data = exposure_response_df,
#'   censoring_date = as.Date("2023-12-31"),
#'   filter_early_responses = TRUE
#' )
#' }
#'
#' @importFrom dplyr filter mutate select arrange group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate days `%--%`
#' @importFrom shiny isRunning withProgress
#'
#' @export
create_dsurv <- function(data,
                         censoring_date = as.Date("2024-12-21"),
                         filter_early_responses = FALSE,
                         type = c("exp_to_resp", "resp_to_death")) {
  type <- match.arg(type)

  internal_function <- function() {
    .safe_inc_progress(1/3)

    if (type == "exp_to_resp") {
      exposure_to_response <- data |>
        dplyr::filter(exp.GROUP == "exposure") |>
        dplyr::mutate(
          epvm = pmin(DATE_MIGRATION, censoring_date, na.rm = TRUE),
          diagnose = trunc((exp.DATE %--% resp.DATE) / days(1)),
          dead = ifelse(!is.na(DATE_DEATH),
                        trunc((exp.DATE %--% DATE_DEATH) / days(1)), NA),
          censoring = trunc((exp.DATE %--% epvm) / days(1)),
          censoring = ifelse(is.na(dead), censoring, NA)
        )

      .safe_inc_progress(2/3)

      d <- exposure_to_response |>
        dplyr::select(ID, resp.DATE, diagnose, dead, censoring) |>
        dplyr::arrange(ID, resp.DATE) |>
        dplyr::filter(if (filter_early_responses) diagnose >= 0 else is.numeric(diagnose)) |>
        dplyr::group_by(ID) |>
        dplyr::summarise(
          diagnose = dplyr::first(diagnose),
          dead = dplyr::first(dead),
          censoring = dplyr::first(censoring),
          .groups = "drop"
        ) |>
        dplyr::mutate(diagnose = ifelse(diagnose < 0, 0, diagnose)) |>
        tidyr::pivot_longer(cols = c(diagnose, dead, censoring)) |>
        dplyr::filter(!is.na(value))
    }

    if (type == "resp_to_death") {
      response_to_death <- data |>
        dplyr::filter(resp.GROUP == "response") |>
        dplyr::filter(if (filter_early_responses) resp.DATE >= exp.DATE else is.numeric(ID)) |>
        dplyr::select(ID, DATE_MIGRATION, DATE_DEATH, resp.DATE, exp.GROUP, exp.DATE) |>
        dplyr::rename(GROUP = exp.GROUP) |>
        dplyr::mutate(
          epvm = pmin(DATE_MIGRATION, censoring_date, na.rm = TRUE),
          dead = ifelse(!is.na(DATE_DEATH),
                        trunc((resp.DATE %--% DATE_DEATH) / days(1)), NA),
          censoring = trunc((resp.DATE %--% epvm) / days(1)),
          censoring = ifelse(is.na(dead), censoring, NA)
        )

      .safe_inc_progress(2/3)

      d <- response_to_death |>
        dplyr::group_by(ID) |>
        tidyr::pivot_longer(cols = c(dead, censoring)) |>
        dplyr::filter(!is.na(value)) |>
        dplyr::mutate(
          status = dplyr::case_when(
            name == "censoring" ~ 0,
            name == "dead" ~ 1,
            TRUE ~ NA_real_
          )
        ) |>
        dplyr::select(ID, GROUP, value, status)
    }

    .safe_inc_progress(3/3)
    return(d)
  }

  # Run with or without shiny progress
  if (shiny::isRunning()) {
    withProgress(message = "Creating Survival Data", value = 0, {
      return(internal_function())
    })
  } else {
    return(internal_function())
  }
}



#' @title Plot Kaplan-Meier Survival Curve (Overall)
#' @description Plots an overall Kaplan-Meier survival curve from a long-format survival dataset
#'              containing response, censoring, and death events.
#'
#' @param data A data frame in long format with the following required columns:
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{name}{Type of event: must include `"diagnose"` for response; other values are treated as censored}
#'   \item{value}{Time (in days) from exposure to event}
#' }
#'
#' @return A Kaplan-Meier survival curve plotted using the base R `plot()` function.
#'
#' @details Internally converts event types to binary (1 = diagnose, 0 = other), constructs a `Surv` object,
#' and fits an overall survival model using `survfit()`. Designed to be used interactively in Shiny with
#' optional progress feedback via `.safe_inc_progress()` and `withProgress()`.
#'
#' @examples
#' \dontrun{
#' km_plot <- plot_survival_km(data = long_surv_df)
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom survival Surv survfit
#' @importFrom shiny isRunning withProgress
#'
#' @export
plot_survival_km <- function(data, color = "#D9534F", plot = c("base", "survminer")){
  internal_function <- function(){
    .safe_inc_progress(1/3)

    ## Event: 0 = censoring/kuollut, 1 = diagnose
    dsurv <- data |>
      dplyr::mutate(event = ifelse(name == "diagnose", 1, 0) )

    .safe_inc_progress(2/3)

    ## Mallinnetaan elinaika-analyysi
    # library(survival)
    # surv_object <- survival::Surv(time = dsurv$value, event = dsurv$event) # delete
    fit1 <- survival::survfit(survival::Surv(time = dsurv$value, event = dsurv$event) ~ 1, data = dsurv, id = ID)

    .safe_inc_progress(3/3)

    # Plot with base
    if(plot == "base"){
      p <- plot(fit1)
    }
    # Plot with survminer
    else if(plot == "survminer"){
      p <- survminer::ggsurvplot(
        fit1,
        data = dsurv,
        conf.int = TRUE,             # Show confidence interval
        surv.median.line = "hv",     # Horizontal and vertical median lines
        palette = color,            # Customize color
        xlab = "Time (days)",
        ylab = "Survival probability",
        ggtheme = ggplot2::theme_minimal(),
        risk.table = TRUE,           # Optional: show risk table
        risk.table.title = "Number at risk"
      )
    }

    return(p)
  }
  if(shiny::isRunning()){
    withProgress(message = "Plot Kaplan Meier", value = 0, {
      return(internal_function())
    })
  }else{
    return(internal_function())
  }
}

#' @title Plot Competing Risks Survival Curve
#' @description Plots a cumulative incidence function from a long-format dataset using a competing risks model.
#'
#' @param data A data frame in long format with the following required columns:
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{name}{Type of event: must include `"diagnose"` for response, `"dead"` for death, and others are treated as censoring}
#'   \item{value}{Time (in days) from exposure to event}
#' }
#' @param colors A character vector of color hex codes to use for the plotted event types. Default is `c("#5CB85C", "#343A40")`.
#'
#' @return A `ggplot2` object showing the cumulative incidence curves for competing risks.
#'
#' @details Internally converts event types into status codes for competing risks analysis:
#' \itemize{
#'   \item 1 = diagnose (response)
#'   \item 2 = dead
#'   \item 3 = censored
#' }
#' Uses `cuminc()` from the `cmprsk` package and visualizes the results using `ggcompetingrisks()`.
#' Designed to run in both interactive and Shiny environments, using `.safe_inc_progress()` for optional progress updates.
#'
#' @examples
#' \dontrun{
#' cr_plot <- plot_survival_cr(data = long_surv_df)
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom cmprsk cuminc
#' @importFrom survminer ggcompetingrisks
#' @importFrom ggplot2 scale_color_manual
#' @importFrom shiny isRunning withProgress
#'
#' @export
plot_survival_cr <- function(data,
                             colors = c("#5CB85C", "#343A40")
){
  internal_function <- function(){

    .safe_inc_progress(1/3)

    ## Data
    dsurv <- data |>
      dplyr::mutate(
        event = ifelse(name == "diagnose", 1, ifelse(name == "dead", 2, 3))
      )

    ## fitting a competing risks model
    CR <- cmprsk::cuminc(ftime = dsurv$value,
                 fstatus = dsurv$event,
                 cencode = 3)

    .safe_inc_progress(2/3)

    plt <- survminer::ggcompetingrisks(fit = CR,
                            multiple_panels = F,
                            xlab = "Time (days)",
                            ylab = "Cumulative incidence",
                            title = "Competing Risks: Response vs. Death") +
      ggplot2::scale_color_manual(name="",
                         values=colors,
                         labels=c("Response Diagnose", "Dead"))

    .safe_inc_progress(3/3)

    return(plt)
  }
  if(shiny::isRunning()){
    withProgress(message = "Plot Competing Risk", value = 0, {
      return(internal_function())
    })
  }else{
    return(internal_function())
  }
}

#' Plot Survival Curve for Mortality Analysis
#'
#' This function generates a Kaplan–Meier survival plot comparing groups based on
#' survival or mortality data. It uses the `survival` and `survminer` packages to
#' fit the model and visualize survival probabilities over time.
#'
#' @param data A data frame containing at least the following variables:
#'   \describe{
#'     \item{value}{Numeric; time to event or censoring in days.}
#'     \item{status}{Binary; event indicator (1 = event occurred, 0 = censored).}
#'     \item{GROUP}{Factor or character; grouping variable for comparison.}
#'   }
#' @param colors A character vector of color hex codes to use for plotting.
#'   Default is \code{c("#D9534F", "#5BC0DE")}.
#'
#' @return A `ggsurvplot` object from the \pkg{survminer} package, containing
#'   the Kaplan–Meier plot with survival probabilities, confidence intervals,
#'   p-value, and risk table.
#'
#' @details
#' The function fits a survival model using \code{survival::survfit()} and
#' visualizes it with \code{survminer::ggsurvplot()}. It is typically used to
#' analyze mortality data between exposure and non-exposure groups.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   value = c(10, 20, 30, 40, 50, 60),
#'   status = c(1, 0, 1, 1, 0, 0),
#'   GROUP = c("Exposure", "No Exposure", "Exposure", "No Exposure", "Exposure", "No Exposure")
#' )
#'
#' plot_survival_death(data)
#' }
#'
#' @importFrom survival Surv survfit
#' @importFrom survminer ggsurvplot
#' @export
plot_survival_death <- function(data, colors = c("#D9534F", "#5BC0DE") ){
  fit <- survival::survfit(survival::Surv(time = value, event = status) ~ GROUP, data = data)
  p <- survminer::ggsurvplot(
    fit,
    data = data,
    pval = TRUE,              # adds p-value from log-rank test
    conf.int = TRUE,          # adds confidence intervals
    risk.table = TRUE,        # shows number at risk table
    surv.median.line = "hv",  # shows median survival
    palette = colors,
    legend.title = "Group",
    legend.labs = c("Exposure", "No Exposure"),
    xlab = "Time (days)",
    ylab = "Survival probability"
  )
  return(p)
}

