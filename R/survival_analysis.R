#' @title Create Long-Format Survival Data from Exposure-Response Events
#' @description Processes raw exposure-response data into a long-format dataset suitable for survival analysis,
#'              including response, death, and censoring times. Supports use in Shiny with progress tracking.
#'
#' @param data A data frame that must include columns: `ID`, `exp.GROUP`, `exp.DATE`, `resp.DATE`, `DATE_DEATH`,
#'             `DATE_MIGRATION`. These are used to calculate event and censoring times.
#' @param censoring_date A `Date` object specifying the administrative censoring date. Default is `"2023-12-21"`.
#' @param filter_early_responses Logical; if `TRUE`, responses occurring before the exposure date (negative time)
#'        are filtered out. If `FALSE`, they are included and recoded to 0. Default is `FALSE`.
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
#' @importFrom dplyr filter mutate select arrange group_by summarise %>%
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate days `%--%`
#' @importFrom shiny isRunning withProgress
#'
#' @export
create_dsurv <- function(data,
                         censoring_date = as.Date("2023-12-21"),
                         filter_early_responses = FALSE
){
  internal_function <- function(){

    .safe_inc_progress(1/3)

    exposure_to_response <- data %>%
      filter(exp.GROUP == "exposure") %>%
      mutate(
        epvm=pmin(DATE_MIGRATION, censoring_date, na.rm = TRUE)
      ) %>%
      mutate(diagnose =  trunc((exp.DATE %--% resp.DATE) / days(1) ),
             dead = ifelse(!is.na(DATE_DEATH), trunc((exp.DATE %--% DATE_DEATH) / days(1) ) , NA),
             censoring = trunc((exp.DATE %--% epvm) / days(1))
      ) %>%
      mutate(censoring = ifelse(is.na(dead), censoring, NA))

    .safe_inc_progress(2/3)

    d <- exposure_to_response %>%
      select(ID, resp.DATE, diagnose, dead, censoring) %>%
      arrange(ID, resp.DATE) %>%
      ## FILTER , take response cases before 0 timepoint, yes/no. This is going to be recoded as 0.
      filter(if(filter_early_responses){diagnose >= 0 }else{is.numeric(diagnose)}) %>%
      arrange(ID, resp.DATE) %>%
      group_by(ID) %>%
      summarise(diagnose = first(diagnose),
                dead = first(dead),
                censoring = first(censoring)) %>%
      mutate(diagnose = ifelse(diagnose < 0, 0 , diagnose)) %>% # Recoding possible before 0 timepoints for response dg
      pivot_longer(cols = c(diagnose, dead, censoring)) %>%
      filter(!is.na(value))

    .safe_inc_progress(3/3)

    return(d)
  }
  if(shiny::isRunning()){
    withProgress(message = "Creating Survival Data", value = 0, {
      return(internal_function())
    })
  }else{
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
#' @importFrom dplyr mutate %>%
#' @importFrom survival Surv survfit
#' @importFrom shiny isRunning withProgress
#'
#' @export
plot_survival_km <- function(data){
  internal_function <- function(){
    .safe_inc_progress(1/3)

    ## Event: 0 = censoring/kuollut, 1 = diagnose
    dsurv <- data %>%
      mutate(event = ifelse(name == "diagnose", 1, 0) )

    .safe_inc_progress(2/3)

    ## Mallinnetaan elinaika-analyysi
    # library(survival)
    surv_object <- survival::Surv(time = dsurv$value, event = dsurv$event)
    fit1 <- survfit(surv_object ~ 1, data = dsurv, id = ID)

    .safe_inc_progress(3/3)

    plot <- plot(fit1)
    return(plot)
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
#' @importFrom dplyr mutate %>%
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
    dsurv <- data %>%
      mutate(
        event = ifelse(name == "diagnose", 1, ifelse(name == "dead", 2, 3))
      )

    ## fitting a competing risks model
    CR <- cuminc(ftime = dsurv$value,
                 fstatus = dsurv$event,
                 cencode = 3)

    .safe_inc_progress(2/3)

    plt <- ggcompetingrisks(fit = CR,
                            multiple_panels = F,
                            xlab = "Days",
                            ylab = "Cumulative incidence of event",
                            title = "Competing Risks Analysis") +
      scale_color_manual(name="",
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
