#' Competing risks survival analysis between exposure and response diagnoses
#'
#' Performs a competing risks survival analysis using registry-based diagnosis
#' dates. The function supports different follow-up starting points
#' (exposure, response, or age 50) and multiple strategies for handling
#' diagnoses that occur before cohort entry. Results include cumulative
#' incidence estimates and ready-made ggplot visualizations.
#'
#' @details
#' The analysis workflow consists of:
#' \enumerate{
#'   \item Constructing individual-level date data (birth, death, migration, diagnoses).
#'   \item Defining follow-up start based on \code{start}.
#'   \item Handling diagnoses occurring before cohort entry using
#'   \code{pre_entry_handling}.
#'   \item Computing event times (exposure, response, death, censoring).
#'   \item Selecting the first occurring event per individual.
#'   \item Estimating cumulative incidence functions using
#'   \code{\link[cmprsk]{cuminc}}.
#' }
#'
#' Time is internally calculated in days and additionally expressed in years
#' (365.25 days). Individuals who die before follow-up start are excluded.
#'
#' @param exposure_diagnoses A data frame containing exposure diagnoses.
#'   Must include columns \code{ID} and \code{DATE}.
#'
#' @param response_diagnoses A data frame containing response diagnoses.
#'   Must include columns \code{ID} and \code{DATE}.
#'
#' @param dpop Population-level data frame with at least the columns
#'   \code{ID}, \code{DATE_BIRTH}, \code{DATE_DEATH}, and \code{DATE_MIGRATION}.
#'
#' @param start Character string defining the start of follow-up.
#'   One of \code{"DATE_EXPOSURE"}, \code{"DATE_RESPONSE"}, or \code{"DATE_50"}.
#'
#' @param censoring_date Date defining administrative censoring
#'   (default: \code{2024-12-21}).
#'
#' @param pre_entry_handling Strategy for handling diagnoses occurring before
#'   cohort entry (age 50):
#'   \describe{
#'     \item{truncate}{Diagnosis date is set to entry date.}
#'     \item{skip}{Diagnoses before entry are ignored; first post-entry diagnosis is used.}
#'     \item{asis}{Diagnosis date is used as recorded.}
#'   }
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{plot_days}{\code{ggplot} object of cumulative incidence (days).}
#'   \item{plot_years}{\code{ggplot} object of cumulative incidence (years).}
#'   \item{CR_days}{\code{cuminc} object with time in days.}
#'   \item{CR_years}{\code{cuminc} object with time in years.}
#'   \item{dsurv}{Final individual-level survival data used in the analysis.}
#' }
#'
#' @seealso
#' \code{\link[cmprsk]{cuminc}},
#' \code{\link[survminer]{ggcompetingrisks}}
#'
#' @importFrom dplyr mutate select filter arrange group_by summarise slice_min left_join
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate days `%--%`
#' @importFrom cmprsk cuminc
#' @importFrom survminer ggcompetingrisks
#'
#' @export
survival_analysis <- function(exposure_diagnoses,
                              response_diagnoses,
                              dpop,
                              start = c("DATE_EXPOSURE", "DATE_RESPONSE", "DATE_50"),
                              # DATE_EXPOSURE
                              # DATE_RESPONSE
                              # DATE_50
                              censoring_date = as.Date("2024-12-21"),
                              pre_entry_handling = c("truncate", "skip", "asis")
                              # truncate: diagnosis before entry is assigned to entry date
                              # skip: diagnosis before entry is ignored; first post-entry diagnosis is used
                              # asis: diagnosis before entry is used as it is
) {
  # type <- match.arg(type)
  internal_function <- function() {
    .safe_inc_progress(1/3)
    ## ESIMERKKI CASE
    # start = "DATE_50"
    # censoring_date = as.Date("2023-12-01")
    # pre_entry_handling = "truncate" #c("truncate", "skip", "asis")
    colors_groups <- c(
      "non-exposure" = "#5BC0DE",
      "exposure"     = "#D9534F",
      "non-response" = "#F0AD4E",
      "response"     = "#5CB85C",
      "dead"         = "#292B2C"
    )

    ## PHASE 0 - MUODOSTETAAN PVM DATA
    ### Aineisto pvm per ID
    if(TRUE){
      ## Populaation oleelliset pvm-muuttujat
      d0 <- dpop |>
        dplyr::mutate(
          DATE_BIRTH = as.Date(DATE_BIRTH),
          # DATE_DEATH = as.Date(DATE_DEATH),
          DATE_DEATH = as.Date(ifelse(DATE_DEATH > censoring_date, NA, DATE_DEATH), origin = "1970-01-01"),
          # DATE_MIGRATION = as.Date(DATE_MIGRATION),
          DATE_MIGRATION = as.Date(ifelse(DATE_MIGRATION > censoring_date, NA, DATE_MIGRATION), origin = "1970-01-01"),
          DATE_50 = DATE_BIRTH + 50 * 365.25 ## TODO this should be in dpop, and named as DATE_START
        ) |>
        select(ID, DATE_BIRTH, DATE_DEATH, DATE_MIGRATION, DATE_50)

      ## EXPOSURE DIAGNOSES
      d1 <- exposure_diagnoses |>
        dplyr::filter(DATE <= censoring_date) |>
        dplyr::left_join(d0, by = "ID") |>
        dplyr::select(ID, DATE, DATE_50) |>
        dplyr::rename(DATE_EXPOSURE = DATE) |>
        dplyr::mutate(DATE_EXPOSURE_ORIGINAL = DATE_EXPOSURE)
      ### pre_entry_handling c("truncate", "skip", "asis")
      if (pre_entry_handling == "truncate") {
        d1 <- d1 |> mutate(
          DATE_EXPOSURE = as.Date(ifelse(DATE_EXPOSURE < DATE_50, DATE_50, DATE_EXPOSURE), origin = "1970-01-01")
        )
      }
      if (pre_entry_handling == "skip") {
        d1 <- dplyr::filter(d1, DATE_EXPOSURE >= DATE_50)
      }
      ### Otetaan vain ensimmäisen exposure tapauksen DATE per id
      d1 <- d1 |>
        arrange(DATE_EXPOSURE) |>
        group_by(ID) |>
        summarise(DATE_EXPOSURE = dplyr::first(DATE_EXPOSURE),
                  DATE_EXPOSURE_ORIGINAL = dplyr::first(DATE_EXPOSURE_ORIGINAL))

      ## RESPONSE_DIAGNOSES
      d2 <- response_diagnoses |>
        dplyr::filter(DATE <= censoring_date) |>
        dplyr::left_join(d0, by = "ID") |>
        dplyr::select(ID, DATE, DATE_50) |>
        dplyr::rename(DATE_RESPONSE = DATE) |>
        dplyr::mutate(DATE_RESPONSE_ORIGINAL = DATE_RESPONSE)
      ### pre_entry_handling c("truncate", "skip", "asis")
      if (pre_entry_handling == "truncate") {
        d2 <- d2 |> mutate(
          DATE_RESPONSE_ORIGINAL = DATE_RESPONSE, # TODO this should be carried along
          DATE_RESPONSE = as.Date(ifelse(DATE_RESPONSE < DATE_50, DATE_50, DATE_RESPONSE) , origin = "1970-01-01")
        )
      }
      if (pre_entry_handling == "skip") {
        d2 <- dplyr::filter(d2, DATE_RESPONSE >= DATE_50)
      }
      ### Otetaan vain ensimmäisen response tapauksen DATE per id
      d2 <- d2 |>
        arrange(DATE_RESPONSE) |>
        group_by(ID) |>
        summarise(DATE_RESPONSE = dplyr::first(DATE_RESPONSE),
                  DATE_RESPONSE_ORIGINAL = dplyr::first(DATE_RESPONSE_ORIGINAL))



      ## FINAL (ensimmäiset DATE_EXPOSURE, DATE_RESPONSE ja DATE_CENSOR -tapaukset)
      df <- d0 |>
        dplyr::left_join(d1, by = "ID") |>
        dplyr::left_join(d2, by = "ID") |>
        filter(is.na(DATE_DEATH) | DATE_DEATH > DATE_50) |> ## tiputetaan kuolleet ennen seurantaa pois
        dplyr::mutate(
          DATE_CENSOR = pmin(DATE_MIGRATION, censoring_date, na.rm = TRUE)
        )
      rm(list = c("d0", "d1", "d2"))

      # df %>% filter(DATE_DEATH < DATE_50)
      # df$DATE_EXPOSURE <- as.Date(df$DATE_EXPOSURE, origin = "1970-01-01")
      # df$DATE_RESPONSE <- as.Date(df$DATE_RESPONSE, origin = "1970-01-01")
    }

    ## DEBUG
    if(FALSE){
      ## Katsotaan kuinka monta tapausta joissa expsure ennen response
      df %>% filter(DATE_EXPOSURE <= DATE_RESPONSE) %>% count()
      ## pysyy oikein
    }


    # Phase1 - calculate times (years from starting point)
    if(TRUE){
      dsurv_uusi <- df |>
        dplyr::mutate(
          ## tämä on joko DATE_EXPOSURE, DATE_RESPONSE tai DATE_50
          apvm =  case_when(start == "DATE_EXPOSURE" ~ DATE_EXPOSURE,
                            start == "DATE_RESPONSE" ~ DATE_RESPONSE,
                            start == "DATE_50" ~ DATE_50,
                            TRUE ~ NA_Date_),
          ## sensurointi
          epvm = DATE_CENSOR,
          ## lasketaan ajat
          time_exposure = trunc((apvm%--% DATE_EXPOSURE) / days(1)),
          # time_exposure = trunc(lubridate::time_length(apvm %--% DATE_EXPOSURE, "years")),
          time_response = trunc((apvm %--% DATE_RESPONSE) / days(1)),
          # time_response = trunc(lubridate::time_length(apvm %--% DATE_RESPONSE, "years")),
          time_dead = ifelse(!is.na(DATE_DEATH),
                             trunc((apvm %--% DATE_DEATH) / days(1)), NA),
          # trunc(lubridate::time_length(apvm %--% DATE_DEATH, "years")), NA),
          time_censoring = trunc((apvm %--% epvm) / days(1))
          # time_censoring = trunc(lubridate::time_length(apvm %--% epvm, "years"))
        )
      ## to long format
      dsurv_uusi2 <- dsurv_uusi |>
        tidyr::pivot_longer(cols = c(time_exposure, time_response, time_dead, time_censoring)) |>
        dplyr::filter(!is.na(value)) |>
        select(ID, name, value)
    }

    ## DEBUG
    if(FALSE){
      ## Katsotaan kuinka monta tapausta exp<=resp
      dsurv_uusi2 %>% filter(name == "time_response") %>% count()
      dsurv_uusi2 %>% filter(name == "time_response" & value >=0) %>% count()
      ## tässä muuttuu tapausten määrä
      ## Tarkastellaan, miten on alkuperäiset DATE määreet
      temp <- dsurv_uusi2 %>% filter(name == "time_response" & value >=0) %>% left_join(df, by = "ID")
      temp <- temp %>% mutate(flag = ifelse(DATE_RESPONSE < DATE_EXPOSURE, 1, 0))
      ## Tämä johtuu siitä että time pyöristää valmiiksi!
      ## TODO lasketaan ajat päivinä. Loppuvaiheessa voidaan muuttaa vuosiksi
    }


    ## Phase3 - create event coding
    if(TRUE){
      dsurv_uusi2 <- dsurv_uusi2 |>
        dplyr::mutate(
          event = case_when(
            ## Start == DATE_EXPOSURE
            start == "DATE_EXPOSURE" ~ case_when(
              name == "time_response"  ~ 1L,
              name == "time_dead"      ~ 2L,
              name == "time_censoring" ~ 3L,
              TRUE ~ NA_integer_
            ),
            ## Start == DATE_RESPONSE
            start == "DATE_RESPONSE" ~ case_when(
              name == "time_exposure"  ~ 1L,
              name == "time_dead"  ~ 2L,
              name == "time_censoring" ~ 3L,
              TRUE ~ NA_integer_
            ),
            # start == "DATE_50"
            start == "DATE_50" ~ case_when(
              name == "time_exposure"  ~ 1L,
              name == "time_response"  ~ 2L,
              name == "time_dead"      ~ 3L,
              name == "time_censoring" ~ 4L,
              TRUE ~ NA_integer_
            ),
            TRUE ~ NA_integer_
          )
        )
    }

    ## Limiting results only from starting point
    dsurv_uusi2 <- dsurv_uusi2 |> filter(value >=0)

    ## PHASE 5 - CR and plot
    if(TRUE){
      ## Otetaan vain ensimmäinen tapaus
      dsurv_cr <- dsurv_uusi2 |>
        filter(!is.na(event)) |> ## Tyhjät tapausrivit pois
        dplyr::group_by(ID) |>
        dplyr::slice_min(value, with_ties = FALSE) |>
        dplyr::ungroup()

      ## Aikamääre päivä ja vuosi
      dsurv_cr <- dsurv_cr %>%
        mutate(
          days = value,
          years = value / 365.25
        )


      ## Sensuroinnin koodaus (sensurointi joko 3 tai 4)
      if(start == "DATE_EXPOSURE" | start == "DATE_RESPONSE") {
        CR_days <- cmprsk::cuminc(
          ftime   = dsurv_cr$days,
          fstatus = dsurv_cr$event,
          cencode = 3
        )
        CR_years <- cmprsk::cuminc(
          ftime   = dsurv_cr$years,
          fstatus = dsurv_cr$event,
          cencode = 3
        )
      }
      if(start == "DATE_50") {
        CR_days <- cmprsk::cuminc(
          ftime   = dsurv_cr$value,
          fstatus = dsurv_cr$event,
          cencode = 4
        )
        CR_years <- cmprsk::cuminc(
          ftime   = dsurv_cr$years,
          fstatus = dsurv_cr$event,
          cencode = 4
        )
      }


      ## Lisätään värit ja nimet
      ### exposure eteenpäin tapaukset Response, Death
      if(start == "DATE_EXPOSURE"){
        ## Piirretään cumulative incidence ratio kuvaaja
        p_days <- survminer::ggcompetingrisks(
          fit = CR_days,
          multiple_panels = FALSE,
          # conf.int = TRUE,
          xlab = "Time after Exposure (days)",
          ylab = "Cumulative incidence",
          title = "Competing Risk Model: Exposure to Response/Death",
          subtitle = paste0("pre_entry_handling=", pre_entry_handling)
        ) +
          ggplot2::scale_color_manual(
            values = c("1" = colors_groups[["response"]],
                       "2" = colors_groups[["dead"]]),
            labels = c("1" = "Response",
                       "2" = "Death"),
            name = "Event"
          )
        ## Piirretään cumulative incidence ratio kuvaaja
        p_years <- survminer::ggcompetingrisks(
          fit = CR_years,
          multiple_panels = FALSE,
          xlab = "Time after Exposure (years)",
          ylab = "Cumulative incidence",
          title = "Competing Risk Model: Exposure to Response/Death",
          subtitle = paste0("pre_entry_handling=", pre_entry_handling)
        ) +
          ggplot2::scale_color_manual(
            values = c("1" = colors_groups[["response"]],
                       "2" = colors_groups[["dead"]]),
            labels = c("1" = "Response",
                       "2" = "Death"),
            name = "Event"
          )
      }
      ### Response eteenpäin tapaukset exposure, Death
      if(start == "DATE_RESPONSE"){
        ## Piirretään cumulative incidence ratio kuvaaja
        p_days <- survminer::ggcompetingrisks(
          fit = CR_days,
          multiple_panels = FALSE,
          xlab = "Time after Response (days)",
          ylab = "Cumulative incidence",
          title = "Competing Risk Model: Response to Exposure/Death",
          subtitle = paste0("pre_entry_handling=", pre_entry_handling)
        )  +
          scale_color_manual(
            values = c("1" = colors_groups[["exposure"]],
                       "2" = colors_groups[["dead"]]),
            labels = c("1" = "Exposure",
                       "2" = "Death"),
            name = "Event"
          )
        ## Piirretään cumulative incidence ratio kuvaaja
        p_years <- survminer::ggcompetingrisks(
          fit = CR_years,
          multiple_panels = FALSE,
          xlab = "Time after Response (years)",
          ylab = "Cumulative incidence",
          title = "Competing Risk Model: Response to Exposure/Death",
          subtitle = paste0("pre_entry_handling=", pre_entry_handling)
        )  +
          scale_color_manual(
            values = c("1" = colors_groups[["exposure"]],
                       "2" = colors_groups[["dead"]]),
            labels = c("1" = "Exposure",
                       "2" = "Death"),
            name = "Event"
          )
      }
      ### 50v eteenpäin Exposure, Response tai Death
      if(start == "DATE_50"){
        ## Piirretään cumulative incidence ratio kuvaaja
        p_days <- survminer::ggcompetingrisks(
          fit = CR_days,
          multiple_panels = FALSE,
          xlab = "Time after follow up start (50 years old, days)",
          ylab = "Cumulative incidence",
          title = "Competing Risk Model: From Follow up to Exposure/Response/Death",
          subtitle = paste0("pre_entry_handling=", pre_entry_handling)
        )  +
          scale_color_manual(
            values = c("1" = colors_groups[["exposure"]],
                       "2" = colors_groups[["response"]],
                       "3" = colors_groups[["dead"]]),
            labels = c("1" = "Exposure",
                       "2" = "Response",
                       "3" = "Death"),
            name = "Event"
          )
        ## Piirretään cumulative incidence ratio kuvaaja
        p_years <- survminer::ggcompetingrisks(
          fit = CR_years,
          multiple_panels = FALSE,
          xlab = "Time after follow up start (50 years old, years)",
          ylab = "Cumulative incidence",
          title = "Competing Risk Model: From Follow up to Exposure/Response/Death",
          subtitle = paste0("pre_entry_handling=", pre_entry_handling)
        )  +
          scale_color_manual(
            values = c("1" = colors_groups[["exposure"]],
                       "2" = colors_groups[["response"]],
                       "3" = colors_groups[["dead"]]),
            labels = c("1" = "Exposure",
                       "2" = "Response",
                       "3" = "Death"),
            name = "Event"
          )
      }
    }
    ## Kootaan kaikki tulokset listaan, joka palautetaan
    d <- list(plot_days = p_days,
              plot_years = p_years,
              CR_days = CR_days,
              CR_years = CR_years,
              dsurv = dsurv_cr
    )
    .safe_inc_progress(3/3)
    return(d)
  }

  # Run with or without shiny progress
  if (shiny::isRunning()) {
    withProgress(message = "Creating Survival Analysis", value = 0, {
      return(internal_function())
    })
  } else {
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
#' @param scale years or days (timeline)
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
plot_survival_death <- function(data, scale = c("days", "years"), surv.median.line = "v" ,colors = c("#D9534F", "#5BC0DE") ){
  ##DEBUG
  if(FALSE){
    data = d
    colors = c("#D9534F", "#5BC0DE")
    scale = "years"
    surv.median.line = "v"
  }

  ## scale: year conversion
  if(scale == "years"){
    data <- data %>%
      mutate(
        value = value / 365.25
      )
    xlab <- "Time (years)"
  }
  else if(scale == "days"){
    xlab <- "Time (days)"
  }

  fit <- survival::survfit(survival::Surv(time = value, event = status) ~ GROUP, data = data)
  p <- survminer::ggsurvplot(
    fit,
    data = data,
    pval = TRUE,              # adds p-value from log-rank test
    conf.int = TRUE,          # adds confidence intervals
    risk.table = TRUE,        # shows number at risk table
    surv.median.line = surv.median.line,  # shows median survival
    palette = colors,
    legend.title = "Group",
    # legend.labs = c("Exposure", "No Exposure"),
    xlab = xlab,
    ylab = "Survival probability"
  )
  return(p)
}
