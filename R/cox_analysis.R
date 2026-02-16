#' Time-dependent Cox Proportional Hazards Analysis with Splines and Diagnostics
#'
#' Fits a Cox proportional hazards model with time-dependent exposure,
#' optional spline-modeled continuous covariates, and categorical covariates.
#' The function constructs start-stop (counting process) survival data,
#' applies reference level releveling, fits the model, and returns
#' diagnostic plots and proportional hazards tests.
#'
#' Designed for registry-based longitudinal data with:
#' \itemize{
#'   \item Baseline date (`vpvmbl`)
#'   \item Exposure date (`exp.DATE`)
#'   \item Response/event date (`resp.DATE`)
#'   \item Migration and death censoring dates
#' }
#'
#' @param dpop A data frame containing population-level variables including
#'   at minimum: `ID`, `DATE_BIRTH`, `DATE_DEATH`, `DATE_MIGRATION`,
#'   `exp.DATE`, and `resp.DATE`.
#'
#' @param data_dates A data frame containing baseline date information.
#'   Must include `ID` and `vpvmbl` (baseline date).
#'
#' @param data_socioeconomic A data frame containing socioeconomic or
#'   additional covariates (e.g., education).
#'
#' @param normal_vars Character vector of covariates to include in the model
#'   as standard (non-spline) terms.
#'
#' @param spline_vars Character vector of continuous variables to be modeled
#'   using cubic B-splines (`splines::bs()`).
#'
#' @param reference_values Named list specifying reference levels for
#'   categorical variables. Used internally for releveling.
#'
#' @param censoring_date Administrative censoring date (Date).
#'
#' @details
#' The function performs the following steps:
#'
#' \enumerate{
#'   \item Merges baseline, population, and socioeconomic data.
#'   \item Calculates age at baseline and follow-up time variables.
#'   \item Handles exposure occurring before baseline by shifting exposure
#'         to baseline.
#'   \item Constructs counting-process format survival data
#'         (`Surv(tstart, tstop, event)`).
#'   \item Creates a time-dependent exposure indicator.
#'   \item Fits a Cox model using `survival::coxph()`.
#'   \item Produces:
#'     \itemize{
#'       \item Forest plot (`survminer::ggforest`)
#'       \item Spline effect plots (`ggeffects::ggpredict`)
#'       \item Proportional hazards test (`cox.zph`)
#'       \item Schoenfeld residual plots (`ggcoxzph`)
#'     }
#' }
#'
#' Tied event times are handled using default `coxph` tie handling.
#'
#' @return A list containing:
#' \describe{
#'   \item{model}{Fitted `coxph` model object.}
#'   \item{test_residual}{Result of `cox.zph()` proportional hazards test.}
#'   \item{plot_diagnostics}{Schoenfeld residual diagnostic plot.}
#'   \item{plot_splines}{Named list of spline effect plots.}
#'   \item{plot_forest}{Forest plot of model estimates.}
#' }
#'
#' @importFrom survival coxph Surv cox.zph
#' @importFrom splines bs
#' @importFrom dplyr filter select left_join mutate
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs theme_minimal
#'
#' @seealso \code{\link[survival]{coxph}},
#'   \code{\link[survminer]{ggforest}},
#'   \code{\link[survminer]{ggcoxzph}}
#'
#' @export

analysis_cox <- function(dpop,
                         data_dates,
                         data_socioeconomic,
                         normal_vars = c("edu"),
                         spline_vars = c("age_bs", "bmi"),
                         reference_values = list("bmi_cat1" = "Healthy Weight",
                                                 "bmi_cat2" = "Healthy Weight",
                                                 "edu" = "1 - Low"),
                         censoring_date = as.Date("2024-12-31")
){

  ## Setting all functionality to internal function so that the Shiny run can have messages.
  internal_function <- function(){

    # Test & DEBUG Function -----
    if(FALSE){
      dpop = dpop
      data_dates = ostpre_vastpaiv    ## needs: spvm, vpvmbl, -> age_bs, t_vars
      data_socioeconomic = population_variables   ## needs: edu (education, factor)
      normal_vars = c("edu")
      spline_vars = c("age_bs", "bmi")
      reference_values = list("bmi_cat1" = "Healthy Weight",
                              "bmi_cat2" = "Healthy Weight",
                              "edu" = "1 - Low")
      censoring_date = as.Date("2024-12-31")
    }

    # Part 1: Data --------------
    if(TRUE){

      ## 1.1 Filter only to baseline people, join with dpop & sosioeconomic variables -----
      healthpopR:::.safe_inc_progress(1/12)
      if(TRUE){
        d1 <- data_dates |>
          dplyr::filter(!is.na(vpvmbl)) |>
          dplyr::select(ID, vpvmbl) |>
          dplyr::left_join(dpop, by = "ID") |>
          dplyr::left_join(data_socioeconomic, by = "ID")

      }

      ## 1.2 Time variable calculations ------
      healthpopR:::.safe_inc_progress(2/12)
      if(TRUE){
        d2 <- d1 |>
          dplyr::mutate(
            age_bs = as.numeric(difftime(as.Date(vpvmbl), as.Date(DATE_BIRTH), unit = "weeks")) / 52.25,
            ## KOODATAAN EARLIER EXP/RESP CASE BASELINE DATEKSI
            new.exp.DATE = as.Date(ifelse(!is.na(exp.DATE) & exp.DATE < vpvmbl, vpvmbl, ifelse(!is.na(exp.DATE), exp.DATE, NA)), origin = "1970-01-01"),
            new.resp.DATE = as.Date(ifelse(!is.na(resp.DATE) & resp.DATE < vpvmbl, vpvmbl, ifelse(!is.na(resp.DATE), resp.DATE, NA)), origin = "1970-01-01"),
            ## Correction: if new.resp.DATE == vpvmbl, add +1 day. Bc model works better this way(?)
            new.resp.DATE = as.Date(ifelse(!is.na(new.resp.DATE) & new.resp.DATE == vpvmbl, new.resp.DATE +1, new.resp.DATE), origin = "1970-01-01"),
            apvm = vpvmbl,
            epvm = pmin(DATE_MIGRATION, DATE_DEATH, censoring_date, new.resp.DATE, na.rm = TRUE),
            ## Aikalaskenta
            t_exposure = as.numeric(new.exp.DATE - apvm),
            t_response = as.numeric(new.resp.DATE - apvm),
            t_censoring = as.numeric(epvm - apvm)
          ) |>
          dplyr::select(ID,exp.DATE, new.exp.DATE, new.resp.DATE, resp.DATE, apvm, epvm, t_exposure, t_response, t_censoring, age_bs, edu, bmi, bmi_cat1, bmi_cat2)
      }

      ## 1.3 Relevel sidevariables ------
      healthpopR:::.safe_inc_progress(3/12)
      if(TRUE){
        d3 <- healthpopR:::.relevel_by_reference(d2, reference_values)
      }

      ## 1.4 Final: long format ------
      healthpopR:::.safe_inc_progress(4/12)
      if(TRUE){
        ### 1.4.1 Help variables -----
        d4 <- d3 %>%
          mutate(
            t_response2  = ifelse(is.na(t_response), Inf, t_response),
            t_end        = pmin(t_response2, t_censoring, na.rm = TRUE),
            event        = ifelse(!is.na(t_response) & t_response <= t_censoring, 1, 0)
          ) %>%
          # Determine which rows need splitting
          mutate(
            split_needed = !is.na(t_exposure) &
              t_exposure > 0 &
              t_exposure < t_end
          )

        ### 1.4.2 Long dataset ----

        # Rows without split (single interval)
        no_split <- d4 %>%
          filter(!split_needed) %>%
          transmute(
            ID,
            tstart = 0,
            tstop  = t_end,
            event,
            exposure_td = 0,
            age_bs,
            edu,
            bmi
          )

        # Rows with split (two intervals)
        split_rows <- d4 %>%
          filter(split_needed)

        split_part1 <- split_rows %>%
          transmute(
            ID,
            tstart = 0,
            tstop  = t_exposure,
            event  = 0,
            exposure_td = 0,
            age_bs,
            edu,
            bmi
          )

        split_part2 <- split_rows %>%
          transmute(
            ID,
            tstart = t_exposure,
            tstop  = t_end,
            event,
            exposure_td = 1,
            age_bs,
            edu,
            bmi
          )

        # Combine everything
        cox_model_data <- bind_rows(
          no_split,
          split_part1,
          split_part2
        ) %>%
          arrange(ID, tstart)
      }

      ## 1.5 Sanity checks -------
      if(FALSE){
        # No zero-length intervals
        stopifnot(all(cox_model_data$tstop > cox_model_data$tstart))
        ## If resp.DATE == apvm --> resp.DATE +1 // This is added.

        # Event only once per ID
        cox_model_data %>%
          group_by(ID) %>%
          summarise(events = sum(event)) %>%
          pull(events) %>%
          stopifnot(all(. <= 1))
        ## check
        tmp <- cox_model_data %>%
          group_by(ID) %>%
          summarise(events = sum(event)) %>%
          pull(events)
      }

      ## 1.6 Cleaning and result ----
      healthpopR:::.safe_inc_progress(5/12)
      rm(list = c("d1", "d2", "d3", "d4", "split_rows", "split_part1", "split_part2"))
    }

    # Part 2: Model ----------
    if(TRUE){
      healthpopR:::.safe_inc_progress(6/12)

      ## Options & Arguments
      id_var = "ID" ## Could be added to arg. This is var which is in data describing patient

      ## 2.1 Model spline variables ------
      mdl_str <- "Surv(tstart, tstop, event) ~ exposure_td"
      for (var in spline_vars) {
        if (!var %in% names(cox_model_data)) {
          stop(paste("Variable", var, "not found in the dataset."))
        }
        if (all(is.na(cox_model_data[[var]]))) {
          stop(paste("Variable", var, "contains only NA values. Cannot create splines."))
        }
        ## TODO splines thoughts
        # survival::coxph(Surv(tstart, tstop, event) ~ exposure_td, data = cox_model_data, ties = "efron")
        # survival::coxph(Surv(tstart, tstop, event) ~ exposure_td + ns(bmi, df = 4) + edu, data = cox_model_data, ties = "efron")
        # survival::coxph(Surv(tstart, tstop, event) ~ exposure_td + bs(bmi, df = 4) + edu, data = cox_model_data, ties = "efron")
        ## Do we use ns or bs?
        # splines::ns
        # splines::bs
        mdl_str <- paste0(mdl_str, " + splines::bs(", var,")")
      }

      ## 2.2 Model normal variables -----
      for (var in normal_vars) {
        if (!var %in% names(cox_model_data)) {
          stop(paste("Variable", var, "not found in the dataset."))
        }
        if (all(is.na(cox_model_data[[var]]))) {
          stop(paste("Variable", var, "contains only NA values."))
        }
        mdl_str <- paste0(mdl_str, " + ", var, " ")
      }

      ## 3.3 Creating a model ------
      healthpopR:::.safe_inc_progress(7/12)
      cox_model <- survival::coxph(as.formula(mdl_str), data = cox_model_data, id = cox_model_data[[id_var]])
      healthpopR:::.safe_inc_progress(8/12)
    }

    # Part 3: Diagnostics ---------
    if(TRUE){

      ## 3.1 Spline plots -----
      spline_plots <- list()
      if(length(spline_vars) > 0){
        for (spline_var in spline_vars) {
          ### TODO Arguments / Options
          title = "Spline effect"
          xlab = NULL
          ylab = "Predicted risk"
          color = "#0072B2"

          ### 3.1.1 Predict effects using ggeffects -----
          mda <- ggeffects::ggpredict(cox_model, terms = paste0(spline_var, " [all]"))

          ### 3.1.2 Create the plot -------
          p <- ggplot2::ggplot(mda, ggplot2::aes(x = x, y = predicted)) +
            ggplot2::geom_line(color = color, linewidth = 1.2) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = color) +
            ggplot2::labs(
              title = title,
              x = ifelse(is.null(xlab), spline_var, xlab),
              y = ylab
            ) +
            ggplot2::theme_minimal()

          ### 3.1.3 Store plot using spline variable name ------
          spline_plots[[spline_var]] <- p
        }
      }

      healthpopR:::.safe_inc_progress(9/12)

      ## 3.2 Forestplot -----
      if(TRUE){
        ## TODO tweak outputs / no need all vars
        plot_forest <- survminer::ggforest(
          model = cox_model,
          data = model.frame(cox_model)
        )
      }

      healthpopR:::.safe_inc_progress(10/12)

      ## 3.3 Residual test & plot ----
      if(TRUE){
        test_residual <- cox.zph(cox_model)
        plot_diagnostics <- survminer::ggcoxzph(test_residual)
      }
      healthpopR:::.safe_inc_progress(11/12)
    }

    # Part 4 - Results ---------
    if(TRUE){
      results <- list(
        model = cox_model,
        # data = cox_model_data, # EI ID tasoista dataa uulos
        test_residual = test_residual,
        plot_diagnostics = plot_diagnostics,
        plot_splines = spline_plots,
        plot_forest = plot_forest
      )
      healthpopR:::.safe_inc_progress(12/12)
    }
    return(results)
  }

  ## Decide if running in Shiny or normal ------
  if (shiny::isRunning()) {
    withProgress(message = paste("Cox Analysis"), value = 0, {
      internal_function()
    })
  } else {
    internal_function()
  }
}






#' Create counting-process dataset for time-dependent Cox model
#'
#' Constructs a long-format dataset suitable for \code{survival::coxph()}
#' using counting-process notation \code{Surv(tstart, tstop, event)}.
#' The function prepares a follow-up cohort starting at baseline and models
#' a time-dependent exposure diagnosis affecting the hazard of a response
#' diagnosis.
#'
#' Baseline covariates (e.g., age, education, BMI) are treated as fixed.
#' Exposure is handled as a time-dependent variable that switches from 0 to 1
#' at the exposure diagnosis date, if it occurs before the end of follow-up.
#'
#' @param dpop A data frame containing population-level variables. Must include
#'   \code{ID}, \code{DATE_BIRTH}, \code{DATE_MIGRATION}, \code{DATE_DEATH},
#'   and diagnosis dates \code{exp.DATE} and \code{resp.DATE}.
#'
#' @param data_dates A data frame containing baseline dates. Must include
#'   \code{ID} and baseline date variable \code{vpvmbl}.
#'
#' @param data_socioeconomic A data frame containing socioeconomic and baseline
#'   questionnaire variables. Must include \code{ID} and covariates such as
#'   \code{edu}, \code{bmi}, \code{bmi_cat1}, and \code{bmi_cat2}.
#'
#' @param reference_values A named list defining reference levels for factor
#'   variables (e.g., education or BMI categories). Passed internally to
#'   \code{healthpopR:::.relevel_by_reference()}.
#'
#' @param censoring_date Administrative censoring date. Default is
#'   \code{as.Date("2024-12-31")}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Filters individuals with a valid baseline date.
#'   \item Computes age at baseline.
#'   \item Recodes exposure and response diagnoses occurring before baseline
#'         to the baseline date.
#'   \item Defines follow-up end as the minimum of migration, death,
#'         administrative censoring, or response diagnosis.
#'   \item Computes follow-up times (in days) from baseline.
#'   \item Splits follow-up into one or two intervals depending on whether
#'         exposure occurs before the end of follow-up.
#' }
#'
#' Each row in the output represents a time interval during which exposure
#' status is constant.
#'
#' @return
#' A long-format data frame with the following variables:
#' \describe{
#'   \item{ID}{Individual identifier}
#'   \item{tstart}{Start of interval (days since baseline)}
#'   \item{tstop}{End of interval (days since baseline)}
#'   \item{event}{Event indicator (1 = response diagnosis, 0 = censored)}
#'   \item{exposure_td}{Time-dependent exposure indicator (0/1)}
#'   \item{age_bs}{Age at baseline (years)}
#'   \item{edu}{Education level (factor)}
#'   \item{bmi}{Baseline BMI}
#' }
#'
#' The output is ready for use in:
#' \preformatted{
#' coxph(Surv(tstart, tstop, event) ~ exposure_td + ..., data = output)
#' }
#'
#' @seealso \code{\link[survival]{coxph}}, \code{\link[survival]{Surv}}
#'
#' @export
cox_create_data <- function(dpop,
                            data_dates,
                            data_socioeconomic,
                            reference_values = list("bmi_cat1" = "Healthy Weight",
                                                    "bmi_cat2" = "Healthy Weight",
                                                    "edu" = "1 - Low"),
                            censoring_date = as.Date("2024-12-31")) {

  ## Testing function
  if(FALSE){
    dpop = dpop
    data_dates = ostpre_vastpaiv    ## needs: spvm, vpvmbl, -> age_bs, t_vars
    data_socioeconomic = population_variables   ## needs: edu (education, factor)
    censoring_date = as.Date("2023-12-31")
    reference_values = list("bmi_cat1" = "Healthy Weight",
                            "bmi_cat2" = "Healthy Weight",
                            "edu" = "1 - Low")
  }

  internal_function <- function(){

    ## 1. Filter only to baseline people, join with dpop & sosioeconomic variables
    healthpopR:::.safe_inc_progress(1/5)
    if(TRUE){
      d1 <- data_dates |>
        dplyr::filter(!is.na(vpvmbl)) |>
        dplyr::select(ID, vpvmbl) |>
        dplyr::left_join(dpop, by = "ID") |>
        dplyr::left_join(data_socioeconomic, by = "ID")

    }

    ## 2. Time variable calculations
    healthpopR:::.safe_inc_progress(2/5)
    if(TRUE){
      d2 <- d1 |>
        dplyr::mutate(
          age_bs = as.numeric(difftime(as.Date(vpvmbl), as.Date(DATE_BIRTH), unit = "weeks")) / 52.25,
          ## KOODATAAN EARLIER EXP/RESP CASE BASELINE DATEKSI
          new.exp.DATE = as.Date(ifelse(!is.na(exp.DATE) & exp.DATE < vpvmbl, vpvmbl, ifelse(!is.na(exp.DATE), exp.DATE, NA)), origin = "1970-01-01"),
          new.resp.DATE = as.Date(ifelse(!is.na(resp.DATE) & resp.DATE < vpvmbl, vpvmbl, ifelse(!is.na(resp.DATE), resp.DATE, NA)), origin = "1970-01-01"),
          ## Correction: if new.resp.DATE == vpvmbl, add +1 day. Bc model works better this way(?)
          new.resp.DATE = as.Date(ifelse(!is.na(new.resp.DATE) & new.resp.DATE == vpvmbl, new.resp.DATE +1, new.resp.DATE), origin = "1970-01-01"),
          apvm = vpvmbl,
          epvm = pmin(DATE_MIGRATION, DATE_DEATH, censoring_date, new.resp.DATE, na.rm = TRUE),
          ## Aikalaskenta
          t_exposure = as.numeric(new.exp.DATE - apvm),
          t_response = as.numeric(new.resp.DATE - apvm),
          t_censoring = as.numeric(epvm - apvm)
        ) |>
        dplyr::select(ID,exp.DATE, new.exp.DATE, new.resp.DATE, resp.DATE, apvm, epvm, t_exposure, t_response, t_censoring, age_bs, edu, bmi, bmi_cat1, bmi_cat2)
    }

    ## 3. Relevel sidevariables
    healthpopR:::.safe_inc_progress(3/5)
    if(TRUE){
      d3 <- healthpopR:::.relevel_by_reference(d2, reference_values)
    }

    ## 4. Final: long format
    healthpopR:::.safe_inc_progress(4/5)
    if(TRUE){
      ## Help variables
      d4 <- d3 %>%
        mutate(
          t_response2  = ifelse(is.na(t_response), Inf, t_response),
          t_end        = pmin(t_response2, t_censoring, na.rm = TRUE),
          event        = ifelse(!is.na(t_response) & t_response <= t_censoring, 1, 0)
        )
      ## Long dataset
      cox_model_data <- d4 %>%
        rowwise() %>%
        do({
          row <- .
          # Does exposure happen before end of follow-up?
          split_needed <- !is.na(row$t_exposure) &&
            row$t_exposure > 0 &&
            row$t_exposure < row$t_end
          ## Splitting or not
          if (!split_needed) {
            # Single interval
            tibble(
              ID = row$ID,
              tstart = 0,
              tstop  = row$t_end,
              event  = row$event,
              exposure_td = 0,
              age_bs = row$age_bs,
              edu = row$edu,
              bmi = row$bmi
            )
          } else {
            # Two intervals
            tibble(
              ID = row$ID,
              tstart = c(0, row$t_exposure),
              tstop  = c(row$t_exposure, row$t_end),
              event  = c(0, row$event),
              exposure_td = c(0, 1),
              age_bs = row$age_bs,
              edu = row$edu,
              bmi = row$bmi
            )
          }
        }) %>%
        ungroup()


      ## Sanity checks
      if(FALSE){
        # No zero-length intervals
        stopifnot(all(cox_model_data$tstop > cox_model_data$tstart))
        ## If resp.DATE == apvm --> resp.DATE +1 // This is added.

        # Event only once per ID
        cox_model_data %>%
          group_by(ID) %>%
          summarise(events = sum(event)) %>%
          pull(events) %>%
          stopifnot(all(. <= 1))
        ## check
        tmp <- cox_model_data %>%
          group_by(ID) %>%
          summarise(events = sum(event)) %>%
          pull(events)
      }
    }
    ## All ready to go
    healthpopR:::.safe_inc_progress(5/5)
    rm(list = c("d1", "d2", "d3", "d4"))
    return(cox_model_data)
  }

  ## Decide if running in Shiny or normal
  if (shiny::isRunning()) {
    withProgress(message = paste("Cox Data"), value = 0, {
      internal_function()
    })
  } else {
    internal_function()
  }

}


#' Create a Cox Proportional Hazards Model with Splines and Covariates
#'
#' Constructs a Cox model using a survival formula and allows for the inclusion of
#' both regular covariates and spline-transformed continuous variables.
#'
#' @param cox_model_data A data frame containing the variables required for the model, including survival time variables and covariates.
#' @param normal_vars A character vector of covariate names to be included as standard (non-spline) variables in the model (e.g., `"edu"`).
#' @param spline_vars A character vector of continuous variables for which natural cubic splines will be used (via `splines::bs()`).
#' @param surv_formula A character string specifying the survival outcome in `Surv()` notation.
#'        Currently not parsed dynamically; the function uses `Surv(tstart, tstop, diagnose)` internally.
#' @param id_var A string giving the column name for individual-level IDs to be used in the `coxph()` call.
#'
#' @return An object of class `coxph` representing the fitted Cox proportional hazards model.
#'
#' @details
#' - The function validates variable presence and ensures spline variables are not entirely missing (`NA`).
#' - Splines are applied using `splines::bs()` to the specified `spline_vars`.
#' - The survival object is internally defined as `Surv(tstart, tstop, diagnose)`.
#'
#' @examples
#' \dontrun{
#' model <- create_cox_model(
#'   cox_model_data = cox_model_data,
#'   normal_vars = c("edu"),
#'   spline_vars = c("age_bs", "bmi")
#' )
#' summary(model)
#' }
#'
#' @importFrom survival Surv coxph
#' @importFrom splines bs
#' @export
cox_create_model <- function(cox_model_data,
                             normal_vars = c("edu"),
                             spline_vars = c("age_bs", "bmi"),
                             surv_formula = "Surv(tstart, tstop, diagnose) ~ exposure",
                             id_var = "ID") {

  ## Testing the function
  if(FALSE){
    cox_model_data= cox_model_data
    normal_vars = c("edu")
    spline_vars = c("age_bs", "bmi")
    # spline_vars = list(age = 4),
    # surv_formula = "Surv(tstart, tstop, diagnose) ~ exposure"
    id_var = "ID"
  }

  internal_function <- function(){

    healthpopR:::.safe_inc_progress(1/3)
    ## Model function / spline variables
    mdl_str <- "Surv(tstart, tstop, event) ~ exposure_td"
    for (var in spline_vars) {
      if (!var %in% names(cox_model_data)) {
        stop(paste("Variable", var, "not found in the dataset."))
      }
      if (all(is.na(cox_model_data[[var]]))) {
        stop(paste("Variable", var, "contains only NA values. Cannot create splines."))
      }
      mdl_str <- paste0(mdl_str, " + splines::bs(", var,")")
    }

    ## Model function / normal variables
    for (var in normal_vars) {
      if (!var %in% names(cox_model_data)) {
        stop(paste("Variable", var, "not found in the dataset."))
      }
      if (all(is.na(cox_model_data[[var]]))) {
        stop(paste("Variable", var, "contains only NA values."))
      }
      mdl_str <- paste0(mdl_str, " + ", var, " ")
    }

    ## Creating a model
    healthpopR:::.safe_inc_progress(2/3)
    cox_model <- survival::coxph(as.formula(mdl_str), data = cox_model_data, id = cox_model_data[[id_var]])

    ## TODO thoughts
    # survival::coxph(Surv(tstart, tstop, event) ~ exposure_td, data = cox_model_data, ties = "efron")
    # survival::coxph(Surv(tstart, tstop, event) ~ exposure_td + ns(bmi, df = 4) + edu, data = cox_model_data, ties = "efron")
    # survival::coxph(Surv(tstart, tstop, event) ~ exposure_td + bs(bmi, df = 4) + edu, data = cox_model_data, ties = "efron")
    ## Do we use ns or bs?
    # splines::ns
    # splines::bs

    healthpopR:::.safe_inc_progress(3/3)
    return(cox_model)
  }

  if (shiny::isRunning()) {
    withProgress(message = paste("Cox Model"), value = 0, {
      internal_function()
    })
  } else {
    internal_function()
  }

}

#' Plot Survival Curves from Cox Model Data
#'
#' Visualizes survival information using different representations (events, cumulative hazard, or survival percentage)
#' for exposure groups based on a precomputed survival object.
#'
#' @param data A data frame containing the survival object (e.g., `Surv`) and exposure variable.
#'        The survival object must already be computed (typically by `cox_create_data()`), and the data should be ready for plotting.
#' @param type Character string indicating the type of plot to generate. Options are:
#'        `"event"` for number of events over time,
#'        `"cumhaz"` for cumulative hazard,
#'        `"pct"` for survival percentage. Default is `"pct"`.
#' @param colors_exposure_groups A character vector of color codes for the exposure groups. Default is Bootstrap-themed: `c("#5BC0DE", "#D9534F")`.
#' @param legend_labels A character vector of labels for the exposure groups shown in the legend. Default is `c("No exposure", "Exposure")`.
#' @param conf_int Logical. Whether to display confidence intervals (only applies if `type = "pct"`). Default is `TRUE`.
#' @param risk_table Logical. Whether to include a risk table below the survival plot (only applies if `type = "pct"`). Default is `FALSE`.
#'
#' @return A `ggsurvplot` object, which can be printed or further modified with `ggplot2` functions.
#'
#' @details
#' The function expects the input data to already include a survival object named `Surv`, such as one created via
#' `Surv(tstart, tstop, diagnose)` within `cox_create_data()`. The exposure variable should also be present and
#' appropriately formatted.
#'
#' @examples
#' \dontrun{
#' plot <- cox_plot_overall(
#'   data = cox_base,
#'   type = "pct",
#'   colors_exposure_groups = c("#5BC0DE", "#D9534F"),
#'   legend_labels = c("No exposure", "Exposure"),
#'   conf_int = TRUE,
#'   risk_table = TRUE
#' )
#' print(plot)
#' }
#'
#' @importFrom survminer ggsurvplot
#' @export
cox_plot_overall <- function(data,
                             # surv_formula = Surv(tstart, tstop, diagnose) ~ exposure,
                             type = c("event", "cumhaz", "pct"),
                             colors_exposure_groups = c("#5BC0DE", "#D9534F"),
                             legend_labels = c("No exposure", "Exposure"),
                             conf_int = TRUE,
                             risk_table = FALSE) {

  if(FALSE){
    data = cox_base
    type = "pct"
    colors_exposure_groups = c("#5BC0DE", "#D9534F")
    legend_labels = c("No exposure", "Exposure")
    conf_int = TRUE
    risk_table = FALSE
  }

  # Fit the survival model
  # TODO not needed bc done in cox_base_data function. How to pick from there?
  fit <- survfit(data$Surv ~ exposure, id = ID, data=data)

  # Choose plot type based on 'type' argument
  plot <- switch(
    type,
    "event" = survminer::ggsurvplot(
      fit, data = data, fun = "event", palette = colors_exposure_groups
    ),
    "cumhaz" = survminer::ggsurvplot(
      fit, data = data, fun = "cumhaz", palette = colors_exposure_groups
    ),
    "pct" = survminer::ggsurvplot(
      fit,
      data = data,
      fun = "pct",
      conf.int = conf_int,
      risk.table = risk_table,
      size = 1,
      linetype = "strata",
      palette = colors_exposure_groups,
      legend = "bottom",
      legend.title = "Group",
      legend.labs = legend_labels
    )
  )

  return(plot)
}

#' Plot Spline Effect from Cox Proportional Hazards Model
#'
#' Generates a plot showing the spline-transformed effect of a continuous variable in a Cox model
#' using predicted marginal effects with confidence intervals.
#'
#' @param data_model A fitted Cox proportional hazards model object created with `coxph()` and spline terms (e.g., using `splines::bs()`).
#' @param spline_var Character string specifying the name of the spline variable in the model to visualize (e.g., `"age_bs"` or `"bmi"`).
#' @param title Character string specifying the title of the plot. Default is `"Spline effect"`.
#' @param xlab Optional label for the x-axis. If `NULL`, the variable name will be used.
#' @param ylab Label for the y-axis. Default is `"Predicted risk"`.
#' @param color Character string with a valid color for the line and confidence ribbon. Default is Bootstrap blue `"#0072B2"`.
#'
#' @return A `ggplot` object showing the effect of the spline-transformed variable with a confidence ribbon.
#'
#' @details
#' The function uses `ggeffects::ggpredict()` to compute marginal effects across the full range of the spline variable.
#' It assumes the model was created using natural splines via `splines::bs()` and includes the variable named in `spline_var`.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' library(splines)
#'
#' # Example model with spline
#' model <- coxph(Surv(time, status) ~ splines::bs(age, df = 4), data = lung)
#'
#' # Plot the spline effect
#' p <- cox_plot_spline(model, spline_var = "age", title = "Effect of Age")
#' print(p)
#' }
#'
#' @importFrom ggeffects ggpredict
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs theme_minimal
#' @export
cox_plot_spline <- function(data_model,
                            spline_var = "age_bs",  # or "bs_bmi"
                            title = "Spline effect",
                            xlab = NULL,
                            ylab = "Predicted risk",
                            color = "#0072B2") {

  # Predict effects using ggeffects
  library(ggeffects)
  # mda <- ggeffect(data_model, terms = var)
  mda <- ggeffects::ggpredict(data_model, terms = paste0(spline_var, " [all]"))

  # Create the plot
  p <- ggplot2::ggplot(mda, ggplot2::aes(x = x, y = predicted)) +
    ggplot2::geom_line(color = color, linewidth = 1.2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = color) +
    ggplot2::labs(
      title = title,
      x = ifelse(is.null(xlab), spline_var, xlab),
      y = ylab
    ) +
    ggplot2::theme_minimal()

  return(p)
}
