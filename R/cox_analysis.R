#' Create Time-to-Event Data for Cox Proportional Hazards Modeling
#'
#' Prepares a dataset for survival analysis with time-dependent covariates using `tmerge`.
#' This function computes baseline age, time variables for exposure and response,
#' and creates a survival object with appropriate censoring.
#'
#' @param data A data frame with baseline population information, including dates like `DATE_BIRTH`, `DATE_DEATH`, `DATE_MIGRATION`, `exp.DATE`, and `resp.DATE`.
#' @param data_dates A data frame containing answering dates per individual. Must include `ID`, `spvm` (start date), and `vpvmbl` (baseline date).
#' @param data_socioeconomic A data frame containing socioeconomic variables such as education level. Must include `ID` and covariates to be relevelled (e.g., `edu`, `bmi_cat1`, `bmi_cat2`).
#' @param reference_values A named list specifying the reference level for factor variables (e.g., `list("bmi_cat1" = "Healthy Weight", "edu" = "1 - Low")`). Used to relevel factors for Cox regression.
#' @param censoring_date A `Date` object indicating the end of follow-up for censoring. Default is `"2023-12-31"`.
#'
#' @return A data frame formatted for survival analysis, containing time-dependent covariates,
#'         `tstart`, `tstop`, event indicators (`diagnose`, `exposure`), and a `Surv` object
#'         ready for use with `coxph`.
#'
#' @details
#' - The function internally computes time-to-event and censoring variables.
#' - Uses `tmerge` to incorporate time-dependent covariates for exposure and outcome.
#' - Applies reference level adjustments via `.relevel_by_reference()` (must be defined elsewhere).
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- cox_create_data(data = dpop,
#'                           data_dates = ostpre_vastpaiv,
#'                           data_socioeconomic = edumiage)
#' }
#'
#' @importFrom dplyr mutate select filter left_join
#' @importFrom survival Surv coxph
#' @export
cox_create_data <- function(data,
                            data_dates,
                            data_socioeconomic,
                            reference_values = list("bmi_cat1" = "Healthy Weight",
                                                    "bmi_cat2" = "Healthy Weight",
                                                    "edu" = "1 - Low"),
                            censoring_date = as.Date("2023-12-31")) {

  if(FALSE){
    data = dpop
    data_dates = ostpre_vastpaiv    ## needs: spvm, vpvmbl, -> age_bs, t_vars
    data_socioeconomic = edumiage   ## needs: edu (education, factor)
    censoring_date = as.Date("2023-12-31")
    reference_values = list("bmi_cat1" = "Healthy Weight",
                            "bmi_cat2" = "Healthy Weight",
                            "edu" = "1 - Low")
  }

  # 1. Prepare answering dates and compute age at baseline
  dadates <- data_dates %>%
    dplyr::mutate(age_bs = as.numeric(difftime(as.Date(vpvmbl), as.Date(spvm), unit = "weeks")) / 52.25) %>%
    dplyr::select(ID, age_bs, vpvmbl)

  # 2. Join answering dates to population data
  d1 <- data %>%
    dplyr::left_join(dadates, by = "ID")

  # 3. Time variable calculations
  d2 <- d1 %>%
    dplyr::filter(!is.na(vpvmbl)) %>%
    dplyr::select(ID, age_bs, DATE_BIRTH, DATE_DEATH, DATE_MIGRATION, exp.DATE, resp.DATE, vpvmbl) %>%
    dplyr::mutate(
      t_exposure = as.numeric(exp.DATE - vpvmbl),
      t_response = as.numeric(resp.DATE - vpvmbl),
      epvm = pmin(DATE_MIGRATION, censoring_date, DATE_DEATH, resp.DATE + 1, na.rm = TRUE),
      t_censoring = as.numeric(epvm - vpvmbl)
    ) %>%
    dplyr::select(ID, age_bs, t_exposure, t_response, t_censoring)

  # 4. Join time variables with data_socioeconomic
  data_base <- data_socioeconomic %>%
    dplyr::left_join(d2, by = "ID") %>%
    dplyr::filter(t_censoring > 0)  # remove invalid rows

  # 5. Relevel with checking
  data_base <- .relevel_by_reference(data_base, reference_values)
  # levels(data_base$bmi_cat1)

  # 6. Create survival time structure with tmerge and time-dependent covariates
  newd1 <- survival::tmerge(data1 = data_base %>% select(ID, age_bs, bmi, bmi_cat1, bmi_cat2, edu),
                  data2 = data_base, id = ID, tstop = t_censoring)

  newd1 <- survival::tmerge(data1 = newd1, data2 = data_base, id = ID, diagnose = event(t_response))
  newd1 <- survival::tmerge(data1 = newd1, data2 = data_base, id = ID, exposure = tdc(t_exposure))

  # 7. Define survival object
  newd1$Surv <- with(newd1, Surv(tstart, tstop, diagnose))

  rm(list = c("d1", "d2", "dadates"))

  return(newd1)
}


#' Create a Cox Proportional Hazards Model with Splines and Covariates
#'
#' Constructs a Cox model using a survival formula and allows for the inclusion of
#' both regular covariates and spline-transformed continuous variables.
#'
#' @param data A data frame containing the variables required for the model, including survival time variables and covariates.
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
#'   data = cox_base,
#'   normal_vars = c("edu"),
#'   spline_vars = c("age_bs", "bmi")
#' )
#' summary(model)
#' }
#'
#' @importFrom survival Surv coxph
#' @importFrom splines bs
#' @export
create_cox_model <- function(data,
                             normal_vars = c("edu"),
                             spline_vars = c("age_bs", "bmi"),
                             surv_formula = "Surv(tstart, tstop, diagnose) ~ exposure",
                             id_var = "ID") {

  if(FALSE){
    data=cox_base
    normal_vars = c("edu")
    spline_vars = c("age_bs", "bmi")
    # spline_vars = list(age = 4),
    # surv_formula = "Surv(tstart, tstop, diagnose) ~ exposure"
    id_var = "ID"
  }

  mdl_str <- "Surv~exposure"
  for (var in spline_vars) {
    if (!var %in% names(data)) {
      stop(paste("Variable", var, "not found in the dataset."))
    }
    if (all(is.na(data[[var]]))) {
      stop(paste("Variable", var, "contains only NA values. Cannot create splines."))
    }
    mdl_str <- paste0(mdl_str, " + splines::bs(", var,")")
  }
  for (var in normal_vars) {
    if (!var %in% names(data)) {
      stop(paste("Variable", var, "not found in the dataset."))
    }
    if (all(is.na(data[[var]]))) {
      stop(paste("Variable", var, "contains only NA values."))
    }
    mdl_str <- paste0(mdl_str, " + ", var, " ")
  }
  model <- survival::coxph(as.formula(mdl_str), data = data, id = data[[id_var]])
  return(model)
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

  # Fit the survival model, not needed bc done in cox_base_data function
  # fit <- survfit(data$Surv ~ exposure, id = ID, data=data)

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
  p <- ggplot2::ggplot(mda, aes(x = x, y = predicted)) +
    ggplot2::geom_line(color = color, size = 1.2) +
    ggplot2::geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = color) +
    ggplot2::labs(
      title = title,
      x = ifelse(is.null(xlab), var, xlab),
      y = ylab
    ) +
    ggplot2::theme_minimal()

  return(p)
}
