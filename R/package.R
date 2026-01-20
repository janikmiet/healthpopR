# Core tidyverse
#' @importFrom dplyr
#'   filter
#'   select
#'   arrange
#'   group_by
#'   summarise
#'   first
#'   mutate
#'   bind_rows
#'   n
#'   row_number
#'   case_when
#'   if_else
#'   left_join
#'
#' @importFrom tidyr
#'   pivot_wider
#'   pivot_longer
#'   replace_na
#'
#' @importFrom tibble
#'   tibble
#'   as_tibble
#'
#' @importFrom magrittr %>%

# Survival analysis
#' @importFrom survival coxph Surv
#' @importFrom splines bs
#' @importFrom stats glm relevel

# Plotting
#' @importFrom ggplot2
#'   ggplot
#'   aes
#'   geom_line
#'   geom_bar
#'   geom_ribbon
#'   coord_flip
#'   labs
#'   scale_y_continuous
#'   scale_fill_manual
#'   scale_fill_gradient
#'   theme
#'   theme_minimal
#' @importFrom ggVennDiagram ggVennDiagram
#' @importFrom hrbrthemes theme_ipsum_rc
#' @importFrom fmsb radarchart
#' @importFrom scales alpha
#' @importFrom ggeffects ggpredict
#' @importFrom ggeffects predict_response

# Shiny
#' @importFrom shiny
#'   isRunning
#'   withProgress

# Data.table
#' @importFrom data.table as.data.table

# Rlang
#' @importFrom rlang :=
#' @importFrom rlang parse_expr

# Utils
#' @importFrom utils globalVariables

# Declare global variables
globalVariables(c(
  "ID",
  "DATE_BIRTH",
  "DATE_DEATH",
  "DATE_MIGRATION",
  "exp.DATE",
  "resp.DATE",
  "vpvmbl",
  "spvm",
  "age_bs",
  "t_exposure",
  "t_response",
  "t_censoring",
  "epvm",
  "bmi",
  "bmi_cat1",
  "bmi_cat2",
  "edu",
  "exposure",
  "diagnose",
  "tstart",
  "tstop",
  "Surv",
  "x",
  "predicted",
  "conf.low",
  "conf.high",
  "DGREG",
  "ICD10_3LETTERS",
  "SRC",
  "DATE",
  "DG",
  "ICD10_CLASS",
  "AGE",
  "DESC",
  "patients",
  "cases",
  "group_pct"
))
