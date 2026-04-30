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
