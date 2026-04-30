#' @title Relevel Factor Variables by Reference
#' @description Internal helper function to relevel specified factor variables in a data frame
#'              using user-defined reference values.
#'
#' @param df A data frame containing the variables to be releveled.
#' @param reference_values A named list specifying the desired reference level for each variable.
#'                         Names should match column names in `df`; values should be valid levels.
#'
#' @return The original data frame with specified factor variables releveled to the given reference.
#'         If a variable is not found in the data frame, a warning is issued and the variable is skipped.
#'
#' @details This function is typically used before modeling to ensure that categorical variables
#'          have the appropriate reference level, particularly when computing contrasts in regression models.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(group = factor(c("A", "B", "A", "C")))
#' ref_vals <- list(group = "B")
#' df <- .relevel_by_reference(df, ref_vals)
#' levels(df$group)  # "B" will now be the reference level
#' }
#'
#' @keywords internal
.relevel_by_reference <- function(df, reference_values) {
  for (var in names(reference_values)) {
    ref_val <- reference_values[[var]]

    if (var %in% names(df)) {
      df[[var]] <- relevel(as.factor(df[[var]]), ref = ref_val)
    } else {
      warning(paste("Variable", var, "not found in the data frame. Skipping."))
    }
  }
  return(df)
}
