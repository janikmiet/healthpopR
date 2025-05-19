#' Clean Diagnosis Regex Strings
#'
#' Prepares diagnosis code regular expressions by removing spaces and converting to uppercase.
#'
#' @param dglist character. A string or vector of diagnosis code patterns.
#'
#' @return A character vector with all spaces removed and converted to uppercase.
#'
#' @examples
#' regex_clean("e11 ")
#' regex_clean(c(" i10", " E11 "))
#'
#' @export
.regex_clean <- function(dglist){
  rgx <- toupper(gsub(pattern = " ", replacement = "", x = dglist))
  return(rgx)
}

#' Safely Increment Shiny Progress Bar
#'
#' A helper function that safely increments a Shiny progress bar
#' only when the Shiny app is running and within a reactive context.
#'
#' This function checks whether the Shiny application is running and whether
#' there is a valid reactive domain before calling `shiny::incProgress()`. It prevents
#' errors when the progress bar is used outside of a reactive environment or during
#' testing/non-interactive use.
#'
#' @param amount Numeric value indicating how much to increment the progress bar by. Default is 1.
#'
#' @return No return value; called for its side effect of updating the progress bar.
#'
#' @seealso [shiny::incProgress()]
#'
#' @examples
#' \dontrun{
#' withProgress(message = "Loading...", {
#'   for (i in 1:10) {
#'     safe_inc_progress(0.1)
#'     Sys.sleep(0.1)
#'   }
#' })
#' }
#'
#' @export
.safe_inc_progress <- function(amount = 1) {
  if (shiny::isRunning() && !is.null(shiny::getDefaultReactiveDomain())) {
    incProgress(amount)
  }
}

#' Capitalize the First Letter of a String
#'
#' This function takes a character string and returns the same string with the
#' first letter converted to uppercase and the remaining letters converted to lowercase.
#'
#' @param s A character string to be capitalized.
#'
#' @return A character string with the first letter capitalized and the rest in lowercase.
#'
#' @examples
#' .capitalize("hello")   # "Hello"
#' .capitalize("WORLD")   # "World"
#' .capitalize("rStuDio") # "Rstudio"
#'
#' @keywords internal
.capitalize <- function(s) {
  paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
}


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
