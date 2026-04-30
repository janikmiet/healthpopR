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
