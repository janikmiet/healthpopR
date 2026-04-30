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
