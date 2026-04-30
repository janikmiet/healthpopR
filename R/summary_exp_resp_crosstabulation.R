#' Generate Crosstabulation of Exposure and Response Variables
#'
#' This function produces a cross-tabulation summary between two variables, `exposure` and `response`,
#' from a given dataset. It returns either a styled HTML table for interactive viewing or a Word-compatible
#' `flextable` for document output.
#'
#' @param data A data frame containing variables `exposure` and `response`.
#' @param output Character string indicating the output type.
#'        Use `"viewer"` to return an `sjPlot` table for interactive viewing, or `"docx"` to return a
#'        `flextable` object for Word reports.
#'
#' @return An object of class `sjTable` (if `output = "viewer"`) or a `flextable` (if `output = "docx"`).
#'
#' @details
#' - When `output = "viewer"`, the function returns a formatted HTML table with row percentages using `sjPlot::tab_xtab()`.
#' - When `output = "docx"`, the cross-tabulation is converted to a data frame using `sjtable2df::xtab2df()` and formatted using `flextable` for inclusion in Word documents.
#'
#' @examples
#' \dontrun{
#'   # Example data
#'   df <- data.frame(
#'     exposure = sample(c(0, 1), 100, replace = TRUE),
#'     response = sample(c(0, 1), 100, replace = TRUE)
#'   )
#'   summary_exp_resp_crosstabulation(df, output = "viewer")
#'   ft <- summary_exp_resp_crosstabulation(df, output = "docx")
#' }
#'
#' @importFrom sjPlot tab_xtab
#' @importFrom sjtable2df xtab2df
#' @importFrom flextable regulartable set_caption
#' @export
summary_exp_resp_crosstabulation <- function(data, output = "viewer") {
  # output <- match.arg(output)
  # data <- dpop

  ## Checking
  if (!output %in% c("viewer", "docx")) {
    stop("Argument 'output' must be either 'viewer' or 'docx'.")
  }

  # Crosstabulation data
  tab <- sjPlot::tab_xtab(
    var.row = data$exposure,
    var.col = data$response,
    title = "Population exposure and response diagnoses",
    show.row.prc = TRUE,
    # print.summary = FALSE,   # Prevent automatic printing
    # return = "html"          # Return a kable-style HTML table
  )

  if (output == "viewer") {
    return(tab)
  } else if (output == "docx") {

    xtab_df <- sjtable2df::xtab2df(xtab = tab, output = "data.frame")

    # Create a flextable for Word
    ft <- flextable::flextable(xtab_df)
    ft <- flextable::set_caption(ft, caption = "Exposure vs Response Diagnoses")
    ft <- flextable::theme_vanilla(ft)

    # Optional styling
    ft <- flextable::fontsize(ft, size = 10, part = "all")
    ft <- flextable::align(ft, align = "center", part = "all")

    return(ft)  # Can be passed to officer::body_add_flextable()
  }
}
