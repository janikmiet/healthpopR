#' Categorize BMI values into clinical weight categories
#'
#' Categorizes BMI values into 4 or 6 clinical categories as ordered factors,
#' with "Healthy Weight" as the reference level.
#'
#' @param bmi A numeric vector of BMI values.
#' @param levels Integer. Use 4 for: Underweight, Healthy Weight, Overweight, Obesity.
#'   Use 6 for detailed obesity classification.
#'
#' @return An ordered factor with "Healthy Weight" as the reference level.
#'
#' @examples
#' df <- tibble::tibble(bmi = c(17, 22, 27, 32, 37, 42, NA))
#' df %>% dplyr::mutate(
#'   bmi_cat4 = categorize_bmi(bmi, levels = 4),
#'   bmi_cat6 = categorize_bmi(bmi, levels = 6)
#' )
#'
#' @export
categorize_bmi <- function(bmi, levels = 4) {
  labels_4 <- c("Underweight", "Healthy Weight", "Overweight", "Obesity")
  labels_6 <- c("Underweight", "Healthy Weight", "Overweight",
                "Class 1 Obesity", "Class 2 Obesity", "Class 3 Obesity")

  cats <- dplyr::case_when(
    levels == 6 & bmi < 18.5 ~ "Underweight",
    levels == 6 & bmi >= 18.5 & bmi < 25 ~ "Healthy Weight",
    levels == 6 & bmi >= 25 & bmi < 30 ~ "Overweight",
    levels == 6 & bmi >= 30 & bmi < 35 ~ "Class 1 Obesity",
    levels == 6 & bmi >= 35 & bmi < 40 ~ "Class 2 Obesity",
    levels == 6 & bmi >= 40 ~ "Class 3 Obesity",

    levels == 4 & bmi < 18.5 ~ "Underweight",
    levels == 4 & bmi >= 18.5 & bmi < 25 ~ "Healthy Weight",
    levels == 4 & bmi >= 25 & bmi < 30 ~ "Overweight",
    levels == 4 & bmi >= 30 ~ "Obesity",

    TRUE ~ NA_character_
  )

  levels_used <- if (levels == 4) labels_4 else labels_6
  factor(cats, levels = levels_used) |>
    stats::relevel(ref = "Healthy Weight")
}
