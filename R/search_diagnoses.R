#' Search Diagnoses by ICD Version and Source
#'
#' Retrieves diagnosis records from a dataset based on ICD-10, ICD-9, or ICD-8 codes and selected data sources.
#'
#' @param regex_icd10 character. Regular expression pattern for ICD-10 diagnoses.
#' @param regex_icd9 character. Regular expression pattern for ICD-9 diagnoses.
#' @param regex_icd8 character. Regular expression pattern for ICD-8 diagnoses.
#' @param registry_source character vector. Data sources to include (e.g., \code{"avohilmo"}, \code{"hilmo"}).
#' @param censoring_date Date to apply censoring to filter out cases after the date. "" if max used.
#' @param data_diagnoses data.frame. Diagnosis dataset to search. Must include columns: \code{ID}, \code{DGREG}, \code{SRC}, \code{DATE}, \code{DG}, \code{ICD10_CLASS}, \code{ICD10_3LETTERS}, \code{AGE}.
#'
#' @return A \code{tibble} containing matched diagnosis records sorted by ID, registry, and date.
#'
#' @examples
#' \dontrun{
#'   search_diagnoses(regex_icd10 = "^I2", registry_source = c("hilmo", "avohilmo"), data_diagnoses = diagnoses)
#' }
#'
#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @export
search_diagnoses <- function(regex_icd10="",
                             regex_icd9="",
                             regex_icd8="",
                             registry_source=c(""),
                             age_range=c(0,120),
                             censoring_date="",
                             data_diagnoses=diagnoses
){

  ## DEBUG
  if(FALSE){
    ## Should we take extra account?
    # regex_extra="",
    # src_extra="",

    regex_icd10 = "^E11"
    regex_icd9 = "^250A"
    regex_icd8 = "^250"
    registry_source = c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")
    age_range=c(0,100)
    censoring_date=""
    data_diagnoses=diagnoses
    # selected_response_icd10 = "^I2[0-5]"
    # selected_response_icd9 = "^41[0-4]"
    # selected_response_icd8 = "^41[0-4]"
    # selected_response_regsrc = c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")
  }



  all <- function(){
    ## Clean Regex Codes
    regex_icd10 <- .regex_clean(regex_icd10)
    regex_icd9 <- .regex_clean(regex_icd9)
    regex_icd8 <- .regex_clean(regex_icd8)

    ## Filterin diagnoses by censoring date & AGE_RANGE
    if(censoring_date == "") censoring_date <- max(data_diagnoses$DATE, na.rm = T)
    data_diagnoses <- data_diagnoses |>
      dplyr::filter(DATE <= censoring_date) |>
      dplyr::filter(AGE_DG >= age_range[1] & AGE_DG <= age_range[2])

    ## Initialize Datasets
    d1 <- tibble(ID = numeric(),
                 DGREG = character(),
                 SRC = character(),
                 DATE = as.Date(x = integer(0), origin = "1970-01-01"),
                 DG = character(),
                 ICD10_CLASS = character(),
                 ICD10_3LETTERS = character()
    ) # ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE
    d2 <- tibble(ID = numeric(),
                 DGREG = character(),
                 SRC = character(),
                 DATE = as.Date(x = integer(0), origin = "1970-01-01"),
                 DG = character(),
                 ICD10_CLASS = character(),
                 ICD10_3LETTERS = character()
    ) # ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE
    d3 <- tibble(ID = numeric(),
                 DGREG = character(),
                 SRC = character(),
                 DATE = as.Date(x = integer(0), origin = "1970-01-01"),
                 DG = character(),
                 ICD10_CLASS = character(),
                 ICD10_3LETTERS = character()
    ) # ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE
    .safe_inc_progress(1/6)
    if(regex_icd10 != ""){
      d1 <- data_diagnoses |>
        dplyr::filter(DGREG == "ICD10") |>
        dplyr::filter(SRC %in% registry_source) |> ## TODO ifelse c("")
        dplyr::filter(grepl(pattern = regex_icd10, x = DG)) |>
        dplyr::select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS)
    }
    .safe_inc_progress(2/6)
    if(regex_icd9 != ""){
      d2 <- data_diagnoses |>
        dplyr::filter(DGREG == "ICD9") |>
        dplyr::filter(SRC %in% registry_source) |>
        dplyr::filter(grepl(pattern = regex_icd9, x = DG))|>
        dplyr::select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS)
    }
    .safe_inc_progress(3/6)
    if(regex_icd8 != ""){
      d3 <- data_diagnoses |>
        dplyr::filter(DGREG == "ICD8") |>
        dplyr::filter(SRC %in% registry_source) |>
        dplyr::filter(grepl(pattern = regex_icd8, x = DG))|>
        dplyr::select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS)
    }
    .safe_inc_progress(4/6)
    #TODO kun src extra mukaan
    # if(regex_extra != ""){
    #   d4 <- data_diagnoses |>
    #     # filter(DGREG == "ICD8") |>
    #     filter(SRC %in% src_extra) |>
    #     filter(grepl(pattern = regex_extra, x = DG))|>
    #     select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE)
    # }
    .safe_inc_progress(5/6)

    ## Kaikki ICD rekisterit yhdessa.
    d <- dplyr::tibble() |>
      dplyr::bind_rows(d1) |>
      dplyr::bind_rows(d2) |>
      dplyr::bind_rows(d3)
    # rbind(if(exists("d4") & nrow(d4)>0) d4)

    rm(list = c("d1", "d2", "d3"))
    d <- dplyr::as_tibble(d) |>
      dplyr::arrange(ID, DGREG, DATE)

    .safe_inc_progress(6/6)
    return(d)
  }

  if(shiny::isRunning()){
    withProgress(message = "Creating Diagnoses Data", value = 0, {
      return(all())
    })
  }else{
    return(all())
  }
}
