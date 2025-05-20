#' Search Diagnoses by ICD Version and Source
#'
#' Retrieves diagnosis records from a dataset based on ICD-10, ICD-9, or ICD-8 codes and selected data sources.
#'
#' @param regex_icd10 character. Regular expression pattern for ICD-10 diagnoses.
#' @param regex_icd9 character. Regular expression pattern for ICD-9 diagnoses.
#' @param regex_icd8 character. Regular expression pattern for ICD-8 diagnoses.
#' @param registry_source character vector. Data sources to include (e.g., \code{"avohilmo"}, \code{"hilmo"}).
#' @param regex_extra character. Optional regular expression for other diagnosis formats (currently not implemented).
#' @param src_extra character. Optional source for extra diagnoses (currently not implemented).
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
                             registry_source=c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
                             regex_extra="",
                             src_extra="",
                             data_diagnoses=diagnoses
){
  all <- function(){
    ## Clean Regex Codes
    regex_icd10 <- .regex_clean(regex_icd10)
    regex_icd9 <- .regex_clean(regex_icd9)
    regex_icd8 <- .regex_clean(regex_icd8)

    ## Initialize Datasets
    d1 <- tibble(ID = numeric(),
                 DGREG = character(),
                 SRC = character(),
                 DATE = as.Date(x = integer(0), origin = "1970-01-01"),
                 DG = character(),
                 ICD10_CLASS = character(),
                 ICD10_3LETTERS = character(),
                 AGE = numeric()
    ) # ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE
    d2 <- tibble(ID = numeric(),
                 DGREG = character(),
                 SRC = character(),
                 DATE = as.Date(x = integer(0), origin = "1970-01-01"),
                 DG = character(),
                 ICD10_CLASS = character(),
                 ICD10_3LETTERS = character(),
                 AGE = numeric()
    ) # ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE
    d3 <- tibble(ID = numeric(),
                 DGREG = character(),
                 SRC = character(),
                 DATE = as.Date(x = integer(0), origin = "1970-01-01"),
                 DG = character(),
                 ICD10_CLASS = character(),
                 ICD10_3LETTERS = character(),
                 AGE = numeric()
    ) # ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE
    .safe_inc_progress(1/6)
    if(regex_icd10 != ""){
      d1 <- data_diagnoses |>
        dplyr::filter(DGREG == "ICD10") |>
        dplyr::filter(SRC %in% registry_source) |>
        dplyr::filter(grepl(pattern = regex_icd10, x = DG)) |>
        dplyr::select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE)
    }
    .safe_inc_progress(2/6)
    if(regex_icd9 != ""){
      d2 <- data_diagnoses |>
        dplyr::filter(DGREG == "ICD9") |>
        dplyr::filter(SRC %in% registry_source) |>
        dplyr::filter(grepl(pattern = regex_icd9, x = DG))|>
        dplyr::select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE)
    }
    .safe_inc_progress(3/6)
    if(regex_icd8 != ""){
      d3 <- data_diagnoses |>
        dplyr::filter(DGREG == "ICD8") |>
        dplyr::filter(SRC %in% registry_source) |>
        dplyr::filter(grepl(pattern = regex_icd8, x = DG))|>
        dplyr::select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE)
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
    d <- tibble() |>
      dplyr::bind_rows(d1) |>
      dplyr::bind_rows(d2) |>
      dplyr::bind_rows(d3)
    # rbind(if(exists("d4") & nrow(d4)>0) d4)

    rm(list = c("d1", "d2", "d3"))
    d <- as_tibble(d) |>
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


#' Plot Venn Diagram of Diagnoses by Source
#'
#' Creates a Venn diagram showing overlaps between diagnosis sources. Optionally selects only the first source per ID or one per ID per source.
#'
#' @param data data.frame. The dataset containing diagnosis records with at least columns \code{ID}, \code{SRC}, and \code{DATE}. You can get this with function 'search_diagnoses()'
#' @param per_source logical. If \code{FALSE} (default), uses only the first source per ID. If \code{TRUE}, includes one record per source per ID.
#'
#' @return A ggplot object with a Venn diagram.
#'
#' @export
plot_diagnoses_src <- function(data, per_source = FALSE) {
  all <- function() {
    .safe_inc_progress(1/4)

    dvenn <- data |>
      dplyr::arrange(ID, DATE)

    # Grouping logic based on per_source flag
    dvenn <- if (per_source) {
      dvenn |>
        dplyr::group_by(ID, SRC) |>
        dplyr::summarise(DATE = first(DATE),
                  SRC = first(SRC), .groups = "drop")|>
        dplyr::select(ID, SRC)
    } else {
      dvenn |>
        dplyr::group_by(ID) |>
        dplyr::summarise(DATE = first(DATE),
                  SRC = first(SRC), .groups = "drop")|>
        dplyr::select(ID, SRC)
    }

    .safe_inc_progress(2/4)

    # Helper to split tibble into named list for Venn plotting
    split_tibble <- function(tibble, column = 'SRC') {
      temp <- tibble |>
        split(., .[[column]]) |>
        lapply(function(x) x[setdiff(names(x), column)]) |>
        unlist(recursive = FALSE)
      names(temp) <- gsub("\\.ID$", "", names(temp))  # Clean names
      return(temp)
    }

    x <- split_tibble(dvenn, 'SRC')

    .safe_inc_progress(3/4)

    plt <- ggVennDiagram::ggVennDiagram(x) +
      ggplot2::scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")

    .safe_inc_progress(4/4)

    return(plt)
  }

  if (shiny::isRunning()) {
    withProgress(message = paste("Plotting Venn", ifelse(per_source, "#2", "#1")), value = 0, {
      return(all())
    })
  } else {
    return(all())
  }
}



#' Summary Table of Diagnoses by Group
#'
#' Creates a summary table of selected diagnoses for a specified group (`exposure` or `response`),
#' including number of patients, number of cases, percentage of group, and diagnosis descriptions.
#'
#' Diagnoses with fewer than a specified number of patients (`sum_small_groups`) will be grouped
#' into a combined "Rest of the diagnoses" row to comply with data privacy practices.
#'
#' @param data A data frame containing diagnoses and group information. Must include columns `ID`, `DG`, `DGREG`, and `GROUP`.
#' @param diagnoses A data frame of population diagnoses over timeline
#' @param group Character. Either `"exposure"` or `"response"`, indicating the group to summarize. Defaults to `"exposure"`.
#' @param sum_small_groups Integer. Diagnoses with fewer than this number of patients are grouped into an aggregate row. Default is 6.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{DG}{Diagnosis group code}
#'   \item{DGREG}{Diagnosis registry group}
#'   \item{patients}{Number of unique patients with the diagnosis in the group}
#'   \item{cases}{Total number of diagnosis cases}
#'   \item{group_pct}{Percentage of patients within the group}
#'   \item{DESC}{Description of the diagnosis}
#' }
#'
#' @details
#' If the function is used inside a running Shiny app, it includes progress indicators for user feedback.
#'
#' @import dplyr
#' @importFrom shiny isRunning withProgress
#' @export
#'
#' @examples
#' \dontrun{
#' summary_table <- table_summary_diagnoses(data = df, diagnoses = c("E10", "E11"), group = "exposure")
#' }
table_summary_diagnoses <- function(data, diagnoses, group = "exposure", sum_small_groups=6){
  ## Creates a summary of selected diagnoses on the group (exposure/response)
  ## and gives summary information (Cases, Patients, PCT, Desc)

  all <- function(){
    .safe_inc_progress(1/3)

    koehenkiloita <- nrow(data[data$GROUP == group,]) ## Group joko exposure tai response
    if (koehenkiloita == 0) {
      return(data.frame(DG = "No data", DGREG = "No data", patients = 0, cases = 0, group_pct = NA, DESC = "No data"))
    }

    d <- diagnoses %>%
      dplyr::group_by(DG, DGREG) %>%
      dplyr::summarise(
        patients = length(unique(ID)),
        cases = n()
      ) %>%
      dplyr::mutate(
        group_pct = round(100 * patients / koehenkiloita, 1)
      ) %>%
      dplyr::left_join(data_codes, by = "DG") %>%
      dplyr::select(DG, DGREG, patients, cases, group_pct, DESC)

    .safe_inc_progress(2/3)

    ## Combine small diagnose groups
    ## Tietosuoja alle 6 tapaukset
    d <- d %>%
      dplyr::filter(patients >= sum_small_groups) %>%
      rbind(
        d %>%
          dplyr::filter(patients < sun_small_groups) %>%
          dplyr::summarise(
            DG = "XX",
            DGREG = "XX",
            patients = sum(patients),
            cases = sum(cases),
            group_pct = NA,#sum(group_pct),
            # Diagnose = "Rest of the diagnoses",
            DESC = "Rest of the diagnoses"
          ) %>%
          dplyr::group_by(DG, DGREG) %>%
          dplyr::summarise(
            patients = sum(patients),
            cases = sum(cases)
          ) %>%
          dplyr::mutate(
            group_pct = round(100 * patients / koehenkiloita, 1)
          )
      )

    .safe_inc_progress(3/3)

    return(d)
  }

  if(shiny::isRunning()){
    withProgress(message = paste0("Table of ", .capitalize(group), " Diagnoses"), value = 0, {
      return(all())
    })
  }else{
    return(all())
  }
}

