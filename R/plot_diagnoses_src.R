
#' Plot Venn Diagram of Diagnoses by Source
#'
#' Creates a Venn diagram showing overlaps between diagnosis sources. Optionally selects only the first source per ID or one per ID per source.
#'
#' @param data data.frame. The dataset containing diagnosis records with at least columns \code{ID}, \code{SRC}, and \code{DATE}. You can get this with function 'search_diagnoses()'
#' @param per_source logical. If \code{FALSE} (default), uses only the first source per ID. If \code{TRUE}, includes one record per source per ID.
#'
#' @return A ggplot object with a Venn diagram.
#' @importFrom magrittr %>%
#' @importFrom ggVennDiagram ggVennDiagram
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom dplyr group_by summarise first select arrange
#' @export
plot_diagnoses_src <- function(data, per_source = FALSE) {

  if(FALSE){
    data=exposure_diagnoses
    per_source = FALSE
  }

  all <- function() {
    .safe_inc_progress(1/4)

    dvenn <- data |>
      dplyr::arrange(ID, DATE)

    # Grouping logic based on per_source flag
    dvenn <- if (per_source) {
      dvenn |>
        dplyr::group_by(ID, SRC) |>
        dplyr::summarise(DATE = dplyr::first(DATE),
                         SRC = dplyr::first(SRC), .groups = "drop")|>
        dplyr::select(ID, SRC)
    } else {
      dvenn |>
        dplyr::group_by(ID) |>
        dplyr::summarise(DATE = dplyr::first(DATE),
                         SRC = dplyr::first(SRC), .groups = "drop")|>
        dplyr::select(ID, SRC)
    }

    .safe_inc_progress(2/4)

    # Helper to split tibble into named list for Venn plotting
    split_tibble <- function(tibble, column = 'SRC') {
      temp <- tibble %>%
        split(., .[[column]]) %>%
        lapply(function(x) x[setdiff(names(x), column)]) %>%
        unlist(recursive = FALSE)
      names(temp) <- gsub("\\.ID$", "", names(temp))  # Clean names
      return(temp)
    }

    if (length(unique(dvenn$SRC)) < 2) {
      message("Only one source found — skipping Venn plot.")
      return(NULL)
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
