#' Prepare Person-Time and Event Data for SIR/IRR Calculations
#'
#' Function needs package 'heaven' installed. Check https://github.com/tagteam/heaven to install the package.
#'
#' This function merges exposure, response, and population-level data to produce
#' an aggregated dataset for person-time and event rate analysis (e.g., Standardized
#' Incidence Ratios or Incidence Rate Ratios). It allows comparison of disease occurrence
#' across different exposure time windows.
#'
#' @param d_exposure A `data.frame` of exposure diagnoses with at least `ID`, `DATE`, and `DG` columns. Created by function `search_diagnoses()`.
#' @param d_response A `data.frame` of response diagnoses with at least `ID`, `DATE`, and `DG` columns. Created by function `search_diagnoses()`. In case of parameter `dg_list` is given, give full list of diagnoses.
#' @param d_population A `data.frame` containing baseline population data, including `ID`, `DATE_BIRTH`, `DATE_DEATH`, and `DATE_MIGRATION`. Created by function `classify_population()`.
#' @param var_age_start Numeric. Age (in years) at which to start follow-up (e.g., 50).
#' @param dg_list Optional named list. Each element is a string of diagnosis groupings (e.g., `"hip+forearm+humerus"`). If `NA`, the default "normal" procedure is applied.
#' @param censoring_date A `Date` object indicating administrative censoring (e.g., `"2022-12-31"`).
#' @param limits Numeric vector of length two. Optional plotting or analysis limits, default is `c(0.3, 3)`.
#'
#' @return A `data.frame` summarizing person-years (`pyrs`), event counts (`Death`), and optional diagnosis counts across exposure categories (`caika`) and age groups.
#' @details
#' The function:
#' \itemize{
#'   \item Calculates first exposure and response dates per individual.
#'   \item Computes time intervals between exposure and response.
#'   \item Splits follow-up time using `heaven::lexisSeq()` and `heaven::lexisTwo()`.
#'   \item Categorizes person-time into pre-defined exposure windows (e.g., `<1y`, `1-4y`, `5-9y`, etc.).
#'   \item Optionally aggregates response diagnoses using `dg_list` expressions.
#'   \item Filters follow-up to start at `var_age_start` (e.g., 50 years).
#' }
#'
#' @import dplyr
#' @import lubridate
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom data.table as.data.table
#' @importFrom shiny isRunning withProgress
#' @importFrom rlang parse_expr
#' @export
#'
#' @examples
#' \dontrun{
#' result <- pirr_data(
#'   d_exposure = exposure_data,
#'   d_response = response_data,
#'   d_population = population_data,
#'   var_age_start = 50,
#'   censoring_date = as.Date("2023-12-31")
#' )
#' }
#' \dontrun{
#' result <- pirr_data(
#'   d_exposure = exposure_data,
#'   d_response = diagnoses %>% filter(DGREG == "FRACTURES" ),
#'   d_population = dpop,
#'   dg_list = list(any_fracture = "ankle+forearm+hip+humerus+vertebral",
#'               ostheoporotic = "forearm+hip+humerus+vertebral"),
#'   var_age_start = 50,
#'   censoring_date = as.Date("2023-12-31")
#' )
#' }
pirr_data <- function(
    d_exposure,
    d_response,
    d_population,
    var_age_start = 50,
    dg_list = NA,
    censoring_date = as.Date("2022-12-31"),
    limits=c(0.3,3) ) {

  ## DEBUG CHUNK
  if(FALSE){
    d_exposure = exposure_diagnoses
    d_response = response_diagnoses
    d_population = dpop
    censoring_date = as.Date("2023-12-31")
    var_age_start = 50 # ikä jolloin seuranta alkaa
    colors = unname(colors_groups[c("non-exposure", "exposure")])
    limits=c(0.3,3)
    # For Fractures test
    dg_list = list(Any_fracture = "ankle+forearm+hip+humerus+vertebral",
                   Osteoporotic = "forearm+hip+humerus+vertebral")
    d_response = diagnoses |>
      filter(DGREG == "FRACTURES")
  }

  if (!requireNamespace("heaven", quietly = TRUE)) {
    stop("The 'heaven' package is required. Please install it from GitHub: https://github.com/tagteam/heaven")
  }


  all <- function(){

    # Diagnoses ----
    ## Exposure -----
    dalt <- d_exposure |>
      dplyr::arrange(ID, DATE) |>
      dplyr::group_by(ID) |> ## TODO if taking FRACTURES or other diagnose sets, we will take full timeline
      dplyr::summarise(DATE_EXPOSURE = first(DATE),
                       DG = first(DG)
      ) |>
      dplyr::rename(DG_EXP = DG) |>
      dplyr::left_join(d_population, by = "ID") |>
      dplyr::mutate(AGE_EXPOSURE = trunc(lubridate::`%--%`(DATE_BIRTH, DATE_EXPOSURE) / lubridate::years(1))) |>
      dplyr::select(ID, DATE_EXPOSURE, AGE_EXPOSURE, DG_EXP)

    if(shiny::isRunning()) .safe_inc_progress(1/10)

    ## Choosing between "Normal" and "Fractures" Procedure
    if(any(is.na(dg_list))){
      ## "Normal" Procedure ------
      ## Vaste
      dvast <- d_response |>
        dplyr::arrange(ID, DATE) |>
        dplyr::group_by(ID) |>
        dplyr::summarise(DATE_RESPONSE = first(DATE),
                         DG = first(DG)
        ) %>%
        dplyr::rename(DG_RES = DG) |>
        dplyr::left_join(d_population, by = "ID") |>
        dplyr::mutate(AGE_RESPONSE = trunc(lubridate::`%--%`(DATE_BIRTH, DATE_RESPONSE) / lubridate::years(1))) |>
        dplyr::select(ID, DATE_RESPONSE, AGE_RESPONSE, DG_RES)

      if(shiny::isRunning()) .safe_inc_progress(2/10)

      ### Further data wrangling
      ## Altisteen ja vasteen yhdistys, ajallinen ero diagnoosien välillä
      d1 <- dvast |>
        dplyr::left_join(dalt, by=c("ID")) |>
        dplyr::mutate(
          ## Difference between response and exposure DATES
          ero = as.numeric(lubridate::decimal_date(DATE_RESPONSE) - lubridate::decimal_date(DATE_EXPOSURE)),
          ## Response and Exposure Time as Factor Variable. This is in future classifyin variable.
          caika = factor(dplyr::case_when(
            is.na(ero) | ero<0 ~ "0 No exposure",
            ero < 1 ~ "1 exposure < 1y",
            ero < 5 ~ "2 exposure 1-4y",
            ero < 10 ~ "3 exposure 5-9y",
            ero < 15 ~ "4 exposure 10-14y",
            TRUE ~ "5 exposure 15+y",
          ))
        )

      d1 <- d1 |>
        dplyr::count(caika, AGE_RESPONSE)

      ## Seurannan aikarajojen muodostaminen
      dat1 <- d_population |>
        dplyr::left_join(dvast %>% select(ID, DATE_RESPONSE), by = "ID") |>
        dplyr::mutate(
          Death = as.integer(!is.na(DATE_DEATH)),
          apvm = pmax(DATE_BIRTH, as.Date("1953-01-01"), na.rm="TRUE"), ## Start Date
          epvm = pmin(DATE_DEATH, DATE_MIGRATION, censoring_date, DATE_RESPONSE, na.rm = TRUE)
        )

    }else{
      ## "Fractures" Procedure ------
      # Build expressions from dg_list
      exprs <- lapply(dg_list, function(x) rlang::parse_expr(x))
      # Convert to named list of expressions
      names(exprs) <- names(dg_list)

      # (Duplicate of normal handling)
      ## Vaste
      dvast <- d_response |>
        dplyr::arrange(ID, DATE) |>
        dplyr::rename(DG_RES = DG,
                      DATE_RESPONSE = DATE) |>
        dplyr::mutate(AGE_RESPONSE = trunc(lubridate::`%--%`(DATE_BIRTH, DATE_RESPONSE) / lubridate::years(1))) |>
        dplyr::select(ID, DATE_RESPONSE, AGE_RESPONSE, DG_RES)

      ### Further data wrangling
      ## Altisteen ja vasteen yhdistys, ajallinen ero diagnoosien välillä
      d1 <- dvast |>
        dplyr::left_join(dalt, by=c("ID")) |>
        dplyr::mutate(
          ## Difference between response and exposure DATES
          ero = as.numeric(lubridate::decimal_date(DATE_RESPONSE) - lubridate::decimal_date(DATE_EXPOSURE)),
          ## Response and Exposure Time as Factor Variable. This is in future classifyin variable.
          caika = factor(dplyr::case_when(
            is.na(ero) | ero<0 ~ "0 No exposure",
            ero < 1 ~ "1 exposure < 1y",
            ero < 5 ~ "2 exposure 1-4y",
            ero < 10 ~ "3 exposure 5-9y",
            ero < 15 ~ "4 exposure 10-14y",
            TRUE ~ "5 exposure 15+y",
          ))
        )
      # (This is end of duplicate of normal handling)

      d1 <- d1 |>
        dplyr::count(DG_RES, AGE_RESPONSE, caika) |>
        tidyr::pivot_wider(names_from=DG_RES, values_from=n, values_fill=0) |>
        dplyr::mutate(!!!exprs)

      ## Seurannan aikarajojen muodostaminen
      dat1 <- d_population |>
        dplyr::mutate(
          Death = as.integer(!is.na(DATE_DEATH)),
          apvm = pmax(DATE_BIRTH, as.Date("1953-01-01"), na.rm="TRUE"), ## Start Date
          epvm = pmin(DATE_DEATH, DATE_MIGRATION, censoring_date, na.rm = TRUE)
        )
    }

    if(shiny::isRunning()) .safe_inc_progress(3/10)

    ages <- c(20, 50:90)*365.25 ## TODO tämä vaikuttaa olevan sidoksissa var_age_start

    ## Perustiedot (päivämäärät) ja altisteen päivämäärät
    cdat <- data.table::as.data.table(d_population) |>
      dplyr::left_join(dalt |>
                         dplyr::select(ID, c00pvm=DATE_EXPOSURE), by=c("ID")) |> # c00pvm on altistediagnoosi aika
      dplyr::mutate(
        c01pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(1)),
        c05pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(5)),
        c10pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(10)),
        c15pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(15))
      )

    ## split functions
    dat2 <- dat1 |>
      heaven::lexisSeq(invars=c("ID","apvm","epvm","Death"),
                       varname="DATE_BIRTH",
                       splitvector=ages,
                       format="vector",
                       value="agec")

    dat3 <- dat2 |>
      heaven::lexisTwo(cdat,
                       c("ID","apvm","epvm","Death"),
                       c("ID","c00pvm","c01pvm","c05pvm","c10pvm","c15pvm")) |>
      dplyr::mutate(
        Age =  round( as.numeric((apvm - DATE_BIRTH) / 365.25), 0)
      )

    if(shiny::isRunning()) .safe_inc_progress(4/10)

    adat <- dat3 |>
      dplyr::mutate(
        caika=factor(case_when(
          c15pvm==1 ~ "5 exposure 15+y",
          c10pvm==1 ~ "4 exposure 10-14y",
          c05pvm==1 ~ "3 exposure 5-9y",
          c01pvm==1 ~ "2 exposure 1-4y",
          c00pvm==1 ~ "1 exposure < 1y",
          TRUE ~ "0 No exposure"
        )),
        ikar=case_when( ## TODO nämä vaikuttaa olevan sidoksissa var_age_start
          agec<2 ~ 20,
          TRUE ~ agec+48
        ),
        kesto=as.numeric(lubridate::decimal_date(epvm)-lubridate::decimal_date(apvm))
      ) |>
      # seuranta alkaa kun henkilö täyttää 50v (cancer-/murtuma-aineisto) / OSTPRE Pelkkä ICD10 filt >= 65
      dplyr::filter(Age >= var_age_start) |>
      dplyr::group_by(Age, caika) |>
      dplyr::summarise(
        pyrs=sum(kesto),
        Death=sum(Death)
      ) |>
      dplyr::left_join(d1, by=c("Age"="AGE_RESPONSE","caika")) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.,0)),
        cever=factor(dplyr::case_when(
          caika=="0 No exposure" ~ "0 No exposure",
          TRUE ~ "1 exposure"
        ))
      ) |>
      dplyr::filter(pyrs>0.01) ## altistusajan filtteri
    if(shiny::isRunning()) .safe_inc_progress(5/10)
    return(adat)
  }

  if(shiny::isRunning()){
    withProgress(message = "Calculating SIR", value = 0, {
      return(all())
    })
  }else{
    return(all())
  }
}


#' Run PIRR-style modeling and summary plots for multiple binary outcomes
#'
#' This function runs Poisson regression models using splines for age and compares
#' effects over two time-related variables (`caika` and `cever`) for each binary outcome
#' variable found in the dataset. It returns model-based predictions, summary tables,
#' and plots for each outcome.
#'
#' @param adat A data frame containing the data. Must include columns `Age`, `caika`, `pyrs`,
#' `cever`, and at least one binary outcome variable (e.g., disease flags). Created by function `pirr_data()`.
#' @param colors A character vector of two hex colors for plotting binary outcome levels
#' in the age vs exposure plots. Default is \code{c("#5BC0DE", "#D9534F")}.
#' @param limits A numeric vector of two values specifying the y-axis limits (log10 scale)
#' for the SIR plots. Default is \code{c(0.3, 3)}.
#'
#' @return A named list of results, one element per binary outcome variable. Each element
#' is a list containing:
#' \describe{
#'   \item{table}{A tibble with summary statistics and model predictions per level of `caika` and `cever`.}
#'   \item{plot1}{A ggplot object of the log-scale SIR across `caika` or `cever`.}
#'   \item{plot2}{A ggplot object showing adjusted exposure by Age and response status.}
#' }
#'
#' @details
#' For each binary outcome, two Poisson regression models are fitted:
#' \itemize{
#'   \item One using `caika` (calendar period) as the main time variable
#'   \item One using `cever` (time since event or similar)
#' }
#' Both models adjust for age using restricted cubic splines and use log(pyrs) as offset.
#' Standard errors are heteroskedasticity-consistent (HC0). Predictions are made at Age = 70
#' and a base rate of 10,000 person-years.
#'
#' @importFrom dplyr group_by summarise mutate row_number bind_rows filter select left_join
#' @importFrom ggeffects predict_response
#' @importFrom ggplot2 labs scale_y_continuous theme
#' @importFrom tibble tibble
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovHC
#' @importFrom stats glm
#' @importFrom splines bs
#'
#' @examples
#' # Example usage with simulated data
#' # results <- pirr_results(adat = my_data)
#'
#' @export
pirr_results <- function(adat,
                         colors=c("#5BC0DE", "#D9534F"),
                         limits = c(0.3,3)
){

  ## Sniff dg list from data
  dglist <- names(adat)[!names(adat) %in% c("Age", "caika", "pyrs", "cever")]

  tulos <- list()
  # i=dglist[2]
  for (i in dglist) {
    ## Preparations
    # adat$vaste <- adat[,dglist[i]][[1]]
    adat$vaste <- adat[,i][[1]]
    nimi <- i#names(dglist[dglist==i])

    ## Summary statistics of caika / All time differences
    n1 <- adat |>
      dplyr::group_by(caika) |>
      dplyr::summarise(
        n=sum(vaste),
        pyrs=sum(pyrs)
      ) |>
      dplyr::mutate(rn=row_number())
    ## Model 1 - caika
    m1 <- glm(vaste ~ splines::bs(Age) + caika, offset = log(pyrs), family = poisson(link = "log"), data = adat)
    pval1 <- lmtest::coeftest(m1,vcov.=sandwich::vcovHC(m1,type="HC0"))
    pres <- tibble::tibble(pval=c(NA,pval1[,"Pr(>|z|)"][grep("exposure",rownames(pval1))])) |>
      mutate(rn = dplyr::row_number())
    mdi <- tibble::tibble(ggeffects::predict_response(m1, terms=c("caika"), condition=(c(Age=70, pyrs=10000)))) |>
      mutate(rn = dplyr::row_number())
    mdp <- ggeffects::predict_response(m1,
                                       terms=c("caika"),
                                       vcov_fun="vcovHC",
                                       vcov_type="HC0",
                                       condition=(c(Age=70, pyrs=10000/mdi$predicted[1])))
    ## TODO ika end funktion muuttujaksi? Miksi 70v

    ## Summary of cever / No time difference
    n2 <- adat |>
      dplyr::group_by(cever) |>
      dplyr::summarise(
        n=sum(vaste),
        pyrs=sum(pyrs)
      ) |>
      dplyr::mutate(rn=row_number())
    ## Model 2 - cever
    m2 <- stats::glm(vaste ~ splines::bs(Age) + cever, offset = log(pyrs), family = poisson(link = "log"), data = adat)
    pval2 <- lmtest::coeftest(m2,vcov.=sandwich::vcovHC(m2,type="HC0"))
    pres2 <- tibble::tibble(pval=c(NA,pval2[,"Pr(>|z|)"][grep("exposure",rownames(pval2))])) |>
      dplyr::mutate(rn = dplyr::row_number())
    mdi2 <- tibble::tibble(ggeffects::predict_response(m2, terms=c("cever"), condition=(c(Age=70, pyrs=10000)))) |>
      dplyr::mutate(rn = dplyr::row_number())
    mdp2 <- ggeffects::predict_response(m2,
                                        terms=c("cever"),
                                        vcov_fun="vcovHC",
                                        vcov_type="HC0",
                                        condition=(c(Age=70, pyrs=10000/mdi2$predicted[1])))
    ## TODO ika end funktion muuttujaksi? Miksi 70v

    # mda2 <- ggeffects::predict_response(m2, terms=c("ikar","cever"), vcov_fun="vcovHC", vcov_type="HC0", condition=(c(pyrs=10000)))
    mda2 <- ggeffects::predict_response(m2, terms=c("Age","cever"), condition=(c(pyrs=10000)))


    ## Summary of Full Results (model1, model2, n1 and n2)
    res <- NULL |>
      dplyr::bind_rows(mdp |>
                         as.data.frame() |>
                         dplyr::mutate(rn=row_number()) |>
                         dplyr::filter(!is.na(x)) |>
                         dplyr::left_join(pres,by="rn") |>
                         dplyr::left_join(n1,by="rn") |>
                         dplyr::left_join(as.data.frame(mdi) |>
                                            dplyr::select(rn, adj=predicted), by="rn")) |>
      dplyr::bind_rows(mdp2 |>
                         as.data.frame() |>
                         dplyr::mutate(rn=row_number()) |>
                         dplyr::filter(!is.na(x)) |>
                         dplyr::left_join(pres2,by="rn") |>
                         dplyr::left_join(n2,by="rn") |>
                         dplyr::left_join(as.data.frame(mdi2) |>
                                            dplyr::select(rn, adj=predicted), by="rn")) |>
      dplyr::mutate(crude=n/pyrs*10000) |>
      dplyr::select(factor=x, n, pyrs, crude, adj, SIR=predicted, conf.low, conf.high, pval)

    ## Plots of the results
    p1 <- plot(mdp) +
      ggplot2::scale_y_continuous(trans="log10",limits=limits) +
      ggplot2::labs(y="Standardized Incidence Ratio (SIR), log scale",x="",title=nimi)
    p2 <- plot(mda2, colors=unname(colors)) +
      ggplot2::labs(y="Exposure per 10000 person years",
                    x="Age",
                    title=nimi,
                    color="Response status") +
      ggplot2::theme(legend.position.inside=c(0.11,0.83))

    ## Store results to a list
    resl <- list(table=res, plot1=p1, plot2=p2)
    tulos[[nimi]] <- resl
  }
  ## Final result out
  return(tulos)
}
