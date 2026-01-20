#' Prepare Person-Time and Event Data for SIR/IRR Calculations
#'
#' This function merges exposure, response, and population-level data to produce
#' an aggregated dataset for person-time and event rate analysis (e.g., Standardized
#' Incidence Ratios [SIR] or Incidence Rate Ratios [IRR]). It compares disease occurrence
#' across exposure time windows using registry-style longitudinal data.
#'
#' The function requires the **heaven** package for Lexis splitting utilities.
#' See: [https://github.com/tagteam/heaven](https://github.com/tagteam/heaven)
#'
#' @param exposure_diagnoses A `data.frame` containing exposure diagnoses. Must include
#'   columns `ID`, `DATE`, and `DG`. Typically created with `search_diagnoses()`.
#' @param response_diagnoses A `data.frame` containing response diagnoses. Must include
#'   columns `ID`, `DATE`, and `DG`. Typically created with `search_diagnoses()`.
#' @param pop_dates A `data.frame` with population registry information, including
#'   `ID`, `DATE_BIRTH`, `DATE_DEATH`, and `DATE_MIGRATION`. Usually from
#'   `classify_population()`.
#' @param all_cases Logical; if `TRUE`, follow-up continues after the first response case.
#'   If `FALSE`, follow-up stops at the first response diagnosis.
#' @param censoring_age Numeric vector (length 2) specifying the lower and upper ages
#'   for follow-up inclusion (e.g., `c(50, 90)`).
#' @param censoring_date A `Date` vector (length 2) defining the administrative start
#'   and end of follow-up (e.g., `c(as.Date("1960-01-01"), as.Date("2022-12-31"))`).
#' @param custom_responses Optional named list defining custom response diagnosis
#'   groupings. For example:
#'   `list(Any_fracture = "ankle+forearm+hip+humerus+vertebral",
#'         Osteoporotic = "forearm+hip+humerus+vertebral",
#'         Hip = "hip")`.
#'
#' @return A `data.frame` summarizing:
#' \describe{
#'   \item{pyrs}{Person-years within each exposure and age stratum.}
#'   \item{Death}{Count of deaths within each stratum.}
#'   \item{Diagnosis counts}{Optional columns for each response diagnosis or
#'   custom grouping defined in `custom_responses`.}
#'   \item{caika}{Exposure time category (e.g., `<1y`, `1–4y`, `5–9y`, etc.).}
#'   \item{Age}{Age group at risk.}
#' }
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Extracts and merges first exposure and response diagnoses per individual.
#'   \item Computes time differences between exposure and response dates.
#'   \item Splits follow-up time using `heaven::lexisSeq()` and `heaven::lexisTwo()`.
#'   \item Categorizes person-time into exposure windows (`<1y`, `1–4y`, `5–9y`, `10–14y`, `15+y`).
#'   \item Optionally aggregates diagnoses using custom groupings from `custom_responses`.
#'   \item Restricts follow-up to the age range specified in `censoring_age`.
#' }
#'
#'
#' @examples
#' \dontrun{
#' result <- pirr_data(
#'   exposure_diagnoses = exposure_data,
#'   response_diagnoses = response_data,
#'   pop_dates = population_data,
#'   censoring_age = c(50, 90),
#'   censoring_date = c(as.Date("1960-01-01"), as.Date("2023-12-31")),
#'   custom_responses = list(
#'     Any_fracture = "ankle+forearm+hip+humerus+vertebral",
#'     Osteoporotic = "forearm+hip+humerus+vertebral",
#'     Hip = "hip"
#'   )
#' )
#' }
#' @export
pirr_data <- function(
    exposure_diagnoses,
    response_diagnoses,
    pop_dates,         ## Variables needed ID, DATE_BIRTH, DATE_DEATH, DATE_MIGRATION
    all_cases = FALSE, ## If you want to continue follow up after first response case.
    censoring_age = c(50, 90),
    censoring_date = c(as.Date("1953-01-01"), as.Date("2024-12-31")),
    custom_responses = list() ## EMPTY if non, otherwise list(Any_fracture = "ankle+forearm+hip+humerus+vertebral", Osteoporotic = "forearm+hip+humerus+vertebral")
) {

  ## DEBUG CHUNK
  if(FALSE){
    exposure_diagnoses = exposure_diagnoses
    response_diagnoses = response_diagnoses # diagnoses |> filter(DGREG == "FRACTURES")
    pop_dates = dpop
    all_cases = FALSE
    censoring_date = c(as.Date("1969-01-01"), as.Date("2023-12-31"))
    censoring_age = 50 # ikä jolloin seuranta alkaa
    custom_responses = list()
    custom_responses = list(Any_fracture = "ankle+forearm+hip+humerus+vertebral",
                            Osteoporotic = "forearm+hip+humerus+vertebral",
                            Hip = "hip")
  }

  if (!requireNamespace("heaven", quietly = TRUE)) {
    stop("The 'heaven' package is required. Please install it from GitHub: https://github.com/tagteam/heaven")
  }

  ## Data Handling Function
  all <- function(){

    ## Exposure -----
    ##### Here we take only the first case of the exposure.
    dalt <- exposure_diagnoses |>
      dplyr::arrange(ID, DATE) |>
      dplyr::group_by(ID) |>
      dplyr::summarise(DATE_EXPOSURE = first(DATE),
                       DG = first(DG)) |>
      dplyr::rename(DG_EXP = DG) |>
      dplyr::left_join(pop_dates, by = "ID") |>
      dplyr::mutate(AGE_EXPOSURE = trunc(lubridate::`%--%`(DATE_BIRTH, DATE_EXPOSURE) / lubridate::years(1))) |>
      dplyr::select(ID, DATE_EXPOSURE, AGE_EXPOSURE, DG_EXP)

    if(shiny::isRunning()) .safe_inc_progress(1/5)

    ## Response -------
    ## Preparation and all cases
    dvast <- response_diagnoses |>
      dplyr::arrange(ID, DATE) |>
      dplyr::rename(DG_RES = DG,
                    DATE_RESPONSE = DATE,
                    AGE_RESPONSE = AGE
      ) |>
      dplyr::select(ID, DATE_RESPONSE, AGE_RESPONSE, DG_RES)
    ## Take only first case
    if(!all_cases){
      dvast <- dvast |>
        dplyr::arrange(ID, DATE_RESPONSE) |>
        dplyr::left_join(pop_dates, by = "ID") |>
        dplyr::group_by(ID) |>
        dplyr::summarise(DATE_RESPONSE = first(DATE_RESPONSE),
                         DG_RES = first(DG_RES),
                         DATE_BIRTH = first(DATE_BIRTH),
                         AGE_RESPONSE = first(AGE_RESPONSE)
        )
    }

    ## Calculate time differences between exp-resp -------
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

    if(shiny::isRunning()) .safe_inc_progress(2/5)

    ## Custom DG list -------
    exprs <- lapply(custom_responses, function(x) rlang::parse_expr(x)) # Build expressions from dg_list
    names(exprs) <- names(custom_responses) # Convert to named list of expressions
    ## No Custom Response Classes / Custom Response Classes
    if(length(custom_responses) == 0){
      d1 <- d1 |>
        dplyr::count(caika, AGE_RESPONSE) |>
        dplyr::rename(DG = n) ## naming diagnose freq to DG
    }else{
      ## Make new custom response dg classes
      d1 <- d1 |>
        dplyr::count(DG_RES, AGE_RESPONSE, caika) |>
        tidyr::pivot_wider(names_from=DG_RES, values_from=n, values_fill=0) |>
        dplyr::mutate(!!!exprs) ## tässä tehdään custom dg classes
    }

    ## Calculate follow up start and end dates ---------
    if(all_cases){
      dat1 <- pop_dates |>
        dplyr::mutate(
          Death = as.integer(!is.na(DATE_DEATH)),
          apvm = pmax(DATE_BIRTH, censoring_date[1], na.rm=TRUE),
          epvm = pmin(DATE_DEATH, DATE_MIGRATION, censoring_date[2], na.rm = TRUE)
        )
    }else{
      dat1 <- pop_dates |>
        dplyr::left_join(dvast %>% select(ID, DATE_RESPONSE), by = "ID") |>
        dplyr::mutate(
          Death = as.integer(!is.na(DATE_DEATH)),
          apvm = pmax(DATE_BIRTH, censoring_date[1], na.rm=TRUE),
          epvm = pmin(DATE_DEATH, DATE_MIGRATION, censoring_date[2], DATE_RESPONSE, na.rm = TRUE)
        )
    }

    if(shiny::isRunning()) .safe_inc_progress(3/5)

    ages <- c(20, censoring_age[1]:censoring_age[2])*365.25 ## TODO tämä vaikuttaa olevan sidoksissa censoring_age / also age_end?

    ## Dates and time --------
    cdat <- data.table::as.data.table(pop_dates) |>
      dplyr::left_join(dalt |> dplyr::select(ID, c00pvm=DATE_EXPOSURE), by=c("ID")) |> # c00pvm on altistediagnoosi aika
      dplyr::mutate(
        c01pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(1)),
        c05pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(5)),
        c10pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(10)),
        c15pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(15))
      )

    ## split functions ----------
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

    if(shiny::isRunning()) .safe_inc_progress(4/5)

    ## Finalizing ---------
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
        ikar=case_when( ## TODO nämä vaikuttaa olevan sidoksissa var_age_start / censoring_age
          agec<2 ~ 20,
          TRUE ~ agec+48
        ),
        kesto=as.numeric(lubridate::decimal_date(epvm)-lubridate::decimal_date(apvm))
      ) |>
      dplyr::filter(Age >= censoring_age[1]) |> # seuranta alkaa kun henkilö täyttää 50v (cancer-/murtuma-aineisto) / OSTPRE Pelkkä ICD10 filt >= 65
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

    if(shiny::isRunning()) .safe_inc_progress(5/5)

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
