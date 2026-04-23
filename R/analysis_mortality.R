#' Multistate survival analysis of mortality with time-dependent exposure
#'
#' Performs data preparation and survival modeling to estimate cumulative
#' incidence of mortality with and without exposure using competing risks
#' and multistate models. The function constructs follow-up times from a
#' response date, applies administrative censoring, handles overlapping
#' event times, and fits survival models using counting process notation.
#'
#' @param dpop A data.frame containing individual-level data. Must include
#'   the following variables:
#'   \itemize{
#'     \item \code{ID}: Unique individual identifier
#'     \item \code{DATE_BIRTH}: Date of birth
#'     \item \code{resp.DATE}: Response date (follow-up start date)
#'     \item \code{exp.DATE}: Exposure date
#'     \item \code{DATE_DEATH}: Date of death
#'     \item \code{DATE_MIGRATION}: Date of migration (censoring event)
#'   }
#'
#' @param censoring_date A Date specifying the administrative censoring date.
#'   Events occurring after this date are ignored.
#'
#' @return A list with the following elements:
#'   \itemize{
#'     \item \code{data}: Processed dataset in counting process format suitable
#'       for survival analysis
#'     \item \code{mortality_plot}: A ggplot object showing cumulative incidence
#'       curves for:
#'       \itemize{
#'         \item Overall mortality
#'         \item Death without exposure
#'         \item Death after exposure
#'         \item Ever exposed
#'       }
#'     \item \code{cox_results1}: Hazard ratios (HR) and confidence intervals
#'       from unadjusted Cox model
#'     \item \code{cox_results2}: Hazard ratios (HR) and confidence intervals
#'       from Cox model adjusted for age (spline)
#'   }
#'
#' @details
#' Follow-up begins at \code{resp.DATE} and ends at the earliest of death,
#' migration, or administrative censoring. Exposure is treated as a
#' time-dependent variable. Individuals exposed before follow-up start are
#' flagged as exposed at baseline.
#'
#' The function uses:
#' \itemize{
#'   \item Competing risks model for death vs exposure
#'   \item Multistate model to distinguish death before and after exposure
#'   \item Cox proportional hazards models with counting process formulation
#' }
#'
#' Small time offsets are introduced when events occur at identical times
#' to ensure model stability.
#'
#' @examples
#' \dontrun{
#' result <- analysis_mortality(
#'   dpop = my_data,
#'   censoring_date = as.Date("2020-12-31")
#' )
#'
#' result$mortality_plot
#' result$cox_results1
#' }
#'
#' @importFrom dplyr mutate filter select bind_rows case_when if_else
#' @importFrom survival tmerge survfit coxph Surv tdc event
#' @importFrom ggplot2 ggplot aes geom_step labs theme_minimal theme
#' @importFrom splines bs
#'
#' @export

analysis_mortality <- function(dpop,
                               censoring_date
){

  # DEBUG
  if(FALSE){
    # dpop = dpop
    # censoring_date = as.Date("2020-12-31")
    # lines = c("Death after exposure", "Death without exposure", "Overall mortality", "Ever exposed")
  }

  if(TRUE){
    ## 1.2 Time variable calculations
    d2 <- dpop |>
      mutate(DATE_MIGRATION = dplyr::if_else(DATE_MIGRATION > censoring_date, NA, DATE_MIGRATION),
             DATE_DEATH = dplyr::if_else(DATE_DEATH > censoring_date, NA, DATE_DEATH),
             resp.DATE = dplyr::if_else(resp.DATE > censoring_date, NA, resp.DATE),
             exp.DATE = dplyr::if_else(exp.DATE > censoring_date, NA, exp.DATE) ) |>
      dplyr::filter(!is.na(resp.DATE )) |>
      dplyr::mutate(
        ## Follow up start and end dates
        apvm = resp.DATE,                                                      ## Survival alkaa RESPONSESTA ja
        epvm = pmin(DATE_MIGRATION, DATE_DEATH, censoring_date, na.rm = TRUE), ## loppuu KUOLEMAAN tai SENSUROINTIIN
        # Time calculations from FUP start (RESPONSE)
        t_exposure = as.numeric(exp.DATE - apvm),
        t_censoring = as.numeric(epvm - apvm),
        t_death = as.numeric(DATE_DEATH - apvm),
        t_epvm = as.numeric(epvm - apvm),
        age_response = trunc(lubridate::time_length(lubridate::interval(DATE_BIRTH, resp.DATE), "years"))
      ) |>
      dplyr::select(ID, t_exposure, t_death, t_censoring, t_epvm, age_response)

    ## 1.22 Exposure tag if exposure date is before fup started
    d2 <- d2 |>
      dplyr::mutate(
        exp = ifelse(!is.na(t_exposure) & t_exposure <= 0, 1L, 0L), ## exposure at starting point
        t_exposure = ifelse(!is.na(t_exposure) & t_exposure <= 0, 0.001, t_exposure),
        event=case_when(
          !is.na(t_death) ~ 1L,
          TRUE ~ 0L)
      )

    ## 1.22 Fixing date overlapping (if exp = resp = death) bc model cant handle same dates (ex. death at the same time than hip fracture diagnose)
    d2 <- d2 %>%
      mutate(
        t_exposure = ifelse(!is.na(t_exposure) & !is.na(t_death) & t_exposure == t_death,
                            t_exposure - 0.0001,
                            t_exposure),

        t_death = ifelse(!is.na(t_death) & t_death == 0,
                         t_death + 0.002,
                         t_death),

        t_censoring = ifelse(!is.na(t_censoring) & t_censoring == 0,
                             t_censoring + 0.002,
                             t_censoring)
      )
  }

  ## Splittin and multistate model ----
  if(TRUE){
    # Datan splittaus tmergellä
    # Lisätään myös erilaiset tapahtumamuuttujat factoreina
    # event_censored ei tarvita, mutta jäi tähän alkukokeilujen
    # takia ja kun muut tapahtumamuuttujat määriteltiin sitä hyödyntäen
    sd2 <- survival::tmerge(data1=d2, data2=d2, id=ID, event=event(t_censoring, event))
    sd2 <- survival::tmerge(data1=sd2, data2=d2, id=ID, expo=tdc(t_exposure)) |>
      dplyr::mutate(
        event_censored=factor(event,levels=c(0,1),labels=c("censor","death")),
        event_competing_risks=factor(dplyr::case_when(
          event_censored=="censor" & t_exposure==tstop ~ "exposure",
          TRUE ~ event_censored
        )),
        event_multistate=factor(dplyr::case_when(
          event_censored=="death" & expo==0 ~ "death without exposure",
          event_censored=="death" & expo==1 ~ "death after exposure",
          TRUE ~ event_competing_risks
        ))
      )

    # Tässä tarvittavien mallien estimointi
    fit_cr <- survival::survfit(Surv(tstart/365.25,tstop/365.25, event=event_competing_risks) ~ 1, data = sd2 |> filter(expo==0), id=ID)
    fit_ms <- survival::survfit(Surv(tstart/365.25,tstop/365.25, event=event_multistate) ~ 1, data = sd2, id=ID)
    #fit_de <- survival::survfit(Surv(tstart/365.25,tstop/365.25, event=event_competing_risks) ~ 1, data = sd2, id=ID)
    ## Tämä pois koska ei toistaiseksi tarvita (Total_mortality among exposed)
    # fit_ex <- survival::survfit(Surv(tstart/365.25,tstop/365.25, event=event_competing_risks) ~ 1, data = sd2 |> filter(expo==1), id=ID) # (fit_ex ei oikeastaan tarvita, katso alta datan koonnista selostukset)
    # fit_cr$transitions
    # fit_ms$transitions
  }


  ## Model results to plot data -----
  if(TRUE){
    df_plot <- NULL |>
      dplyr::bind_rows(
        # Tämä Responsen jälkeinen kuolleisuus ylipäätään eli komponenttien summa:
        # Ilman Responsea kuolleet + Exposureen sairaustuneet kuolleet
        # (edellä erikseen estimoituna (fit_de) ja siksi kommentoitu se pois, kun tästä tulee sama)
        # Kokonaiskuolleisuutta ei ihan pakko raportoida, mutta tätä helpoin vertailla muihin tutkimuksiin
        data.frame(
          time = fit_ms$time,
          cif = fit_ms$pstate[,"death without exposure"]+fit_ms$pstate[,"death after exposure"],
          group = "Overall mortality"
        )
      ) |>
      dplyr::bind_rows(
        # Tieto siitä,kuinka iso osa sairastunut Exposureen
        # (tätä isompi osa Exposurea-ryhmaa ei voi kuolla)
        # "referenssitaso" sille kuinka iso osa exposure-ryhmasta on hengissä
        data.frame(
          time = fit_cr$time,
          cif = fit_cr$pstate[,"exposure"],
          group = "Ever exposed"
        )
      ) |>
      dplyr::bind_rows(
        # Responsen jälkeinen kuolema ilman Exposurea
        # Estimoitu kilpailevien riskien mallista, jossa tapahtumina kuolema tai Exposureen sairastuminen
        # Kertoo kuinka iso osa kuollut ennen dementiaan sairastumista
        # Exposurea ei saa mallintaa sensurointina (tulisi aivan liian korkea kuolleisuus silloin)
        data.frame(
          time = fit_cr$time,
          cif = fit_cr$pstate[,"death"],
          group = "Death without exposure"
        )
      ) |>
      dplyr::bind_rows(
        # Exposure sairastumisen jälkeinen response-kuolleisuus
        # Mallinnettu monitilamallina, jossa kuolemille omat absorboivat tilat
        # Tämä todellinen kumulatiivinen insidenssi, jää aika matalaksi, koska
        # kuolleita ei voi olla enempää kuin exposureen sairastuneita. Siksi
        # syytä tulkita suhteessa exposuren osuuteen.
        data.frame(
          time = fit_ms$time,
          cif = fit_ms$pstate[,"death after exposure"],
          group = "Death after exposure"
        )
      )
  }

  ## Results 1: Cumulative Incidence of Mortality  -------
  if(TRUE){
    ## Plot Mortality Rates
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = time, y = cif, color = group)) +
      ggplot2::geom_step(
        ggplot2::aes(linewidth = group, alpha = group, linetype = group)
      ) +
      ggplot2::scale_linewidth_manual(values = c(
        "Death after exposure" = 1,
        "Death without exposure" = 1,
        "Overall mortality" = 2,
        "Ever exposed" = 2
      ), guide = "none") +
      ggplot2::scale_alpha_manual(values = c(
        "Death after exposure" = 1,
        "Death without exposure" = 1,
        "Overall mortality" = 0.15,
        "Ever exposed" = 0.15
      ), guide = "none") +
      ggplot2::scale_linetype_manual(values = c(
        "Death after exposure" = "solid",
        "Death without exposure" = "solid",
        "Overall mortality" = "longdash",
        "Ever exposed" = "dashed"
      ), guide = "none") +
      ggplot2::labs(
        x = "Years since Response",
        y = "Cumulative incidence"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "inside",
        legend.position.inside = c(.05,.95),
        legend.justification = c("left","top"),
        legend.title = ggplot2::element_blank()
      )
  }

  ## Results 2: Cox Model ------
  if(TRUE){
    dsurv <- survival::coxph(formula = survival::Surv(tstart, tstop, expo) ~ event, data = sd2, id = ID)
    # adjusted model
    cox_results1 <- exp(cbind(HR = coef(dsurv), confint(dsurv)))
    dsurv2 <- survival::coxph(formula = survival::Surv(tstart, tstop, expo) ~ event + splines::bs(age_response), data = sd2, id = ID)
    # unadjusted model
    cox_results2 <- exp(cbind(HR = coef(dsurv2), confint(dsurv2)))
  }

  ## Full results ------
  d <- list(
    data = sd2,
    mortality_plot = p,
    cox_results1 = cox_results1,
    cox_results2 = cox_results2
  )

  return(d)
}
