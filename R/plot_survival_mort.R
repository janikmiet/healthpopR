#' Plot cumulative incidence of mortality and exposure states
#'
#' Estimates cumulative incidence curves for mortality before and after
#' exposure using competing risks and multistate survival models. The function
#' constructs follow-up time variables, models exposure as a time-dependent
#' event, and produces a publication-ready cumulative incidence plot.
#'
#' @param dpop Data frame containing the study population and follow-up
#'   information.
#' @param censoring_date Date. Administrative censoring date for follow-up.
#' @param lines Character vector specifying which curves to include in the
#'   plot. Default options are:
#'   \itemize{
#'   \item `"Death after exposure"`
#'   \item `"Death without exposure"`
#'   \item `"Overall mortality"`
#'   \item `"Ever exposed"`
#'   \item `"Total mortality among exposed"`
#'   }
#'
#' @section Required variables in `dpop`:
#'
#' | Variable | Description |
#' |--------|-------------|
#' | `ID` | Unique individual identifier |
#' | `resp.DATE` | Start of follow-up (index/response date) |
#' | `exp.DATE` | Exposure date |
#' | `DATE_DEATH` | Date of death |
#' | `DATE_MIGRATION` | Date of emigration or loss to follow-up |
#'
#' @details
#' Follow-up starts at `resp.DATE` and ends at the earliest of:
#' death, migration, or the administrative censoring date.
#'
#' Exposure is treated as a time-dependent event using
#' `survival::tmerge`. The function estimates:
#'
#' * Competing risk models for exposure and death before exposure
#' * Multistate models for death after exposure
#'
#' When events occur at identical time points (e.g., exposure and death
#' on the same day), small time offsets are introduced to avoid numerical
#' issues in survival estimation.
#'
#' The resulting curves represent cumulative incidence for:
#'
#' * Death without exposure
#' * Death after exposure
#' * Overall mortality
#' * Exposure incidence
#'
#' @return A list containing:
#' \itemize{
#' \item `data` Data frame used for plotting cumulative incidence curves.
#' \item `plot` A `ggplot2` object containing the cumulative incidence plot.
#' }
#'
#' @examples
#' \dontrun{
#' res <- plot_surv_mort(
#'   dpop = population_data,
#'   censoring_date = as.Date("2023-12-31")
#' )
#'
#' # Show plot
#' res$plot
#'
#' # Access plotting data
#' head(res$data)
#' }
#'
#' @importFrom survival Surv survfit tmerge
#' @importFrom dplyr filter mutate select case_when bind_rows
#' @importFrom ggplot2 ggplot aes geom_step labs theme_minimal theme
#' @export

plot_surv_mort <- function(dpop,
                           censoring_date,
                           lines = c("Death after exposure", "Death without exposure", "Overall mortality", "Ever exposed", "Total mortality among exposed")){



  # Survival Method --------
  # dpop = dpop
  # censoring_date = as.Date("2023-12-31")
  # lines = c("Death after exposure", "Death without exposure", "Overall mortality", "Ever exposed")


  ## Part 1: Data Wrangling --------------
  # Tässä vain pieniä muutoksia aikamuuttujiin, että saadaan varmasti
  # kaikki siirtymät mukaan. Valmiiksi altistuneita voisi malleissa käsitellä
  # annettujen alkutilojen avulla, mutta tämä "pakotussplittaus" toimii
  # ihan hyvin myös.
  if(TRUE){
    # TODO koska käytetään dpop condition pvm; miten käsitellä ennen alkua tapaukset? Optio, jossa voisi hakea dg-taulusta seuraavan dg:n?

    ## 1.2 Time variable calculations
    d2 <- dpop |>
      filter(!is.na(resp.DATE )) |>
      dplyr::mutate(
        ## Follow up start and end dates
        apvm = resp.DATE,                                                      ## Survival alkaa RESPONSESTA ja
        epvm = pmin(DATE_MIGRATION, DATE_DEATH, censoring_date, na.rm = TRUE), ## loppuu KUOLEMAAN tai SENSUROINTIIN

        # Time calculations from FUP start (RESPONSE)
        t_exposure = as.numeric(exp.DATE - apvm),
        t_censoring = as.numeric(epvm - apvm),
        t_death = as.numeric(DATE_DEATH - apvm),
      ) |>
      dplyr::select(ID, t_exposure, t_death, t_censoring)



    ## 1.21 Fixing date overlapping (if exp = resp = death) bc model cant handle same dates (ex. death at the same time than hip fracture diagnose)
    d2 <- d2 %>%
      mutate(
        t_exposure = ifelse(!is.na(t_exposure) & t_exposure == t_death,
                            t_exposure - 0.001,
                            t_exposure),

        t_death = ifelse(!is.na(t_death) & t_death == 0,
                         t_death + 0.001,
                         t_death),

        t_censoring = ifelse(!is.na(t_censoring) & t_censoring == 0,
                             t_censoring + 0.001,
                             t_censoring)
      )

    ## 1.22 Exposure tag if exposure date is before fup started
    d2 <- d2 |>
      mutate(
        exp = ifelse(!is.na(t_exposure) & t_exposure <= 0, 1L, 0L), ## exposure at starting point
        t_exposure = ifelse(!is.na(t_exposure) & t_exposure <= 0, 0.001, t_exposure),
        event=case_when(
          !is.na(t_exposure) & t_exposure==t_censoring ~ 0L,
          !is.na(t_death) & t_death==t_censoring ~ 1L,
          TRUE ~ 0L)
      )

    ## Summary of data
    # summary(d2)
  }

  # Datan splittaus tmergellä
  # Lisätään myös erilaiset tapahtumamuuttujat factoreina
  # event_censored ei tarvita, mutta jäi tähän alkukokeilujen
  # takia ja kun muut tapahtumamuuttujat määriteltiin sitä hyödyntäen
  sd2 <- survival::tmerge(data1=d2, data2=d2, id=ID, event=event(t_censoring, event))
  sd2 <- survival::tmerge(data1=sd2, data2=d2, id=ID, expo=tdc(t_exposure)) |>
    mutate(
      event_censored=factor(event,levels=c(0,1),labels=c("censor","death")),
      event_competing_risks=factor(case_when(
        event_censored=="censor" & t_exposure==tstop ~ "exposure",
        TRUE ~ event_censored
      )),
      event_multistate=factor(case_when(
        event_censored=="death" & expo==0 ~ "death without exposure",
        event_censored=="death" & expo==1 ~ "death after exposure",
        TRUE ~ event_competing_risks
      ))
    )

  # Tässä tarvittavien mallien estimointi
  fit_cr <- survival::survfit(Surv(tstart/365.25,tstop/365.25, event=event_competing_risks) ~ 1, data = sd2 |> filter(expo==0), id=ID)
  #fit_de <- survival::survfit(Surv(tstart/365.25,tstop/365.25, event=event_competing_risks) ~ 1, data = sd2, id=ID)
  fit_ex <- survival::survfit(Surv(tstart/365.25,tstop/365.25, event=event_competing_risks) ~ 1, data = sd2 |> filter(expo==1), id=ID) # (fit_ex ei oikeastaan tarvita, katso alta datan koonnista selostukset)
  fit_ms <- survival::survfit(Surv(tstart/365.25,tstop/365.25, event=event_multistate) ~ 1, data = sd2, id=ID)
  # fit_cr$transitions
  # fit_ms$transitions

  df_plot <- NULL |>
    bind_rows(
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
    bind_rows(
      # Tieto siitä,kuinka iso osa sairastunut Exposureen
      # (tätä isompi osa Exposurea-ryhmaa ei voi kuolla)
      # Tämä on syytä raportoida, koska tämä "referenssitaso" sille,
      # että kuinka iso osa exposure-ryhmasta on hengissä
      data.frame(
        time = fit_cr$time,
        cif = fit_cr$pstate[,"exposure"],
        group = "Ever exposed"
      )
    ) |>
    bind_rows(
      # Responsen jälkeinen kuolema ilman Exposurea
      # Estimoitu kilpailevien riskien mallista, jossa tapahtumina kuolema tai Exposureen sairastuminen
      # Tämä pitää raportoida, kertoo kuinka iso osa kuollut ennen dementiaan sairastumista
      # Exposurea ei saa mallintaa sensurointina (tulisi aivan liian korkea kuolleisuus silloin)
      data.frame(
        time = fit_cr$time,
        cif = fit_cr$pstate[,"death"],
        group = "Death without exposure"
      )
    ) |>
    bind_rows(
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
    ) |>
    bind_rows(
      # Tässä vielä erikseen mallinnettu Exposure-kuolleisuus Responseen
      # sairastuneiden joukossa. Tätä hankalampi tulkita, kun ei ihan
      # selvää, että mistä seuranta varsinaisesti alkaa. Nyt counting process
      # ajoilla, mutta luultavasti tulisi sama myös "clock resetillä".
      # Tätä ei tarve raportoida, nyt mukana vain kokeiluna. Tämä ei ole
      # täsmälleen sama asia kuin responsekuolleisuus exposure-ryhmalla
      # suhteutettuna exposure sairastuneiden osuuteen (sitä ei nyt
      # piirrettynä, kun niitä vastaavat käyrät eivät ole samoille
      # aikapisteille ja pitäisi siis "täsmäyttää")
      data.frame(
        time = fit_ex$time,
        cif = fit_ex$pstate[,"death"],
        group = "Total mortality among exposed"
      )
    )

  # Plot Version 0
  # ggplot2::ggplot(df_plot, ggplot2::aes(x=time, y=cif, color=group)) +
  #   ggplot2::geom_step() +
  #   ggplot2::labs(
  #     x = "Years since index event",
  #     y = "Cumulative incidence",
  #   ) +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     legend.position = "inside",
  #     legend.position.inside = c(.05,.95),
  #     legend.justification = c("left", "top"),
  #     legend.title=ggplot2::element_blank()
  #   )
  #
  #

  ## Final plot
  df_plot2 <- df_plot %>%
    filter(group %in% lines)

  ## Plots
  p <- ggplot2::ggplot(df_plot2, ggplot2::aes(x = time, y = cif, color = group)) +
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

  d <- list(
    data = df_plot2,
    plot = p
  )

  return(d)
}
