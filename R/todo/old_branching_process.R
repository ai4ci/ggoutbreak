#' Generate a line list from a branching process model parameterised by
#'    reproduction number
#'
#' @param changes a dataframe containing a `t` time column and `R_t` reproduction
#'    number parameter.
#' @param kappa a dispersion parameter controlling the likelihood of individual
#'    super-spreading. This must be between 1 and Inf with 1 being standard poisson
#'    dispersion and large values representing over dispersion.
#' @param ip a data frame with `time`, and `probability` columns
#' @param max_time maximum duration of simulation
#' @param initial initial outbreak size
#' @param seed random seen
#' @param summarise if `FALSE` return the line list of one entry per person
#' @inheritDotParams .summarise_with_ascertainment -df
#'
#' @return either a line list of cases, with individual ids, infection times,
#'    and infector ids or a summary case count time series,
#' @keywords internal
#'
#' @examples
#' if (interactive()) {
#'
#' tmp = sim_branching_process(
#'   changes = tibble::tibble(t = c(0,20,40,60,80,110), R_t = c(1.8,1.5,0.9,1.5,0.8,1.2)),
#'   kappa = 2,
#'   max_time = 120,
#'   p.asc= 0.8,
#'   kappa.asc = 1.01,
#'   seed = 100
#' )
#'
#' mean(tmp$dispersion)
#' ggplot2::ggplot(tmp, ggplot2::aes(x=time,y=count))+ggplot2::geom_point()
#'
#' }
#'
sim_branching_process = function(
    changes = tibble::tibble(
      t = c(0,40),
      R_t = c(2.5,0.8)
    ),
    kappa = 1,
    ip = ggoutbreak::covid_ip,
    max_time = 80,
    initial = 30,
    seed = Sys.time(),
    summarise = TRUE,
    fn_Rt = cfg_step_fn(changes),
    ...
) {
  withr::with_seed(seed,{

    omega_t = ip %>%
      dplyr::group_by(boot) %>%
      dplyr::summarise(probability = mean(probability)) %>%
      dplyr::pull(probability)




    out = tibble::tibble(
      id = 1:initial,
      time = 1,
      infector = NA,
      generation = 0
    )

    last_ts = out

    t=2
    while(nrow(last_ts) > 0 && t<=max_time) {
      message(".",appendLF = FALSE)

      rt_case = fn_Rt(t)

      # remove recovered (i.e. no longer any probability of infection)
      next_ts = last_ts %>% filter(time >= (t-length(omega_t)))

      # browser()

      # for every infected individual the probability of infecting someone is
      # daily reproduction number * daily probability of infection on that day
      next_ts = next_ts %>% mutate(
        t2 = t,
        E_infections = rt_case * omega_t[t-time],
        infected_at_t = .sample_contacts(E_infections,kappa)
      )

      # Next generation
      max_id = max(last_ts$id)
      infected_today = tibble::tibble(
        infector = rep(next_ts$id, next_ts$infected_at_t),
        generation = rep(next_ts$generation, next_ts$infected_at_t)+1,
        time = t,
        rt_inst = rt_case
      ) %>% mutate(
        id = row_number() + max_id
      )

      t=t+1
      out = dplyr::bind_rows(out, infected_today)
      last_ts = dplyr::bind_rows(next_ts, infected_today)
    }

    message("complete")

    if (summarise) return(.summarise_with_ascertainment(out,.fn_Rt,seed=seed,...))

    return(out)
  })
}
