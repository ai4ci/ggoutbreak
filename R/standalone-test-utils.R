# ---
# repo: ai4ci/ggoutbreak
# file: standalone-test-utils.R
# last-updated: 2024-08-06
# license: https://unlicense.org
# imports:
#   - stats
#   - EpiEstim
#   - withr
#   - ggplot2
#   - tibble
#   - dplyr
#   - tidyr
#   - ggoutbreak
# ---
# This set of functions are simulations or conversion functions.

## Serial Interval ----

#' Generate a set of serial interval distributions using EpiEstim
#'
#' @param mean_of_mean the mean of the si mean posteriors
#' @param sd_of_mean the sd of the si mean posteriors
#' @param mean_of_sd the mean of the si sd posteriors
#' @param sd_of_sd the sd of the si sd posteriors
#' @param days length of desired SI distribution
#' @param boots number of bootstraps to generate
#' @keywords internal
#'
#' @return A dataframe containing the following columns:
#'
#' * boot (anything) - a bootstrap identifier
#' * time (positive_double) - the end of the time period (in days)
#' * probability (proportion) - the probability of infection between previous time period until `time`
#'
#' Grouped by: boot (exactly).
#'
#' @examples
#' # N.B. EpiEstim cannot reconstruct an accurate set of distributions that
#' # match the inputs parameters due to truncation of the SI distribution.
#'
#' if (interactive()) {
#'
#' ggplot2::ggplot(
#'     .epiestim_si(5,1,3,1,boots = 100, days = 14),
#'     ggplot2::aes(x=time,ymin=probability,ymax=probability,group=time)
#'   )+ggplot2::geom_errorbar(alpha=0.2)
#'
#' .epiestim_si(5,1,3,1,boots = 1000, days = 14) %>%
#'   dplyr::summarise(
#'     E = sum(probability*time),
#'     E2 = sum(probability*time^2)
#'   ) %>%
#'   dplyr::summarise(
#'     mean_of_mean = mean(E), sd_of_mean = stats::sd(E),
#'     mean_of_sd = mean(sqrt(E2 - E^2)),
#'     sd_of_sd = stats::sd(sqrt(E2 - E^2))
#'   )
#'
#' }
#'
#' # This becomes less of an issue with larger SI days parameter.
.epiestim_si = function(
    mean_of_mean, sd_of_mean, mean_of_sd, sd_of_sd,
    days = 14, boots = 100, seed = Sys.time()
  ) {
  withr::with_seed(seed, {
    mean = stats::rlnorm(boots,
                         meanlog = log(mean_of_mean/ sqrt(sd_of_mean^2/mean_of_mean^2+1)),
                         sdlog = sqrt(log(sd_of_mean^2/mean_of_mean^2+1))
    )
    sd = stats::rgamma(boots,
                       shape = mean_of_sd^2/sd_of_sd^2,
                       scale = sd_of_sd^2/mean_of_sd # this should maybe hard limited to 2 - as SD is chisq distributed and chisq is gamma with scale 2 (or rate 0.5).
    )
    return(
      lapply(1:boots, function(b) {
        tibble::tibble(
          boot = b,
          probability = EpiEstim::discr_si(1:days, mean[b], sd[b]),
          time = 1:days
        ) %>% dplyr::mutate(
          probability = ifelse(time==days, 1-sum(probability), probability)
        )
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(boot)
    )
  })
}

## Test data models ----

#' Generate an outbreak case count series defined by growth rates using a poisson model.
#'
#' @param changes a dataframe holding change time points (`time`) and
#'   growth rate per week (`r`) columns
#' @param initial the size of the initial outbreak
#' @param seed a random seed
#' @param kappa dispersion parameter. 1 is no dispersion, smaller values mean more dispersion.
#' @param max_time the desired length of the time series
#'
#' @keywords internal
#' @return A dataframe containing the following columns:
#'
#' * count (positive_integer) - Positive case counts associated with the specified timeframe
#' * time (ggoutbreak::time_period + group_unique) - A (usually complete) set of singular observations per unit time as a `time_period`
#'
#' Ungrouped.
#'
#' @examples
#' if (interactive()) {
#'
#' ggplot2::ggplot(
#'   .test_poisson_model(kappa=0.1, seed=100),
#'   ggplot2::aes(x=time,y=count)
#' )+
#' ggplot2::geom_point()
#'
#' }
.test_poisson_model = function(
    changes = tibble::tibble(time = c(0,20,40,60,80), r = c(0.1,0,-0.1,0,0.1)),
    kappa = 1,
    initial = 100,
    max_time = 104,
    seed = Sys.time()
) {
  withr::with_seed(seed,{
    return(tibble::tibble(time = 0:max_time) %>%
      dplyr::left_join(changes, by="time") %>%
      tidyr::fill(r) %>%
      dplyr::mutate(rate = initial*exp(cumsum(r))) %>%
      dplyr::mutate(
        count =
          if (kappa == 1)
            stats::rpois(dplyr::n(), rate)
        else
          stats::rnbinom(dplyr::n(), size = rate / (1/kappa - 1), prob=kappa)
        ,
        denom = max(count)*2,
        time = as.time_period(time, unit = "7 days")
      ))
  })
}

#' Generate a multinomial outbreak defined by per class growth rates and a
#' poisson model
#'
#' @param changes a list of time points and growth rates per week per class.
#' @param initial the size of the inital outbreak per class
#' @inheritDotParams .test_poisson_model
#'
#' @return a case count time series including `class`, `count` and `time` columns
#' @export
#'
#' @examples
#' if (interactive()) {
#'
#' ggplot2::ggplot(
#'   .test_multinomial(),
#'   ggplot2::aes(x=time,y=count,colour=class)
#' )+ggplot2::geom_point()
#'
#' }
.test_multinomial = function(
    changes = tibble::tibble(
      time = c(0,20,40,60,80),
      variant1 = c(0.1,0,-0.1,0,0.1),
      variant2 = c(0.15,0.05,-0.05,-0.01,0.05),
      variant3 = c(0,0.05,-0.05,+0.05,-0.05),
    ),
    initial=c(100,1,100),
    ...) {
  cols = setdiff(colnames(changes),"time")
  if (length(cols) != length(initial)) stop("Must be one initial value for each column (class) specified in `changes`")
  out = tibble::tibble()
  i = 1
  for (col in cols) {
    out = dplyr::bind_rows(
      out,
      .test_poisson_model(changes = changes %>% dplyr::select(time, r=!!col), initial=initial[[i]], ...) %>%
        dplyr::mutate(class = col, time=as.numeric(time)) %>% tibble::as_tibble()
    )
  }

  out = out %>%
    dplyr::mutate(time = as.time_period(time, unit = "7 days",start_date = "2020-01-01")) %>%
    dplyr::group_by(time) %>%
    # The relative growth rate of one variant is the weighted average growth rate of the other variants (excluding current one)
    dplyr::mutate(
      proportion = rate/sum(rate),
      proportion.obs = count/sum(count),
      relative.r = r - (sum(r*rate)-r*rate)/(sum(rate)-rate)
    ) %>%
    dplyr::ungroup()

  return(out)
}



.lookup_rt = function(infection_t, changes) {
  tmp = stats::approx(x=changes$t,y=changes$R_t,xout=infection_t,method="constant",rule=2)$y
  return(tmp)
}




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
#' tmp = .test_branching_process(
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
.test_branching_process = function(
    changes = tibble::tibble(
      t = c(0,40),
      R_t = c(2.5,0.8)
    ),
    kappa = 1,
    ip = ggoutbreak::covid_infectivity_profile,
    max_time = 80,
    initial = 30,
    seed = Sys.time(),
    summarise = TRUE,
    ...
) {
  withr::with_seed(seed,{
    omega_t = ip %>% dplyr::filter(boot == dplyr::first(boot)) %>% dplyr::pull(probability)


    .sample_contacts = function(R_t) {
      if (kappa < 1) stop("does not support underdispersion")
      if (kappa == 1) return(stats::rpois(length(R_t),lambda = R_t))
      size=R_t / (R_t*kappa^2 - 1)
      prob=1/(R_t*kappa^2)

      tmp = ifelse(
        kappa > sqrt(1/R_t),
        suppressWarnings(stats::rnbinom(length(R_t), size = size,  prob = prob)),
        stats::rpois(length(R_t),lambda = R_t)
      )

      if (any(is.na(tmp))) browser()

      return(tmp)
    }
    .sample_generation_time = function(t) {
      if (length(t) == 0) return(t)
      tmp = t+sapply(stats::runif(length(t)), function(x_p) min(which(cumsum(omega_t)>=x_p)))
      # if (any(is.na(tmp))) browser()
      return(tmp)
    }
    # kappa is mean/sd. small values <1 result in super-spreading
    out = tibble::tibble(
      id = 1:initial,
      time = 1,
      origin = NA,
      generation = 0
    ) %>% dplyr::mutate(
      R_t = .lookup_rt(time, changes),
      contacts = .sample_contacts(R_t)
    )

    last_gen = out

    i = 1
    while(nrow(last_gen) > 0) {
      message(".",appendLF = FALSE)
      # Next generation
      max_id = max(last_gen$id)
      next_gen = tibble::tibble(
        origin = rep(last_gen$id, last_gen$contacts),
        last_time = rep(last_gen$time, last_gen$contacts)
      ) %>%
        # next_gen = last_gen %>%
        # dplyr::mutate(origin = lapply(seq_along(id), function(i) rep(id[i],contacts[i]))) %>%
        # tidyr::unnest(origin) %>%
        dplyr::mutate(
          id = dplyr::row_number()+max_id,
          time = .sample_generation_time(last_time),
          R_t = .lookup_rt(time, changes),
          contacts = .sample_contacts(R_t),
          generation = i
        ) %>%
        dplyr::filter(
          time < max_time
        ) %>%
        dplyr::select(
          -last_time
        )

      i = i+1
      out = dplyr::bind_rows(out,next_gen)
      last_gen = next_gen
    }

    message("complete")

    if (summarise) return(.summarise_with_ascertainment(out,...))

    return(out)
  })
}



#' Summarise a line list with daily case ascertainment variability
#'
#' This function introduces additional noise into a case series by ensuing a
#' random proportion of the cases are detected each day. The randomness is
#' defined by a logit-normal distribution parametrised by baseline probability
#' `p.asc` and day to day variability in terms of a dispersion parameter
#' `kappa.asc`.
#'
#' @param df the line list
#' @param p.asc the background probability of a case being detected
#' @param kappa.asc a dispersion parameter controlling the day to day
#'   variability of ascertainment between 0 (no dispersion) and Inf (maximum)
#'
#' @return a count data frame.
#' @keywords internal
#' @examples
#' if (interactive()) {
#'
#' tmp = .test_branching_process(
#'   changes = tibble::tibble(t = c(0,20,40,60,80,110), R_t = c(1.8,1.5,0.9,1.5,0.8,1.2)),
#'   kappa = 2,
#'   max_time = 120,
#'   p.asc= 0.8,
#'   kappa.asc = 1.01,
#'   seed = 100,
#'   summarise = FALSE
#' )
#'
#' tmp2 = dplyr::bind_rows(lapply(c(0,0.5,1,2), function(d) {
#'   tmp %>% .summarise_with_ascertainment(0.7,d) %>% dplyr::mutate(kappa = d)
#' }))
#'
#' ggplot2::ggplot(
#'   tmp2,
#'   ggplot2::aes(x=time,y=count, colour=as.factor(kappa))
#' )+
#' ggplot2::geom_line()+
#' ggplot2::geom_point()
#'
#' }
.summarise_with_ascertainment = function(df, p.asc = 1, kappa.asc = 1) {
  tmp = df %>% dplyr::group_by(time) %>%
    dplyr::filter(!is.na(origin)) %>%
    dplyr::summarise(
      count = dplyr::n(),
      R_t = mean(contacts),
      dispersion = stats::sd(contacts)/mean(contacts)
    ) %>% dplyr::mutate(
      time = as.time_period(time, "1 day")
    )
  if (p.asc == 1) {
    return(tmp)
  }
  mu = stats::qlogis(p.asc)
  sigma = kappa.asc*mu
  # kappa.asc = min(c(sqrt((1-p.asc)/p.asc), kappa.asc))
  # k2 = kappa.asc^2
  # alpha = (1 - p.asc - k2*p.asc) / k2
  # beta = alpha*(1-p.asc)/p.asc
  return(
    tmp %>% dplyr::mutate(
      infected = count,
      count = floor(count*stats::plogis(stats::rnorm(dplyr::n(), mu, sigma)))
    )
  )
}
