## Function generator for configuration ----

#' Step function from dataframe
#'
#' @param changes a dataframe with `t` and `<col_name>` columns which define the cut points
#' for a step function.
#' @param col_name the value column (optional if only 2 columns)
#' @param ... not used
#'
#' @return a function that inputs a vector `t` and returns the next smallest
#' corresponding value in `<col_name>` (or the first one)
#' @export
#' @concept test
cfg_step_fn = function(
  changes,
  ...,
  col_name = setdiff(colnames(changes), "t")
) {
  #TODO: interfacer
  fn = stats::approxfun(
    changes$t,
    changes[[col_name]],
    rule = 2:2,
    method = "constant"
  )
  return(function(..., t = ..1) fn(t))
}


#' Linear function from dataframe
#'
#' @param changes a dataframe with `t` and `<col_name>` columns which define the change
#' points for a piecewise linear function.
#' @param col_name the value column (optional if only 2 columns)
#' @param ... not used
#'
#' @return a function that inputs a vector `t` and returns a linearly
#'   interpolated value from `<col_name>`
#' @export
#' @concept test
cfg_linear_fn = function(
  changes,
  ...,
  col_name = setdiff(colnames(changes), "t")
) {
  fn = stats::approxfun(
    changes$t,
    changes[[col_name]],
    rule = 2:2,
    method = "linear"
  )
  return(function(..., t = ..1) fn(t))
}

#' Random probability function with day of week effect
#'
#' This function returns a random probability generator that has a weekly period
#' and configurable degree of additional variability around weekly pattern. It is
#' suited to probabilities of things like testing which may depend on the day
#' of week.
#'
#' @param prob the rates of e.g. ascertainment for each day of the week.
#' @param kappa dispersion parameter between 0 and 1. O is no dispersion. 1 is maximum
#' @param week_starts locale description of first day of week (default is a "Monday").
#'
#' @return a random number generator function taking `t` time parameter and
#'   returning a probability of ascertainment for that time, depending on day of
#'   week etc.
#' @export
#' @concept test
#'
#' @examples
#' fn = cfg_weekly_proportion_rng(c(0.9,0.9,0.9,0.9,0.9,0.1,0.1))
#' matrix(fn(1:42),ncol=7,byrow=TRUE)
cfg_weekly_proportion_rng = function(
  prob = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.5, 0.5),
  kappa = 0.1,
  week_starts = weekdays(as.Date("2024-10-14"))
) {
  idx = which(weekdays(as.time_period(0:6, "1 days")) == week_starts)
  if (length(idx) != 1) {
    stop(
      "`week_starts` cannot be matched to one of: ",
      paste0(weekdays(as.time_period(0:6, "1 days")), collapse = ", ")
    )
  }
  if (length(prob) != 7) {
    stop("ascertainment must be length 7")
  }
  kappa = rep(kappa, length.out = length(prob))
  return(function(..., t = ..1) {
    if (is.time_period(t) && .get_meta(t)$unit != lubridate::days()) {
      stop(
        "`cfg_weekly_proportion_rng()` only works with daily interval time periods"
      )
    }
    dow = (floor(t + 8 - idx) %% 7) + 1
    dow_prob = prob[dow]
    dow_kappa = kappa[dow]
    # warnings can be produced if value of t is NA.
    suppressWarnings(rbeta2(t, prob = dow_prob, kappa = dow_kappa))
  })
}

#' Weekly delay function with day of week effect
#'
#' This function returns a random number generator from a gamma distribution
#' that has a weekly period and configurable degree of additional variability
#' around weekly pattern. It is suited to delays of things like testing which
#' may depend on the day of week.
#'
#' @param mean a mean amount of delay for each day of the week
#' @param sd the SD of the delay
#' @param week_starts locale description of first day of week (default is a "Monday").
#'
#' @return a random number generator taking `t` time parameter and returning a
#'   duration the that time `t` depending on the day of the week.
#' @export
#' @concept test
#'
#' @examples
#' fn = cfg_weekly_gamma_rng(c(1,1,1,1,4,3,2))
#' matrix(fn(1:42),ncol=7,byrow=TRUE)
cfg_weekly_gamma_rng = function(
  mean = c(1, 1, 1, 1, 4, 3, 2),
  sd = sqrt(mean),
  week_starts = weekdays(as.Date("2024-10-14"))
) {
  idx = which(weekdays(as.time_period(0:6, "1 days")) == week_starts)
  if (length(idx) != 1) {
    stop(
      "`week_starts` cannot be matched to one of: ",
      paste0(weekdays(as.time_period(0:6, "1 days")), collapse = ", ")
    )
  }

  if (length(mean) != 7) {
    stop("delay must be length 7")
  }
  sd = rep(sd, length.out = length(mean))
  return(function(..., t = ..1) {
    if (is.time_period(t) && .get_meta(t)$unit != lubridate::days()) {
      stop(
        "`cfg_weekly_gamma_rng()` only works with daily interval time periods"
      )
    }
    dow = (floor(t + 8 - idx) %% 7) + 1
    dow_mean = mean[dow]
    dow_sd = sd[dow]
    # warnings can be produced if value of t is NA.
    suppressWarnings(rgamma2(t, mean = dow_mean, sd = dow_sd))
  })
}


#' Weekly convolution distribution function
#'
#' @param mean The means of a gamma distributed delay function for each weekday
#' @param sd The sds of a gamma distributed delay function for each weekday
#' @param week_starts locale description of first day of week (default is a "Monday").
#'
#' @return a time dependent function that inputs a time (as a time_period) and
#'   generates an IP delay distribution for each day varying by day of week
#' @export
#' @concept test
#'
#' @examples
#' cat(sapply(cfg_weekly_ip_fn()(1:7),format_ip),sep = "\n")
cfg_weekly_ip_fn = function(
  mean = c(1, 1, 1, 1, 4, 3, 2),
  sd = sqrt(mean),
  week_starts = weekdays(as.Date("2024-10-14"))
) {
  idx = which(weekdays(as.time_period(0:6, "1 days")) == week_starts)
  if (length(idx) != 1) {
    stop(
      "`week_starts` cannot be matched to one of: ",
      paste0(weekdays(as.time_period(0:6, "1 days")), collapse = ", ")
    )
  }
  if (length(mean) != 7) {
    stop("delay must be length 7")
  }
  sd = rep(sd, length.out = length(mean))
  return(function(..., t = ..1) {
    if (is.time_period(t) && .get_meta(t)$unit != lubridate::days()) {
      stop("`cfg_weekly_ip_fn()` only works with daily interval time periods")
    }
    dow = (floor(t + 8 - idx) %% 7) + 1
    dow_mean = mean[dow]
    dow_sd = sd[dow]
    # warnings can be produced if value of t is NA.
    purrr::map2(
      dow_mean,
      dow_sd,
      ~ make_gamma_ip(median_of_mean = .x, median_of_sd = .y)
    )
  })
}

#' Get a IP generating function from time varying mean and SD of a gamma function
#'
#' @param mean_fn a function which gives the time-varying mean of a gamma
#'   distribution, The function will be called minimally with `.x` or `t` which
#'   will be the time as a time period. Other variables may be present.
#' @param sd_fn a function which gives the time-varying mean of a gamma
#'   distribution. The function will be called with minimally `.x` or `t` which
#'   will be the time period and `.y` or `mean` will be the mean. Other variables
#'   may be present.
#'
#' @return a time dependent function that inputs a time (as a time_period) and
#'   returns an ip delay distribution for each day defined by the `mean_fn` and `sd_fn`
#' @export
#' @concept test
#'
#' @examples
#' fn = cfg_gamma_ip_fn(mean_fn = \(t) ifelse(t < 5, 4, 2))
#' # a gamma function that changes mean at time 5
#' fn(4)
#' fn(7)
cfg_gamma_ip_fn = function(mean_fn = ~2, sd_fn = \(mean) sqrt(mean)) {
  mean_fn = .fn_check(mean_fn)
  sd_fn = .fn_check(sd_fn)

  return(function(..., t = ..1) {
    mean = mean_fn(t = t, ...)
    sd = sd_fn(t = t, mean = mean, ...)

    if (length(mean) == 1) {
      mean = rep(mean, length.out = length(t))
    }
    if (length(sd) == 1) {
      sd = rep(sd, length.out = length(t))
    }

    purrr::map2(
      mean,
      sd,
      ~ make_gamma_ip(median_of_mean = .x, median_of_sd = .y)
    )
  })
}

#' Generate a random probability based on features of the simulation
#'
#' @param probability_fn a function which gives the time-varying mean of a beta
#'   distribution, The function will be called minimally with `.x` or `t`` which
#'   will be the time as a time period. Other variables may be present.
#' @param kappa_fn a function which gives the time-varying dispersion of a beta
#'   distribution. The function will be called with minimally `.x` or `t` which
#'   will be the time period and `.y` or `mean` will be the mean. Other variables
#'   may be present.
#' @seealso [rbeta2()]
#'
#' @return a time dependent function that inputs a time (as a time_period) and
#'   returns an probability for each day defined by the `probability_fn` and `kappa_fn`
#' @export
#' @concept test
#'
#' @examples
#' fn = cfg_beta_prob_rng(~ ifelse(.x<=5,0.1,0.9))
#' fn(1:10)
cfg_beta_prob_rng = function(probability_fn = ~0.8, kappa_fn = ~0.1) {
  probability_fn = .fn_check(probability_fn)
  kappa_fn = .fn_check(kappa_fn)

  return(function(..., t = ..1) {
    probability = probability_fn(t, ...)
    kappa = kappa_fn(t, ...)
    probability = rep(probability, length.out = length(t))
    kappa = rep(kappa, length.out = length(t))
    rbeta2(length(t), prob = probability, kappa = kappa)
  })
}

#' Randomly sample from an empirical distribution
#'
#' This is used for random sampling from the infectivity profile for times to
#' infection, for example. There is nothing to stop you putting in a delay
#' distribution with negative times but strange things may happen in your
#' simulation.
#'
#' @iparam ip a long format empirical distribution
#'
#' @return a function which accepts `n` parameter which produces random samples
#' from the `ip` distribution
#' @export
#' @concept test
#' @examples
#' tmp = cfg_ip_sampler_rng(ganyani_ip_2)(10000)
#'
#' # This discretised ganyani distribution is based on these figures:
#' # mean: 5.2 (3.78-6.78) and sd: 1.72 (0.91-3.93)
#' format_ip(ganyani_ip_2)
#'
#' mean(tmp) # Should be about 5.2
#' stats::sd(tmp) # Should be about 1.72
cfg_ip_sampler_rng = function(ip = i_empirical_ip) {
  qfun = .ip_quantile_function(ip)
  return(function(n, ...) {
    qfun(stats::runif(n))
  })
}


#' Sample from a multinomial transition matrix
#'
#' This is particularly designed for use within the `fn_list_next_gen` parameter
#' of [sim_branching_process()] to allow age group contract matrices to be applied
#' for example (assuming a categorical age).
#'
#' @param transition a transition matrix or long format dataframe. If a matrix
#' the columns should add to 1, and the column names are the input class. row names are
#' the output class. the data frame format must have input, output, and probability
#' columns.
#'
#' @return a function that given an input will return samples of the output
#'   class according to the probability distributions.
#' @export
#' @concept test
#'
#' @examples
#' age = rep(c("child","adult","elderly"),100)
#'
#' fn = cfg_transition_fn(tibble::tribble(
#'   ~input, ~output, ~probability,
#'   "child", "child", 0.5,
#'   "child", "adult", 0.5,
#'   "adult", "child", 0.5,
#'   "adult", "adult", 0.3,
#'   "adult", "elderly", 0.2,
#'   "elderly","elderly", 0.5,
#'   "elderly","adult", 0.5,
#' ))
#'
#' table(fn(age),age)
cfg_transition_fn = function(transition) {
  if (is.matrix(transition)) {
    transition = .matrix_to_long(transition)
  }
  return(
    function(..., .x = ..1) {
      .sample_from_long(.x, transition)
    }
  )
}


## Test data models ----

#' Generate a simple time-series of cases based on a growth rate step function
#'
#' This time-series has no statistical noise and is useful for testing things.
#' It has a fixed known value for infections and growth rate (fixed at 0.05 and
#' -0.05 per day), and a instantaneous reproduction number which is based on the
#' provided infectivity profile.  A fixed denominator gives a known proportion
#' and a relative growth rate that is the same as the growth rate.
#'
#' @param ip an infectivity profile. any uncertainty will be collapsed into the
#'   central distribution.
#' @param duration the total length of the time-series
#' @param period the duration of each positive or negative growth phase
#'
#' @return a time series with `count`, `incidence`, `growth`, `rt`,
#'   `proportion` and `relative.growth` columns
#' @export
#' @concept test
#'
#' @examples
#' sim_test_data() %>% dplyr::glimpse()
sim_test_data = function(
  ip = ggoutbreak::test_ip,
  duration = 500,
  period = 50
) {
  ip = summarise_ip(ip)

  changes = tibble::tibble(
    t = seq(0, duration - period, period),
    growth = rep(c(0.05, -0.05), length.out = duration %/% period)
  )
  test = tibble::tibble(
    time = as.time_period(0:duration, "1 day")
  ) %>%
    dplyr::mutate(
      growth = cfg_step_fn(changes)(time),
      incidence = 100 * exp(dplyr::lag(cumsum(growth), default = 0))
    )

  foi = test %>%
    dplyr::cross_join(ip) %>%
    dplyr::group_by(time = time + tau) %>%
    dplyr::summarise(
      force = sum(incidence * probability),
      n = dplyr::n()
    ) %>%
    dplyr::filter(n == max(n))

  data = test %>%
    dplyr::left_join(foi, by = "time") %>%
    dplyr::mutate(rt = incidence / force) %>%
    dplyr::select(-force, -n) %>%
    dplyr::mutate(
      denom = round(max(incidence) * 10),
      proportion = incidence / (max(incidence) * 10),
      relative.growth = growth,
      count = round(incidence)
    )

  return(data)
}

#' Generate an outbreak case count series defined by growth rates using a poisson model.
#'
#' @param changes a dataframe holding change time points (`t`) and
#'   growth rate per week (`growth`) columns
#' @param fn_imports a function that takes input vector `t` and returns the
#'   number of imported cases at times `t`.
#' @param seed a random seed
#' @param kappa a dispersion parameter. 1 is no dispersion (compared to poisson), smaller values mean more dispersion.
#' @param max_time the desired length of the time series,
#' @param fn_growth a function that takes input vector `t` and returns the growth rates at times `t`
#' @param time_unit e.g. a daily or weekly time series: "1 day", "7 days"
#'
#' @export
#' @concept test
#' @ireturn A dataframe of case counts
#' @examples
#'
#' tmp2 = sim_poisson_model(seed=100, fn_imports = ~ ifelse(.x %in% c(0,50),100,0))
#'
#' if (interactive()) {
#'   ggplot2::ggplot(tmp2)+ggplot2::geom_point(ggplot2::aes(x=time,y=count))
#' }
#'
sim_poisson_model = function(
  changes = tibble::tibble(
    t = c(0, 20, 40, 60, 80),
    growth = c(0.1, 0, -0.1, 0, 0.1)
  ),
  kappa = 1,
  max_time = 104,
  seed = Sys.time(),
  fn_growth = cfg_step_fn(changes),
  fn_imports = ~ ifelse(.x == 0, 100, 0),
  time_unit = "1 day"
) {
  fn_growth = .fn_check(fn_growth)
  fn_imports = .fn_check(fn_imports)

  withr::with_seed(seed, {
    imports = fn_imports(0:max_time)
    r = fn_growth(0:max_time)
    rate = rep(-1, length(imports))
    rate[1] = imports[1]
    for (i in 2:length(rate)) {
      rate[i] = rate[i - 1] * exp(r[i - 1]) + imports[i]
    }

    tmp = tibble::tibble(
      time = 0:max_time,
      growth = r,
      imports = imports,
      rate = rate
    ) %>%
      dplyr::mutate(
        count = .sample_contacts(rate, kappa),
        time = ggoutbreak::as.time_period(time, unit = time_unit)
      )

    tmp = tmp %>%
      dplyr::mutate(
        statistic = "infections"
      ) %>%
      dplyr::group_by(statistic)
  })

  interfacer::ireturn(
    structure(
      tmp,
      events = .extract_events(tmp, growth, time, "growth=%1.2f"),
      fn = fn_growth
    ),
    i_sim_count_data
  )
}


#' Generate an outbreak case count series defined by Reproduction number using a poisson model.
#'
#' @param changes a dataframe holding change time points (`t`) and
#'   reproduction number (`rt`) columns
#' @param seed a random seed
#' @param kappa a dispersion parameter. 1 is no dispersion (compared to poisson), smaller values mean more dispersion.
#' @param max_time the desired length of the time series,
#' @param fn_Rt a function that takes input vector `t` and returns the instantaneous reproduction
#'   number at time `t`
#' @param fn_imports a function that takes input vector `t` and returns the
#'   number of imported cases at times `t`.
#' @param fn_ip a function that takes input vector `t` and returns an
#'   infectivity profile at times `t`.
#' @param time_unit e.g. a daily or weekly time series: "1 day", "7 days"
#'
#' @export
#' @concept test
#' @ireturn A dataframe of case counts
#' @examples
#'
#' tmp = sim_poisson_Rt_model(kappa=1, seed=100, fn_imports = ~ ifelse(.x %in% c(0,50),100,0))
#'
#' if (interactive()) {
#'   ggplot2::ggplot(tmp,ggplot2::aes(x=time,y=count))+ggplot2::geom_point()+
#'     ggplot2::geom_line()
#' }
#'
sim_poisson_Rt_model = function(
  changes = tibble::tibble(
    t = c(0, 40),
    rt = c(2.5, 0.8)
  ),
  kappa = 1,
  max_time = 80,
  seed = Sys.time(),
  fn_Rt = cfg_step_fn(changes),
  fn_imports = ~ ifelse(.x == 0, 30, 0),
  fn_ip = ~ ggoutbreak::test_ip,
  time_unit = "1 day"
) {
  fn_Rt = .fn_check(fn_Rt)
  fn_imports = .fn_check(fn_imports)
  fn_ip = .fn_check(fn_ip)

  withr::with_seed(seed, {
    imports = fn_imports(0:max_time)
    R = fn_Rt(0:max_time)
    rate = rep(-1, length(imports))
    rate[1] = imports[1]
    for (i in 2:length(rate)) {
      ip = .summary_ip_no_chk(fn_ip(i))
      # reverse counts
      if (i < nrow(ip)) {
        tmp = c(rev(rate[1:(i - 1)]), rep(0, nrow(ip) - i))
      } else {
        tmp = rev(rate[(i - nrow(ip) + 1):(i - 1)])
      }

      rate[i] = sum(ip$probability[-1] * tmp) * R[i] + imports[i]
    }

    tmp = tibble::tibble(
      time = 0:max_time,
      rt = R,
      imports = imports,
      rate = rate
    ) %>%
      dplyr::mutate(
        count = .sample_contacts(rate, kappa),
        time = ggoutbreak::as.time_period(time, unit = time_unit),
        statistic = "infections"
      ) %>%
      dplyr::group_by(statistic)
  })

  interfacer::ireturn(
    structure(
      tmp,
      events = .extract_events(tmp, rt, time, "Rt=%1.2f"),
      fn = fn_Rt
    ),
    i_sim_count_data
  )
}

#' Generate a multinomial outbreak defined by per class growth rates and a
#' poisson model
#'
#' @param changes a list of time points in column `t` and growth rates per week
#'   per class, in other columns.
#' @param initial the size of the initial outbreak per class. There should be
#'   one entry per class
#' @inheritDotParams sim_poisson_model
#' @param time_unit e.g. a daily or weekly time series: "1 day", "7 days"
#'
#' @return a case count time series including `class`, `count` and `time` columns
#' @concept test
#' @export
#'
#' @examples
#' if (interactive()) {
#'   plot_counts(
#'     sim_multinomial() %>% dplyr::glimpse()
#'   )
#' }
sim_multinomial = function(
  changes = tibble::tibble(
    t = c(0, 20, 40, 60, 80),
    variant1 = c(0.1, 0, -0.1, 0, 0.1),
    variant2 = c(0.15, 0.05, -0.05, -0.01, 0.05),
    variant3 = c(0, 0.05, -0.05, +0.05, -0.05),
  ),
  initial = c(100, 100, 100),
  time_unit = "1 day",
  ...
) {
  cols = setdiff(colnames(changes), "t")
  if (length(cols) != length(initial)) {
    stop(
      "Must be one initial value for each column (class) specified in `changes`"
    )
  }
  out = tibble::tibble()
  i = 1
  for (col in cols) {
    out = dplyr::bind_rows(
      out,
      sim_poisson_model(
        changes = changes %>% dplyr::select(t, r = !!col),
        fn_imports = ~ ifelse(.x == 0, initial[[i]], 0),
        time_unit = time_unit,
        ...
      ) %>%
        dplyr::mutate(class = col, time = as.numeric(time)) %>%
        tibble::as_tibble()
    )
    i = i + 1
  }

  out = out %>%
    dplyr::mutate(time = ggoutbreak::as.time_period(time, unit = time_unit)) %>%
    dplyr::group_by(time) %>%
    # The relative growth rate of one variant is compared to the weighted average
    # growth rate of the other variants (excluding current one)
    dplyr::mutate(
      proportion = rate / sum(rate),
      proportion.obs = count / sum(count),
      relative.growth = growth -
        (sum(growth * rate) - growth * rate) / (sum(rate) - rate)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(class)

  #TODO: make this more like the branching process models.
  events = changes %>%
    tidyr::pivot_longer(
      cols = -t,
      names_to = "class",
      values_to = "r"
    ) %>%
    dplyr::transmute(
      start = as.Date(as.time_period(t, unit = time_unit)),
      end = NA,
      label = sprintf("r=%1.2f", r)
    )

  return(
    structure(out, events = .extract_events(out, growth, time, "r=%1.2f"))
  )
}


#TODO: a multinomial model based on R_t?
# this would need a serial interval per variant.

# sim_deterministic_sir = function(
#     changes = tibble::tibble(
#       t = c(0,40),
#       R_t = c(2.5,0.8)
#     ),
#     kappa = 1,
#     gamma = 1/10,
#     # ip = ggoutbreak::covid_ip,
#     max_time = 80,
#     initial = 30,
#     seed = Sys.time()
#   ) {
#
#   SIR = function(time, current_state, params ){
#
#     with(c(as.list(c(current_state, params))),{
#
#       R_t = .lookup_rt(time, changes)
#       beta = R_t * gamma / (S+I+R)
#
#       # Change in Susceptibles
#       dS <- - beta * S * I # + delta * R
#       # Change in Infecteds
#       dI <- beta * S * I - gamma * I
#       # Change in Recovereds
#       dR <- gamma * I # - delta * R
#       return(list(c(dS, dI, dR)))
#     })
#
#
#   }
#
#   params <- c(gamma = gamma, changes = changes)
#   initial_state <- c(S = 10000-initial, I = initial, R = 0)
#   times <- 0:max_time
#   model <- deSolve::ode(initial_state, times, SIR, params)
#   return(model)
#
# }

#' Generate a line list from a branching process model parametrised by
#'    reproduction number
#'
#' @param changes a dataframe containing a `t` time column and `R` reproduction
#'    number parameter. This parameter is optional if `fn_Rt` is specified
#' @param fn_kappa a vectorised function taking `t` and other imported case
#'   metadata returning a dispersion parameter controlling the likelihood of
#'   individual super-spreading. This must be between 1 and `Inf` with 1 being
#'   standard poisson dispersion and larger values representing over dispersion.
#' @param max_time maximum duration of simulation
#' @param seed random seed
#' @param fn_Rt can be specified instead of `changes` df. This is a vectorised
#'   function that accepts a time parameter and returns a reproduction number.
#'   If both this and `changes` are specified this takes preference.
#' @param fn_ip a function that takes input vector `t` (and/or `class`) and
#'   returns an infectivity profile at times `t`.
#' @param imports_df a data frame containing minimally `time` and `count` columns
#'   plus any metadata about the imports in additional columns. Metadata columns
#'   can inform the `fn_Rt`,`fn_kappa` and `fn_ip` functions as additional parameters.
#' @param fn_imports a time varying function the defines the number of infected
#'    importations. If `imports_df` is defined then this is used instead
#' @param fn_list_next_gen a named list of functions. The name corresponds to
#'   metadata columns in the simulation, the function is a `purrr` style mapping that
#'   will replace the old value in the named column with a new one. Such a function can be
#'   generated with [cfg_transition_fn()] when a transition probability matrix is involved,
#'   of it can be specified directly as a `case_when` style function. The function
#'   must be vectorised and assume no grouping structure. If the function has
#'   named parameters it can reference any of the metadata columns, or time
#'   (as `t`). The [rcategorical()] function may be useful in this scenario.
#' @param ... not used
#'
#' @ireturn a line list of cases for a simulated outbreak
#' @export
#' @concept test
#'
#' @examples
#' tmp = sim_branching_process(
#'   changes = tibble::tibble(t = c(0,40), R = c(1.5,0.8)),
#'   max_time = 120,
#'   seed = 100,
#'   fn_imports = ~ ifelse(.x<10,1,0)
#' )
#'
#' if(interactive()) {
#'   plot_cases(tmp, mapping=ggplot2::aes(fill=as.factor(generation)),linewidth=0.1)
#' }
#'
#' # imports can also be specified as a dataframe, which allows additional
#' # metadata in the line list. An example of which is as follows:
#' imports_df = tibble::tribble(
#'   ~time, ~variant, ~count,
#'   0:4, "wild-type", 100,
#'   10:14, "alpha", 5,
#' )
sim_branching_process = function(
  changes = tibble::tibble(
    t = c(0, 40),
    rt = c(2.5, 0.8)
  ),
  max_time = 80,
  seed = Sys.time(),
  fn_Rt = cfg_step_fn(changes),
  fn_ip = ~ ggoutbreak::test_ip,
  fn_kappa = ~1,
  imports_df = NULL,
  fn_imports = ~ ifelse(.x == 0, 30, 0),
  fn_list_next_gen = list(),
  ...
) {
  fn_Rt = .fn_check(fn_Rt)
  fn_ip = .fn_check(fn_ip)
  fn_kappa = .fn_check(fn_kappa)
  fn_mutate_next_gen = .fn_transition(fn_list_next_gen)

  if (is.null(imports_df)) {
    imports_df = .fn_to_imports_df(fn_imports, max_time, seed)
  }

  imports_df = imports_df %>%
    tidyr::unnest(time) %>%
    dplyr::mutate(time = as.numeric(time)) %>%
    dplyr::group_by(dplyr::across(c(-time, -count)))

  # empty cache w/ correct grouping
  ip_cache = imports_df %>% dplyr::select(-count) %>% dplyr::filter(FALSE)

  # grps is the metadata columns
  grps = imports_df %>% dplyr::groups()

  withr::with_seed(seed, {
    out = imports_df %>%
      dplyr::slice(rep(1:dplyr::n(), times = count)) %>%
      dplyr::select(-count) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        id = dplyr::row_number(),
        generation_interval = NA,
        infector = NA,
        generation = 0
      ) %>%
      dplyr::group_by(!!!grps)

    # join by should be the metadata columns and time
    join_by = intersect(colnames(out), colnames(ip_cache))

    last_gen = out

    while (nrow(last_gen) > 0) {
      message(".", appendLF = FALSE)

      # for every infected individual the probability of infecting someone

      new_ip_cache = last_gen %>%
        dplyr::ungroup() %>%
        dplyr::select(!!!grps, time) %>%
        dplyr::anti_join(ip_cache, by = join_by) %>%
        dplyr::distinct()

      if (nrow(new_ip_cache) > 0) {
        new_ip_cache = new_ip_cache %>%

          dplyr::mutate(t = time) %>%
          dplyr::mutate(
            ip = .ts_evaluate(fn_ip, .) %>%
              purrr::map(.summary_ip_no_chk) %>%
              purrr::map(
                ~ .x %>%
                  dplyr::ungroup() %>%
                  dplyr::select(tau, probability)
              )
          ) %>%
          .unnest_dt("ip") %>%
          dplyr::mutate(t = time + tau) %>%
          dplyr::mutate(
            Rt_tau = .ts_evaluate(fn_Rt, .),
            kappa_t_tau = .ts_evaluate(fn_kappa, .),
            E_infections_t_tau = Rt_tau * probability,
            t_tau = t
          ) %>%
          dplyr::select(-t)

        ip_cache = dplyr::bind_rows(ip_cache, new_ip_cache)
      }

      next_gen = last_gen %>%
        dplyr::inner_join(ip_cache, by = join_by) %>%
        dplyr::mutate(
          infector = id,
          infected_time = t_tau,
          #TODO: is it OK to sample from each t_tau instead of combining them to
          # get a count on day t_tau? Basically this comes down to whether the
          # dispersion of sum of multiple small negative binomials is the same as
          # of one single one for combination. I think
          # it is OK for the poisson case. In all cases kappa will be the same.
          # kappa dispersion is only dependent on p parameter & sum of independents has
          # same p parameter. so it should be OK as long as kappa is the same for
          # all items combined (which it should usually be here...) ....
          infected = .sample_contacts(E_infections_t_tau, kappa_t_tau)
        ) %>%
        dplyr::select(
          -kappa_t_tau,
          -E_infections_t_tau,
          -Rt_tau,
          -t_tau,
          -tau,
          -probability
        ) %>%
        dplyr::filter(infected > 0) %>%
        dplyr::group_by(!!!grps)
      # we never need to summarise this as we are instead expanding it to
      # individual counts of infected:

      # Next generation
      max_id = max(last_gen$id)
      infected = next_gen %>%
        dplyr::slice(.rep2(1:dplyr::n(), times = infected)) %>%
        dplyr::select(-infected) %>%
        dplyr::mutate(
          generation = generation + 1,
          generation_interval = infected_time - time
        ) %>%
        dplyr::select(-time) %>%
        dplyr::rename(time = infected_time) %>%
        dplyr::filter(time <= max_time) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(id = dplyr::row_number() + max_id) %>%
        dplyr::group_by(!!!grps)

      infected = infected %>% fn_mutate_next_gen()

      out = dplyr::bind_rows(out, infected)
      last_gen = infected
    }

    message("complete")

    events = ip_cache %>%
      dplyr::filter(tau == 0) %>%
      dplyr::select(!!!grps, Rt = Rt_tau, time) %>%
      dplyr::arrange(time) %>%
      dplyr::distinct() %>%
      .extract_events(Rt, time, "Rt=%1.2f")

    out = out %>%
      dplyr::mutate(time = as.time_period(time, unit = "1 day"))

    interfacer::ireturn(
      structure(
        out,
        ip_cache = ip_cache %>%
          dplyr::mutate(
            time = as.time_period(time, unit = "1 day"),
            t_tau = as.time_period(t_tau, unit = "1 day")
          ),
        events = events
      ),
      i_sim_linelist
    )
  })
}


#' SEIR model with time-varying transmission parameter
#'
#' This function simulates an SEIR (Susceptible-Exposed-Infectious-Recovered) model
#' where the transmission rate (\code{beta}) varies over time.
#'
#' The latent period (time from infection to becoming infectious) is assumed to be
#' exponentially distributed. The infectious period is derived from the given
#' mean of the generation time and the latent period.
#'
#' @param changes a dataframe holding change time points (`t`) and
#'   the proportional increase in the transmission rate in the `dBeta` column
#' @param seed a random seed
#' @param mean_latent_period Mean time from infection to becoming infectious (E to I),
#'        assumed to be exponentially distributed.
#' @param mean_gen_time The average generation time (will be latent period+infectious duration).
#' @param R0 the initial reproduction number
#' @param fn_dBeta A function of time \code{t} that returns the multiple of
#'   transmission rate at that time point. Transmission (beta) at time zero is
#'   defined by R0 and the infectious duration, This is multiplied at each time
#'   by this factor.
#' @param N Total population size. Defaults to 10,000.
#' @param imports Initial number of infectious individuals. Defaults to 10.
#' @param max_time Maximum simulation time (in days or time units). Defaults to 100.
#'
#' @return `r i_sim_count_data`
#' @export
#' @concept test
#'
#' @details
#' The model assumes:
#' - Latent period ~ Exponential(sigma), where sigma = 1 / \code{mean_latent_period}
#' - Infectious period ~ Exponential(gamma), where gamma = 1 / (mean_gen_time - mean_latent_period)
#' - Generation time ~ Exponential(sigma) + Exponential(gamma)
#' - beta0 = R0 * gamma
#' - Transmission rate beta(t) = fn_dBeta(t) * beta0
#'
#' The generation time is defined as the sum of the latent and infectious periods.
#'
#' @examples
#' # Example: Lockdown after day 30
#'
#' seir_output <- sim_seir_model(
#'   mean_latent_period = 4,
#'   mean_gen_time = 7,
#'   R0 = 2.5,
#'   fn_dBeta = function(t) ifelse(t < 30, 1, 0.5)
#' )
#'
#' if (interactive()) {
#'   plot_ip(attr(seir_output,"ip"))
#'   plot_counts(seir_output)
#' }
#'
sim_seir_model <- function(
  changes = tibble::tibble(
    t = c(0, 30),
    dBeta = c(1, 0.5)
  ),
  mean_latent_period,
  mean_gen_time,
  R0 = 2.5,
  fn_dBeta = cfg_step_fn(changes),
  N = 10000,
  imports = 10,
  max_time = 104,
  seed = Sys.time()
) {
  # --- Step 2: Validate input: latent period must be less than generation time ---
  if (mean_latent_period >= mean_gen_time) {
    stop("mean_latent_period must be less than mean_gen_time")
  }

  # --- Step 3: Derive infectious period and gamma ---
  mean_inf_period <- mean_gen_time - mean_latent_period
  gamma <- 1 / mean_inf_period
  sigma <- 1 / mean_latent_period

  # --- Step 4: Initial conditions ---
  S0 <- N - imports
  y0 <- c(S = S0, E = 0, I = imports, R = 0, CumI = 0)

  beta0 <- R0 * gamma

  # --- Step 5: Time vector ---
  times <- seq(0, max_time, by = 1)

  # --- Step 6: Define the ODE system ---
  SEIR <- function(t, y, ...) {
    with(as.list(y), {
      # Get current R_t and compute beta

      # beta_t <- Rt * gamma * N / S
      beta_t = beta0 * fn_dBeta(t)

      # ODEs
      dS <- -beta_t * S * I / N
      dE <- beta_t * S * I / N - sigma * E
      dI <- sigma * E - gamma * I
      dR <- gamma * I

      dCumI <- sigma * E

      list(c(dS, dE, dI, dR, dCumI))
    })
  }

  # --- Step 7: Solve the ODEs ---
  out <- deSolve::ode(y = y0, times = times, func = SEIR)

  # --- Step 8: Convert output to data frame ---
  colnames(out)[2:6] <- c("S", "E", "I", "R", "cumulative_I")
  withr::with_seed(seed, {
    out_df <- as.data.frame(out) %>%
      dplyr::mutate(
        beta = beta0 * fn_dBeta(time),
        rt = beta / gamma * S / N,
        time = as.time_period(time, unit = "1 day"),
        statistic = "infections",
        population = N,
        rate = cumulative_I - dplyr::lag(cumulative_I, default = 0),
        count = stats::rpois(dplyr::n(), rate),
        imports = ifelse(time == 0, imports, 0)
      )

    ip = ggoutbreak::make_resampled_ip(
      stats::rexp(1000, sigma) + stats::rexp(1000, gamma)
    )
  })

  return(structure(
    out_df,
    ip = ip
  ))
}

# Plot helpers ----

#' Extract the events dataframe from a simulation output
#'
#' All the simulations should include details of major changes in the simulation
#' input parameters, particularly for step functions. This set of events can be
#' directly represented on a `ggoutbreak` plot using the `events` parameter
#'
#' @param df the output of a `ggoutbreak` simulation
#'
#' @returns an events dataframe
#' @export
#' @concept test
#'
#' @examples
#' sim_events(test_poisson_rt)
sim_events = function(df) {
  tmp = attr(df, "events")
  if (is.null(tmp)) {
    tmp = interfacer::iproto(i_events)
  }
  return(tmp)
}

#' The principal input function to a `ggoutbreak` simulation as a `ggplot2` layer.
#'
#' simulations are typically parameterised by time varying $R_t$ or by growth
#' rate, and this relationship is embedded in the simulation outputs. Plotting
#' the value of these on the default `ggoutbreak` plots requires extracting this
#' function and rescaling it to align with the dates, which is what this function
#' does.
#'
#' @param df the output of a `ggoutbreak` simulation, typically this is going to
#' be the input of an estimator.
#' @inheritDotParams ggplot2::geom_function
#'
#' @returns a `geom_function` of the parameter
#' @export
#' @concept test
#'
#' @examples
#' ggplot2::ggplot()+
#'   sim_geom_function(test_poisson_rt, xlim=as.Date("2019-12-29")+c(0,80))+
#'   ggplot2::scale_x_date()
sim_geom_function = function(df, ...) {
  tmp_fn = attr(df, "fn")
  if (is.null(tmp_fn)) {
    return(NULL)
  }
  return(ggplot2::geom_function(
    fun = \(x) tmp_fn(date_to_time(x, df$time)),
    ...
  ))
}

## Count model functions ----

#' Apply delay distributions to count data
#'
#' This function uses convolution of time delay functions (which can themselves
#' be a function of time, e.g. weekly periodicity) with incident infections to
#' generate realistic looking outbreak metrics, including admission, death,
#' symptom onset and testing.
#'
#' @param df the output of [sim_poisson_model()] or [sim_summarise_linelist()],
#'   including a `count` column and a `time` column
#' @inheritParams sim_apply_delay
#' @param fn_symptom_profile,fn_admission_profile,fn_death_profile a function
#'   that takes time and returns the probability density of symptoms,
#'   admissions, or deaths over time since infection (i.e. `tau`) as an ip
#'   delay distribution. If possible it is a very good idea to pre-compute these
#'   distributions as they need to be assigned to every line in the input and
#'   this can be very slow.
#' @param fn_sample_profile a function that takes time and returns the
#'   probability density of test sample being taken over time since symptoms.
#' @param fn_result_profile a function that takes time and returns the
#'   probability density of test result being available over time since test
#'   sampling.
#'
#' @return a long format set of counts of infections, symptom, admitted, death,
#'   sample (tests taken), results (test results).
#' @export
#' @keywords internal
#'
#' @examples
#' tmp = sim_poisson_model(seed=100) %>% sim_apply_delay()
#'
#' if(interactive()) {
#'   plot_counts(tmp, mapping=ggplot2::aes(colour=statistic))+
#'     ggplot2::geom_line()
#' }
sim_apply_delay.count_data = function(
  df,
  ...,
  fn_p_symptomatic = ~0.5,
  fn_symptom_profile = cfg_gamma_ip_fn(~5),
  fn_p_admitted = ~0.1,
  fn_admission_profile = cfg_weekly_ip_fn(c(8, 8, 8, 8, 8, 9.5, 9)),
  fn_p_died = ~0.05,
  fn_death_profile = cfg_gamma_ip_fn(~14),
  fn_p_tested = ~0.8,
  fn_sample_profile = cfg_weekly_ip_fn(c(1, 1, 1, 1, 1, 1.5, 1.4)),
  fn_result_profile = cfg_weekly_ip_fn(c(1, 1, 1, 1, 1, 1.6, 1.5)),
  seed = Sys.time()
) {
  events = attr(df, "events")

  df = df %>%
    sim_convolution(
      fn_p_symptomatic,
      fn_symptom_profile,
      input = "infections",
      output = "symptom"
    ) %>%
    sim_convolution(
      fn_p_admitted,
      fn_admission_profile,
      input = "infections",
      output = "admitted"
    ) %>%
    sim_convolution(
      fn_p_died,
      fn_death_profile,
      input = "infections",
      output = "death"
    ) %>%
    sim_convolution(
      fn_p_tested,
      fn_sample_profile,
      input = "symptom",
      output = "sample"
    ) %>%
    sim_convolution(
      rlang::as_function(~1),
      fn_result_profile,
      input = "sample",
      output = "result"
    ) %>%
    sim_delayed_observation(
      fn_result_profile,
      input = "sample",
      output = "sample"
    ) %>%
    dplyr::group_by(statistic)

  attr(df, "events") = events
  return(df)
}

#' Apply a ascertainment bias to the observed case counts.
#'
#' @iparam df a count dataframe from e.g. [sim_poisson_model()] or
#'   [sim_summarise_linelist()]
#' @param fn_asc a function that takes a single input vector `t` and returns a
#'   probability of ascertainment, e.g. `~ stats::rbeta(.x, 20, 80)` or
#'   `~ rbeta2(.x,prob=<probability>,kappa=<dispersion>)`. or
#'   [cfg_weekly_proportion_rng()]
#' @param seed a RNG seed
#'
#' @return a dataframe with `original` column, and `count` column modified to
#'   include ascertainment bias.
#' @export
#' @concept test
#'
#' @examples
#' tibble::tibble(
#'   statistic = "incidence",
#'   time=as.time_period(1:10,"1 day"),
#'   count=rep(100,10)
#' ) %>% dplyr::group_by(statistic) %>% sim_apply_ascertainment(~ ifelse(.x<=5,0.1,0.9))
sim_apply_ascertainment = function(
  df = i_sim_count_data,
  fn_asc = ~1,
  seed = Sys.time()
) {
  df = interfacer::ivalidate(df)

  fn_asc = .fn_check(fn_asc)

  withr::with_seed(seed, {
    df = df %>%
      dplyr::mutate(
        original = count,
        ascertainment = .ts_evaluate(fn_asc, .),
        count = stats::rbinom(count, size = count, prob = ascertainment)
      )
  })

  return(df)
}

#' Apply a time varying probability and convolution to count data
#'
#' Standard convolution assumes one delay distribution. This is actually not
#' what we see in reality as delays can depend on any factors, including the
#' day of week. This function applies a convolution to an input time-series
#' when that convolution is expressed as a function (usually of time, but can be
#' of anything in the input dataframe). The convolution is then sampled using
#' a poisson or negative binomial
#'
#' @iparam df a count dataframe from e.g. [sim_poisson_model()] or
#'   [sim_summarise_linelist()]
#' @param p_fn a function that takes a time parameter and potentially and
#'   returns probability of observation given something occurs. a no-op for this
#'   parameter is `~ 1`.
#' @param delay_fn a function that takes time and returns the probability of
#'   observation (given it occurred) over time since infection (i.e. `tau`) as an
#'   ip delay distribution. This does not have to sum to 1 (e.g. mapping incidence
#'   to prevalence) but if not then the combination of `p_fn` and `delay_fn` is
#'   less easy to interpret. This should behave sensibly if p changes halfway
#'   through a convolution. See
#'   [cfg_weekly_ip_fn()] and [cfg_gamma_ip_fn()] for helper functions to
#'   construct this parameter. A no-op for this parameter would be `~ ifelse(.x==0,1,0)`.
#' @param input the input statistic
#' @param output the output statistic
#' @param kappa dispersion. scaled such that poisson dispersion is 1. Values
#'   must be 0 (no dispersion), 1 (poisson dispersion) or greater than 1 for
#' over-dispersion.
#' @param from Controls if you base future counts on previous counts or on
#'   underlying rate, defaults to `count` but `rate` is a possibility if you
#'   want to base the convolution off a more theoretical value rather than
#'   observed cases. Either way the convolution generates a new rate which is
#'   in turn sampled into a poisson or negative binomial count.
#' @param ... not used
#'
#' @return return the result of applying this convolution to the data.
#' @export
#' @concept test
#'
#' @examples
#' weekday_delay = make_gamma_ip(median_of_mean = 5, median_of_sd = 2)
#' weekend_delay = make_gamma_ip(median_of_mean = 6, median_of_sd = 2)
#'
#' delay_fn = ~ ifelse(.x %% 7 %in% c(6,7), list(weekend_delay), list(weekday_delay))
#' p_fn = ~ ifelse(.x < 20, 0.5, 0.75)
#'
#' data = tibble::tibble(
#'     time=1:40,
#'     count = rep(100,40),
#'     rate = rep(100,40),
#'     statistic="infections") %>% dplyr::group_by(statistic)
#' delayed = data %>%
#'     sim_convolution(p_fn,delay_fn,output="delayed") %>%
#'     dplyr::filter(statistic=="delayed")
#' if (interactive()) ggplot2::ggplot(delayed,ggplot2::aes(x=time))+
#'   ggplot2::geom_line(ggplot2::aes(y=rate))+
#'   ggplot2::geom_line(ggplot2::aes(y=count))
#'
#' # other example delay functions
#' delay_fn = cfg_gamma_ip_fn( ~ ifelse(.x<5, 8, 4))
#  delay_fn = cfg_weekly_ip_fn()
#'
sim_convolution = function(
  df = i_sim_count_data,
  p_fn,
  delay_fn,
  ...,
  input = "infections",
  output,
  kappa = 1,
  from = c("count", "rate")
) {
  events = attr(df, "events")
  fn = attr(df, "fn")

  df = interfacer::ivalidate(df)

  rlang::check_dots_empty()
  grps = df %>% dplyr::groups()

  from = match.arg(from)
  p_fn = .fn_check(p_fn)
  delay_fn = .fn_check(delay_fn)

  tmp = df %>%
    dplyr::select(time, tidyselect::everything()) %>%
    dplyr::filter(statistic == input) %>%
    dplyr::mutate(
      .p = .ts_evaluate(p_fn, .),
      .prof = .ts_evaluate(delay_fn, .) %>%
        purrr::map(.summary_ip_no_chk) %>%
        purrr::map(
          ~ .x %>% dplyr::ungroup() %>% dplyr::select(tau, probability)
        )
    ) %>%
    # .prof will have a `boot` column and it will have a `time` column.
    .unnest_dt(col = ".prof")

  tmp = tmp %>%
    dplyr::mutate(
      # x_t_tau is the proportion of input from time t attributable to the
      # future timepoint t+tau after delay. This assumes ip is in regular
      # interval date periods.
      t_tau = time + tau,
      count_t_tau = .p * count * probability,
      rate_t_tau = .p * rate * probability,
    ) %>%
    dplyr::group_by(!!!grps, time = t_tau) %>%
    dplyr::summarise(
      count = sum(count_t_tau),
      rate = sum(rate_t_tau)
    )

  if (from == "count") {
    tmp = tmp %>%
      dplyr::mutate(
        count = .sample_contacts(count, kappa)
      )
  } else {
    tmp = tmp %>%
      dplyr::mutate(
        count = .sample_contacts(rate, kappa)
      )
  }

  tmp = tmp %>%
    dplyr::mutate(statistic = output) %>%
    dplyr::filter(time < max(df$time))

  df = df %>% dplyr::filter(statistic != output)
  out = dplyr::bind_rows(df, tmp)
  attr(out, "events") = events
  attr(out, "fn") = fn

  return(out %>% dplyr::group_by(!!!unique(c(grps, rlang::sym("statistic")))))
}

#' Apply a right censoring to count data.
#'
#' Delayed observations means that if, for example a case is only attributed to
#' a disease after a delay there is right censoring of the data. There can be
#' very complex patterns of right censoring if for example observations are
#' batched and published weekly. During COVID in the UK some death data was
#' published frequently but some was retrospectively reported on monthly
#' intervals, depending on where the patient died, which lead to complex time
#' dependent biases in the death data. Given the description of the delay, this
#' function will simulate this effect for count data. In another example there
#' were delays reporting test results in the run up to Christmas which resulted
#' in case rates apparently dropping as schools broke up. This could have
#' affected timing of the 2021 Christmas lockdown.
#'
#' @param df a count dataframe from e.g. [sim_poisson_model()] or
#'   [sim_summarise_linelist()]
#' @param delay_fn a function that takes time and returns the probability of
#'   observation (given it occurred) over time since infection (i.e. tau) as an
#'   ip delay distribution. This does not have to sum to 1 (e.g. mapping
#'   incidence to prevalence) but if not then it will behave as if some fraction
#'   or events are not observed (or observed multiple times). See
#'   [cfg_weekly_ip_fn()] and [cfg_gamma_ip_fn()] for helper functions to
#'   construct this parameter.
#' @param input the input statistic (defaults to `count`)
#' @param output the output column name (defaults to same as `input`)
#' @param max_time the date on which censoring is taking place.
#' @param ... not used
#'
#' @return the result of applying this right censoring to the data.
#' @export
#' @concept test
#'
#' @examples
#' weekday_delay = make_gamma_ip(median_of_mean = 5, median_of_sd = 2)
#' weekend_delay = make_gamma_ip(median_of_mean = 7, median_of_sd = 2)
#'
#' delay_fn = ~ ifelse(.x %% 7 %in% c(6,7), list(weekend_delay), list(weekday_delay))
#'
#' data = tibble::tibble(time=1:40, count = rep(100,40), statistic="infections") %>%
#'   dplyr::group_by(statistic) %>%
#'   sim_delayed_observation(delay_fn,output="delayed")
#'
#' if (interactive()) ggplot2::ggplot(data,ggplot2::aes(x=time,colour=statistic))+
#'   ggplot2::geom_line(ggplot2::aes(y=count))
sim_delayed_observation = function(
  df = i_sim_count_data,
  delay_fn,
  ...,
  input = "infections",
  output = input,
  max_time = max(df$time)
) {
  events = attr(df, "events")
  fn = attr(df, "fn")

  df = interfacer::ivalidate(df)

  rlang::check_dots_empty()
  grps = df %>% dplyr::groups()

  delay_fn = .fn_check(delay_fn)

  tmp = df %>%
    dplyr::filter(statistic == input) %>%
    dplyr::select(time, tidyselect::everything()) %>%
    dplyr::mutate(
      .prof = .ts_evaluate(delay_fn, .) %>%
        purrr::map(.summary_ip_no_chk) %>%
        purrr::map(
          ~ .x %>% dplyr::ungroup() %>% dplyr::select(tau, probability)
        )
    ) %>%
    .unnest_dt(col = ".prof") %>%
    dplyr::mutate(
      # x_t_tau is the proportion of input from time t attributable to the
      # future timepoint t+tau after delay. If this is after max_time it will
      # not be observed.
      t_tau = time + tau,
      count_t_tau = count * probability # probability is from the .prof
    ) %>%
    dplyr::filter(t_tau <= max_time) %>%
    # time here is the original time point (not t_tau)
    # the following line works because there is no actual convolution in time
    # so other columns are not duplicated.
    dplyr::group_by(dplyr::pick(-c(t_tau, tau, count_t_tau, probability))) %>%
    dplyr::summarise(
      count = as.integer(floor(sum(count_t_tau) + 0.5))
    ) %>%
    dplyr::mutate(statistic = output) %>%
    dplyr::group_by(!!!grps)

  df = df %>% dplyr::filter(statistic != output)
  out = dplyr::bind_rows(df, tmp)
  attr(out, "events") = events
  attr(out, "fn") = fn

  return(out %>% dplyr::group_by(!!!unique(c(grps, rlang::sym("statistic")))))
}


## Line list functions ----

#' Apply a time-varying probability and delay function to linelist data
#'
#' @iparam df a line list dataframe arising from e.g. [sim_branching_process()]
#' @param p_fn Function that returns a probability between 0 and 1 for each row
#'   of the input dataframe. A `purrr` style lambda is OK (e.g. `~ 1` for always
#'   true) the first parameter of this will be time of infection. The function
#'   must be vectorised on its inputs (and consume additional inputs with `...`)
#' @param delay_fn A function that calculates the time to event onset from the
#'   `input` time. This will be called with a vector of infection times as the
#'   first parameter (`time`) but all other columns of `df` are also available
#'   as well as the `symptomatic`,`died`,and `admitted` flags. The function must
#'   be vectorised on its inputs (and consume additional inputs with `...`). A
#'   `purrr` style lambda is OK e.g. `~ stats::rgamma(.x, shape = 3)`, and the
#'   first parameter will be infection time. if you have an discrete probability
#'   profile for this then you can use `cfg_ip_sampler_rng(ip_symptoms)` without
#'   the tilde.
#' @param input a time column from which to calculate the delay from.
#' @param output an output column set name (defaults to `"event"`)
#' @param seed RNG seed for reproducibility
#'
#' @return the line list with extra columns with prefix given by `output`,
#'   specifying whether the event was observed, the delay and the simulation
#'   time.
#' @export
#' @concept test
#'
#' @examples
#'
#' tmp = sim_branching_process(
#'   changes = tibble::tibble(t = c(0,20,40,60,80,110), R = c(1.8,1.5,0.9,1.5,0.8,1.2)),
#'   max_time = 120,
#'   seed = 100
#' )
#'
#' tmp2 = tmp %>% sim_delay(
#'   p_fn = ~ rbern(.x, 0.8),
#'   delay_fn = ~ rgamma2(.x, mean = 5),
#' )
#' tmp2 %>% dplyr::glimpse()
#'
sim_delay = function(
  df = i_sim_linelist,
  p_fn,
  delay_fn,
  input = "time",
  output = "event",
  seed = Sys.time()
) {
  events = attr(df, "events")
  df = interfacer::ivalidate(df)

  input = rlang::ensym(input)
  cols = colnames(df)

  withr::with_seed(seed, {
    df = df %>%
      dplyr::relocate(!!input) %>%
      dplyr::mutate(
        .t = !!input,
        .p = rbern(dplyr::n(), .ts_evaluate(p_fn, .)),
        .delay = ifelse(!.p, NA, .ts_evaluate(delay_fn, .))
      ) %>%
      dplyr::mutate(
        !!(output) := .p,
        !!(sprintf("%s_delay", output)) := .delay,
        !!(sprintf("%s_time", output)) := .t + .delay
      ) %>%
      dplyr::select(-.p, -.t, -.delay) %>%
      dplyr::relocate(!!!cols)
  })

  attr(df, "events") = events
  return(df)
}


#' Apply delay distribution to count or linelist data
#'
#' Events include symptom onset, admission, death, test sampling, test
#' processing
#'
#' @iparam df a line list dataframe arising from e.g. [sim_branching_process()]
#' @param fn_p_symptomatic,fn_p_admitted,fn_p_died,fn_p_tested Function that
#'   returns a probability between 0 and 1 for each row of the input dataframe.
#'   A `purrr` style lambda is OK (e.g. `~ 1` for always true) the first
#'   parameter of this will be time of infection. The function must be
#'   vectorised on its inputs (and consume additional inputs with `...`)
#' @param seed RNG seed for reproducibility
#' @inheritDotParams sim_apply_delay.linelist
#' @inheritDotParams sim_apply_delay.count_data
#'
#' @return Depends on input, either:
#' * a wide format line list with additional `XX`, `XX_time` and `XX_delay` columns,
#'   for each of the set of statistics generated.
#' * a long format set of counts of different statistics i.e. `infections`,
#'   `symptoms`, `admission`, `death`, `sample` (tests taken), `results` (test results) .
#' @export
#' @concept test
#'
#' @examples
#' tmp = sim_branching_process(
#'   changes = tibble::tibble(t = c(0,20,40,60,80,110), R = c(1.8,1.5,0.9,1.5,0.8,1.2)),
#'   max_time = 120,
#'   seed = 100
#' )
#'
#' tmp2 = tmp %>% sim_apply_delay()
#' tmp2 %>% dplyr::glimpse()
#'
sim_apply_delay = function(
  df,
  ...,
  fn_p_symptomatic = ~0.5,
  fn_p_admitted = ~0.1,
  fn_p_died = ~0.05,
  fn_p_tested = ~0.8,
  seed = Sys.time()
) {
  events = attr(df, "events")

  out = interfacer::idispatch(
    df,
    sim_apply_delay.linelist = i_sim_linelist,
    sim_apply_delay.count_data = i_sim_count_data
  )

  attr(out, "events") = events

  return(out)
}


#' Augment a line list of infection with a set of events
#'
#' Events include symptom onset, admission, death, test sampling, test
#' processing
#'
#' @iparam df a line list dataframe arising from e.g. [sim_branching_process()]
#' @inheritParams sim_apply_delay
#' @param fn_symptom_delay,fn_admission_delay,fn_death_delay, a function that
#'   calculates the time to event onset from infection. This will be called with
#'   a vector of infection times as the first parameter (`time`) but all other
#'   columns of `df` are also available as well as the `symptomatic`,`died`,and
#'   `admitted` flags. The function must be vectorised on its inputs (and
#'   consume additional inputs with `...`). A `purrr` style lambda is OK e.g. `~
#'   stats::rgamma(.x, shape = 3)`, and the first parameter will be infection
#'   time. if you have an discrete probability profile for this then you can use
#'   `cfg_ip_sampler_rng(ip_symptoms)`.
#' @param fn_sample_delay This function returns the time from either symptom
#'   onset (symptomatic) or from infection (asymptomatic) until a sample is
#'   taken. (N.B. this might be better to do a screening test probability plus
#'   screening test frequency rather than overloading this.)
#' @param fn_result_delay Identical to other functions except the first
#'   parameter will be `sample_time` rather than time of infection. This is the
#'   time from sampling to the result being available.
#' @param seed RNG seed for reproducibility
#'
#' @return a line list with additional time and delay columns.
#' @export
#' @keywords internal
#'
#' @examples
#' tmp = sim_branching_process(
#'   changes = tibble::tibble(t = c(0,20,40,60,80,110), R = c(1.8,1.5,0.9,1.5,0.8,1.2)),
#'   max_time = 120,
#'   seed = 100
#' )
#'
#' tmp2 = tmp %>% sim_apply_delay()
#' tmp2 %>% dplyr::glimpse()
#'
sim_apply_delay.linelist = function(
  df = i_sim_linelist,
  ...,
  fn_p_symptomatic = ~0.5,
  fn_symptom_delay = ~ rgamma2(.x, mean = 5),
  fn_p_admitted = ~0.1,
  fn_admission_delay = cfg_weekly_gamma_rng(c(8, 8, 8, 8, 8, 9.5, 9)),
  fn_p_died = ~0.05,
  fn_death_delay = ~ rgamma2(.x, mean = 14),
  fn_p_tested = ~0.8,
  fn_sample_delay = cfg_weekly_gamma_rng(c(1, 1, 1, 1, 1, 1.5, 1.4)),
  fn_result_delay = cfg_weekly_gamma_rng(c(1, 1, 1, 1, 1, 1.6, 1.5)),
  seed = Sys.time()
) {
  # rlang::check_dots_empty()

  df = interfacer::ivalidate(df)

  events = attr(df, "events")

  withr::with_seed(seed, {
    df = df %>%
      sim_delay(
        p_fn = fn_p_symptomatic,
        delay_fn = fn_symptom_delay,
        input = "time",
        "symptom"
      ) %>%
      sim_delay(
        p_fn = fn_p_admitted,
        delay_fn = fn_admission_delay,
        input = "time",
        "admitted"
      ) %>%
      sim_delay(
        p_fn = fn_p_died,
        delay_fn = fn_death_delay,
        input = "time",
        "death"
      ) %>%
      sim_delay(
        p_fn = fn_p_tested,
        delay_fn = fn_sample_delay,
        input = "time",
        "sample"
      ) %>%
      dplyr::rename(tested = sample) %>%
      sim_delay(
        p_fn = \(t, tested) tested,
        delay_fn = fn_result_delay,
        input = "sample_time",
        "result"
      ) %>%
      dplyr::select(-result)
  })

  attr(df, "events") = events

  return(df)
}


#' Summarise a line list
#'
#' This function converts a line list into a daily count of incident cases, plus
#' infections, admissions, deaths, test samples, test results if present.
#' Censoring of these counts can also be defined. Whilst summarising various
#' network measures such as the forward looking case reproduction number are
#' also calculated.
#'
#' @iparam df a line list dataframe arising from e.g. [sim_branching_process()]
#' @param ... the grouping to include in the summarisation.
#' @param censoring a named list of column names (without the `_time` suffix) of
#'   the kind created by [sim_delay()] or [sim_apply_delay()], and an associated function defining the
#'   delay of reporting that the column experiences. For this
#'   function `t` (or `.x` for a purrr lambda) will refer to the `XX_time`
#'   column, i.e. whenever the event that is being reported happened and `time`
#'   the simulation infection time. N.B. since infection is not observed you
#'   can't censor it.
#' @param max_time the censoring time for this observation. If this is a vector
#'   there will be multiple time series in the output
#' @ireturn a count data frame with additional statistics.
#' @concept test
#' @export
#' @examples
#'
#' sim = sim_branching_process(
#'   changes = tibble::tibble(t = c(0,40), R = c(1.7,0.8)),
#'   max_time = 120,
#'   seed = 100,
#'   fn_imports = ~ ifelse(.x==0,100,0)
#' )
#'
#' tmp = sim %>% sim_summarise_linelist()
#'
#' p1 = plot_counts(tmp)
#'
#' p2 = ggplot2::ggplot(tmp, ggplot2::aes(x=as.Date(time)))+
#'   ggplot2::geom_point(ggplot2::aes(y=rt.case,colour="case"))+
#'   ggplot2::geom_point(ggplot2::aes(y=rt.inst,colour="instantaneous"))+
#'   ggplot2::geom_line(ggplot2::aes(y=rt.weighted))+
#'   ggplot2::coord_cartesian(ylim=c(0,3.5))+
#'   ggplot2::xlab(NULL)
#'
#' patchwork::wrap_plots(p1,p2,ncol=1,axes="collect")
#'
sim_summarise_linelist = function(
  df = i_sim_linelist,
  ...,
  censoring = list(
    admitted = \(t) rgamma2(t, mean = 5),
    death = \(t) rgamma2(t, mean = 10),
    sample = \(t, result_delay) result_delay
  ),
  max_time = max(df$time)
) {
  oldGroups = df %>% dplyr::groups()
  grps = rlang::ensyms(...)
  events = attr(df, "events")
  ip_cache = attr(df, "ip_cache")
  ip_cache_join = intersect(
    unlist(c(dplyr::group_vars(df), "time")),
    colnames(ip_cache)
  )
  join_cols = unlist(c(sapply(grps, rlang::as_label), "time"))

  df = interfacer::ivalidate(df)

  # calculates the 2 different types of observable Rt from the line
  # list data based on renewal eq for instantaneous Rt, outgoing network
  # degree for case Rt.

  # The actual number of infectees for this infector
  # a forward looking reproduction number.
  # If we truncate time this needs to be changed
  supercount = df %>%
    dplyr::left_join(
      df %>%
        dplyr::group_by(infector) %>%
        dplyr::summarise(infectee_n = dplyr::n()),
      by = c("id" = "infector")
    ) %>%
    dplyr::mutate(
      infectee_n = ifelse(is.na(infectee_n), 0, infectee_n),
      time = floor(time)
    ) %>%
    dplyr::group_by(!!!grps, time) %>%
    dplyr::summarise(
      infections = dplyr::n(),
      rt.case = mean(infectee_n),
      dispersion = stats::sd(infectee_n) / mean(infectee_n)
    ) %>%
    tidyr::complete(
      time = ggoutbreak::date_seq(time, 1),
      fill = list(infections = 0, rt.case = NA_real_, dispersion = NA_real_)
    )

  # Force of infection is calculated at an individual level.
  subcount = df %>%
    dplyr::group_by(!!!oldGroups, time) %>%
    dplyr::summarise(infections = dplyr::n()) %>%
    dplyr::left_join(ip_cache, by = ip_cache_join)

  foi = subcount %>%
    dplyr::group_by(!!!oldGroups, time = t_tau) %>%
    dplyr::summarise(force = sum(probability * infections)) %>%
    dplyr::group_by(!!!grps, time) %>%
    dplyr::summarise(force = sum(force))

  rt_df = subcount %>%
    dplyr::filter(tau == 0) %>%
    dplyr::group_by(!!!grps, time) %>%
    # weighted sum of Rt, weighted by daily infections in the case where
    # there are more than one Rt values per group (e.g. when we are combining
    # Rt across 2 variants for example)
    dplyr::summarise(rt.weighted = sum(Rt_tau * infections / sum(infections)))

  out = supercount %>%
    dplyr::left_join(foi, by = join_cols) %>%
    dplyr::left_join(rt_df, by = join_cols) %>%
    dplyr::group_by(!!!grps) %>%
    dplyr::mutate(
      rt.inst = infections / force
    )

  # Deal with various time columns and perform count

  tmp2 = df %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!grps, tidyselect::ends_with("_time")) %>%
    dplyr::select(-tidyselect::where(~ all(is.na(.x))))

  for (col in setdiff(colnames(tmp2), join_cols)) {
    tgts = stringr::str_remove(col, "_time")
    col = as.symbol(col)
    tgt = as.symbol(tgts)

    # Censoring function?
    fn_delay = ~0
    if (!is.null(censoring[[tgts]])) {
      fn_delay = censoring[[tgts]]
    }
    fn_delay = .fn_check(fn_delay)

    tmp3 = df %>%
      dplyr::mutate(t = !!col) %>%
      dplyr::relocate(t) %>%
      # Censoring filter
      dplyr::mutate(.delay = .ts_evaluate(fn_delay, .))

    out2 = dplyr::bind_rows(lapply(max_time, function(mt) {
      tmp3 %>%
        dplyr::filter(t + .delay <= mt) %>%
        dplyr::transmute(!!!grps, time = floor(!!col)) %>%
        dplyr::group_by(!!!grps, time) %>%
        dplyr::summarise(
          .n = dplyr::n()
        ) %>%
        tidyr::complete(
          time = ggoutbreak::as.time_period(0:mt, time),
          fill = list(.n = 0)
        ) %>%
        dplyr::rename(!!tgt := .n) %>%
        dplyr::mutate(obs_time = mt)
    }))

    if ("obs_time" %in% colnames(out)) {
      # from the second iteration we will have observation date columns
      out = out %>% dplyr::left_join(out2, by = c(join_cols, "obs_time"))
    } else {
      out = out %>% dplyr::left_join(out2, by = join_cols)
    }
  }

  if (!"obs_time" %in% colnames(out)) {
    out = out %>% dplyr::mutate(obs_time = max(time))
  }

  out = out %>%
    tidyr::pivot_longer(
      cols = -c(
        !!!grps,
        obs_time,
        time,
        rt.case,
        dispersion,
        rt.weighted,
        force,
        rt.inst
      ),
      names_to = "statistic",
      values_to = "count"
    ) %>%
    dplyr::mutate(count = as.integer(ifelse(is.na(count), 0, count))) %>%
    dplyr::mutate(
      time = ggoutbreak::as.time_period(time, df$time),
      obs_time = ggoutbreak::as.time_period(obs_time, df$time),
    ) %>%
    dplyr::group_by(obs_time, statistic, !!!grps)

  attr(out, "events") = events
  interfacer::ireturn(out, i_sim_count_data)
}


## Internal helper functions ----

#' Generate a smooth inverse function from an empirical probability distribution
#'
#' This is used for random sampling from the infectivity profile for times to
#' infection, when you need to weight the input. There is nothing to stop you
#' putting in a negative serial interval distribution but strange things may
#' happen in your simulation.
#'
#' @param ip the empirical distributions
#'
#' @return a function which will take a quantile and return a value for the mixture
#' @noRd
#'
#' @examples
#' tmp = .ip_quantile_function(covid_ip)(stats::runif(10000))
#'
#' if(interactive()) {
#'   plot_ip(covid_ip, alpha=0.01) +
#'     ggplot2::geom_density(data = tibble::tibble(x=tmp), mapping = ggplot2::aes(x=x), bounds = c(0.5,13.5))
#' }
.ip_quantile_function = function(ip = i_empirical_ip) {
  # Add in defaults for a0 and a1 if they are not defined:
  ip = interfacer::ivalidate(
    ip,
    .imap = interfacer::imapper(a0 = pmax(tau - 0.5, 0), a1 = tau + 0.5)
  )

  tmp = ip %>%
    dplyr::group_by(a1) %>%
    dplyr::summarise(probability = mean(probability)) %>%
    dplyr::mutate(cum = cumsum(probability))

  # if (abs(max(tmp$cum)-1) > .Machine$double.eps^2) stop("input does not seem to be a valid probability distribution")

  left = min(which(tmp$cum > 0))
  right = max(which(tmp$cum < 1))

  x = c(0, tmp$cum[left:right], 1)
  y = c(
    if (left == 1) ip$a0[1] else ip$a1[left - 1],
    # we know that at least 1 value of tmp$cum will be 1 as input is a probability
    # distribution
    ip$a1[left:(right + 1)]
  )

  # sf = stats::splinefun(x=c(0,tmp$cum), y=c(min(ip$a0),tmp$a1), method = "monoH.FC", ties=max)
  sf = stats::approxfun(x = x, y = y)
  return(function(x, ...) sf(x))
}


#' Sampler that produces a poisson or neg binomial
#'
#' @param mu the mean of the rate
#' @param kappa dispersion. scaled such that poisson dispersion is 1. Values
#'   must be 0 (no dispersion) or 1 or greater
#'
#' @return either a NB or a poisson sample for each mu and kappa
#' @noRd
#' @examples
#' .sample_contacts(rep(2.5,1000),1) %>% mean()
#' .sample_contacts(rep(0.5,1000),1) %>% mean()
#' .sample_contacts(rep(0.5,1000),1.5) %>% mean()
#' .sample_contacts(rep(0.05,1000),2) %>% mean()
#' .sample_contacts(rep(2.5,1000),0.5) %>% mean()
#' .sample_contacts(rep(0.3,1000),stats::runif(1000)+1) %>% {stats::sd(.)/mean(.)}
.sample_contacts = function(mu, kappa) {
  interfacer::recycle(mu, kappa)
  if (all(kappa == 0)) {
    return(floor(mu + 0.5))
  }
  # if (any(kappa > 0 & kappa < 1)) stop("underdispesion is not supported (except for zero)")
  if (all(kappa == 1)) {
    return(stats::rpois(length(mu), lambda = mu))
  }
  # scale kappa such that 1 is poisson
  kappa = kappa * sqrt(1 / mu)
  size = mu / (mu * kappa^2 - 1)
  prob = 1 / (mu * kappa^2)
  tmp = dplyr::case_when(
    kappa == 0 ~ floor(mu + 0.5), # Avoid IEC rounding craziness.
    kappa == sqrt(1 / mu) ~ stats::rpois(length(mu), lambda = mu),
    kappa > sqrt(1 / mu) ~
      suppressWarnings(stats::rnbinom(length(mu), size = size, prob = prob)),
    TRUE ~ rdiscgamma(length(mu), mean = mu, kappa = kappa)
  )
  if (any(is.na(tmp))) {
    browser()
  }
  return(tmp)
}


#' Evaluate a function in a timeseries dataframe
#'
#' N.B. only exported as used in one vignette for demo purposes
#'
#' @param fn a function.
#' @param df a dataframe with a numeric time column, plus other columns
#'
#' @return a vector, the result of applying the function to df.
#' @export
#' @keywords internal
#'
#' @examples
#' test = tibble::tibble(
#'   time = 1:10,
#'   class = rep(c("one","two"),5)
#' )
#' .ts_evaluate(\(t,class) {print(t); print(class); 1}, test )
#'
.ts_evaluate = function(fn, df) {
  fn = .fn_check(fn)

  if (!"t" %in% colnames(df)) {
    if (!"time" %in% colnames(df)) {
      stop("input must have a `t` or `time` column")
    }
    df = df %>% dplyr::rename(t = time)
    cols = colnames(df)
  }

  df = df %>% dplyr::relocate(t)

  tmp = do.call(fn, df)
  if (rlang::is_closure(tmp)) {
    stop(
      "A function has evaluated to another function instead of a value. Did you put a tilde before a function generator?"
    )
  }
  if (is.data.frame(tmp)) {
    tmp = list(tmp)
  }

  return(tmp)
}

# internal
# returns a function that inputs an arbitrary list of parameters
# suppressing unused parameter error
# checks inputs against the formal parameters and throws a helpful error
# if they don't match, ignoring defaults at the moment.
# fn = fn = \(a,b,c) a+b+c
# try(.fn_check(fn)(x=1,y=2,z=3))
# .fn_check(fn)(a=1,b=2,c=3,d=4)
# try(.fn_check(fn)(a=1,y=2,z=3))
.fn_check = function(fn) {
  fn = rlang::as_function(fn)
  if ("..." %in% names(formals(fn))) {
    return(fn)
  }
  return(function(...) {
    args = rlang::dots_list(..., .named = NULL)
    fmls = names(formals(fn))
    if (length(fmls) == 0) {
      stop(
        "Function must define at least one parameter, available values are: ",
        paste0(names(args), collapse = ", "),
        call. = FALSE
      )
    }
    # unnamed
    missing = dplyr::setdiff(fmls, names(args))
    j = 1
    for (m in missing) {
      while (j <= length(args) && !is.null(names(args)[[j]])) {
        j = j + 1
      }
      if (j <= length(args)) {
        names(args)[[j]] = m
      } else {
        break
      }
    }
    missing = dplyr::setdiff(fmls, names(args))
    if (length(missing) > 0) {
      stop(
        "Function expects the wrong parameters (",
        paste0(fmls, collapse = ", "),
        "), available values are: ",
        paste0(names(args), collapse = ", "),
        call. = FALSE
      )
    }
    args = args[names(args) %in% fmls]
    tmp = do.call(fn, args)
    len = length(args[[1]])
    if (is.data.frame(tmp)) {
      if (len > 1) tmp = rep(list(tmp), len)
      # if output is a dataframe and expected length is 1 then return unwrapped dataframe
    } else {
      if (length(tmp) == 1) {
        tmp = rep(tmp, length.out = len)
      }
      if (length(tmp) != len) {
        stop(
          "Incompatible length of function output. ",
          length(tmp),
          " should have been ",
          len,
          ". Function must be vectorised over inputs."
        )
      }
    }
    return(tmp)
  })
}

#' Takes an input mapping specification and applies data transofmrs
#' @noRd
#' @examples
#' fn_list = list(
#'   Species = ~ toupper(.x),
#'   Sepal.Width = \(Sepal.Length, Sepal.Width) Sepal.Length+Sepal.Width
#' )
#' fn = .fn_transition(fn_list)
#' fn(iris %>% dplyr::mutate(t=dplyr::row_number()))
.fn_transition = function(fn_list) {
  fn_list2 = lapply(fn_list, rlang::as_function)
  function(df) {
    df2 = df
    for (i in seq_along(fn_list2)) {
      col = names(fn_list2)[i]
      if (!col %in% colnames(df)) {
        stop(
          "Incorrect column name: ",
          col,
          ". Valid columns are: ",
          paste0(colnames(df), collapse = ", "),
          call. = FALSE
        )
      }
      fn = fn_list2[[i]]
      fn = .fn_check(fn)

      if (!"t" %in% colnames(df)) {
        if (!"time" %in% colnames(df)) {
          stop("input must have a `t` or `time` column")
        }
        df2 = df2 %>% dplyr::rename(t = time)
      }

      tmp = do.call(fn, df2 %>% dplyr::relocate(!!col, t))
      if (is.data.frame(tmp)) {
        tmp = list(tmp)
      }

      df[[col]] = tmp
    }
    return(df)
  }
}

# .make_gamma_ip = memoise::memoise(make_gamma_ip)

# deal with potentially empty times parameter]
.rep2 = function(x, times) {
  if (length(times) == 0) {
    return(x[FALSE])
  }
  return(rep(x, times))
}


# default imports dataframe when imports given as a function of time
.fn_to_imports_df = function(fn_imports, max_time, seed = Sys.time()) {
  withr::with_seed(seed, {
    fn_imports = .fn_check(fn_imports)
    imports = fn_imports(0:max_time)
    # Set up the imported infections
    tmp = tibble::tibble(
      time = 0:max_time,
      count = imports,
    ) %>%
      dplyr::filter(!is.na(count) & count > 0)

    return(tmp)
  })
}

.extract_events = function(df, col, time_col, label_fmt) {
  if ("statistic" %in% colnames(df)) {
    df = df %>%
      dplyr::filter(statistic == "infections")
  }

  grps = df %>% dplyr::groups()
  col = rlang::ensym(col)
  time_col = rlang::ensym(time_col)

  events = df %>%
    dplyr::group_by(!!!grps) %>%
    dplyr::mutate(.diff = !!col - dplyr::lag(!!col, default = -Inf)) %>%
    dplyr::mutate(.diff2 = .diff - dplyr::lag(.diff, default = -Inf)) %>%
    dplyr::filter(.diff2 != 0 & .diff != 0) %>%
    dplyr::transmute(
      label = sprintf(label_fmt, !!col),
      start = !!time_col,
      end = NA
    ) %>%
    dplyr::mutate(
      start = as.Date(as.time_period(start, "1 day"))
    )

  return(events)
}


.unnest_dt <- function(tbl, col) {
  copies = rep(1:nrow(tbl), sapply(tbl[[col]], nrow))
  out = tbl
  out[[col]] = NULL
  out = dplyr::slice(out, copies)
  dplyr::bind_cols(
    out,
    dplyr::bind_rows(tbl[[col]])
  )
}
# tbl = iris %>% tidyr::nest(-Species)
#iris %>% tidyr::nest(-Species) %>% .unnest_dt("data") %>% dplyr::glimpse()
