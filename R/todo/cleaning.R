#### smoothing and imputing ----

#' @description Low tech smoothing and missing value imputation, based on a log1p transform and a sgolayfilter to preserve ends of timeseries. This also does anomaly detection and is the one stop shop to minimally clean a timeseries.
#' @param window - the window length for the rolling average
#' @return - the timeseries with an Imputed.value, a RollMean.value column and an Imputed flag.
imputeAndWeeklyAverage = function(covidTimeseries, window=7, ...) {
  if ("RollMean.value" %in% colnames(covidTimeseries)) {
    return(covidTimeseries)
  }
  ts=covidTimeseries
  self$getHashCached(object = covidTimeseries, operation="IMPUTE", ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
    tmp = ts %>%
      covidStandardGrouping() %>%
      self$completeAndRemoveAnomalies(valueVar = value, originalValueVar = value.original) %>%
      dplyr::mutate(
        logValue1 = log(value+1)) %>%
      covidStandardGrouping() %>%
      dplyr::group_modify(function(d,g,...) {
        d = d %>% dplyr::arrange(date)
        if (sum(!is.na(d$logValue1)) < 2) {
          d$logValue1 = rep(NA,length(d$logValue1))
        } else {
          d$logValue1 = forecast::na.interp(d$logValue1)
        }
        if (length(d$logValue1) > window) {
          d$logValue2 = signal::sgolayfilt(d$logValue1,p=1,n=window)
        } else {
          d$logValue2 = rep(NA,length(d$logValue1))
        }
        return(d)
      }) %>%
      dplyr::mutate(
        Imputed.value = ifelse(logValue1 < 0,0,exp(logValue1)-1),
        Imputed = is.na(value) & !is.na(Imputed.value),
        RollMean.value = ifelse(logValue2 < 0,0,exp(logValue2)-1),
        Window.RollMean.value = window
      ) %>%
      dplyr::select(-logValue1, -logValue2) %>%
      dplyr::ungroup()
    #browser(expr = self$debug)
    return(tmp)
  }})
}

#' @description Identify and make NA any days for which the timeseries contains no non zero values
#' @param valueVar - the value to look at
#' @return - the same timeseries with totally missing days replaced with NA
#' TODO: do we want to store the original somewhere?
removeZeroDays = function(r0Timeseries, valueVar = "value") {
  valueVar = rlang::ensym(valueVar)
  r0Timeseries %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(.anyNonZero = any(!!valueVar!=0)) %>%
    dplyr::mutate(!!valueVar := ifelse(.anyNonZero,!!valueVar,NA_real_)) %>%
    dplyr::select(-.anyNonZero) %>%
    return()
}

#' @description Ensure completeness timeseries and remove anomalies. Anomolies are detected as improbable observations based on a an approximate log normal of surrounding obs.
#' @param r0timeseries a time series includeing incidence totals
#' @param smoothExpr an expression to evaluate in the context of the dataframe to generate a variable to be smoothed (e.g. log(value+1))
#' @param ... - not used
#' @param window - the window over which to check anomaly status (defaults to 9)
#' @param p - how unlikely is an oservation before removed.
#' @param min - the minimum SD allowed (to prevent runs of identical values causing issues)
#' @param valueVar - the column to screen
#' @param originalValueVar - the column to save to originals to
#' @param allowZeroDays - whether to identify dates for which there are zero counts across all groups. This helps in the detection of totally missing days.
#' @return an timeseries with Anomaly column
completeAndRemoveAnomalies = function(r0Timeseries, window=9, p = 0.999, min = 1, valueVar = "value", originalValueVar = "value.original", allowZeroDays=FALSE) {covidTimeseriesFormat %def% {

  #TODO: generate more tests for this from manual review of:
  # utils::View(symptomaticTimeseries %>% dplyr::filter(is.na(stats::filter(value,rep(1,9)))))

  valueVar = rlang::ensym(valueVar)
  originalValueVar = rlang::ensym(originalValueVar)
  if ("Anomaly" %in% colnames(r0Timeseries)) {
    # detection already performed
    return(r0Timeseries)
  }
  # trim tailing NAs
  tmp = self$trimNAs(r0Timeseries)
  groups = tmp %>% covidStandardGrouping() %>% dplyr::n_groups()
  if (!allowZeroDays & groups>1) tmp = tmp %>% self$removeZeroDays()

  # the minimum value of lambda is set so that 0 is within the confidence limits.
  # usually this will allow noise at low levels
  minLambda = -log(2*(1-p))

  tmp = tmp %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!originalValueVar := !!valueVar) %>%
    self$tidyr::complete() %>%
    covidStandardGrouping() %>%

    dplyr::group_modify(function(d,g,...) {

      d = d %>% dplyr::mutate(Anomaly = FALSE)

      fn = function(e) {
        y_orig = e %>% dplyr::pull(!!valueVar)
        # #TODO: make this a more generic way of doing windowing & tidyify it
        y = log(y_orig+1)
        i = 1:(length(y))
        w2 = floor(window/2)
        # set anomalous values to NA
        y[d$Anomaly] = NA
        # add in head and tail
        v = c(rep(NA,w2), y, rep(NA,w2))
        # turn time series into matrix with columns for each window
        m = sapply(i,function(j) {v[j:(j+w2*2)]})
        # get rid of central value
        m[w2+1,] = NA

        intercept = function(y) {
          #browser()
          x = (-w2:w2)[!is.na(y)]
          y = y[!is.na(y)]
          n = length(y)
          m = (n*sum(x*y)-sum(x)*sum(y))/(n*sum(x^2)-sum(x)^2)
          c = (sum(y) - m*sum(x))/n

          return(c)
        }

        m_mean = apply(m, 2, intercept) #2 here means apply mean col-wise
        m_sd = pmax(apply(m, 2, sd, na.rm=TRUE),log(min+1)) #2 here means apply mean col-wise

        # m_lambda = pmax(exp(m_mean)-1, minLambda)
        # m_p = 1-m_mean/m_sd^2
        # m_r = m_mean^2/(m_sd^2-m_mean)
        # p_obs = abs(stats::ppois(y_orig, m_lambda)-0.5)*2
        p_obs = abs(stats::pnorm(y, mean = m_mean,sd = m_sd)-0.5)*2
        Anomaly = p_obs > p | !is.finite(p_obs) #& !(m_lambda==0 & y_orig==0)
        # browser()
        return(Anomaly)
      }

      i=1
      repeat {
        newAnomaly = fn(d)
        if (i>5) {
          warning("anomaly detection did not converge")
          break;
        }
        if (all(newAnomaly==d$Anomaly)) break;
        d$Anomaly=newAnomaly
        i=i+1
      }
      # d$Anomaly = fn(d)
      # browser()
      # d$Anomaly = fn(d)
      # #TODO: repeat the anomaly removal stage to detect anything ?

      d = d %>% dplyr::mutate(!!valueVar := ifelse(Anomaly,NA,!!valueVar), Anomaly = Anomaly & !is.na(!!originalValueVar))
      # browser()
      return(d)
    }) %>%
    dplyr::ungroup()
  return(tmp)
}}
