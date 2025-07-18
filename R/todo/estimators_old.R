#' #TODO: OLD
#' # 1) Write lag analysis for estimators.
#' # Simple one for growth rate
#' # N.B. for Rt this will need wallinga et al Rt calculation of Rt using infectivity profile.
#' # 2) default estimator functions combining best practice.
#' # classes for estimates e.g. growthratetimeseries, rttimeseries, poissonratetimeserris, proportions
#' # simple timeseries as S3 class
#' # Multiple time series as S3
#' # 3) plotting functions
#' # consider a new library to hold class definitions and plotting functions
#' # or a new library to hold basic data manipulation and validation functions and a new library for plotting
#' # 4) restructure appendix vignettes to use latex versions from PhD and save figures somewhere.
#' # rename outputs to remove dates
#' # check time-series plots still working / fix timeseries plots
#' # 5) re-architect validation functions
#' # into new package?
#'
#' ensureExists = function(df, column, orElse = function(df) {stop("Missing column: ",column)},...) {ensure_exists(df, column, orElse, ...)}
#'
#' # make sure all the columns exist or report what the problems are
#' checkValid = function(df,columns) {
#'   success = all(sapply(columns, function(colname) {
#'     if(!(colname %in% colnames(df))) {
#'       message("Missing column: ",colname)
#'       return(FALSE)
#'     }
#'     return(TRUE)
#'   }))
#'   if(!success) stop("Invalid dataframe")
#' }
#'
#' # create a weekday and is.weekend column
#' weekdayFromDates = function(df) {
#'   checkValid(df,"date")
#'   df %>% dplyr::mutate(
#'     weekday = ordered(lubridate::wday(date),levels=1:7, labels=c("sun","mon","tue","wed","thur","fri","sat")),
#'     is.weekend = weekday %in% c("sat","sun")
#'   )
#' }
#'
#' #' Calculates a weighting to apply to each day of week
#' #'
#' #' @param simpleTimeseries a COVID-19 timeseries data frame
#' #' @param ...
#' #' @param valueVar the variable with the weekly periodicity
#' #'
#' #' @return the dataframe with a weekday.wt column which says how much that value is over expressed in the data
#' weekendEffect = function(simpleTimeseries, valueVar="value", ...) {
#'   valueVar = rlang::ensym(valueVar)
#'   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#'
#'   simpleTimeseries = simpleTimeseries %>% weekdayFromDates()
#'
#'   # set the default uniform weighting
#'   defaultWt = tibble::tibble(
#'     weekday = ordered(1:7,labels=c("sun","mon","tue","wed","thur","fri","sat")),
#'     weekday.wt = rep(1,7)
#'   )
#'
#'   if(nrow(simpleTimeseries)>=21) {
#'
#'     # if there is enough data estimate how much weight each day should have
#'     weight = simpleTimeseries %>%
#'       dplyr::mutate(.percentBias =
#'               log(!!valueVar+1) /
#'                 slider::slide_dbl(log(!!valueVar+1), .before=3, .after=3,.f = mean, na.rm=TRUE,.complete = TRUE)-1
#'       ) %>%
#'       dplyr::group_by(weekday,.add=TRUE) %>%
#'       dplyr::summarise(
#'         weekday.wt = exp(abs(mean(.percentBias, na.rm=TRUE))),
#'         .groups="drop"
#'       ) %>%
#'       dplyr::mutate(weekday.wt=weekday.wt/mean(weekday.wt, na.rm=TRUE))
#'
#'     if(nrow(weight) !=7 | any(is.na(weight$weekday.wt))) {
#'       weight = defaultWt
#'     }
#'
#'   } else {
#'     weight = defaultWt
#'   }
#'
#'   simpleTimeseries %>% dplyr::inner_join(weight, by="weekday") %>% return()
#' }
#'
#' ## `Locfit` estimate outputs ----
#'
#'
#' # This is just to format locfit results given a locfit model.
#' # extract the locfit result from the locfit model and format it
#' locfitExtractResult = function(df, model, estimate, modelName, link = "value") {
#'
#'   tryCatch({
#'
#'     points = stats::preplot(model,where = "fitp",se.fit = TRUE,band="local")
#'
#'     t = points$tr
#'     fit = points$fit
#'     se.fit = tryCatch({
#'       forecast::na.interp(points$se.fit)
#'     }, error = function(e) {
#'       rep(NA,length(fit))
#'     })
#'
#'     df %>% formatResult(fit,se.fit,t,estimate,modelName,link)
#'
#'   }, error = function(e) {
#'
#'     df %>% nullResult(estimate,modelName,link,error = e$message)
#'
#'   })
#' }
#'
#' # or else NA
#' opt = function(expr) tryCatch(expr,error=function(e) NA_real_)
#'
#' # format a transformed normally distributed variable into quantiles
#' formatResult = function(df, fit, se.fit, t, estimate, modelName,link) {
#'   df %>% dplyr::mutate(
#'     !!(paste0(estimate,".",link)) := fit,
#'     !!(paste0(estimate,".SE.",link)) := se.fit,
#'     !!(paste0(estimate,".Quantile.0.025")) := opt(t(stats::qnorm(0.025,fit,se.fit))),
#'     !!(paste0(estimate,".Quantile.0.05")) := opt(t(stats::qnorm(0.05,fit,se.fit))),
#'     !!(paste0(estimate,".Quantile.0.25")) := opt(t(stats::qnorm(0.25,fit,se.fit))),
#'     !!(paste0(estimate,".Quantile.0.5")) := t(fit),
#'     !!(paste0(estimate,".Quantile.0.75")) := opt(t(stats::qnorm(0.75,fit,se.fit))),
#'     !!(paste0(estimate,".Quantile.0.95")) := opt(t(stats::qnorm(0.95,fit,se.fit))),
#'     !!(paste0(estimate,".Quantile.0.975")) := opt(t(stats::qnorm(0.975,fit,se.fit))),
#'     !!(paste0(estimate,".model")) := modelName)
#' }
#'
#' # extract the locfit result from the locfit model and format it
#' nullResult = function(df, estimate, modelName, link = "value", error = "unknown error", centralValue = 0) {
#'   df %>% formatResult(fit = centralValue, se.fit=NA_real_, t=function(x) x, estimate, modelName, link) %>%
#'     dplyr::mutate(
#'       !!(paste0(estimate,".error")) := error
#'     )
#' }
#'
#'
#' #' Rename a growth rate estimate by placing a prefix in front o
#' #'
#' #' @param df the datafram with the Growth rate, possion rate, R_t or proportion estimates
#' #' @param prefix the prefix to add
#' #' @param estimates which estimates to rename (defaults to all of "Growth","Est","Proportion" and "Rt")
#' #'
#' #' @return the dataframe with the columns renamed
#' #' @export
#' renameResult = function(df, prefix, estimates = c("Growth","Est","Proportion","Rt","doublingTime")) {
#'   for (estimate in estimates) {
#'     df = df %>% dplyr::rename_with(.cols = tidyselect::starts_with("Growth"), .fn = ~ paste0(prefix,".",.x))
#'   }
#' }
#'
#' ## Locfit estimators ----
#'
#' # Generate the formula for a locfit model based on things I understand
#' locfitFormula = function(valueVar, nrowDf, window, polynomialDegree, nearestNeighbours = TRUE, ...) {
#'   valueVar=rlang::ensym(valueVar)
#'   tmp_alpha = min(window/nrowDf,1)
#'   tmp_alpha_2 = min((window*2+1)/nrowDf,1)
#'   lpParams = list(
#'     nn = if( nearestNeighbours ) tmp_alpha_2 else tmp_alpha, # this is given in fraction of total observations
#'     h = if( !nearestNeighbours ) window else 0, # this is given in units of X
#'     deg = polynomialDegree
#'   )
#'   lpParamsText = paste(names(lpParams),lpParams,sep="=",collapse=", ")
#'   lpFormula = stats::as.formula(paste0(rlang::as_label(valueVar), " ~ locfit::lp(time, ",lpParamsText,")"))
#'   return(lpFormula)
#' }
#'
#' #' Generate a smoothed estimate of the proportion of cases compared to some total.
#' #'
#' #' @param simpleTimeseries - a minimal time-series including date, value, and if available total. If total is present the proportion is value/total. otherwise it is value.
#' #' @param degree the polynomial degree
#' #' @param window the data window in days
#' #' @param estimateMean there is no closed form estimate of the mean of a logit transformed normal. it can be calculated by integration by this is relatively expensive and not done unless explicitly needed,
#' #' @param ... may include "nearestNeigbour=FALSE" to disable the tail behaviour of locfit
#' #'
#' #' @return a timeseries with binomial proportion estimates (columns starting with "Proportion")
#' #' @export
#' locfitProportionEstimate = function(simpleTimeseries, degree = 2, window = 14, estimateMean = FALSE,... ) { #, weightBySampleSize = FALSE, weightByWeekday = FALSE, ...) {
#'   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#'
#'   simpleTimeseries %>% checkValid(c("date","value"))
#'   simpleTimeseries = simpleTimeseries %>%
#'     dplyr::arrange(date) %>%
#'     ensureExists("total", orElse = function(ts,...) ts %>% dplyr::mutate(total=1)) %>%
#'     #ensureExists("weekday.wt", orElse = function(ts,...) ts %>% weekendEffect(valueVar=total)) %>%
#'     #ensureExists("sample.wt", orElse = function(ts,...) ts %>% sampleSizeEffect(window=window, sampleSizeVar=total)) %>%
#'     ensureExists("time", orElse = function(ts,...) ts %>% dplyr::mutate(time = as.integer(date-max(date)))) %>%
#'     dplyr::mutate(.prop = ifelse(total==0,NA,value/total))
#'
#'   if(any(simpleTimeseries$.prop > 1,na.rm = TRUE)) stop("Proportions model has values greater than 1. Did you specify total column correctly?")
#'
#'   if(sum(stats::na.omit(simpleTimeseries$.prop) != 0) < degree) {
#'     return(simpleTimeseries %>% nullResult(estimate = "Proportion", modelName = glue::glue("binomial:{degree}:{window}"), link = "logit", error = "not enough non zero values", centralValue = 0))
#'   }
#'
#'   if(sum(stats::na.omit(simpleTimeseries$.prop) != 1) < degree) {
#'     return(simpleTimeseries %>% nullResult(estimate = "Proportion", modelName = glue::glue("binomial:{degree}:{window}"), link = "logit", error = "not enough non unitary values", centralValue = 1))
#'   }
#'
#'   # simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = 1)
#'   # if(weightBySampleSize) simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = fit.wt*sample.wt)
#'   # if(weightByWeekday) simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = fit.wt*weekday.wt)
#'   #
#'   # if(weightBySampleSize) {
#'   #   simpleTimeseries = simpleTimeseries %>% dplyr::select(-.prop) %>% dplyr::group_by_all() %>% dplyr::summarise(
#'   #     .prop = c(rep(1,value),rep(1,total-value))
#'   #   )
#'   # }
#'
#'   tryCatch({
#'     model = locfit::locfit(
#'       locfitFormula(.prop, nrowDf = nrow(simpleTimeseries), window, degree, ...),
#'       # weights = fit.wt,
#'       data=simpleTimeseries,
#'       family="qbinomial",
#'       link="logit",
#'       ev=simpleTimeseries$time
#'     )}, error=function(e) browser()
#'   )
#'
#'   # weightLbl = dplyr::case_when(
#'   #   weightBySampleSize & weightByWeekday ~ "both",
#'   #   weightBySampleSize ~ "sample",
#'   #   weightByWeekday ~ "weekday",
#'   #   TRUE ~ "none"
#'   # )
#'
#'   weightLbl = "none"
#'
#'   simpleTimeseries = simpleTimeseries %>%
#'     locfitExtractResult(model, estimate = "Proportion", modelName = glue::glue("binomial:{degree}:{window}:{weightLbl}"), link = "logit") %>%
#'     dplyr::select(-.prop)
#'
#'   if (estimateMean) {
#'     simpleTimeseries = simpleTimeseries %>%
#'       dplyr::mutate(
#'         Proportion.value = dplyr::map2_dbl(Proportion.logit, Proportion.SE.logit, .f = ~ ifelse(is.na(.y),.x,logitnorm::momentsLogitnorm(.x,.y)[["mean"]])) #();NA_real_))
#'       )
#'   }
#'
#'   return(simpleTimeseries)
#' }
#'
#' #' Generate a smoothed estimate of the relative growth rate of cases compared to some baseline using proportions.
#' #'
#' #' @param simpleTimeseries - a minimal time-series including date, value, and if available total. If total is present the proportion is value/total. otherwise it is value, and total is assumed to be 1.
#' #' @param degree the polynomial degree
#' #' @param window the data window in days
#' #' @param ... may include "nearestNeigbour=FALSE" to disable the tail behaviour of locfit
#' #'
#' #' @return a timeseries with growth rate estimates (columns starting with "Growth")
#' #' @export
#' locfitProportionalGrowthEstimate = function(simpleTimeseries, degree = 2, window = 14, ...) { #}, weightBySampleSize = FALSE, weightByWeekday = FALSE, ...) {
#'   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#'
#'   simpleTimeseries %>% checkValid(c("date","value"))
#'   simpleTimeseries = simpleTimeseries %>%
#'     dplyr::arrange(date) %>%
#'     ensureExists("total", orElse = function(ts,...) {
#'       message("No total column in proportional timeseries - assuming value is a fraction, and total is 1.")
#'       ts %>% dplyr::mutate(total=1)
#'     }) %>%
#'     # ensureExists("weekday.wt", orElse = function(ts,...) ts %>% weekendEffect(valueVar=total)) %>%
#'     # ensureExists("sample.wt", orElse = function(ts,...) ts %>% sampleSizeEffect(window=window, sampleSizeVar=total)) %>%
#'     ensureExists("time", orElse = function(ts,...) ts %>% dplyr::mutate(time = as.integer(date-max(date)))) %>%
#'     dplyr::mutate(.prop = ifelse(total==0,NA,value/total))
#'
#'   if(any(simpleTimeseries$.prop > 1,na.rm = TRUE)) stop("Proportions model contains fractions greater than 1. Did you specify total column correctly?")
#'
#'   if(sum(stats::na.omit(simpleTimeseries$.prop) != 0) < degree) {
#'     return(simpleTimeseries %>% nullResult(estimate = "Growth", modelName = glue::glue("binomial:{degree}:{window}"), link = "value",error = "not enough non zero values", centralValue = 0))
#'   }
#'
#'   if(sum(stats::na.omit(simpleTimeseries$.prop) != 1) < degree) {
#'     return(simpleTimeseries %>% nullResult(estimate = "Proportion", modelName = glue::glue("binomial:{degree}:{window}"), link = "value", error = "not enough non unitary values", centralValue = 0))
#'   }
#'
#'   # simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = 1)
#'   # if(weightBySampleSize) simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = fit.wt*sample.wt)
#'   # if(weightByWeekday) simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = fit.wt*weekday.wt)
#'
#'   model = locfit::locfit(
#'     locfitFormula(.prop, nrowDf = nrow(simpleTimeseries), window, degree, ...),
#'     # weights = fit.wt,
#'     data=simpleTimeseries,
#'     family="qbinomial",
#'     link="logit",
#'     deriv=1,
#'     ev=simpleTimeseries$time
#'   )
#'
#'   # weightLbl = dplyr::case_when(
#'   #   weightBySampleSize & weightByWeekday ~ "both",
#'   #   weightBySampleSize ~ "sample",
#'   #   weightByWeekday ~ "weekday",
#'   #   TRUE ~ "none"
#'   # )
#'
#'   weightLbl = "none"
#'
#'   # no link function in growth rate as the derivative
#'   simpleTimeseries %>%
#'     locfitExtractResult(model = model, estimate = "Growth", modelName = glue::glue("binomial:{degree}:{window}:{weightLbl}"), link = "value") %>%
#'     dplyr::select(-.prop)
#' }
#'
#' #' Generate a smoothed estimate of the absolute growth rate of cases using a poisson model.
#' #'
#' #' @param simpleTimeseries - a minimal time-series including date, value, and if available total. If total is present the proportion is value/total. otherwise it is value.
#' #' @param degree the polynomial degree
#' #' @param window the data window in days
#' #' @param ... may include "nearestNeigbour=FALSE" to disable the tail behaviour of locfit
#' #'
#' #' @return a timeseries with poisson rate estimates (columns starting with "Est")
#' #' @export
#' locfitPoissonRateEstimate = function(simpleTimeseries, degree = 2, window = 14, weightByWeekday = FALSE, ...) {
#'   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#'
#'   simpleTimeseries %>% checkValid(c("date","value"))
#'   simpleTimeseries = simpleTimeseries %>%
#'     dplyr::arrange(date) %>%
#'     ensureExists("weekday.wt", orElse = function(ts,...) ts %>% weekendEffect(valueVar=value)) %>%
#'     ensureExists("time", orElse = function(ts,...) ts %>% dplyr::mutate(time = as.integer(date-max(date)))) %>%
#'     dplyr::mutate(.prop = value)
#'
#'   if(sum(stats::na.omit(simpleTimeseries$.prop) != 0) < degree) {
#'     return(simpleTimeseries %>% nullResult(estimate = "Est", modelName = glue::glue("poisson:{degree}:{window}"), link = "log",error = "not enough non zero values", centralValue = 0))
#'   }
#'
#'   simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = 1)
#'   if(weightByWeekday) simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = fit.wt*weekday.wt)
#'
#'   model = locfit::locfit(
#'     locfitFormula(.prop, nrowDf = nrow(simpleTimeseries), window, degree, ...),
#'     weights = fit.wt,
#'     data=simpleTimeseries,
#'     family="qpoisson",
#'     link="log",
#'     ev=simpleTimeseries$time
#'   )
#'
#'   weightLbl = dplyr::case_when(
#'     weightByWeekday ~ "weekday",
#'     TRUE ~ "none"
#'   )
#'
#'   # no link function in growth rate as the derivative
#'   simpleTimeseries %>%
#'     locfitExtractResult(model, estimate = "Est", modelName = glue::glue("poisson:{degree}:{window}:{weightLbl}"), link="log") %>%
#'     dplyr::select(-.prop)
#' }
#'
#' #' Generate a smoothed estimate of the absolute growth rate of cases using a poisson model.
#' #'
#' #' @param simpleTimeseries - a minimal time-series including date, value, and if available total. If total is present the proportion is value/total. otherwise it is value.
#' #' @param degree the polynomial degree
#' #' @param window the data window in days
#' #' @param ... may include "nearestNeigbour=FALSE" to disable the tail behaviour of locfit
#' #'
#' #' @return a timeseries with growth rate estimates (columns starting with "Growth")
#' #' @export
#' locfitGrowthEstimate = function(simpleTimeseries, degree = 2, window = 14, weightByWeekday = FALSE, ...) {
#'   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#'
#'   simpleTimeseries %>% checkValid(c("date","value"))
#'   simpleTimeseries = simpleTimeseries %>%
#'     dplyr::arrange(date) %>%
#'     ensureExists("weekday.wt", orElse = function(ts,...) weekendEffect(ts,valueVar=value)) %>%
#'     ensureExists("time", orElse = function(ts,...) dplyr::mutate(ts, time = as.integer(date-max(date)))) %>%
#'     dplyr::mutate(.prop = value)
#'
#'   if(sum(stats::na.omit(simpleTimeseries$.prop) != 0) < degree) {
#'     return(simpleTimeseries %>% nullResult(estimate = "Growth", modelName = glue::glue("poisson:{degree}:{window}"), link = "value",error = "not enough non zero values", centralValue = 0))
#'   }
#'
#'   simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = 1)
#'   if(weightByWeekday) simpleTimeseries = simpleTimeseries %>% dplyr::mutate(fit.wt = fit.wt*weekday.wt)
#'
#'   model = locfit::locfit(
#'     locfitFormula(.prop, nrowDf = nrow(simpleTimeseries), window, degree, ...),
#'     weights = fit.wt,
#'     data=simpleTimeseries,
#'     family="qpoisson",
#'     link="log",
#'     deriv=1,
#'     ev=simpleTimeseries$time
#'   )
#'
#'   weightLbl = dplyr::case_when(
#'     weightByWeekday ~ "weekday",
#'     TRUE ~ "none"
#'   )
#'
#'   # no link function in growth rate as the derivative
#'   simpleTimeseries %>%
#'     locfitExtractResult(model = model, estimate = "Growth", modelName = glue::glue("poisson:{degree}:{window}:{weightLbl}"), link = "value") %>%
#'     #TODO: more statistics here?
#'     dplyr::select(-.prop)
#' }
#'
#'
#' #' Calucualte a doubling time with quantiles for any timeseries with Growth rate estimates
#' #'
#' #' @param simpleTimeseries
#' #'
#' #' @return  a timeseries with doubling time estimates (columns starting with "doublingTime")
#' #' @export
#' doublingTimeFromGrowthRate = function(simpleTimeseries) {
#'   reorder = function(x) (1-(stringr::str_extract(x,"[0-9]\\.[0-9]+") %>% as.numeric())) %>% sprintf(fmt="doublingTime.Quantile.%1.3g")
#'   simpleTimeseries %>% dplyr::mutate(across(.cols = tidyselect::starts_with("Growth.Quantile"), .fns = ~ log(2)/.x, .names = "{stats::reorder(.col)}"))
#' }
#'
#'
#' #' Calculate a reproduction number estimate using the Wallinga 2007 estimation using empirical generation time distribution. This uses resampling to transmit uncertainty in growth rate estimates
#' #'
#' #' @param simpleTimeseries - With a "Growth" estimate as a normally distributed quantility
#' #' @param yMatrix - the matrix of possible infectivity profiles as discrete distributions
#' #' @param aVector - the upper boundaries of the time cut-offs for the infectivity profiles
#' #' @param bootstraps - the number of bootstraps to take to calculate for each point.
#' #' @param quantiles - quantiles to calculate.
#' #' @import logitnorm
#' #'
#' #' @return a timeseries with "Rt" estimates
#' #' @export
#' rtFromGrowthRate = function(simpleTimeseries, infectivityProfile, yMatrix = infectivityProfile$yMatrix, aVector = infectivityProfile$aVector, bootstraps = 20*dim(yMatrix)[2], quantiles = c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)) {
#'   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#'
#'   simpleTimeseries %>% checkValid(c("date","value","Growth.value","Growth.SE.value"))
#'
#'   # grab the y matrix from the list column
#'   y_cols = matrix(yMatrix)
#'   a = aVector
#'   # figure out how many bootstraps we need:
#'   bootsPerInf = max(c(bootstraps %/% dim(yMatrix)[2],1))
#'   # lose the zero values in y and a, if present (which they will be):
#'   if (a[1]==0) {
#'     y_cols = matrix(y_cols[-1,])
#'     a = a[-1]
#'   }
#'   # get the infectivity profiles as a list of vectors, each bootstrap profile will be a vector.
#'   ys = asplit(y_cols, MARGIN=2)
#'
#'   d3 = simpleTimeseries %>% dplyr::mutate(R = dplyr::map2(Growth.value, Growth.SE.value, function(mean_r,sd_r) {
#'
#'     #r_samples = stats::rnorm(bootsPerInf*length(ys),mean_r,sd_r)
#'     qnts = seq(0,1,length.out = bootsPerInf)[2:(bootsPerInf-1)]
#'     r_samples = stats::qnorm(p=qnts,mean_r,sd_r)
#'     rs = asplit(matrix(r_samples,nrow=length(ys)), MARGIN=1)
#'     # browser()
#'     out = dplyr::map2(rs,ys,function(r10,y) {
#'       # browser()
#'       R10 = sapply(r10, function(r) {
#'         # browser()
#'         R = r/sum(y*(exp(-r*dplyr::lag(a,default=0))-exp(-r*a))/(a - dplyr::lag(a,default=0)))
#'       })
#'     })
#'     R_out = as.vector(sapply(out,c))
#'     R_q = stats::quantile(R_out, quantiles)
#'     names(R_q) = paste0("Rt.Quantile.",quantiles)
#'     R_summ = tibble::enframe(R_q) %>% tidyr::pivot_wider() %>% dplyr::mutate(Rt.value = mean(R_out), Rt.SE.value = stats::sd(R_out))
#'     return(R_summ)
#'   }))
#'
#'   return(d3 %>% tidyr::unnest(R) %>% dplyr::mutate(Rt.model = "wallinga:growth-rate"))
#' }
#'
#' ## Manchester growth rate ----
#' # ## Adapted from code
#' # Copyright (c) 2020 Ian Hall
#' # See LICENCE for licensing information
#'
#' # Growth rate estimates for confirmed cases in Europe and for different metrics in Italy using GAM
#' # Figure 1 (main text) and figures S1 and S2 (electronic supplementary material) of:
#' #
#' # Pellis L, Scarabel F, Stage HB, Overton CE, Chappell LHK, Fearon E, Bennett E,
#' # University of Manchester COVID-19 Modelling Group, Lythgoe KA, House TA and Hall I,
#' # "Challenges in control of COVID-19: short doubling time and long delay to effect of interventions",
#' # Philosophical Transactions of the Royal Society B (2021)
#' #
#' # gamGrowthEstimate = function(simpleTimeseries, meth="GCV.Cp", FE='WD'){
#' #   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#' #
#' #   simpleTimeseries %>% checkValid(c("date","value"))
#' #   simpleTimeseries = simpleTimeseries %>%
#' #     dplyr::arrange(date) %>%
#' #     ensureExists("time", orElse = function(ts,...) ts %>% dplyr::mutate(time = as.integer(date-max(date)))) %>%
#' #     dplyr::mutate(.incidence = value)
#' #
#' #   #res <- data.frame(sdt=rep(0,npts),sdtup=rep(0,npts),sdtlow=rep(0,npts),doub=rep(0,npts),doubup=rep(0,npts),doublow=rep(0,npts))
#' #   #Tv <- timev
#' #
#' #   if(FE=='None') {
#' #     MGAM <- mcgv::gam(.incidence ~ mcgv::s(time), data = simpleTimeseries, family=quasipoisson, method=meth)
#' #   } else {
#' #     simpleTimeseries = simpleTimeseries %>%
#' #       ensureExists("weekday", orElse = function(ts,...) ts %>% weekendEffect(valueVar=value)) %>%
#' #       ensureExists("is.weekend", orElse = function(ts,...) ts %>% weekendEffect(valueVar=value))
#' #     if(FE=='WE'){
#' #       MGAM <- mcgv::gam(.incidence ~ mcgv::s(time)+is.weekend, data = simpleTimeseries, family=quasipoisson, method=meth)
#' #     } else {
#' #       MGAM <- mcgv::gam(.incidence ~ mcgv::s(time)+weekday, data = simpleTimeseries, family=quasipoisson, method=meth)
#' #     }
#' #   }
#' #
#' #   X0 <- stats::predict(MGAM, simpleTimeseries %>% dplyr::mutate(time=time-eps), type="lpmatrix")
#' #   eps <- 1e-7 ## finite difference interval
#' #   X1 <- stats::predict(MGAM, simpleTimeseries %>% dplyr::mutate(time=time+eps),type="lpmatrix")
#' #   Xp <- (X1-X0)/(2*eps) ## maps coefficients to (fd approx.) derivatives
#' #   # something to do with extracting the coefficients
#' #   off <- ifelse(FE=='None',1,ifelse(FE=='WE',2,7))
#' #   Xi <- Xp*0
#' #   Xi[,1:9+off] <- Xp[,1:9+off] ## weekend Xi%*%stats::coef(MGAM) = smooth deriv i
#' #   df <- Xi%*%stats::coef(MGAM)              ## ith smooth derivative
#' #   df.sd <- rowSums(Xi%*%MGAM$Vp*Xi)^.5 ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
#' #   ## derivative calculation, pers comm S. N. Wood, found in mgcv:  Mixed  GAM  Computation  Vehicle  with  automatic  smoothness  estimation.  R  packageversion 1.8-31 (2019) https://CRAN.R-project.org/package=mgcv.
#' #
#' #   simpleTimeseries %>% formatResult(fit = df, se.fit = df.sd,t = function(x) x, estimate = "Growth", modelName = glue::glue("poisson:gam-{meth}:{FE}"), link = "value")
#' #
#' # }
#'
#' ## Point estimators ----
#'
#'
#' #' Calculate a slightly more robust estimate of growth rate and proportion based on a single model at a range of
#' #'
#' #' @param simpleTimeseries - the timeseries containing date, value and total
#' #' @param dates - dates at which to evaluate the model
#' #' @param window - the window of data
#' #' @param weekly - either "weekday","weekend" or "none" to define whether to fit a fixed effect model to the weekday, or the is.weekend
#' #' @param includeModel - keep the fitted model as a list column for fit analysis
#' #' @param ...
#' #'
#' #' @return a dataframe of evaluation dates, growth rates, proportions and model fit
#' #' @export
#' pointProportionEstimate = function(simpleTimeseries, dates = max(simpleTimeseries$date)-3, window = 14, weekly = "weekday", includeModel = TRUE,...) {
#'   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#'
#'   if (find.package("logitnorm", quiet = TRUE) %>% length %>% magrittr::equals(0)) {
#'     message("Installing logitnorm needed for analyses")
#'     utils::install.packages("logitnorm", repos = "https://cloud.r-project.org")
#'   }
#'
#'   predictDates = as.Date(dates)
#'
#'   simpleTimeseries %>% checkValid(c("date","value"))
#'   simpleTimeseries = simpleTimeseries %>%
#'     dplyr::arrange(date) %>%
#'     ensureExists("total", orElse = function(ts,...) ts %>% dplyr::mutate(total=1)) %>%
#'     ensureExists("time", orElse = function(ts,...) ts %>% dplyr::mutate(time = as.integer(date-max(date)))) %>%
#'     ensureExists(c("weekday","is.weekend"), orElse = function(ts,...) ts %>% weekdayFromDates()) %>%
#'     dplyr::mutate(.prop = ifelse(total==0,NA,value/total))
#'
#'   if(any(simpleTimeseries$.prop > 1,na.rm = TRUE)) stop("Proportions model has values greater than 1. Did you specify total column correctly?")
#'
#'   if (weekly=="weekday") {
#'     modelFormula = .prop ~ time + weekday
#'   } else if (weekly=="weekend") {
#'     modelFormula = .prop ~ time + is.weekend
#'   } else {
#'     modelFormula = .prop ~ time
#'   }
#'
#'   dplyr::bind_rows(lapply(predictDates, function(predictDate) {
#'
#'     dateMin = as.Date(predictDate)-floor(window/2)
#'     dateMax = as.Date(predictDate)+floor(window/2)
#'
#'     suppressWarnings({
#'       model = stats::glm(
#'         modelFormula,
#'         data=simpleTimeseries %>% dplyr::filter(date >= dateMin & date <= dateMax) %>% dplyr::mutate(sample.wt = total/mean(total,na.rm=TRUE)),
#'         family="binomial",
#'         weights=sample.wt
#'       )
#'     })
#'
#'     predictAt = tibble::tibble(
#'       date = predictDate,
#'       time = as.integer(date-max(simpleTimeseries$date)),
#'     ) %>% weekdayFromDates()
#'
#'     predicted = stats::predict(model,newdata = predictAt,se.fit = TRUE, type="link")
#'     linkFn = stats::family(model)$linkinv
#'
#'     predictAt = formatResult(predictAt, unname(predicted$fit), unname(predicted$se.fit), linkFn, "Proportion", "glm", "logit")
#'     predictAt = predictAt %>% dplyr::mutate(
#'       Proportion.value = dplyr::map2_dbl(Proportion.logit, Proportion.SE.logit, .f = ~ logitnorm::momentsLogitnorm(.x, .y)[["mean"]])
#'     )
#'     gr = summary(model)$coefficients["time",]
#'     predictAt = formatResult(predictAt, gr[[1]], gr[[2]], function(x) x, "Growth", "glm", "value")
#'
#'     if(includeModel) predictAt %>% dplyr::mutate(fit = list(model))
#'
#'   }))
#'
#' }
#'
#' #' Calculate a slightly more robust estimate of growth rate and  poisson rate based on a single model
#' #'
#' #' @param simpleTimeseries - the timeseries containing date and value
#' #' @param dates - dates at which to evaluate the model
#' #' @param window - the window of data
#' #' @param weekly - either "weekday","weekend" or "none" to define whether to fit a fixed effect model to the weekday, or the is.weekend
#' #' @param includeModel - keep the fitted model as a list column for fit analysis
#' #' @param ...
#' #'
#' #' @return a dataframe of evaluation dates, growth rates, poisson rates and model fit
#' #' @export
#' pointPoissonEstimate = function(simpleTimeseries, dates, window, weekly = "weekday", includeModel = TRUE,...) {
#'   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#'
#'   predictDates = as.Date(dates)
#'
#'   simpleTimeseries %>% checkValid(c("date","value"))
#'   simpleTimeseries = simpleTimeseries %>%
#'     dplyr::arrange(date) %>%
#'     ensureExists("time", orElse = function(ts,...) ts %>% dplyr::mutate(time = as.integer(date-max(date)))) %>%
#'     ensureExists(c("weekday","is.weekend"), orElse = function(ts,...) ts %>% weekdayFromDates()) %>%
#'     dplyr::mutate(.prop = value)
#'
#'   if (weekly=="weekday") {
#'     modelFormula = .prop ~ time + weekday
#'   } else if (weekly=="weekend") {
#'     modelFormula = .prop ~ time + is.weekend
#'   } else {
#'     modelFormula = .prop ~ time
#'   }
#'
#'   dplyr::bind_rows(lapply(predictDates, function(predictDate) {
#'
#'     dateMin = as.Date(predictDate)-floor(window/2)
#'     dateMax = as.Date(predictDate)+floor(window/2)
#'
#'     model = stats::glm(
#'       modelFormula,
#'       data=simpleTimeseries %>% dplyr::filter(date >= dateMin & date <= dateMax),
#'       family="poisson"
#'     )
#'
#'     predictAt = tibble::tibble(
#'       date = predictDate,
#'       time = as.integer(date-max(simpleTimeseries$date)),
#'     ) %>% weekdayFromDates()
#'
#'     predicted = stats::predict(model,newdata = predictAt,se.fit = TRUE, type="link")
#'     linkFn = stats::family(model)$linkinv
#'
#'     predictAt = formatResult(predictAt, unname(predicted$fit), unname(predicted$se.fit), linkFn, "Est", "glm", "log")
#'     gr = summary(model)$coefficients["time",]
#'     predictAt = formatResult(predictAt, gr[[1]], gr[[2]], function(x) x, "Growth", "glm", "value")
#'
#'     if(includeModel) predictAt %>% dplyr::mutate(fit = list(model))
#'   }))
#'
#' }
#'
#'
#' ## EpiEstim wrapper ----
#' #' Minimal epiestim wrapper to execute a time series R_t using a discrete infectivity profile matrix, and format the result to be consistent with the rest of this..
#' epiestimRtEstimate = function(simpleTimeseries, yMatrix, bootstraps = 10*dim(yMatrix)[2], window = 14) {
#'   if (simpleTimeseries %>% dplyr::is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#'
#'   siConfig = EpiEstim::make_config(method = "si_from_sample")
#'   tmp = simpleTimeseries %>% dplyr::select(dates=date,I=value)
#'
#'   simpleTimeseries = simpleTimeseries %>% dplyr::mutate(seq_id=dplyr::row_number())
#'   bootsPerInf = max(c(bootstraps %/% dim(yMatrix)[2],1))
#'
#'   siConfig$t_start = c(2:(nrow(tmp)-window))
#'   siConfig$t_end = siConfig$t_start+window
#'   siConfig$n2 = bootsPerInf
#'
#'   warn = NA
#'
#'   tmp4 =
#'     withCallingHandlers(
#'       tryCatch(EpiEstim::estimate_R(tmp, method = "si_from_sample",config=siConfig,si_sample = yMatrix), error = stop), warning= function(w) {
#'         warn <<- w$message
#'         invokeRestart("muffleWarning")
#'       })
#'   tmp5 = tmp4$R %>% dplyr::mutate(seq_id=t_end, errors=NA, `Rt.window`=window) #warn)
#'   tmp5 = tmp5 %>% dplyr::rename_with(.cols = tidyselect::contains("(R)"),.fn=function(x) paste0("Rt.",stringr::str_remove(x,fixed("(R)")))) %>%
#'     dplyr::rename(`Rt.Quantile.0.5` = Rt.Median)
#'   tmp6 = simpleTimeseries %>% dplyr::left_join(tmp5, by="seq_id")
#'   return(tmp6 %>% dplyr::select(-seq_id))
#'
#' }
#'
#' # plotProportionEstimate = function(simpleTimeseries, mapping = ggplot2::aes(), ...) {
#' #
#' #   simpleTimeseries = simpleTimeseries %>% ensureExists("Proportion.Quantile.0.5", orElse = estimateProportions(simpleTimeseries,...))
#' #   # We are going to pretend there is just one
#' #   simpleTimeseries
#' #   tmp2 = tmp %>% dplyr::filter(date <= max(date)-1) %>% dplyr::mutate(
#' #     binom::binom.confint(Negative,n,method="wilson")
#' #   )
#' #
#' #   ggplot2::ggplot(estimate,ggplot2::aes(x=date,y=fit,ymin=lo,ymax=hi))+ggplot2::geom_ribbon(alpha=0.3)+ggplot2::geom_line(colour="blue")+
#' #     ggplot2::geom_point(data=tmp2,mapping=ggplot2::aes(x=date,y=mean),inherit.aes = FALSE)+
#' #     ggplot2::geom_errorbar(data=tmp2,mapping=ggplot2::aes(x=date,ymin=lower,ymax=upper),inherit.aes = FALSE)+
#' #     ggplot2::scale_y_continuous(trans = "logit")
#' # }
#'
#'
#' ## Weekly wrappers - doubled up with jepidemic
#'
#' # takes a line list of patient admissions and estimates weekly rates based on
#' # a quasi-poisson model fitted to count data using local regression.
#' # expects admissions to contain admission_week columns only defining the date of admission
#' estimateWeeklyRate = function(admissions, ... ,nn=0.2,deg=2) {
#'   admissionCounts = admissions %>% dplyr::group_by(admission_week) %>% dplyr::count()
#'   fit = locfit::locfit(n~locfit::lp(admission_week,nn=nn,deg=deg),data = admissionCounts,family="qpoisson")
#'   weeks = seq(min(admissionCounts$admission_week),max(admissionCounts$admission_week),by = 1/7)
#'   tmp = stats::preplot(fit,newdata=weeks,se.fit = TRUE,band="local")
#'   t = tmp$tr
#'   tibble::tibble(
#'     admission_week = weeks,
#'     admission_date = .weeks_to_date(weeks),
#'     lower = .opt(t(stats::qnorm(0.05,tmp$fit,tmp$se.fit))),
#'     median = t(stats::qnorm(0.5,tmp$fit,tmp$se.fit)),
#'     upper = .opt(t(stats::qnorm(0.95,tmp$fit,tmp$se.fit)))
#'   )
#' }
#'
#' ## Date utility ----
#'
#' # guess the intervals between dates
#' .day_interval = function(dates) {
#'   dates = sort(unique(dates))
#'   if (length(dates) < 4) return(1)
#'   interval = .gcd(stats::na.omit(dates-dplyr::lag(dates)))
#'   return(interval)
#' }
#'
#' # greatest common denominator
#' .gcd2 = function(a, b) {
#'   if (b == 0) a else Recall(b, a %% b)
#' }
#'
#' .gcd <- function(...) {
#'   Reduce(.gcd2, c(...))
#' }
#'
#' # convert a date column to a numeric count of intervals from day_zero
#' # the default value for day zero is a sunday at the start of the covid outbreak.
#' .date_to_time = function(dates, interval=1) {
#'   day_zero = as.Date(getOption("day_zero","2019-12-29"))
#'   out = floor(as.numeric(dates-day_zero)+interval/100)/interval
#'   attr(out,"interval")=interval
#'   return(out)
#' }
#'
#' # convert a time column of intervals from day_zero to a date
#' .time_to_date = function(times, interval=NA) {
#'   day_zero = as.Date(getOption("day_zero","2019-12-29"))
#'   if(is.na(interval)) interval=attr(times,"interval")
#'   return(floor(times*interval+interval/100)+day_zero)
#' }
#'
#' .date_seq_times = function(times, interval=NA) {
#'   orig_interval=attr(times,"interval")
#'   if(is.null(orig_interval)) stop("the original timeseries has lost its metadata")
#'   if (is.na(interval)) interval=orig_interval
#'   out = seq(min(times),max(times),by = interval/orig_interval)
#'   attr(out,"interval")=orig_interval
#'   return(out)
#' }
#'
#' # turn a random set of dates into an evenly spaced set separated by an interval that makes sense given the data
#' # intervals will be inside the data
#' .date_seq_dates = function(dates, interval=.day_interval(dates), truncate_partials = FALSE) {
#'   times = .date_to_time(dates,interval)
#'   # the +1/interval - 1 here ensures we have a full final interval as defined by the
#'   # dates. if interval is one this resolves to the max period
#'   # otherwise it depends on the day of the week of the largest date - a larges date on a Saturday completes a week beginning on a Sunday.
#'   if (truncate_partials) time_seq = seq(ceiling(min(times)),floor(max(times)+1/interval)-1,1)
#'   else time_seq = seq(ceiling(min(times-0.01)),floor(max(times+0.01)),1)
#'   date_seq = .time_to_date(time_seq, interval)
#'   return(date_seq)
#' }
#'
#' #TODO: test this a bit.
#' # full seq dates should be start of periods. interval is length of period.
#' # checks to see if a date or dates is within a range of dates where the dates define the start of a period
#' # of size defined by interval parameter as an integer.
#' .within_sequence = function(dates, date_seq_dates, interval = .day_interval(date_seq_dates)) {
#'   times = .date_to_time(dates,interval)
#'   test = .date_to_time(date_seq_dates, interval)
#'   return(times >= min(test) & times < max(test)+1)
#' }
#'
#' # floor to intervals from with zero_day as reference point
#' # makes a set of dates line up to the lower end of a period
#' .floor_sequence = function(dates, interval) {
#'   times = .date_to_time(dates,interval)
#'   return(.time_to_date(floor(times+1/100)))
#' }
#'
#' #.date_seq_dates(c(Sys.Date(),Sys.Date()-7,Sys.Date()-21,Sys.Date()-28))
#' #.day_interval(Sys.Date()+c(4,8,24))
#' #.date_seq_dates(Sys.Date()+c(4,8,24))
#'
#' ## .specification_from_formula( formula = ~ date + age + gender + region )
#' ## .specification_from_formula( formula = ~ date + reported(report_date) + age + gender + region )
#' ## .specification_from_formula( formula = class(variant) ~ date + reported(report_date) + age + gender + region )
#'
#' ## .specification_from_formula( formula = n ~ specimen_date + age + gender + region )
#' ## .specification_from_formula( formula = n ~ non_date(something) + specimen_date + age + gender + region )
#' ## .specification_from_formula( formula = dplyr::count() ~ date() + age + gender + region )
#' ## .specification_from_formula( formula = class(variant) + dplyr::count(n) ~ date() + age + gender + region )
#' ## .specification_from_formula( formula = class(variant) + dplyr::count(n) ~ date(specimen) + age + gender + region )
#' ## .specification_from_formula( formula = growth.rate() + Rt() + class(variant) + dplyr::count(n) ~ date(specimen) + age + gender + region )
#' ## parse$observations = named_list_lhs
#' ## parse$predictors = named_list_rhs
#' ## parse$groups = unnamed_list_rhs
#'
#' ## Formula utility ----
#'
#' # rhs of a formula as a string or a one sided formula
#' .rhs = function(formula, as_formula=FALSE) {
#'   tmp = as.character(formula)
#'   if(length(tmp) == 2) tmp = tmp[[2]]
#'   else tmp = tmp[[3]]
#'   if (as_formula) {
#'     return(stats::as.formula(paste0("~",tmp)))
#'   } else {
#'     return(tmp)
#'   }
#' }
#'
#' # lhs of a formula as a string or a one sided formula
#' .lhs = function(formula, as_formula=FALSE) {
#'   tmp = as.character(formula)
#'   if(length(tmp) == 2) return(NULL)
#'   else tmp = tmp[[2]]
#'   if (as_formula) {
#'     return(stats::as.formula(paste0("~",tmp)))
#'   } else {
#'     return(tmp)
#'   }
#' }
#'
#' # combine a rhs and a lhs into a single formula
#' .join_sides = function(lhs,rhs) {
#'   if (rlang::is_formula(rhs)) rhs = .rhs(rhs)
#'   if (is.null(lhs)) {
#'     return(stats::as.formula(paste0("~",rhs)))
#'   } else {
#'     # if lhs is a formula then it is a right sided formula
#'     if (rlang::is_formula(lhs)) lhs = .rhs(lhs)
#'     return(stats::as.formula(sprintf("%s ~ %s", lhs,rhs)))
#'   }
#' }
#'
#' # Update a formula using a new one treating rhs and lhs in the same way
#' .update = function(x, ...) {
#'   UseMethod(".update",x)
#' }
#'
#' .update.formula = function(x, new) {
#'   # switch rhs to lhs
#'   x_rhs = .rhs(x,as_formula=TRUE)
#'   new_rhs = .rhs(new,as_formula=TRUE)
#'   update_rhs = stats::update(x_rhs,new_rhs)
#'
#'   if(is.null(.lhs(x))) {
#'     # If the original lhs is empty
#'     if(is.null(.lhs(new))) {
#'       # the new lhs is also empty
#'       update_lhs = NULL
#'     } else {
#'       # the updated_lh will be the new_lhs but in formulae like
#'       # new = x + . ~ y
#'       # the + . term is problematic as it does not get removed
#'       new_lhs = .lhs(new,as_formula=TRUE)
#'       if(any(all.vars(new_lhs)==".")) {
#'         tmp = .lhs(new,as_formula=FALSE) # this gets the LHS as a character.
#'         tmp = stringr::str_remove(tmp[[1]],"(\\s\\+\\s)?\\.(\\s\\+\\s)?")
#'         new_lhs = stats::as.formula(paste0("~",tmp))
#'       }
#'       update_lhs = new_lhs
#'     }
#'   } else {
#'     # the original lhs is not empty
#'     x_lhs = .lhs(x,as_formula=TRUE)
#'     new_lhs = .lhs(new,as_formula=TRUE)
#'     if (is.null(new_lhs)) {
#'       update_lhs = NULL
#'     } else {
#'       update_lhs = stats::update(x_lhs,new_lhs)
#'     }
#'   }
#'   browser()
#'   return(.join_sides(update_lhs,update_rhs))
#'   # as both updates are of the RHS
#' }
#'
#' .update.epimetadata = function(x, new) {
#'   formula = x$formula
#'   as.epimetadata(.update.formula(formula, new), type=x$type, interval = x$interval)
#' }
#'
#' .update.epi_ts = function(x, new) {
#'   epi = x %>% get_meta()
#'   return(x %>% set_meta(.update.epimetadata(epi,new)))
#' }
#'
#' .update.epi_ll = function(x, new) {
#'   epi = x %>% get_meta()
#'   return(x %>% set_meta(.update.epimetadata(epi,new)))
#' }
#'
#' ## Metadata utility ----
#'
#' # parse a formula into a specification
#' .specification_from_formula = function(formula) {
#'
#'   if (is.null(formula)) return(NULL)
#'   # vars = all.vars(formula)
#'   form_chr = as.character(formula)[-1]
#'   if (length(form_chr) == 1) form_chr = c("",form_chr)
#'   names(form_chr) = c("lhs","rhs")
#'
#'   form_chr = form_chr %>%
#'     purrr::map(stringr::str_split,fixed("+"),n=Inf) %>%
#'     purrr::flatten() %>%
#'     purrr::map(stringr::str_trim)
#'   form_df = form_chr %>% tibble::enframe() %>% tidyr::unnest(c(value)) %>% dplyr::mutate(
#'       mapping = value %>% stringr::str_extract("(.*)\\(.*\\)") %>% stringr::str_remove("\\(.*\\)"),
#'       mapped = value %>% stringr::str_remove("(.*)\\(") %>% stringr::str_remove("\\)") %>% stringr::str_remove_all("`"),
#'       value = ifelse(mapped == "", mapping, mapped)
#'     ) %>%
#'     dplyr::select(-mapped) %>%
#'     dplyr::rename(side = name) %>%
#'     dplyr::mutate(value = lapply(value,as.symbol))
#'
#'   form_df %>% dplyr::filter(is.na(mapping)) %>% dplyr::pull(value)
#'
#'   if (!any(form_df$mapping=="date",na.rm = TRUE)) {
#'     form_df = form_df %>% dplyr::group_by(side) %>% dplyr::mutate(mapping = replace(mapping,is.na(mapping) & side == "rhs" & !is.na(dplyr::lag(mapping,default = "")),"date")) %>% dplyr::ungroup()
#'     if (!any(form_df$mapping=="date",na.rm = TRUE)) stop("No date column identified. Either date must be first term on the rhs or specifically named date(...)")
#'   }
#'
#'   # This will pick up a value only if there is at least one term on the lhs (and it is not already named)
#'   if (!any(form_df$mapping=="count",na.rm = TRUE)) {
#'     form_df = form_df %>% dplyr::group_by(side) %>% dplyr::mutate(mapping = replace(mapping,is.na(mapping) & side == "lhs" & !is.na(dplyr::lag(mapping,default = "")),"count")) %>% dplyr::ungroup()
#'   }
#'
#'   if (any(duplicated(stats::na.omit(form_df$mapping)))) stop("duplicate mappings specified in formula: "+formula)
#'   class(form_df) = c("specification",class(form_df))
#'   return(form_df)
#' }
#'
#' # convert a specification back into a formula
#' .formula_from_specification = function(specification) {
#'   specification %>% dplyr::mutate(
#'     label = sapply(value,as_label),
#'     term = dplyr::case_when(
#'       is.na(mapping) ~ label,
#'       mapping == label ~ paste0(mapping,"()"),
#'       TRUE ~ paste0(sprintf("%s(%s)",mapping,label)))
#'   ) %>% dplyr::group_by(side) %>%
#'     dplyr::summarise(term = paste0(term,collapse=" + ")) %>%
#'     dplyr::summarise(formula = paste0(term,collapse=" ~ ")) %>%
#'     dplyr::pull(formula) %>% stats::as.formula()
#' }
#'
#' # construst a list of utility functions from a specification object.
#' .mapper = function(x,...) {
#'   v = x
#'   return(list(
#'     grps = v %>% dplyr::filter(side=="rhs" & is.na(mapping)) %>% dplyr::pull(value),
#'     date = v %>% dplyr::filter(side=="rhs" & mapping=="date") %>% dplyr::pull(value) %>% `[[`(1),
#'     incidentals = v %>% dplyr::filter(side=="lhs" & is.na(mapping)) %>% dplyr::pull(value),
#'     get = function(type) {
#'       tmp = v %>% dplyr::filter(mapping==type) %>% dplyr::pull(value)
#'       if (length(tmp) == 0) return(NULL)
#'       tmp[[1]]
#'     },
#'     predictor = function(type) {
#'       tmp = v %>% dplyr::filter(side=="rhs" & mapping==type) %>% dplyr::pull(value)
#'       if (length(tmp) == 0) return(NULL)
#'       tmp[[1]]
#'     },
#'     observation = function(type="count") {
#'       tmp = v %>% dplyr::filter(side=="lhs" & mapping==type) %>% dplyr::pull(value)
#'       # can subst NULL using !! and it behaves as expected in ggplot and tidyselect
#'       if (length(tmp) == 0) return(NULL)
#'       tmp[[1]]
#'     },
#'     has_observation = function(type="count") {
#'       nrow(v %>% dplyr::filter(side=="lhs" & mapping==type)) > 0
#'     },
#'     has_predictor = function(type) {
#'       nrow(v %>% dplyr::filter(side=="rhs" & mapping==type)) > 0
#'     }
#'   ))
#' }
#'
#'
#' # .var_from_rhs = function(formula, match="date") {
#' #   if (is.null(formula)) return(NULL)
#' #   v = .specification_from_formula(formula)
#' #   sym = v %>% dplyr::filter(side=="rhs" & mapping == match) %>% dplyr::pull(value)
#' #   if (length(sym)==0) return(NULL)
#' #   return(sym)
#' # }
#' #
#' # .vars_from_rhs = function(formula) {
#' #   if (is.null(formula)) return(NULL)
#' #   v = .specification_from_formula(formula)
#' #   sym = v %>% dplyr::filter(side=="rhs" & !is.na(mapping)) %>% dplyr::select(mapping,value) %>% tibble::deframe()
#' #   return(sym)
#' # }
#' #
#' # .grps_from_rhs = function(formula) {
#' #   if (is.null(formula)) return(NULL)
#' #   v = .specification_from_formula(formula)
#' #   sym = v %>% dplyr::filter(side=="rhs" & is.na(mapping)) %>% dplyr::pull(value)
#' #   return(sym)
#' # }
#' #
#' # .value_from_lhs = function(formula) {
#' #   if (is.null(formula)) return(NULL)
#' #   # formula = n ~ date + type(cls) + report(spec) + age+gender+region+code
#' #   v = all.vars(rlang::f_lhs(formula))
#' #   if (length(v) > 1) stop("Only zero or one variable on lhs allowed, defining the value (e.g. case count)")
#' #   if (length(v) == 0) return(NULL)
#' #   return(as.symbol(v))
#' # }
#'
#'
#'
#' # .rdeframe = function(form_df, ...) {
#' #   vars = rlang::ensyms(...)
#' #   if (length(vars) == 1) {
#' #     return(form_df %>% dplyr::pull(!!vars[[1]]) %>% unlist())
#' #   }
#' #   form_df %>%
#' #     dplyr::rename(.name = !!vars[[1]]) %>%
#' #     dplyr::mutate(.name = ifelse(is.na(.name),"na",as.character(.name))) %>%
#' #     dplyr::group_by(.name) %>% dplyr::group_modify(function(d,g,...) {
#' #
#' #       tibble::tibble(.value = list(.rdeframe(d,!!!vars[-1])))
#' #   }) %>% tibble::deframe()
#' # }
#' #
#' # .map
#' #
#' # tmp = .rdeframe(form_df, side, mapping, value)
#'
#'
#'
#' ## metadata object ----
#'
#' as.epimetadata = function(x, ...) {
#'   UseMethod("as.epimetadata", x)
#' }
#'
#' as.epimetadata.formula = function(x, type, interval = 1, ...) {
#'   specification = .specification_from_formula(x)
#'   .make_metadata(x, specification, type, interval)
#' }
#'
#' as.epimetadata.specification = function(x, type, interval = 1, ...) {
#'   formula = .formula_from_specification(x)
#'   .make_metadata(formula, x, type, interval)
#' }
#'
#' .make_metadata = function(
#'   formula,
#'   specification,
#'   type,
#'   interval
#' ) {
#'   return(structure(
#'     list(
#'       specification = specification,
#'       formula = formula,
#'       interval = interval,
#'       type = type,
#'       m = .mapper(specification)
#'     ),
#'     class = "epimetadata"
#'   ))
#' }
#'
#' .set_interval = function(meta, interval) {
#'   meta$interval = interval
#'   return(meta)
#' }
#'
#' ## generic epi data functions ----
#'
#' .make_epidata = function(x, meta, cls) {
#'   .check_conformant(x,meta)
#'   x = x %>% dplyr::ungroup() %>% dplyr::mutate(.id=dplyr::row_number())
#'   x = x %>% set_meta(meta)
#'   class(x) = c(cls, class(x))
#'   return(x)
#' }
#'
#' .guess_type = function(x,meta) {
#'   if (any(c("epi_ts","epi_ll") %in% class(x))) return(x)
#'   date = meta$m$date
#'   grps = meta$m$grps
#'   cls = meta$m$observation("class")
#'   if (meta$m$has_observation("count")) {
#'     # can infer type from metadata
#'     return(as.epi_ts.data.frame(x, meta$formula, interval = meta$interval))
#'   }
#'   if (meta$type=="ts") {
#'     return(as.epi_ts.data.frame(x, meta$formula, interval = meta$interval))
#'   }
#'
#'   grpwise_count_R2 = x %>% dplyr::group_by(!!!grps,!!cls,!!date) %>% dplyr::count() %>% dplyr::pull(n) %>% magrittr::subtract(1) %>% magrittr::raise_to_power(2) %>% mean()
#'   full = .date_seq_dates(x %>% dplyr::pull(!!date),interval)
#'   incomplete_ts = x %>% dplyr::group_by(!!!grps,!!cls) %>% dplyr::summarise(matched = sum(!!date %in% full)) %>% dplyr::mutate(missing = length(full)-matched, total=length(full)) %>% dplyr::ungroup() %>% dplyr::summarise(prop = sum(missing)/sum(total)) %>% dplyr::pull(prop)
#'   if (incomplete_ts < 0.05 & grpwise_count_R2 < 0.01) {
#'     browser()
#'     as.epi_ts.data.frame(x, meta$formula, interval = meta$interval)
#'   } else {
#'     as.epi_ll.data.frame(x, meta$formula)
#'   }
#' }
#'
#' .check_conformant = function(x, meta) {
#'   if (!is.data.frame(x)) stop("epi data must be a data frame")
#'   ok = TRUE
#'   for (col in meta$specification$value) {
#'     if (!(rlang::as_label(col) %in% colnames(x))) {
#'       message("No column found: ",rlang::as_label(col))
#'       ok = FALSE
#'     }
#'   }
#'   if (!ok) stop("Input is not conformant to specification provided: ",meta$formula)
#' }
#'
#' get_meta = function(x) {
#'   return(attr(x,"epi"))
#' }
#'
#' set_meta = function(x, metadata) {
#'   .check_conformant(x,metadata)
#'   attr(x,"epi")=metadata
#'   return(x)
#' }
#'
#' is.epi_ll = function(x,...) {
#'   return(any(c("epi_ll","std_ll") %in% class(x)))
#' }
#'
#' is.epi_ts = function(x,...) {
#'   return(any(c("epi_ts","std_ts") %in% class(x)))
#' }
#'
#' ## Line list object ----
#'
#' as.epi_ll = function(x, ...) {
#'   UseMethod("as.epi_ll",x)
#' }
#'
#' as.epi_ll.default = function(x, ...) {
#'   stop("Can't make a epi line list out of a ",paste0(class(x),collapse=", "))
#' }
#'
#' as.epi_ll.data.frame = function(x, formula) {
#'   # interval in line list defaults to 1.
#'   meta = as.epimetadata(formula, type="ll")
#'
#'   m = meta$m
#'   # date = m$date
#'   cls = m$observation("class")
#'   multinom = !rlang::is_null(cls)
#'   # grps = m$grps
#'   out_class = "epi_ll"
#'   if (multinom) out_class = c("epi_multi",out_class)
#'
#'   if (m$has_observation("count")) stop("Attempting to make a line list out of a object with a 'count' column. Did you mean to use as.epi_ts()?")
#'
#'   return(.make_epidata(x,meta,out_class))
#' }
#'
#' as.epi_ll.epi_ts = function(x, jitter=FALSE) {
#'   meta = x %>% get_meta()
#'   m = meta$m
#'   interval = meta$interval
#'   cls = m$observation("class")
#'   grps = m$grps
#'   date = m$date
#'   count = m$observation("count")
#'   multinom = !rlang::is_null(cls)
#'   # grps = m$grps
#'   out_class = "epi_ll"
#'   if (multinom) out_class = c("epi_multi",out_class)
#'
#'   if(is.null(count)) stop("count column must be present")
#'   y = x %>% dplyr::group_by(!!!grps,!!date,!!cls) %>% dplyr::group_modify(function(d,g,..) {
#'     join = unlist(dplyr::map2(d %>% dplyr::pull(.id), d %>% dplyr::pull(!!count), ~ rep(.x,.y)))
#'     return(d %>% dplyr::select(-count) %>% dplyr::inner_join(tibble::tibble(.id=join), by=".id") %>% dplyr::select(-.id))
#'   })
#'   y = y %>% dplyr::ungroup() %>% dplyr::mutate(.id=dplyr::row_number())
#'   if (jitter & interval > 1) {
#'     y = y %>% dplyr::mutate(!!date := !!date+floor(stats::runif(nrow(y),max=interval)))
#'   }
#'   specification = meta$specification %>% dplyr::filter(!(side == "lhs" & mapping %in% c("count")))
#'   meta = as.epimetadata(specification, type="ll")
#'   return(.make_epidata(y,meta,out_class))
#' }
#'
#' summary.epi_ll = function(x,...) {
#'   epi = x %>% get_meta()
#'   m = epi$m
#'   cat(sprintf("linelist: %1.0f line list(s), %1.0f entries", length(m$grps)+1, nrow(x)),"\n")
#'   print(epi$formula, showEnv = FALSE)
#' }
#'
#' print.epi_ll = function(x,...) {
#'   summary(x,...)
#'   NextMethod(x,...)
#' }
#'
#' glimpse.epi_ll = function(x,...) {
#'   summary(x,...)
#'   NextMethod(x,...)
#' }
#'
#' ## Incidence ts object ----
#'
#' as.epi_ts = function(x, ...) {
#'   UseMethod("as.epi_ts", x)
#' }
#'
#' as.epi_ts.default = function(x, ...) {
#'   stop("Can't make a epi time series out of a ",paste0(class(x),collapse=", "))
#' }
#'
#' as.epi_ts.Date = function(x, count, class = NULL, ...) {
#'
#'   x = tibble::tibble(date = x, count = count, class = class)
#'   formula = dplyr::count() ~ date()
#'   if(!is.null(class)) formula = .update(formula, class() + . ~ .)
#'   as.epidata.data.frame(x, formula, ...)
#' }
#'
#' .null_na = function(x) {ifelse(suppressWarnings(is.na(x)),NULL,x)}
#'
#' as.epi_ts.data.frame = function(x, formula, interval = NA, rectangular = FALSE, dates = NULL) {
#'   meta = as.epimetadata(formula, type="ts", interval=interval)
#'   date = meta$m$date
#'   # Determine the frequency of the time series
#'   # either asserted, or by reduction from the data
#'   if (is.na(meta$interval)) meta$interval = .day_interval(x %>% dplyr::pull(!!date))
#'   .convert_dataframe(x, meta, rectangular = FALSE, verbose = TRUE, dates = dates)
#' }
#'
#' as.epi_ts.epi_ll = function(x, formula = dplyr::count() + . ~ ., interval = 1, dates = NULL) {
#'   meta = x %>% get_meta()
#'   new_meta = .update(meta, formula)
#'   m = new_meta$m
#'   new_count = m$observation("count")
#'
#'   # what dates are we looking at?
#'   orig_dates = x %>% dplyr::pull(!!m$date)
#'   if (is.null(dates)) dates = orig_dates
#'   dates = .date_seq_dates(dates,interval, truncate_partials = TRUE)
#'
#'   y = .convert_dataframe(x %>% dplyr::mutate(!!new_count == 1), new_meta, rectangular = TRUE, verbose = FALSE, dates = dates)
#'   return(y %>% set_meta(new_meta))
#' }
#'
#' .convert_dataframe = function(x, meta, rectangular = FALSE, verbose=FALSE, dates = NULL) {
#'   if (nrow(x)<2) stop("need multiple time points for a timeseries")
#'   interval = meta$interval
#'   m = meta$m
#'   date = m$date
#'   cls = m$observation("class")
#'   value = m$observation("count")
#'   grps = m$grps
#'
#'   out_class = c("epi_ts")
#'   multinom = !rlang::is_null(cls)
#'   if (multinom) out_class = c("epi_multi",out_class)
#'
#'   dates_given = !is.null(dates)
#'   orig_dates = x %>% dplyr::pull(!!date)
#'   if(!dates_given) dates = .date_seq_dates(orig_dates,interval)
#'
#'   # make sure data dates are within the range of the desired interval dates
#'
#'   if (interval > 1) {
#'     # this is good for linelist type data where we want to make sure we have whole intervals
#'     # not so good for data already in time series which may "finish" on the first date of an interval.
#'     x = x %>% dplyr::filter(.within_sequence(!!date, dates, interval))
#'     x = x %>% dplyr::mutate(!!date := .floor_sequence(!!date, interval))
#'   }
#'
#'   # check count values are unique for each combination of date, grouping, and multinom class
#'   tmp = x %>% dplyr::group_by(!!!grps, !!cls, !!date) %>% dplyr::count()
#'
#'   if (any(tmp$n > 1)) {
#'     browser()
#'     #TODO have to reconsider this as count is a very optional column of time series but others must be
#'     if (verbose) message("Input dataframe has more than one row per date (and class combination), which may be intentional. Combining (class) counts in multiple rows by summation, any other observations will be lost.")
#'     if (!is.null(value)) {
#'       if(any(is.na(tmp %>% dplyr::pull(!!value)))) warning("Count column contains some NA values. The combined count will be NA for these rows.")
#'       x = x %>% dplyr::group_by(!!!grps, !!cls, !!date) %>% dplyr::summarise(!!value := sum(!!value))
#'     }
#'     # since group by summarise steps will remove all other observations we need to make sure that the metadata is updated with the lhs including only class and count.
#'     specification = meta$specification %>% dplyr::filter(!(side == "lhs" & mapping %in% c("class","count")))
#'     meta = as.epimetadata(specification, type=meta$type, interval=meta$interval)
#'   }
#'
#'   # ensure completeness of dates and (optionally) class on a per group basis
#'   # step 1 setup the complete combination of dates and classes (if present)
#'   if (multinom) {
#'     # ensure counts are complete for each of the outcome classes also as well as for each date.
#'     clsses = tibble::tibble(x) %>% dplyr::pull(!!cls) %>% unique() %>% sort()
#'     join_cols = c(rlang::as_label(date),rlang::as_label(cls))
#'   } else {
#'     clsses = NULL
#'     join_cols = rlang::as_label(date)
#'   }
#'   # step 2 generate a crossing dataframe of all combination of dates and optionally classes
#'   # this is the version for rectangular time series, where a single source of data contains
#'   # the full range of time points for all sources - i.e. there is known to be no differential
#'   # reporting delay.
#'   lhs = .dates_and_classes(date,dates,cls,clsses)
#'   # step 3 left join crossing dataframe with data and fill missing counts with zero.
#'   # ensuring that the result is valid
#'   x = tibble::tibble(x) %>%
#'     dplyr::group_by(!!!grps) %>%
#'     dplyr::group_modify(function(d,g,...) {
#'       # do a groupwise join. the lhs is either all dates or all dates and class levels
#'       # or if we are not using rectangular time series then calculate a group-wise lhs
#'       # including the range present in the data.
#'       if (!rectangular & !dates_given) {
#'         tmp = d %>% dplyr::pull(!!date)
#'         dates = .date_seq_dates(tmp,interval)
#'         lhs = .dates_and_classes(date,dates,cls,clsses)
#'       }
#'       # do the fill for missing counts.
#'       d = lhs %>%
#'         dplyr::left_join(d, by = join_cols)
#'       if (!is.null(value)) {
#'         #TODO: what about other observations?
#'         d = d %>% dplyr::mutate(!!value := ifelse(is.na(!!value),0,!!value))
#'       }
#'       return(d)
#'
#'   }) %>% dplyr::ungroup()
#'
#'   if (!".id" %in% colnames(x)) {
#'     # add an .id column only if there is not one already.
#'     x = x %>% dplyr::mutate(.id=dplyr::row_number())
#'   }
#'
#'   return(.make_epidata(
#'       tibble::as_tibble(x),
#'       meta,
#'       out_class))
#' }
#'
#' .dates_and_classes = function(date, dates, cls, clsses) {
#'   if (!is.null(clsses)) {
#'     lhs = tidyr::crossing(!!date := dates, !!cls := clsses)
#'   } else {
#'     lhs = tibble::tibble(!!date := dates)
#'   }
#' }
#'
#' summary.epi_ts = function(x, ...) {
#'   epi = x %>% get_meta()
#'   m = epi$m
#'   dates = x %>% dplyr::pull(!!(m$date)) %>% range()
#'   grpCount = x %>% dplyr::select(!!!m$grps) %>% dplyr::distinct() %>% nrow()
#'   cat(sprintf("%1.0f timeseries, with interval %s lubridate::day(s), from %s up to (but not including) %s, %1.0f total records", grpCount, epi$interval, dates[[1]], dates[[2]]+1+epi$interval, nrow(x)),"\n")
#'   print(epi$formula, showEnv = FALSE)
#' }
#'
#' print.epi_ts = function(x,...) {
#'   summary(x,...)
#'   NextMethod(x,...)
#' }
#'
#'
#' glimpse.epi_ts = function(x,...) {
#'   summary(x,...)
#'   NextMethod(x,...)
#' }
#'
#' ## standardised formats ----
#'
#' # force the rename of all observations and predictors, add in .time and .grpId (and optionally .subGrpId if multinomial) columns
#' .normalise = function(x, interval = NA, ...) {
#'   meta = x %>% get_meta()
#'   m = meta$m
#'   date = m$date
#'   cls = m$observation("class")
#'   multinom = !is.null(cls)
#'   grps = m$grps
#'   y = x
#'   for (map in meta$specification$mapping) {
#'     # rename the columns to their canonical names defined in the specification.
#'     if (!is.na(map)) {
#'       value = m$get(map)
#'       y = y %>% dplyr::mutate(!!map := !!value)
#'     }
#'   }
#'   all = meta$specification$value
#'   if (is.na(interval)) interval = .day_interval(y$date)
#'
#'   y = y %>%
#'     dplyr::select(tidyselect::all_of(stats::na.omit(meta$specification$mapping)), !!!grps, .id)
#'
#'   y = y %>% dplyr::group_by(!!!grps) %>% dplyr::mutate(
#'     .grpId = dplyr::cur_group_id(),
#'     .time = .date_to_time(date, interval),
#'   ) %>% dplyr::group_by(!!!grps, .grpId)
#'   if (multinom) {
#'     y = y %>% dplyr::group_by(!!!grps, class) %>% dplyr::mutate(.subGrpId = dplyr::cur_group_id()) %>% dplyr::group_by(!!!grps, .grpId)
#'     class(y) = c("epi_multi",class(y))
#'   } else {
#'     class(y) = c("epi_simple",class(y))
#'   }
#'   return(y)
#' }
#'
#' .denormalise = function(x, meta) {
#'   y=x
#'   if (".time" %in% colnames(y)) {
#'     if (!("date" %in% colnames(y))) {
#'       y = y %>% dplyr::mutate(date = .time_to_date(.time))
#'     } else if (any(is.na(y$date))) {
#'       y = y %>% dplyr::mutate(date = .time_to_date(.time))
#'     }
#'   }
#'   y = y %>% dplyr::select(-.time)
#'   for (map in meta$specification$mapping) {
#'     # rename the columns to their denormalised names defined in the specification.
#'     # group columns will probably not have changed name
#'     if (!is.na(map)) {
#'       value = m$get(map)
#'       if (map %in% colnames(y)) {
#'         y = y %>% dplyr::rename(!!value := !!map)
#'       }
#'     }
#'   }
#'
#'   old_cols = sapply(meta$specification$value,as_label, USE.NAMES = FALSE)
#'   new_cols = colnames(y)[!colnames(y) %in% old_cols]
#'   new_obs = new_cols %>% magrittr::extract(!stringr::str_starts(.,stringr::fixed(".")))
#'   # all new cols are added as new observations onto the lhs
#'   new_cols_df = tibble::tibble(
#'     side = "lhs",
#'     value = sapply(new_obs, as.symbol,USE.NAMES = FALSE),
#'     mapping = new_obs
#'   )
#'
#'   new_spec = dplyr::bind_rows(
#'     meta$specification %>% dplyr::filter(sapply(value, as_label, USE.NAMES = FALSE) %in% colnames(y)),
#'     new_cols_df)
#'   new_meta = as.epimetadata.specification(new_spec, type=meta$type, interval = meta$interval)
#'
#'   y = y %>% dplyr::ungroup() %>% dplyr::select(tidyselect::any_of(old_cols),tidyselect::all_of(new_obs),tidyselect::any_of(".id"))
#'   # y = y %>% dplyr::ungroup() %>% dplyr::select(c(!tidyselect::starts_with("."),.id)) %>% dplyr::glimpse()
#'   return(y %>% .guess_type(new_meta))
#' }
#'
#' ## Execute a simple timeseries processing step on a standard data format ---
#'
#'
#' execute_epifunction = function(x, .f, ...) {
#'   # x =tmp
#'   # .f=estimate_proportion
#'   # check input is valid for the function
#'   # this is defined in the function
#'   meta = x %>% get_meta()
#'   require_str = formals(.f)[[".require"]]
#'   input = .normalise(x)
#'   browser()
#'   if (!is.null(require_str)) {
#'     requires = eval(require_str)
#'     if (!all(requires %in% meta$specification$mapping)) {
#'       warning("Input data frame does not have all the required columns:")
#'       warning(meta$formula)
#'       stop("must contain column mappings for: ",paste0(requires,collapse = ","))
#'     }
#'   }
#'   output = input %>% dplyr::group_modify(function(d,g,...) {
#'     if ("epi_ll" %in% class(x)) {
#'       class(d) = c("std_ll",class(d))
#'       return(.f(d, g=g, ...))
#'     } else {
#'       class(d) = c("std_ts",class(d))
#'       #TODO: informative error messages
#'       return(.f(d, g=g, ..., interval = meta$interval))
#'     }
#'     # execute the epifunction call
#'
#'   })
#'   if (!"date" %in% colnames(output)) {
#'     #TODO: consider autoconverting .time to date
#'     stop("the result of an epifunction must include a date column")
#'   }
#'   return(.denormalise(output, meta))
#' }
#'
#' ## Simple timeseries functions ----
#'
#' # a set of estimators for the simple single time series situations
#' # the estimates target a range of outputs such as poisson rate, proportion model, growth rate, etc.
#' # these are aimed to be tidy but assume (and enforce) column naming conventions are adhered to
#' # these do not work on grouped data. they assume the input has been sanitised before hand, although should tolerate NA values.
#'
#' # make sure a given column exists and create it with the orElse function if not.
#' ensure_exists = function(df, column, or_else = function(df) {stop("Missing column: ",column)},...) {
#'   or_else = purrr::as_mapper(or_else)
#'   out = df
#'   if(!(all(column %in% colnames(df)))) {
#'     out = or_else(df,...)
#'   }
#'   out
#' }
#'
#' # or else NA
#' .opt = function(expr) tryCatch(expr,error=function(e) NA_real_)
#'
#' # format a transformed normally distributed variable into quantiles
#' .format_result = function(df, fit, se.fit, t, estimate, modelName,link, error=NA_character_) {
#'   est = #purrr::map2(fit,se.fit,.f = function(fit,se.fit) {
#'     tibble::tibble(
#'       !!(paste0(link,"(x)")) := fit,
#'       !!(paste0("SE.",link,"(x)")) := se.fit,
#'       Quantile.0.025 = .opt(t(stats::qnorm(0.025,fit,se.fit))),
#'       Quantile.0.05 = .opt(t(stats::qnorm(0.05,fit,se.fit))),
#'       Quantile.0.25 = .opt(t(stats::qnorm(0.25,fit,se.fit))),
#'       Quantile.0.5 = t(fit),
#'       Quantile.0.75 = .opt(t(stats::qnorm(0.75,fit,se.fit))),
#'       Quantile.0.95 = .opt(t(stats::qnorm(0.95,fit,se.fit))),
#'       Quantile.0.975 = .opt(t(stats::qnorm(0.975,fit,se.fit))),
#'       model = modelName,
#'       error = error)
#'     #})
#'   df %>% dplyr::mutate(!!estimate := est)
#' }
#'
#' # extract the locfit result from the locfit model and format it
#' .fixed_result = function(df, estimate, modelName, link, mean = NA_real_, se = NA_real_, error = "unknown error") {
#'   df %>% .format_result(fit = mean, se.fit= se, t=.inv[[link]], estimate, modelName, link, error)
#' }
#'
#' .empty_result = function(df, estimate) {
#'   df %>% dplyr::mutate(!!estimate := tibble::tibble())
#' }
#'
#' .inv = list(
#'   value = function(x) x,
#'   log = function(x) {ifelse(x==-Inf, 0, exp(x))},
#'   logit = function(x) {dplyr::case_when(x==-Inf ~ 0,x==Inf ~ 1, TRUE ~ 1/(1+exp(-x)))}
#' )
#'
#' # This is just to format locfit results given a locfit model.
#' # extract the locfit result from the locfit model and format it
#' # ... could be where="fitp", or newdata=.time points....
#' .locfit_extract_result = function(df, model, estimate, modelName, link = "value") {
#'
#'   tryCatch({
#'     points = stats::preplot(model,se.fit = TRUE,band="local", newdata = df)
#'
#'     t = points$trans
#'     fit = points$fit
#'     se.fit = tryCatch({
#'       as.vector(forecast::na.interp(points$se.fit))
#'     }, error = function(e) {
#'       rep(NA,length(fit))
#'     })
#'
#'     df %>% .format_result(fit,se.fit,t,estimate,modelName,link)
#'
#'   }, error = function(e) {
#'
#'     df %>% .fixed_result(estimate,modelName,link,error = e$message)
#'
#'   })
#' }
#'
#' ## Proportion estimation ----
#'
#' # Generate the formula for a locfit model based on things I understand
#' .locfit_formula = function(valueVar, nrowDf, window, polynomialDegree, nearestNeighbours = TRUE, ...) {
#'   valueVar=rlang::ensym(valueVar)
#'   tmp_alpha = min(window/nrowDf,1)
#'   tmp_alpha_2 = min((window*2+1)/nrowDf,1)
#'   lpParams = list(
#'     nn = if( nearestNeighbours ) tmp_alpha_2 else tmp_alpha, # this is given in fraction of total observations
#'     h = if( !nearestNeighbours ) window else 0, # this is given in units of X
#'     deg = polynomialDegree
#'   )
#'   lpParamsText = paste(names(lpParams),lpParams,sep="=",collapse=", ")
#'   lpFormula = stats::as.formula(paste0(rlang::as_label(valueVar), " ~ locfit::lp(.time, ",lpParamsText,")"))
#'   return(lpFormula)
#' }
#'
#'
#' .has_count = function(x, ...) {
#'   "count" %in% colnames(x)
#' }
#'
#' # takes a line list or incidence count of patient admissions with a multinomial class label, and fits
#' # a quasi-binomial model with logit link using local regression. This expects a dataframe
#' # with an .time column and a class column. Multinomial class is either treated as a
#' # set of 1 versus others binomials, if the class is unordered (or not a factor) or as a set of less than or equal versus more
#' # than binomials (if the multinomial class is ordered)
#' estimate_proportion = function(d, ..., interval = .day_interval(d$date), window = 28, degree=2, quick=NA) {
#'
#'   d = d %>% ensure_exists("date")
#'   d = d %>% ensure_exists("class")
#'   # convert dates to times
#'   d = d %>% ensure_exists(".time", or_else = ~ dplyr::mutate(., .time = .date_to_time(date, interval)))
#'   if (is.na(quick)) {
#'     quick = !((.has_count(d) & sum(d$count) < 10000) | (!.has_count(d) & nrow(d) < 10000))
#'   }
#'
#'   is_ts = .has_count(d)
#'   time_span = (max(d$.time)-min(d$.time))*interval
#'   # get the output as fractional weeks - and convert to days.
#'   data_times = seq(min(d$.time),max(d$.time), 1)
#'   predict_times = seq(min(d$.time),max(d$.time), 1/interval)
#'
#'   cumulative = is.ordered(d$class)
#'   model_name = sprintf("locfit:probability:%s:%s:%1.0f*%1.0f:%1.0f",if(cumulative) "cumulative" else "binomial",if(quick) "counts" else "linelist", window, interval,degree)
#'
#'   out = tibble::tibble()
#'   # repeat once for each class level. This is a binomial comparison (x vs magrittr::not(x)) or cumulative (<=x) vs (>x)
#'   for (level in sort(unique(d$class))) {
#'
#'     if (cumulative) {
#'       tmpdf = d %>% dplyr::mutate(class_bool = class <= level)
#'       est_name = "probability.cumulative"
#'     } else {
#'       tmpdf = d %>% dplyr::mutate(class_bool = class == level)
#'       est_name = "probability"
#'     }
#'     if (is_ts) {
#'       # summarise the counts
#'       tmpdf_quick = tmpdf %>% dplyr::group_by(.time,class_bool) %>% dplyr::summarise(count = sum(count),.groups="drop")
#'       if(!quick) tmpdf_slow = tmpdf_quick %>% dplyr::group_by(.time,class_bool) %>% dplyr::group_modify(function(d,g,..) {return(tibble::tibble(count = rep(1,d$count)))})
#'     } %>% {
#'       tmpdf_slow = tmpdf
#'       if(quick) tmpdf_quick = tmpdf %>% dplyr::group_by(.time,class_bool) %>% dplyr::summarise(count = dplyr::n(),.groups="drop") %>% tidyr::complete(.time = data_times, class_bool, fill=list(count=0) )
#'     }
#'
#'     result = tibble::tibble(.time=predict_times, class=level)
#'
#'     if (nrow(tmpdf) == 0) {
#'       # empty estimate
#'       out = out %>% dplyr::bind_rows(result %>% .fixed_result(est_name,model_name,link = "logit",mean = NA,se = NA, error = "no data"))
#'     } else if (sum(tmpdf$class_bool) < degree) {
#'       # zero estimate
#'       out = out %>% dplyr::bind_rows(result %>% .fixed_result(est_name,model_name,link = "logit",mean = -Inf,se = 10000, error = "all zeros"))
#'     } else if (sum(!tmpdf$class_bool) < degree) {
#'       # one estimate
#'       out = out %>% dplyr::bind_rows(result %>% .fixed_result(est_name,model_name,link = "logit",mean = Inf,se = 10000, error = "all ones"))
#'     } else {
#'
#'       tryCatch({
#'         if (quick) {
#'           # This is how I expect it to work:
#'           # lf_form = .locfit_formula(class_bool, time_span, window = window, polynomialDegree = degree, nearestNeighbours = !is_ts)
#'           # fit = locfit::locfit(lf_form,
#'           #    data = tmpdf,
#'           #    weights = count,
#'           #    family="qbinomial", link="logit")
#'
#'           # This is what seems to work but does not include any sample size in weighting.
#'           tmpdf_quick = tmpdf_quick %>% dplyr::group_by(.time) %>% dplyr::mutate(total = sum(count), p=count/total) %>%
#'             dplyr::filter(class_bool) # %>%
#'             # this bit does not work either
#'             # dplyr::mutate(inv_var = 1/(total*p*(1-p))) %>% dplyr::mutate(inv_var = ifelse(is.finite(inv_var),inv_var,1))
#'
#'           lf_form = .locfit_formula(p, time_span, window = window, polynomialDegree = degree, nearestNeighbours = FALSE)
#'           # timeseries model when there are counts
#'           fit = locfit::locfit(lf_form,
#'              data = tmpdf_quick,
#'              # weights = total, weights =1/total
#'              # weights = inv_var,
#'              family="qbinomial", link="logit", maxit = 5000, maxk=5000)
#'           # browser()
#'
#'         } else {
#'           # this is the line list version.
#'
#'           lf_form = .locfit_formula(class_bool, time_span, window = window, polynomialDegree = degree, nearestNeighbours = TRUE)
#'           # line list model when there are no counts
#'           fit = locfit::locfit(lf_form,
#'              data = tmpdf_slow,
#'              family="qbinomial", link="logit", maxit = 5000, maxk=5000)
#'         }
#'         tmp = result %>% .locfit_extract_result(model = fit, estimate = est_name, modelName = model_name, link = "logit")
#'
#'         out = out %>% dplyr::bind_rows(tmp)
#'
#'       }, error = function(e) {
#'         browser()
#'         out = out %>% dplyr::bind_rows(result %>% .fixed_result(est_name,model_name,link="logit",error = e$message))
#'
#'       })
#'
#'     }
#'
#'   }
#'
#'   # convert times back to dates
#'   out = out %>% dplyr::mutate(date = .time_to_date(.time))
#'
#'   # swap factor levels back in
#'   if (is.factor(d$class)) out = out %>% dplyr::mutate(class = factor(class, levels(d$class), ordered = is.ordered(d$class)))
#'   return(out)
#'
#' }
#'
#'
#'
#' # takes a line list of patient admissions with a multinomial class label, and fits
#' # a quasi-binomial model with logit link using local regression. This expects a dataframe
#' # with an admission_week column and a class column. Multinomial class is either treated as a
#' # set of 1 versus others binomials (cumulative = FALSE) or as a set of less than or equal versus more
#' # than binomials (cumulative = TRUE, which assumes multinomial class is ordered)
#' estimateProportion = function(admissions, ... ,nn=0.2, deg=2, cumulative = is.ordered(admissions$class)) {
#'   # get the output as fractional weeks - we wil convert this to days later.
#'   weeks = seq(min(admissions$admission_week),max(admissions$admission_week),by = 1/7)
#'
#'   out = tibble::tibble()
#'   # we are doing a binomial this for each level in the factor versus all other levels.
#'   # this lets us create an estimate for multinomial data that I'm going to use later.
#'   # I've never been sure about whether multinomial proportions can be treated as the sum of
#'   # binomial 1 vs others, my suspicion is they can't, but I'm going to do it anyway
#'   for (level in levels(admissions$class)) {
#'     if (cumulative) {
#'       tmpdf = admissions %>% dplyr::mutate(class_bool = class <= level)
#'     } else {
#'       tmpdf = admissions %>% dplyr::mutate(class_bool = class == level)
#'     }
#'     if (any(is.na(tmpdf$class_bool))) browser()
#'     # detect some edge cases
#'     if (nrow(tmpdf) == 0) {
#'       # data set is empty
#'       out = out %>% dplyr::bind_rows(
#'         tibble::tibble(
#'           class = level,
#'           admission_week = weeks,
#'           admission_date = .weeks_to_date(weeks),
#'           lower = 0,
#'           median = 0,
#'           upper = 1
#'         )
#'       )
#'     } else if (!any(tmpdf$class_bool)) {
#'       # for a given class there is no data or all observations are negative
#'       out = out %>% dplyr::bind_rows(
#'         tibble::tibble(
#'           class = level,
#'           admission_week = weeks,
#'           admission_date = .weeks_to_date(weeks),
#'           lower = 0,
#'           median = 0,
#'           upper = 0
#'         )
#'       )
#'     } else if (all(tmpdf$class_bool)) {
#'       # for a given class all the observations are positive
#'       out = out %>% dplyr::bind_rows(
#'         tibble::tibble(
#'           class = level,
#'           admission_week = weeks,
#'           admission_date = .weeks_to_date(weeks),
#'           lower = 1,
#'           median = 1,
#'           upper = 1
#'         )
#'       )
#'     } else {
#'       fit = locfit::locfit(class_bool ~ locfit::lp(admission_week,nn=nn,deg=deg),
#'                            data = tmpdf,family="qbinomial", link="logit")
#'       tmp = stats::preplot(fit,newdata=weeks,se.fit = TRUE,band="local")
#'       t = tmp$tr
#'       out = out %>% dplyr::bind_rows(
#'         tibble::tibble(
#'           class = level,
#'           admission_week = weeks,
#'           admission_date = .weeks_to_date(weeks),
#'           lower = .opt(t(stats::qnorm(0.05,tmp$fit,tmp$se.fit))),
#'           median = t(tmp$fit), #only because fit is normally distributed so mean=median
#'           upper = .opt(t(stats::qnorm(0.95,tmp$fit,tmp$se.fit)))
#'         )
#'       )
#'     }
#'   }
#'   out = out %>% dplyr::mutate(class = factor(class, levels(admissions$class)))
#'   return(out)
#'
#' }
#'
