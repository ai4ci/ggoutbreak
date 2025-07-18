
## Utility ----



# return NA for errors
.opt = function(expr) tryCatch(expr,error=function(e) NA_real_)

# check columns are present in df
.has_cols = function(df, ...) {
  cols = unlist(rlang::list2(...))
  if (is.symbol(cols)) cols = rlang::as_label(cols)
  return(all(cols %in% colnames(df)))
}

.result_from_fit = function(new_data, type, fit = new_data[[paste0(type,".fit")]], se.fit = new_data[[paste0(type,".se.fit")]], inv = function(x) x, qfn = ~ stats::qnorm(.x, fit, se.fit)) {

  qfn = rlang::as_function(qfn)

  .opt_inv = function(x) {
    purrr::map_dbl(x, ~ tryCatch(inv(.x) %>% ifelse(is.finite(.), ., NA_real_), error=function(e) NA_real_))
  }

  return(new_data %>% dplyr::mutate(
    !!(paste0(type,".fit")) := unname(fit),
    !!(paste0(type,".se.fit")) := unname(se.fit),
    !!(paste0(type,".0.025")) := .opt_inv(qfn(0.025)),
    !!(paste0(type,".0.05")) := .opt_inv(qfn(0.05)),
    !!(paste0(type,".0.25")) := .opt_inv(qfn(0.25)),
    !!(paste0(type,".0.5")) := .opt_inv(qfn(0.5)),
    !!(paste0(type,".0.75")) := .opt_inv(qfn(0.75)),
    !!(paste0(type,".0.95")) := .opt_inv(qfn(0.95)),
    !!(paste0(type,".0.975")) := .opt_inv(qfn(0.975))
  ))
}

.null_result = function(new_time, ...) {
  dots = rlang::list2(...)
  out = tibble::tibble(time = new_time)
  for (k in names(dots)) {
    v = dots[[k]]
    out = out %>% dplyr::mutate(
      !!paste0(k,".0.025") := v[1],
      !!paste0(k,".0.05") := NA,
      !!paste0(k,".0.25") := NA,
      !!paste0(k,".0.5") := v[2],
      !!paste0(k,".0.75") := NA,
      !!paste0(k,".0.95") := NA,
      !!paste0(k,".0.975") := v[3]
    )
  }
  return(out)
}


.has_time = function(df) {
  return(.has_cols(df, "time") && is.time_period(df$time))
}


# .stop_if_not_daily(as.time_period(Sys.Date()+1:10))
# .stop_if_not_daily(as.time_period(Sys.Date()+1:10+0.1))
# .stop_if_not_daily(as.time_period(Sys.Date()+(1:10*7)))
# .stop_if_not_daily(as.time_period(Sys.Date()+(1:70/7),unit = "1 day"))
.stop_if_not_daily = function(time) {
  param = deparse(substitute(time))
  unit = attr(time,"unit")
  if (is.null(unit)) stop(sprintf("`%s` not a time period",param),call. = FALSE)
  if (unit != lubridate::days(1)) stop(sprintf("unit of time period `%s` is not 1 day.",param),call. = FALSE)
  time = sort(unique(time))
  diff = stats::na.omit(time-dplyr::lag(time))
  if (any(abs(diff - round(diff)) > sqrt(.Machine$double.eps)))
    stop(sprintf("time period `%s` does not have a daily interval.",param),call. = FALSE)
}

# preprocess_data = function(df, multinom, ...,  date_col = "date") {
#
#   grps = df %>% dplyr::groups()
#
#   # Is this a dated line list? i.e. a datafram with a date, or a time, but no count:
#   if (!.has_cols(df, "count")) {
#
#     if (.has_time(df) && !.has_cols(df, "date")) {
#       # need a date column to summarise
#       df = df %>% dplyr::mutate(date = as.Date(time))
#     } else {
#       dateVar = rlang::ensym(date_col)
#       df = df %>% dplyr::rename(date = !!dateVar)
#     }
#
#     # this does everything:
#     df = df %>% time_summarise(...)
#
#   } else {
#
#     # this is a time series already with a count column
#     # could check here it is complete and fill it?
#
#     # make sure there is a time column.
#     if (!.has_time(df)) {
#       dateVar = rlang::ensym(date_col)
#       df = df %>% dplyr::mutate(time = as.time_period(!!dateVar, ...))
#     }
#
#     if (.has_cols(df, "class") && !.has_cols(df, "denom")) {
#       df = df %>%
#         dplyr::group_by(!!!grps, time) %>%
#         dplyr::mutate(denom = sum(count)) %>%
#         dplyr::group_by(!!!grps)
#     }
#
#   }
#
#   if (!multinom) {df = df %>% dplyr::group_by(class, .add = TRUE)}
#
#   return(df)
# }

.logit = function(x) {
  log(x/(1-x))
}

.expit = function(x) {
  return(1/(1+exp(-x)))
}


.callid = function() {
  lobstr::obj_addr(sys.frame(which=1))
}

.message_once = function(...) {
  msg=paste0(...)
  if (!.messaged(msg)) message(msg)
}

.warn_once = function(...) {
  msg=paste0(...)
  if (!.messaged(msg)) warning(msg)
}

cache_env <- new.env(parent = emptyenv())
cache_env$map = list()

.messaged = function(msg) {
  id = .callid()
  sent = cache_env$map[[id]]
  if (is.null(sent)) {
    cache_env$map[[id]] = msg
    return(FALSE)
  }
  if (!msg %in% sent) {
    cache_env$map[[id]] = c(sent,msg)
    return(FALSE)
  }
  return(TRUE)
}



