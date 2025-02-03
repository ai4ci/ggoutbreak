#' @noRd
#' from a set of infector times (t) generate a set of
#' infectee times, accounting for changes in the R_t over the
#' future and the infectivity profile.
#' @examples
#' changes = tibble::tibble(
#'   t = c(0,40),
#'   R = c(2.5,0.8)
#' )
#' fn_Rt = cfg_step_fn(changes)
#' fn_quantile_ip = .ip_quantile_function(ganyani_ip)
#' hist(fn_quantile_ip(runif(10000)),breaks = 100)
#'
#' omega_t = summarise_ip(ganyani_ip) %>% pull(probability)
#' ip_length = omega_t %>% length()
#' t=rep(0,10000)
#' hist(tmp,breaks=100)
#' t=40-5
#' tmp = .sample_generation_time(rep(t,10000),ip_length, fn_quantile_ip, fn_Rt)
#' sum(fn_Rt(t:t+ip_length)*omega_t)
#' hist(tmp,breaks=100)
.sample_generation_time = function(t, ip_length, fn_quantile_ip, fn_Rt) {
  if (length(t) == 0) return(t)
  tmp = t + fn_quantile_ip(
    # runif weighted to lookahead R_t
    .rtrans(
      xmin = t,
      xmax = t + ip_length,
      pdf = fn_Rt
    ))
  # if (any(is.na(tmp))) browser()
  return(tmp)
}

# TODO: underdispersed poisson:
# https://github.com/lotze/COMPoissonReg/blob/master/R/cmp.R

#' Generate a weighted random samples from a subset of a distribution
#'
#' This is used to weight future R_t changes when determining the
#' time distribution of infections. This weighted random number is used as
#' a quantile for the inverse of the infectivity profile CDF.
#' This function is complex as it is vectorised on xmin and xmax.
#' It evaluates the CDF for the maximal range of xmin and xmax
#' It produces an inverse approximation for the whole range.
#' It then maps a quantile into the subdomain for each individual combination
#' of xmin and xmax, calculates the X value and maps it back to a quantile
#' using xmax and xmin. The result is that we get a RNG for the next `x` days of
#' a timeseries that is weighted by the in this case `R_t` values over this period.
#' This is needed because when the branching process generates a new set of
#' infections for the average of the `R_t` over the infectious period, the time
#' distribution of these cases needs to be weighted by the `R_t` values in the
#' future, to account for changes in behaviour. The output of this RNG is fed into
#' the infectivity profile quantile function to get a future date for infections.
#'
#' @param xmin A vector of the minimum of the domain e.g. start time
#' @param xmax A vector of the maximum of the domain e.g. end time
#' @param pdf A vectorised function that gives a value for a time period. e.g. R(t)
#' @param n The number of samples you want (must be some multiple of the length of xmin/xmax)
#'
#' @return A set of random samples between 0 and 1 that are weighted to the distribution
#'   of the `pdf` function between individual pairs of `xmin` and `xmax`. This will be
#'   a matrix, with one row per `xmin`/`xmax` entry and columns to make up to `n`
#' @keywords internal
#' @noRd
#'
#' @examples
#' changes = tibble::tibble(t = c(0,20,40,60,80,110), R = c(1.8,1.5,0.2,1.5,0.8,1.2))
#' pdf = cfg_step_fn(changes)
#' # 10,000 samples for each
#' tmp = .rtrans(xmin=c(0,40,30), xmax=c(10,50,50), pdf=pdf, n=10000*3)
#' hist(tmp[1,]) # should be uniform
#' hist(tmp[2,]) # also uniform
#' hist(tmp[3,]) # step function at 0.5
.rtrans = function(xmin, xmax, pdf, n = length(xmin)) {
  mn = min(xmin)
  mx = max(xmax)
  # Set up inverse function
  x = seq(mn,mx,length.out=(mx-mn+1)*100)
  p = pdf(head(x,-1))
  sumP = c(0,cumsum(p/sum(p)))
  cdf = approxfun(x=x, y=sumP, rule = 2,method = "linear", ties="ordered")
  qf = approxfun(x=sumP, y=x, rule = 2,method = "linear", ties="ordered")

  # Scale the input to be within the range of xmin to
  q0 = runif(n)
  qmin = cdf(xmin)
  qmax = cdf(xmax)
  q1 = q0*(qmax-qmin)+qmin

  # evaluate the quantile and transform it to range form 0 to 1
  x1 = qf(q1)
  x0 = (x1-xmin)/(xmax-xmin)

  # matrix(x1,nrow=length(xmin), byrow=FALSE)[,1:20]
  # each row should be the same.

  return(matrix(x0,nrow=length(xmin), byrow=FALSE))
}
