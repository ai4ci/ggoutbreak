# Generalised Poisson ----
# https://github.com/gamlss-dev/gamlss.dist/tree/main
# theta = E_X/V_X
# sigma = (1/sqrt(theta) - 1) / mu
# sigma = (1/sqrt(E_X/V_X) - 1) / E_X
#

#' Generalised poisson
#'
#' @param x vector of (non-negative integer) quantiles
#' @param mu vector of positive `mu`
#' @param sigma vector of positive dispersion parameter `sigma` <1 is under-dispersion.
#' @param p vector of probabilities
#' @param q vector of quantiles
#' @param n number of random values to return
#' @param log,log.p logical; if TRUE, probabilities p are given as `log(p)`
#' @param lower.tail logical; if TRUE (default), probabilities are `P[X <= x]` otherwise, `P[X > x]`
#' @param max.value a constant, set to the default value of 10000 for how far the algorithm  should look for q
#'
#' @return a vector of probabilities, quantiles, densities or samples.
#' @name genpois
#' @examples
#'
#' pgenpois(seq(0,1,0.1), sigma = 1)
#' dwedge(seq(0,1,0.1), a=1)
#' qwedge(c(0.25,0.5,0.75), a=-1)
#'
#' stats::cor(
#'   stats::qnorm(rwedge(1000, a=2)),
#'   stats::qnorm(rwedge(1000, a=-2))
#' )
#'
NULL

#' @describeIn genpois density function
#' @export
dgenpois = function(x, mu = 1, sigma = 1, log = FALSE) {
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", ""))
  # if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
  interfacer::recycle(x,mu,sigma)

  logL <- x*log(mu/(1+sigma*mu))+(x-1)*log(1+sigma*x)+(-mu*(1+sigma*x))/(1+sigma*mu)-lgamma(x+1)
  Lik <- if (log) logL else exp(logL)
  if (length(sigma)>1) fy <- ifelse(sigma>0.0000001,
                                    Lik,
                                    dpois(x, lambda = mu, log = log))
  else fy <- if (sigma<0.0001) dpois(x, lambda = mu, log = log)
  else Lik
  fy <-ifelse(x < 0, 0, fy)
  fy
}

#' @describeIn genpois cumulative probability function
#' @export
pgenpois = function(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", ""))
  # if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
  ly <- length(q)
  FFF <- rep(0,ly)
  nsigma <- rep(sigma, length = ly)
  nmu <- rep(mu, length = ly)
  j <- seq(along=q)
  for (i in j)
  {
    y.y <- q[i]
    mm <- nmu[i]
    nsig <- nsigma[i]
    allval <- seq(0,y.y)
    pdfall <- dgenpois(allval, mu = mm, sigma = nsig,  log = FALSE)
    FFF[i] <- sum(pdfall)
  }
  cdf <- FFF
  cdf <- if(lower.tail==TRUE) cdf else 1-cdf
  cdf <- if(log.p==FALSE) cdf else log(cdf)
  #  cdf
  if (length(sigma)>1) cdf <- ifelse(sigma>0.0001,
                                     cdf,
                                     ppois(q, lambda = mu, log.p = log.p, lower.tail=lower.tail))
  else cdf <- if (sigma<0.0001)   ppois(q, lambda = mu, log.p = log.p, lower.tail=lower.tail)
  else cdf
  cdf <-ifelse(q < 0, 0, cdf)
  cdf
}

#' @describeIn genpois quantile function
#' @export
qgenpois = function(p, mu = 1, sigma = 1,  lower.tail = TRUE, log.p = FALSE, max.value = 10000) {
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", ""))
  # if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
  #  if (any(p < 0) | any(p > 1.0001))  stop(paste("p must be between 0 and 1", "\n", ""))
  if (log.p==TRUE) p <- exp(p) else p <- p
  if (lower.tail==TRUE) p <- p else p <- 1-p
  ly <- length(p)
  QQQ <- rep(0,ly)
  nsigma <- rep(sigma, length = ly)
  nmu <- rep(mu, length = ly)
  for (i in seq(along=p))
  {
    cumpro <- 0
    if (p[i]+0.000000001 >= 1) QQQ[i] <- Inf
    else
    {
      for (j in seq(from = 0, to = max.value))
      {
        cumpro <-  pgenpois(j, mu = nmu[i], sigma = nsigma[i], log.p = FALSE)
        # else  cumpro+dSICHEL(j, mu = nmu[i], sigma = nsigma[i], nu = nnu[i], log = FALSE)# the above is faster
        QQQ[i] <- j
        if  (p[i] <= cumpro ) break
      }
    }
  }
  QQQ[p == 0] <- 0
  QQQ[p == 1] <- Inf
  QQQ[p <  0] <- NaN
  QQQ[p >  1] <- NaN
  return(QQQ)
}

#' @describeIn genpois random number generator
#' @export
rgenpois =  function(n, mu = 1, sigma = 1, max.value = 10000) {
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", ""))
  # if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", ""))
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))
  n <- ceiling(n)
  p <- runif(n)
  r <- qgenpois(p, mu=mu, sigma=sigma, , max.value = max.value)
  r
}



