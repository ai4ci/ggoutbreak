# ---
# repo: ai4ci/ggoutbreak
# file: standalone-distributions.R
# last-updated: 2024-10-10
# license: https://unlicense.org
# imports:
# - dplyr
# - stats
# - interfacer
# ---

#' Wedge distribution
#'
#' The wedge distribution has a domain of 0 to 1 and has a linear probability
#' density function over that domain.
#'
#' The `rwedge` can be combined with quantile functions to
#' skew standard distributions, or introduce correlation or down weight
#' certain parts of the distribution.
#'
#' @param n number of observations
#' @param x vector of quantiles
#' @param q vector of quantiles
#' @param p vector of probabilities
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are `P[X<=x]`
#'   otherwise `P[X>x]`.
#' @param a a gradient from -2 (left skewed) to 2 (right skewed)
#' @return a vector of probabilities, quantiles, densities or samples.
#' @name wedge
#' @concept distributions
#' @examples
#'
#' pwedge(seq(0,1,0.1), a=1)
#' dwedge(seq(0,1,0.1), a=1)
#' qwedge(c(0.25,0.5,0.75), a=-1)
#'
#' stats::cor(
#'   stats::qnorm(rwedge(1000, a=2)),
#'   stats::qnorm(rwedge(1000, a=-2))
#' )
NULL


#' @inherit wedge
#' @concept distributions
#' @export
pwedge = function(q, a, log.p = FALSE) {
  a = rep(a, length.out = length(q))
  b = (1 - a / 2)
  y = dplyr::case_when(
    a > 2 | a < -2 ~ NaN,
    q < 0 | q > 1 ~ NaN,
    TRUE ~ a * q + b
  )
  if (log.p) {
    return(log(y))
  }
  return(y)
}

#' @inherit wedge
#' @concept distributions
#' @export
dwedge = function(x, a, lower.tail = TRUE, log = FALSE) {
  a = rep(a, length.out = length(x))
  b = (1 - a / 2)
  y = dplyr::case_when(
    a > 2 | a < -2 ~ NaN,
    x < 0 | x > 1 ~ NaN,
    TRUE ~ a / 2 * x^2 + b * x
  )
  if (!lower.tail) {
    y = 1 - y
  }
  if (log) {
    return(log(y))
  }
  return(y)
}

#' @inherit wedge
#' @concept distributions
#' @export
qwedge = function(p, a, lower.tail = TRUE, log.p = FALSE) {
  a = rep(a, length.out = length(p))
  b = (1 - a / 2)
  if (log.p) {
    p = exp(p)
  }
  if (!lower.tail) {
    p = 1 - p
  }
  x = dplyr::case_when(
    a > 2 | a < -2 ~ NaN,
    p < 0 | p > 1 ~ NaN,
    TRUE ~
      ifelse(
        a > 0,
        -(b / a) + sqrt(2 * p / a + (b^2) / (a^2)),
        -(b / a) - sqrt(2 * p / a + (b^2) / (a^2))
      )
  )
  return(x)
}

#' @inherit wedge
#' @concept distributions
#' @export
rwedge = function(n, a) {
  return(qwedge(stats::runif(n), a))
}

## Bernoulli / Categorical ----

#' A random Bernoulli sample as a logical value
#'
#' @inheritParams reparam-dist
#' @return a vector of logical values of size `n`
#'
#' @export
#' @concept distributions
#'
#' @examples
#' table(rbern(100, 0.25))
rbern = function(n, prob) {
  as.logical(stats::runif(n) < prob)
}

#' Sampling from the multinomial equivalent of the Bernoulli distribution
#'
#' @param n a sample size
#' @param prob a (optionally named) vector of probabilities that will be
#'   normalised to sum to 1
#' @param factor if not FALSE then factor levels will either be taken from the
#'   names of `prob` first, or if this is a character vector from this.
#' @return a vector of random class labels of length `n`. Labels come from names
#'   of `prob` or from a character vector in `factor`.
#'
#' @export
#' @concept distributions
#' @examples
#'
#' prob = c("one"=0.1,"two"=0.2,"seven"=0.7)
#' table(rcategorical(1000,prob))
#' rcategorical(10,prob,factor=TRUE)
#' rcategorical(10,rep(1,26),factor=letters)
#'
rcategorical = function(n, prob, factor = FALSE) {
  if (length(n) > 0) {
    prob = prob / sum(prob)
    to_test = cumsum(prob)
    against = stats::runif(
      if (is.numeric(n) && length(n) == 1) n else length(n)
    )
    tmp = matrix(
      sapply(to_test, FUN = function(x) x <= against),
      nrow = length(against)
    )
    which = apply(tmp, MARGIN = 1, sum) + 1
  } else {
    which = numeric()
  }
  if (isFALSE(factor)) {
    if (is.null(names(prob))) {
      return(which)
    } else {
      return(names(prob)[which])
    }
  } else {
    if (is.null(names(prob))) {
      if (is.character(factor)) {
        if (length(factor) != length(prob)) {
          stop(
            "If levels are given in `factor`, it must be the same length as `prob`."
          )
        }
        return(factor(factor[which], levels = factor))
      } else {
        stop(
          "if a factor is wanted then either `prob` must be named or the levels must be provided as a character vector in `factor`"
        )
      }
    } else {
      if (is.character(factor)) {
        warning(
          "Factor levels are supplied in both names of `prob` and `factor` - defaulting to names of `prob`"
        )
      }
      return(factor(names(prob)[which], levels = names(prob)))
    }
  }
}


## Beta parametrisations ----

#' Re-parametrised distributions
#'
#' @param n number of observations
#' @param x vector of quantiles
#' @param q vector of quantiles
#' @param p vector of probabilities
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are `P[X<=x]`
#'   otherwise `P[X>x]`.
#' @param convex Show a warning if the distribution selected is not a convex
#'    function
#' @param prob the mean probability (vectorised)
#' @param kappa a coefficient of variation. where 0 is no variability and 1 is
#'   maximally variability (vectorised)
#' @param mean the mean value on the true scale (vectorised)
#' @param sd the standard deviation on the true scale (vectorised)
#'
#' @concept distributions
#'
#' @name reparam-dist
NULL

#' @inherit stats::rbeta return title description
#' @seealso [stats::rbeta()]
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' rbeta2(3, c(0.1,0.5,0.9),0.1)
rbeta2 = function(n, prob, kappa) {
  params = .reparam_beta(prob, kappa)
  return(
    dplyr::case_when(
      prob == 1 | prob == 0 ~ prob,
      kappa == 0 ~ prob,
      TRUE ~ stats::rbeta(n, params$alpha, params$beta)
    )
  )
}

#' @inherit stats::dbeta return title description
#' @seealso [stats::dbeta()]
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' dbeta2(c(0.25,0.5,0.75), 0.5, 0.25)
dbeta2 = function(x, prob, kappa, log = FALSE) {
  params = .reparam_beta(prob, kappa)
  return(
    dplyr::case_when(
      prob == 1 | prob == 0 ~ ifelse(x < prob, 0, 1),
      kappa == 0 ~ ifelse(x < prob, 0, 1),
      TRUE ~ stats::dbeta(x, params$alpha, params$beta, log = log)
    )
  )
}

#' @inherit stats::pbeta return title description
#' @seealso [stats::pbeta()]
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' pbeta2(c(0.25,0.5,0.75), 0.5, 0.25)
pbeta2 = function(q, prob, kappa, lower.tail = TRUE, log.p = FALSE) {
  params = .reparam_beta(prob, kappa)
  return(
    dplyr::case_when(
      prob == 1 | prob == 0 ~ ifelse(q == prob, Inf, 0),
      kappa == 0 ~ ifelse(q == prob, Inf, 0),
      TRUE ~
        stats::pbeta(
          q,
          params$alpha,
          params$beta,
          lower.tail = lower.tail,
          log.p = log.p
        )
    )
  )
}

#' @inherit stats::qbeta return title description
#' @seealso [stats::qbeta()]
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' qbeta2(c(0.25,0.5,0.75), 0.5, 0.25)
qbeta2 = function(p, prob, kappa, lower.tail = TRUE, log.p = FALSE) {
  params = .reparam_beta(prob, kappa)
  return(
    dplyr::case_when(
      prob == 1 | prob == 0 ~ prob,
      kappa == 0 ~ prob,
      TRUE ~
        stats::qbeta(
          p,
          params$alpha,
          params$beta,
          lower.tail = lower.tail,
          log.p = log.p
        )
    )
  )
}


.reparam_beta = function(p, kappa) {
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop("p is a probability and must be between 0 and 1")
  }
  if (any(kappa < 0 | kappa > 1, na.rm = TRUE)) {
    stop("kappa must be between 0 (no variability) to 1 (max variability)")
  }

  if (length(kappa) != length(p) & length(kappa) != 1) {
    stop("kappa must be compatible length with p.")
  }
  kappa = rep(kappa, length.out = length(p))

  # for beta distribution to be unimodal 3 conditions have to be true
  # alpha > 1 and beta > 1 and alpha+beta>2
  # this puts constraints on kappa.
  kmax1 = sqrt((1 - p) / (1 + p))
  kmax2 = sqrt(1 / (2 * p - p^2) - 1)
  kmax3 = sqrt((1 - p) / (3 * p))
  kmax = pmin(kmax1, kmax2, kmax3)

  # kappa is actually 0 to 1 where 1 is the most dispersed it is possible to be
  # with a mean of p (and still be unimodal)
  kappa2 = kmax * kappa

  return(list(
    alpha = ((1 - p) / (kappa2^2 * p) - 1) * p,
    beta = ((1 - p) / (kappa2^2 * p) - 1) * (1 - p)
  ))
}

## Log normal ----

.reparam_lnorm = function(mean, sd) {
  if (any(mean < 0, na.rm = TRUE)) {
    stop("means must be greater than zero")
  }
  if (any(sd < 0, na.rm = TRUE)) {
    stop("sds must be greater than zero")
  }
  var = sd^2
  return(list(
    mu = log(mean / sqrt(var / (mean^2) + 1)),
    sigma = sqrt(log(var / (mean^2) + 1))
  ))
}

#' @inherit stats::dlnorm
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' dlnorm2(seq(0,4,0.25), 2, 1)
dlnorm2 = function(x, mean = 1, sd = sqrt(exp(1) - 1), log = FALSE) {
  params = .reparam_lnorm(mean, sd)
  stats::dlnorm(x, meanlog = params$mu, sdlog = params$sigma, log = log)
}

#' @inherit stats::plnorm
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' plnorm2(seq(0,4,0.25), 2, 1)
plnorm2 = function(
  q,
  mean = 1,
  sd = sqrt(exp(1) - 1),
  lower.tail = TRUE,
  log.p = FALSE
) {
  params = .reparam_lnorm(mean, sd)
  stats::plnorm(
    q,
    meanlog = params$mu,
    sdlog = params$sigma,
    lower.tail = lower.tail,
    log.p = log.p
  )
}

#' @inherit stats::qlnorm
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' qlnorm2(c(0.25,0.5,0.72), 2, 1)
qlnorm2 = function(
  p,
  mean = 1,
  sd = sqrt(exp(1) - 1),
  lower.tail = TRUE,
  log.p = FALSE
) {
  params = .reparam_lnorm(mean, sd)
  stats::qlnorm(
    p,
    meanlog = params$mu,
    sdlog = params$sigma,
    lower.tail = lower.tail,
    log.p = log.p
  )
}

#' @inherit stats::rlnorm
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' rlnorm2(10, 2, 1)
rlnorm2 = function(n, mean = 1, sd = sqrt(exp(1) - 1)) {
  params = .reparam_lnorm(mean, sd)
  stats::rlnorm(n, meanlog = params$mu, sdlog = params$sigma)
}

# Gamma ----

.reparam_gamma = function(mean, sd, convex = TRUE) {
  if (any(mean < 0, na.rm = TRUE)) {
    stop("means must be greater than zero")
  }
  if (any(sd < 0, na.rm = TRUE)) {
    stop("sds must be greater than zero")
  }
  if (any(sd > mean, na.rm = TRUE) & convex) {
    warning("gammas with sd > mean are not convex")
  }
  var = sd^2
  return(list(
    shape = mean^2 / var, # if shape < 1 then is concave
    rate = mean / var
  ))
}

#' @inherit stats::rgamma return title description
#' @seealso [stats::rgamma()]
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' rgamma2(10, 2, 1)
rgamma2 = function(n, mean, sd = sqrt(mean), convex = TRUE) {
  params = .reparam_gamma(mean, sd, convex = convex)
  return(
    stats::rgamma(n, params$shape, params$rate)
  )
}

#' @inherit stats::qgamma return title description
#' @seealso [stats::qgamma()]
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' qgamma2(c(0.25,0.5,0.75), 2, 1)
qgamma2 = function(
  p,
  mean,
  sd = sqrt(mean),
  lower.tail = TRUE,
  log.p = FALSE,
  convex = TRUE
) {
  params = .reparam_gamma(mean, sd, convex = convex)
  return(
    stats::qgamma(
      p,
      params$shape,
      params$rate,
      lower.tail = lower.tail,
      log.p = log.p
    )
  )
}

#' @inherit stats::dgamma return title description
#' @seealso [stats::dgamma()]
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' dgamma2(seq(0,4,0.25), 2, 1)
dgamma2 = function(x, mean, sd = sqrt(mean), log = FALSE, convex = TRUE) {
  params = .reparam_gamma(mean, sd, convex = convex)
  return(
    stats::dgamma(x, params$shape, params$rate, log = log)
  )
}

#' @inherit stats::pgamma return title description
#' @seealso [stats::pgamma()]
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' pgamma2(seq(0,4,0.25), 2, 1)
pgamma2 = function(
  q,
  mean,
  sd = sqrt(mean),
  lower.tail = TRUE,
  log.p = FALSE,
  convex = TRUE
) {
  params = .reparam_gamma(mean, sd, convex = convex)
  return(
    stats::pgamma(
      q,
      params$shape,
      params$rate,
      lower.tail = lower.tail,
      log.p = log.p
    )
  )
}

# Negative binomial; ----

.reparam_nbinom = function(mean, sd) {
  if (any(mean < 0, na.rm = TRUE)) {
    stop("means must be greater than zero")
  }
  if (any(sd < 0, na.rm = TRUE)) {
    stop("sds must be greater than zero")
  }
  if (any(sd < sqrt(mean), na.rm = TRUE)) {
    stop("sds must be greater than the sqrt of the mean")
  }
  var = sd^2
  return(list(
    size = mean^2 / (var - mean), # if shape < 1 then is concave
    prob = mean / var
  ))
}

#' @inherit stats::rnbinom
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' rnbinom2(10, 5, sqrt(5))
rnbinom2 = function(n, mean, sd = sqrt(mean)) {
  params = .reparam_nbinom(mean, sd)
  return(
    stats::rnbinom(n, params$size, params$prob)
  )
}

#' @inherit stats::qnbinom
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' qnbinom2(c(0.25,0.5,0.75), 5, sqrt(5))
qnbinom2 = function(
  p,
  mean,
  sd = sqrt(mean),
  lower.tail = TRUE,
  log.p = FALSE
) {
  params = .reparam_nbinom(mean, sd)
  return(
    stats::qnbinom(
      p,
      params$size,
      params$prob,
      lower.tail = lower.tail,
      log.p = log.p
    )
  )
}

#' @inherit stats::dnbinom
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' dnbinom2(0:5, 2, sqrt(2))
dnbinom2 = function(x, mean, sd = sqrt(mean), log = FALSE) {
  params = .reparam_nbinom(mean, sd)
  return(
    stats::dnbinom(x, params$size, params$prob, log = log)
  )
}

#' @inherit stats::pnbinom
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' pnbinom2(0:5, 2, sqrt(2))
pnbinom2 = function(
  q,
  mean,
  sd = sqrt(mean),
  lower.tail = TRUE,
  log.p = FALSE
) {
  params = .reparam_nbinom(mean, sd)
  return(
    stats::pnbinom(
      q,
      params$size,
      params$prob,
      lower.tail = lower.tail,
      log.p = log.p
    )
  )
}

# Discrete gamma ----
# A hacky solution for under-dispersion this uses a gamma distribution rounded
# to integer values. It is parameterised with mean and sd (or a dispersion
# parameter where 1 is poisson dispersion, i.e. sd = sqrt(mean) and )

#' Random count data from a discrete gamma distribution
#'
#' For count data that is under-dispersed we can use a gamma distribution rounded
#' to the nearest whole number. This is the same as the method of discretisation
#' in `make_gamma_ip`, and suits delay distributions where there is less variability
#' than can be represented with a Poisson or negative binomial distribution.
#'
#' @returns an integer valued vector from a gamma distribution.
#' @seealso [stats::rgamma()]
#' @inheritParams reparam-dist
#'
#' @export
#' @concept distributions
#'
#' @examples
#' rdiscgamma(10, 2, 1)
rdiscgamma = function(n, mean, sd, kappa) {
  interfacer::resolve_missing(
    kappa = sd / mean,
    mean = sd / kappa,
    sd = mean * kappa
  )
  params = .reparam_gamma(mean, sd, convex = FALSE)
  return(
    floor(stats::rgamma(n, params$shape, params$rate) + 0.5)
  )
}
