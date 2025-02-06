# package depends
# c("stats") %>% lapply(usethis::use_package)
#TODO: make an internal function?

#' The cumulative density function of a mixture of lognormal distributions
#'
#' @param q vector of quantiles.
#' @param mus a vector of lognormal distribution mus (log scale)
#' @param sigmas  a vector of lognormal distribution sigmas (log scale)
#' @param weights  a vector of weights
#' @param na.rm remove distributions which have NA for mean or sd
#'
#' @return the pdf of the mixture distribution.
#' @export
#' @examples
#' pmixlnorm(q=c(2,20), mus=c(10,13,14), sigmas=c(1,1,2), weights=c(2,2,3))
pmixlnorm = function(q, mus, sigmas, weights=rep(1,length(mus)), na.rm=FALSE) {

  r = p = NULL  # remove global binding note

  if(length(sigmas) != length(mus)) stop("mus and sigmas vectors must be the same length")

  if (na.rm == TRUE) {
    filt = is.na(mus) | is.na(sigmas)
    mus = mus[filt]
    sigmas = sigmas[filt]
  }

  if (any(is.na(mus)) | length(mus)==0 | any(is.na(sigmas)) | length(sigmas)==0 ) {
    return(rep(NA_real_, length(r)))
  }

  if (.allEqual(mus) & .allEqual(sigmas)) {
    return(stats::plnorm(p,mus[1],sigmas[1]))
  }

  m = weights/sum(weights) * sapply(q, function(x) stats::plnorm(x, mus, sigmas))
  apply(m,MARGIN=2,sum)

}

.allEqual = function(mus, tol = 10^{-10}) all(sapply(mus, function(x) abs(mus - x) < tol))



#' A quantile function for a mixture of lognormal distributions
#'
#' @param p vector of probabilities.
#' @param mus a vector of lognormal distribution mus (log scale)
#' @param sigmas  a vector of lognormal distribution sigmas (log scale)
#' @param weights  a vector of weights
#' @param na.rm remove distributions with NA values for mean or sd
#'
#' @return the value of the yth quantile
#' @export
#'
#' @examples
#' qmixlnorm(p=c(0.025,0.5,0.975), mus=c(0.10,0.13,0.14), sigmas=c(1,1,2))
#' samples = c(
#'   rlnorm(10000,0.1,1),
#'   rlnorm(10000,0.13,1),
#'   rlnorm(10000,0.14,2)
#' )
#' quantile(samples,c(0.025,0.5,0.975))
qmixlnorm = function(p, mus, sigmas, weights=rep(1,length(mus)), na.rm=FALSE) {

  if(length(sigmas) != length(mus)) stop("mus and sigmas vectors must be the same length")

  if (na.rm == TRUE) {
    filt = is.na(mus) | is.na(sigmas)
    mus = mus[filt]
    sigmas = sigmas[filt]
  }

  if (any(is.na(mus)) | length(mus)==0 | any(is.na(sigmas)) | length(sigmas)==0 ) {
    solve = rep(NA_real_, length(p))
    names(solve) = paste0("Q.",p)
    return(solve)
  }

  if (.allEqual(mus) & .allEqual(sigmas)) {
    return(stats::qnorm(p,mus[1],sigmas[1]))
  }
  # the min minmax below is computed to supply a range to the solver
  # the solution must be between the min and max
  # quantile of the mixed distributions
  minmax <- range(sapply(p, function(x) stats::qlnorm(x,mus,sigmas)))
  solve = tryCatch({
    sapply(p, function(qPrime) stats::uniroot(function(x) pmixlnorm(x,mus,sigmas,weights)-qPrime,interval = minmax,tol = 10^{-16})$root)
  }, error = browser)
  names(solve) = paste0("Q.",p)
  return(solve)
}
