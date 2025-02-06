# Log Normal ----
# N.B. these functions have to beable to do two things
# firstly they must be vectorised on their first input
# particularly the pmixXX functions as this is needed
# for efficient use of uniroot
# Secondly their parameter inputs should either be a vector
# of the mixture parameters or a list of vectors,
# In the latter case which is needed for getting the quantiles
# in the tidy dataframe setting the first parameter will be
# generally unique.

# must be vectorised on x
.pmixlnorm = function(x, meanlog = 0, sdlog = 1) {

  if (is.list(meanlog)) {
    interfacer::recycle(x,meanlog,sdlog)
    return(purrr::map_dbl(seq_along(x), function(i) {
      .pmixlnorm(x[i],meanlog[[i]],sdlog[[i]])
    }))
  }

  if (length(meanlog) == 1 || (
    length(unique(meanlog))==1 && length(unique(sdlog))==1
  )) return(stats::plnorm(x,meanlog[1],sdlog[1]))
  tmp = sapply(
    x,
    FUN = function(x2) {
      return(stats::plnorm(x2, meanlog = meanlog, sdlog = sdlog))
    }
  )
  apply(tmp,MARGIN=2,mean)
}

# solve
.qmixlnorm = function(p, meanlog = 0, sdlog = 1) {

  if (is.list(meanlog)) {
    interfacer::recycle(p,meanlog,sdlog)
    return(purrr::map_dbl(seq_along(p), function(i) {
      .pmixlnorm(p[i],meanlog[[i]],sdlog[[i]])
    }))
  }

  if (length(meanlog) == 1 || (
    length(unique(meanlog))==1 && length(unique(sdlog))==1
        )) return(stats::qlnorm(p,meanlog[1],sdlog[1]))
  sapply(p, function(p3) {
    minmax = range(stats::qlnorm(p3,meanlog,sdlog))
    cdf = function(x) .pmixlnorm(x, meanlog, sdlog) - p3
    stats::uniroot(
      cdf,
      lower=minmax[1],
      upper = minmax[2],
      tol = 0.01
      )$root
  })
}

# Gammas ----

# must be vectorised on x
.pmixgamma = function(x, shape, rate) {

  if (is.list(shape)) {
    interfacer::recycle(x,shape,rate)
    return(purrr::map_dbl(seq_along(x), function(i) {
      .pmixgamma(x[i],shape[[i]],rate[[i]])
    }))
  }

  if (length(shape) == 1 || (
    length(unique(shape))==1 && length(unique(rate))==1
  )) return(stats::pgamma(x,shape = shape[1],rate = rate[1]))
  tmp = sapply(
    x,
    FUN = function(x2) {
      return(stats::pgamma(x2, shape = shape, rate = rate))
    }
  )
  apply(tmp,MARGIN=2,mean)
}

# solve
# This function is vectorised on p and parameters. as mixture parameters
# must be a list of vectors.
.qmixgamma = function(p, shape, rate) {

  if (is.list(shape)) {
    interfacer::recycle(p,shape,rate)
    return(purrr::map_dbl(seq_along(p), function(i) {
      .qmixgamma(p[i],shape[[i]],rate[[i]])
    }))
  }

  if (length(shape) == 1 || (
    length(unique(shape))==1 && length(unique(rate))==1
  )) return(stats::qgamma(p,shape = shape[1],rate = rate[1]))
  sapply(p, function(p3) {
    minmax = range(stats::qgamma(p3,shape=shape,rate=rate))
    cdf = function(x) .pmixgamma(x, shape=shape, rate=rate) - p3
    stats::uniroot(
      cdf,
      lower=minmax[1],
      upper = minmax[2],
      tol = 0.01
    )$root
  })
}

# Normal ----

# must be vectorised on x
.pmixnorm = function(x, mean, sd) {

  if (is.list(mean)) {
    interfacer::recycle(x,mean,sd)
    return(purrr::map_dbl(seq_along(x), function(i) {
      .pmixnorm(x[i],mean[[i]],sd[[i]])
    }))
  }

  if (length(mean) == 1 || (
    length(unique(mean))==1 && length(unique(sd))==1
  )) return(stats::pnorm(x,mean = mean[1],sd = sd[1]))
  tmp = sapply(
    x,
    FUN = function(x2) {
      return(stats::pnorm(x2, mean=mean, sd=sd))
    }
  )
  apply(tmp,MARGIN=2,mean)
}

# solve
# This function is vectorised on p and parameters. as mixture parameters
# must be a list of vectors.
.qmixnorm = function(p, mean, sd) {

  if (is.list(mean)) {
    interfacer::recycle(p,mean,sd)
    return(purrr::map_dbl(seq_along(p), function(i) {
      .qmixnorm(p[i],mean[[i]],sd[[i]])
    }))
  }

  if (length(mean) == 1 || (
    length(unique(mean))==1 && length(unique(sd))==1
  )) return(stats::qnorm(p,mean=mean[1],sd=sd[1]))
  sapply(p, function(p3) {
    minmax = range(stats::qnorm(p3,mean=mean,sd=sd))
    cdf = function(x) .pmixnorm(x, mean=mean, sd=sd) - p3
    stats::uniroot(
      cdf,
      lower=minmax[1],
      upper = minmax[2],
      tol = 0.01
    )$root
  })
}

# meanlog = stats::rnorm(100,2,0.5)
# sdlog = stats::rlnorm(100,0.5,0.25)
# system.time(for(i in 1:1000) {.qmixlnorm(c(0.025,0.5,0.975), meanlog,sdlog)})

# .pmixlnorm(0:10, c(0,1,2,3), c(1,1,1,1))
# stats::qlnorm(p=c(0.025,0.975))
# .qmixlnorm(p=c(0.025,0.975))
# sapply(c(0.025,0.975), FUN=qlnorm, meanlog = c(0,1), sdlog = c(1,2))
# .qmixlnorm(p=c(0.025,0.975), meanlog = c(0,1), sdlog = c(1,2))

# .qmixlnorm(p=c(0.025,0.975), meanlog = c(1,1), sdlog = c(1,1))
# .qmixlnorm(p=c(0.025,0.975), meanlog = c(1), sdlog = c(1))
# stats::qlnorm(p=c(0.025,0.975), meanlog = c(1,1), sdlog = c(1,1))
#
# .qmixlnorm(p=c(0.025,0.975), meanlog = c(1,1), sdlog = c(1,2))

# .qmixlnorm(p=c(0.025,0.5,0.975), c(0,1,2,3), c(1,1,1,1))
# .qmixlnormfun(c(0,1,2,3), c(1,1,1,1))(c(0.025,0.5,0.975))
# stats::quantile(p=c(0.025,0.5,0.975), stats::rlnorm(100000, c(0,1,2,3), c(1,1,1,1)))

#TODO: Implement mixture distribution quantile algorithm
# There is an algorithm here that goes something like this.
# If we order the distributions by mean.
# probably the 0.25 quantile of the mixture is smaller than the 0.25 quantiles of the mean - Proof?
# probably the 0.25 quantile of the mixture is near than the mean of the 0.25 quantiles - Proof?
# There will be a set of distributions we can discard as irrelevant if the CDF at the current estimate is nearly zero
# suppose we discard the top 50 out of 100. Then we are no long looking for the 0.25 quantile but rather the 0.5 quantile
# as we have discarded 50% of the mass.
# we can do the same for the bottom, until we have a set that are actually relevant.
# This could speed things up a lot in the situation where there is little overlap between the mixtures and they are a set of
# spikes.


# CDF from arbitrary input ----
# We want to be able to calculate CDF measures from either samples
# standard distributions, or mixture distributions, in a unified
# dataframe friendly way.

# .cdf_generator(stats::rnorm(1000))
# .cdf_generator(list(stats::rnorm(1000),stats::rnorm(1000,2,1)))
# tmp = .cdf_generator(mean=2,sd=1)
# tmp2 = .cdf_generator(mean=list(c(1,2,3),c(3,4,5)),sd=list(c(1,1,1),c(2,2,2)))
# lapply(tmp2, function(fn) fn(0:10))
# logit_cdf = .cdf_generator(mean=0,sd=1,trans_fn=.logit)
# lapply(logit_cdf, \(f) f(seq(0,1,0.1)))
# trans here would be .logit or log,
.cdf_generator = function(..., trans_fn = ~ .x) {
  dots = rlang::list2(...)
  trans_fn = rlang::as_function(trans_fn)

  if (length(dots)==1) {
    # If called with 1 list only this is samples and we want an list of empirical CDFs
    samples = dots[[1]]
    if (!is.list(samples)) samples=list(samples)
    return(lapply(samples, .ecdf))
  }

  if (identical(names(dots),c("mean","sd"))) {
    if (is.list(dots[["mean"]])) fn = .pmixnorm
    else fn = stats::pnorm
  } else if (identical(names(dots),c("meanlog","sdlog"))) {
    if (is.list(dots[["meanlog"]])) fn = .pmixlnorm
    else fn = stats::plnorm
  } else if (identical(names(dots),c("shape","rate"))) {
    if (is.list(dots[["shape"]])) fn = .pmixgamma
    else fn = stats::pgamma
  }

  lapply(seq_along(dots[[1]]), function(i) {
    p1 = dots[[1]][[i]]
    p2 = dots[[2]][[i]]
    function(x) {
      # this is the CDF function:
      fn(trans_fn(x),p1,p2)
    }
  })

}


.ecdf = function(samples) {
  x = sort(unique(samples))
  y = as.vector(table(samples))
  y = cumsum(y)/(sum(y)+1)
  x=c(2*x[1]-x[2], x, 2*x[length(x)]-x[length(x)-1])
  y=c(0,y,1)
  sf = stats::approxfun(x,y,yleft=0,yright=1)
  return(sf)
}


