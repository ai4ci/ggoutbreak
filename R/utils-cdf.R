# # N.B. these functions have to be able to do two things
# # firstly they must be vectorised on their first input
# # particularly the pmixXX functions as this is needed
# # for efficient use of uniroot
# # Secondly their parameter inputs should either be a vector
# # of the mixture parameters or a list of vectors,
# # In the latter case which is needed for getting the quantiles
# # in the tidy dataframe setting the first parameter will be
# # generally unique.
#
# # must be vectorised on x
# .pmixlnorm = function(x, meanlog = 0, sdlog = 1) {
#
#   if (is.list(meanlog)) {
#     interfacer::recycle(x,meanlog,sdlog)
#     return(purrr::map_dbl(seq_along(x), function(i) {
#       .pmixlnorm(x[i],meanlog[[i]],sdlog[[i]])
#     }))
#   }
#
#   if (length(meanlog) == 1 || (
#     length(unique(meanlog))==1 && length(unique(sdlog))==1
#   )) return(stats::plnorm(x,meanlog[1],sdlog[1]))
#   tmp = sapply(
#     x,
#     FUN = function(x2) {
#       return(stats::plnorm(x2, meanlog = meanlog, sdlog = sdlog))
#     }
#   )
#   apply(tmp,MARGIN=2,mean)
# }
#
# # solve
# .qmixlnorm = function(p, meanlog = 0, sdlog = 1) {
#
#   if (is.list(meanlog)) {
#     interfacer::recycle(p,meanlog,sdlog)
#     return(purrr::map_dbl(seq_along(p), function(i) {
#       .pmixlnorm(p[i],meanlog[[i]],sdlog[[i]])
#     }))
#   }
#
#   if (length(meanlog) == 1 || (
#     length(unique(meanlog))==1 && length(unique(sdlog))==1
#         )) return(stats::qlnorm(p,meanlog[1],sdlog[1]))
#   sapply(p, function(p3) {
#     minmax = range(stats::qlnorm(p3,meanlog,sdlog))
#     cdf = function(x) .pmixlnorm(x, meanlog, sdlog) - p3
#     stats::uniroot(
#       cdf,
#       lower=minmax[1],
#       upper = minmax[2],
#       tol = 0.01
#       )$root
#   })
# }
#
#
# # must be vectorised on x
# .pmixgamma = function(x, shape, rate) {
#
#   if (is.list(shape)) {
#     interfacer::recycle(x,shape,rate)
#     return(purrr::map_dbl(seq_along(x), function(i) {
#       .pmixgamma(x[i],shape[[i]],rate[[i]])
#     }))
#   }
#
#   if (length(shape) == 1 || (
#     length(unique(shape))==1 && length(unique(rate))==1
#   )) return(stats::pgamma(x,shape = shape[1],rate = rate[1]))
#   tmp = sapply(
#     x,
#     FUN = function(x2) {
#       return(stats::pgamma(x2, shape = shape, rate = rate))
#     }
#   )
#   apply(tmp,MARGIN=2,mean)
# }
#
# # solve
# # This function is vectorised on p and parameters. as mixture parameters
# # must be a list of vectors.
# .qmixgamma = function(p, shape, rate) {
#
#   if (is.list(shape)) {
#     interfacer::recycle(p,shape,rate)
#     return(purrr::map_dbl(seq_along(p), function(i) {
#       .qmixgamma(p[i],shape[[i]],rate[[i]])
#     }))
#   }
#
#   if (length(shape) == 1 || (
#     length(unique(shape))==1 && length(unique(rate))==1
#   )) return(stats::qgamma(p,shape = shape[1],rate = rate[1]))
#   sapply(p, function(p3) {
#     minmax = range(stats::qgamma(p3,shape=shape,rate=rate))
#     cdf = function(x) .pmixgamma(x, shape=shape, rate=rate) - p3
#     stats::uniroot(
#       cdf,
#       lower=minmax[1],
#       upper = minmax[2],
#       tol = 0.01
#     )$root
#   })
# }
#
#
# # must be vectorised on x
# .pmixnorm = function(x, mean, sd) {
#
#   if (is.list(mean)) {
#     interfacer::recycle(x,mean,sd)
#     return(purrr::map_dbl(seq_along(x), function(i) {
#       .pmixnorm(x[i],mean[[i]],sd[[i]])
#     }))
#   }
#
#   if (length(mean) == 1 || (
#     length(unique(mean))==1 && length(unique(sd))==1
#   )) return(stats::pnorm(x,mean = mean[1],sd = sd[1]))
#   tmp = sapply(
#     x,
#     FUN = function(x2) {
#       return(stats::pnorm(x2, mean=mean, sd=sd))
#     }
#   )
#   apply(tmp,MARGIN=2,mean)
# }
#
# # solve
# # This function is vectorised on p and parameters. as mixture parameters
# # must be a list of vectors.
# .qmixnorm = function(p, mean, sd) {
#
#   if (is.list(mean)) {
#     interfacer::recycle(p,mean,sd)
#     return(purrr::map_dbl(seq_along(p), function(i) {
#       .qmixnorm(p[i],mean[[i]],sd[[i]])
#     }))
#   }
#
#   if (length(mean) == 1 || (
#     length(unique(mean))==1 && length(unique(sd))==1
#   )) return(stats::qnorm(p,mean=mean[1],sd=sd[1]))
#   sapply(p, function(p3) {
#     minmax = range(stats::qnorm(p3,mean=mean,sd=sd))
#     cdf = function(x) .pmixnorm(x, mean=mean, sd=sd) - p3
#     stats::uniroot(
#       cdf,
#       lower=minmax[1],
#       upper = minmax[2],
#       tol = 0.01
#     )$root
#   })
# }

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

.keep_cdf = function(
  new_data,
  type,
  ...,
  link = c("identity", "log", "logit")
) {
  link = match.arg(link)
  trans_fn = .trans_fn(link)
  keep = getOption("ggoutbreak.keep_cdf", default = FALSE)
  if (keep) {
    if (length(rlang::list2(...)) > 0) {
      new_data = new_data %>%
        dplyr::mutate(
          !!paste0(type, ".cdf") := .cdf_generator(..., trans_fn = trans_fn)
        )
    } else {
      new_data = new_data %>% .infer_cdf(type, trans_fn)
    }
    new_data = new_data %>%
      dplyr::mutate(
        !!paste0(type, ".link") := link
      )
  }
  return(new_data)
}

.trans_fn = function(link) {
  link = unique(stats::na.omit(link))
  if (length(link) != 1) {
    stop("mixture of link functions found: ", paste0(link, collapse = ", "))
  }
  return(stats::make.link(link)$linkfun)
}

.inv_fn = function(link) {
  link = unique(stats::na.omit(link))
  if (length(link) != 1) {
    stop("mixture of link functions found: ", paste0(link, collapse = ", "))
  }
  return(stats::make.link(link)$linkinv)
}

.min_domain = function(link) {
  return(rep(.inv_fn(link)(-Inf), length(link)))
}

.max_domain = function(link) {
  return(rep(.inv_fn(link)(Inf), length(link)))
}


# use the quantiles to infer an empirical cdf.
# assumes the naming convention e.g. `rt.0.025`, `rt.0.05`, etc.
.infer_cdf = function(new_data, type, trans_fn = ~.x) {
  p = name = value = NULL
  df = new_data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::select(id, dplyr::starts_with(paste0(type, ".0."))) %>%
    tidyr::pivot_longer(names_prefix = paste0(type, "."), cols = -id) %>%
    dplyr::transmute(id, p = as.numeric(name), value) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      !!paste0(type, ".cdf") := list(.ecdf(
        value,
        probs = p,
        trans_fn = trans_fn
      ))
    ) %>%
    dplyr::select(-id)
  new_data = dplyr::bind_cols(new_data, df) # retains grouping.
  return(new_data)
}


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
.cdf_generator = function(..., trans_fn = ~.x) {
  dots = rlang::list2(...)
  trans_fn = rlang::as_function(trans_fn)

  if (length(dots) == 1) {
    # If called with 1 list only this is samples and we want an list of empirical CDFs
    samples = dots[[1]]
    if (!is.list(samples)) {
      samples = list(samples)
    }
    return(lapply(samples, \(x) .ecdf(x, trans_fn = trans_fn)))
  }

  if (identical(names(dots), c("mean", "sd"))) {
    if (is.list(dots[["mean"]])) {
      fn = .pmixnorm
    } else {
      fn = stats::pnorm
    }
  } else if (identical(names(dots), c("meanlog", "sdlog"))) {
    if (is.list(dots[["meanlog"]])) {
      fn = .pmixlnorm
    } else {
      fn = stats::plnorm
    }
  } else if (identical(names(dots), c("shape", "rate"))) {
    if (is.list(dots[["shape"]])) {
      fn = .pmixgamma
    } else {
      fn = stats::pgamma
    }
  }

  lapply(seq_along(dots[[1]]), function(i) {
    p1 = dots[[1]][[i]]
    p2 = dots[[2]][[i]]
    function(x) {
      # this is the CDF function:
      fn(x, p1, p2)
    }
  })
}

# generate an empirical CDF from either data or quantiles
# x is either samples, when prob is not given or
# quantiles with p as probs.
# linear interpolation is done between quantiles on a logit
# transformation which is linearly extended until the 0.00001th quantile.
.ecdf = function(x, probs = NULL, trans_fn = ~.x) {
  trans_fn = rlang::as_function(trans_fn)
  x = trans_fn(x)
  if (is.null(probs)) {
    x = sort(unique(x))
    y = as.vector(table(x))
    y = (cumsum(y) + 1 / 3) / (sum(y) + 2 / 3) #median of beta
  } else {
    y = probs[order(x)]
    x = x[order(x)]
  }

  project = function(x, x1, x2, y1, y2) {
    (y2 - y1) / (x2 - x1) * (x - x1) + y1
  }
  # project(0,1,2,1,2) == 0
  # project(3,1,2,1,2) == 3
  eps = .logit(0.00001)
  y = .logit(y)
  x = c(
    project(eps, x[1], x[2], y[1], y[2]),
    x,
    project(
      -eps,
      x[length(x) - 1],
      x[length(x)],
      y[length(y) - 1],
      y[length(y)]
    )
  )
  y = c(eps, y, -eps)
  sf = function(x1) {
    .expit(stats::approxfun(x, y, yleft = -Inf, yright = Inf)(trans_fn(x1)))
  }
  return(sf)
}

# fast
.logit = qlogis

# slow (but faster that plogis)
.expit = function(x) {
  return(1 / (1 + exp(-x)))
}

# sf = .ecdf(qnorm(seq(0.1,0.9,0.1)), seq(0.1,0.9,0.1))
#
# tmp = c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)
# sf = .ecdf(qnorm(tmp), tmp)
#
# sf = .ecdf(qlnorm(tmp), tmp, trans_fn = log)

# sf(qnorm(tmp))
# sf(seq(0.1,0.9,0.1)) - pnorm(seq(0.1,0.9,0.1))
#
# sf2 = .ecdf(rnorm(100))
# sf2(seq(0.1,0.9,0.1)) - pnorm(seq(0.1,0.9,0.1))

# Link scale normal from quantiles ----

# median preserving:
# .infer_normal(p=seq(0.1,0.9,0.1), x=qnorm(seq(0.1,0.9,0.1),4,2))
# .infer_normal(p=seq(0.1,0.9,0.1), x=qlnorm(seq(0.1,0.9,0.1),0.4,0.2), link="log")
# tmp = .infer_normal(p=seq(0.1,0.9,0.1), x=qbeta2(seq(0.1,0.9,0.1),0.4,0.1), link="logit")
# .expit(qnorm(seq(0.1,0.9,0.1), tmp$mean, tmp$sd))
# qbeta2(seq(0.1,0.9,0.1),0.4,0.1)
.infer_normal = function(p, x, link = "identity") {
  trans_fn = .trans_fn(link)
  q = trans_fn(x)
  q2 = stats::qnorm(p)
  fit = .fit_lm_1d(q, q2)
  mean = stats::approx(q2, q, 0)
  #mean = fit["c"]
  return(dplyr::tibble(mean = mean$y, sd = unname(fit["m"])))
}


#' Fit a weighted 1D linear model and predict output
#'
#' If all the weights are NA they are ignored.
#'
#' @param y the y values. At least 2.
#' @param x the x values. At least 2.
#'
#' @returns a vector with intercept (`c`) and gradient (`m`)
#' @keywords internal
#' @unit
#' testthat::expect_equal(.fit_lm_1d(y = 1:10 * 2 + 5, x = 1:10), c(c = 5, m = 2))
.fit_lm_1d = function(y, x) {
  if (length(x) != length(y)) {
    stop("unequal length LM coordinates")
  }
  missing = is.na(x) | is.na(y)

  # Handle missing values
  x = x[!missing]
  y = y[!missing]

  # Check for sufficient data
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    stop("not enough data in LM")
  }

  # Prepare design matrix
  X = matrix(c(rep(1, length(x)), x), ncol = 2)
  Y = matrix(y, ncol = 1)

  beta = solve(t(X) %*% X) %*% t(X) %*% Y
  return(c(c = beta[1, 1], m = beta[2, 1]))
}
