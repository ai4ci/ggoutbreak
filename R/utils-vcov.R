# identity matrix:
# tmp  = matrix(nrow=4,ncol=4,data = 0)
# diag(tmp) = 1
# t(tmp * c(1,2,3,4)) * c(1,2,3,4) - would give us vcov matrix

# Create distance matrix:
# time_matrix <- outer(time_points, time_points, function(x, y) abs(x - y))
# Exponential decay covariance function
# Correlation decays exponentially with distance
# correlation_matrix <- exp(-time_matrix / bandwidth)

# GAM and GLMs
predict_derivative = function(model, newdata, dimension = "time") {
  dimension = rlang::ensym(dimension)
  eps = 1e-7 ## finite difference interval
  X0 = stats::predict(
    model,
    newdata %>% dplyr::mutate(!!dimension := !!dimension - eps),
    type = "lpmatrix"
  )
  X1 = stats::predict(
    model,
    newdata %>% dplyr::mutate(!!dimension := !!dimension + eps),
    type = "lpmatrix"
  )
  Xp = (X1 - X0) / (2 * eps) ## maps coefficients to (fd approx.) derivatives
  fit = Xp %*% stats::coef(model)
  # this is doing a Xp %*% vcov(model) %*% t(Xp) where we only need sequential
  # values (i.e. the diagonals)
  se.fit = rowSums(Xp %*% stats::vcov(model) * Xp)^.5

  return(list(
    fit = as.numeric(fit),
    se.fit = as.numeric(se.fit)
  ))
}

# Example usage
# deriv_result <- predict_derivative(best_model, newdata)

.gam_glm_cor = function(model, newdata) {
  Xp <- stats::predict(model, newdata, type = "lpmatrix")
  pred_vcov <- Xp %*% stats::vcov(model) %*% t(Xp)
  return(stats::cov2cor(pred_vcov))
}

# generate a vcov matrix for locfit based on time difference alone.
# .locfit_vcov = function(
#   model,
#   newdata,
#   bw = model$dp$h,
#   decay = c("exponential", "gaussian", "power")
# ) {
#   tmp = stats::preplot(
#     fit,
#     newdata = newdata,
#     se.fit = TRUE,
#     band = "local",
#     maxit = 5000,
#     maxk = 5000
#   )
#   .ts_vcov(tmp$se.fit, bw = bw, decay = decay)
# }
#
# .ts_vcov = function(se.fit, bw, decay = c("exponential", "gaussian", "power")) {
#   correlation_matrix = .time_based_correlation(
#     seq_along(se.fit),
#     bw = bw,
#     decay = decay
#   )
#   se_matrix = outer(tmp$se.fit, tmp$se.fit)
#   cov_matrix = correlation_matrix * se_matrix
# }

# .time_based_correlation(1:10, index_time = 4)
# .time_based_correlation(1:10)
.time_based_correlation = function(
  times,
  bw = 1,
  index_time = NULL,
  decay = c("exponential", "gaussian", "power")
) {
  if (is.null(index_time)) {
    return(sapply(times, function(i) {
      .time_based_correlation(times, bw, i, decay)
    }))
  }

  time_distances <- abs(times - index_time)
  decay = match.arg(decay)

  # Different correlation decay functions
  if (decay == "exponential") {
    alpha = 1 / bw
    rho = exp(-alpha * time_distances)
  } else if (decay == "gaussian") {
    alpha = 1 / (bw^2)
    rho = exp(-alpha * time_distances^2)
  } else if (decay == "power") {
    alpha = 1 / log(bw + 1)
    rho = (1 + time_distances)^(-alpha)
  } else {
    stop("correlation_decay must be 'exponential', 'gaussian', or 'power'")
  }

  return(rho)
}
