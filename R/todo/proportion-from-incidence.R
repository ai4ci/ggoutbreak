proportion_from_incidence = function(
  mu,
  sigma,
  time = 1:length(mu),
  max_time = NULL,
  correlation_decay = "exponential",
  bw = 3
) {
  mean <- exp(mu + sigma^2 / 2)
  if (is.null(max_time)) {
    max_time = time[which.max(mean)]
  }
  max_idx <- which(time == max_time)
  if (length(max_idx) == 0) {
    stop("`max_time` must be in `time`")
  }

  mu_C = mu[max_idx]
  sigma_C = sigma[max_idx]
  mean_C <- mean[max_idx]

  # Calculate variances
  var <- (exp(sigma^2) - 1) * exp(2 * mu + sigma^2)
  var_C <- (exp(sigma_C^2) - 1) * exp(2 * mu_C + sigma_C^2)

  # Calculate correlation based on time distance from maximum
  # Assuming the maximum occurs at the time point with highest mu
  # We could if sigma A is a matrix select a row for the time_C
  # and gives us the cov vector directly...

  rho = .time_based_correlation(
    time,
    bw = bw,
    index_time = max_time,
    decay = correlation_decay
  )

  # Calculate covariance between A and C
  covC <- rho * sqrt(var * var_C)

  # Calculate variance of B = C - A
  var_B <- var_C + var - 2 * covC

  # Ensure non-negative variance (numerical stability)
  var_B <- pmax(var_B, 0)

  # Convert to log-normal parameters for B
  # For log-normal: if X ~ LN(μ,σ²), then Var(X) = (exp(σ²)-1)exp(2μ+σ²)
  # We need to solve for μ_B, σ_B given E[B] and Var[B]

  # Mean of B
  mean_B <- pmax(mean_C - mean, 0)

  # Solve for log-normal parameters of B
  # μ_B = log(E[B]) - σ_B²/2
  # Var(B) = (exp(σ_B²) - 1) * exp(2μ_B + σ_B²) = (exp(σ_B²) - 1) * E[B]²

  sigma_B <- numeric(length(var))
  mu_B <- numeric(length(var))

  # Solve for sigma_B: Var[B] = (exp(σ²) - 1) * E[B]²
  # This gives: exp(σ²) = Var[B]/E[B]² + 1
  exp_sigma2 <- ifelse(var_B == 0 & mean_B == 0, 1, var_B / (mean_B^2) + 1)
  sigma_B <- sqrt(log(exp_sigma2))

  # Then μ_B = log(E[B]) - σ_B²/2
  mu_B <- log(mean_B) - sigma_B^2 / 2

  # Logit-normal parameters for A/(A+B) = A/C
  # If A ~ LN(μ, σ²) and B ~ LN(μ_B, σ_B²) are independent
  # Then log(A/B) ~ N(μ - μ_B, σ² + σ_B²)
  # So A/C = A/(A+B) follows Logit-Normal(μ - μ_B, σ² + σ_B²)

  mu_logit <- mu - mu_B
  sigma_logit <- sqrt(sigma^2 + sigma_B^2)

  return(list(
    mu_logit = mu_logit,
    sigma_logit = sigma_logit
  ))
}

# Example usage:
# params <- logit_normal_params(
#   mu = c(1, 1.2, 1.5, 1.3, 1.1),
#   sigma = c(0.2, 0.2, 0.2, 0.2, 0.2),
#   time = c(1, 2, 3, 4, 5),
#   mu_C = 1.6,
#   sigma_C = 0.25
# )
