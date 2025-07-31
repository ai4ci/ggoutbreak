proportion_from_incidence = function(
  mu_A,
  sigma_A,
  time_A,
  correlation_decay = "exponential",
  bw = 5
) {
  max_idx = which.max(mu_A)
  mu_C = mu_A[max_idx]
  sigma_C = sigma_A[max_idx]

  # Calculate variances
  var_A = (exp(sigma_A^2) - 1) * exp(2 * mu_A + sigma_A^2)
  var_C = (exp(sigma_C^2) - 1) * exp(2 * mu_C + sigma_C^2)

  # Calculate correlation based on time distance from maximum
  # Assuming the maximum occurs at the time point with highest mu_A

  max_time = time_A[max_idx]
  time_distances = abs(time_A - max_time)

  # Different correlation decay functions
  if (correlation_decay == "exponential") {
    alpha = log(2) / bw
    rho = exp(-alpha * time_distances)
  } else if (correlation_decay == "gaussian") {
    alpha = log(2) / (bw^2)
    rho = exp(-alpha * time_distances^2)
  } else if (correlation_decay == "power") {
    alpha = log(2) / log(bw + 1)
    rho = (1 + time_distances)^(-alpha)
  } else {
    stop("correlation_decay must be 'exponential', 'gaussian', or 'power'")
  }

  # Calculate covariance between A and C
  cov_AC = rho * sqrt(var_A * var_C)

  # Calculate variance of B = C - A
  var_B = var_C + var_A - 2 * cov_AC

  # Ensure non-negative variance (numerical stability)
  var_B = pmax(var_B, 1e-10)

  # Convert to log-normal parameters for B
  # For log-normal: if X ~ LN(μ,σ²), then Var(X) = (exp(σ²)-1)exp(2μ+σ²)
  # We need to solve for μ_B, σ_B given E[B] and Var[B]

  # Mean of B
  mean_A = exp(mu_A + sigma_A^2 / 2)
  mean_C = exp(mu_C + sigma_C^2 / 2)
  mean_B = mean_C - mean_A

  # Solve for log-normal parameters of B
  # μ_B = log(E[B]) - σ_B²/2
  # Var(B) = (exp(σ_B²) - 1) * exp(2μ_B + σ_B²) = (exp(σ_B²) - 1) * E[B]²

  sigma_B = numeric(length(var_A))
  mu_B = numeric(length(var_A))

  for (i in 1:length(var_A)) {
    if (mean_B[i] > 0) {
      # Solve for sigma_B: Var[B] = (exp(σ²) - 1) * E[B]²
      # This gives: exp(σ²) = Var[B]/E[B]² + 1
      exp_sigma2 = var_B[i] / (mean_B[i]^2) + 1
      sigma_B[i] = sqrt(log(exp_sigma2))

      # Then μ_B = log(E[B]) - σ_B²/2
      mu_B[i] = log(mean_B[i]) - sigma_B[i]^2 / 2
    } else {
      # Handle case where mean_B <= 0
      sigma_B[i] = sigma_C # fallback
      mu_B[i] = mu_C # fallback
      warning(paste(
        "Negative mean for B at time point",
        i,
        "- using fallback values"
      ))
    }
  }

  # Logit-normal parameters for A/(A+B) = A/C
  # If A ~ LN(μ_A, σ_A²) and B ~ LN(μ_B, σ_B²) are independent
  # Then log(A/B) ~ N(μ_A - μ_B, σ_A² + σ_B²)
  # So A/C = A/(A+B) follows Logit-Normal(μ_A - μ_B, σ_A² + σ_B²)

  mu_logit = mu_A - mu_B
  sigma_logit = sqrt(sigma_A^2 + sigma_B^2)

  return(list(
    mu_logit = mu_logit,
    sigma_logit = sigma_logit,
    link = "logit"
  ))
}

# Example usage:
# params = logit_normal_params(
#   mu_A = c(1, 1.2, 1.5, 1.3, 1.1),
#   sigma_A = c(0.2, 0.2, 0.2, 0.2, 0.2),
#   time_A = c(1, 2, 3, 4, 5),
#   mu_C = 1.6,
#   sigma_C = 0.25
# )
