multi_stage_seir <- function(
  S0 = 1e6,
  imports = 10,
  mean_latent = 4,
  mean_inf = 6,
  shape_latent = 2,
  shape_inf = 3,
  beta = 0.5,
  max_time = 100
) {
  # Number of stages
  k_latent <- shape_latent
  k_inf <- shape_inf

  # Stage transition rates
  r_latent <- k_latent / mean_latent
  r_inf <- k_inf / mean_inf

  # Initial conditions
  E <- rep(0, k_latent)
  I <- rep(0, k_inf)
  I[1] <- imports
  R <- 0
  S <- S0 - imports
  y0 <- c(S = S, E = E, I = I, R = R)

  # Time vector
  times <- seq(0, max_time, by = 1)

  # ODE function
  SEIR <- function(t, y, ...) {
    with(as.list(y), {
      # Transitions
      dE <- numeric(k_latent)
      dI <- numeric(k_inf)

      # First latent stage
      dE[1] <- beta * S * sum(I) / S0 - r_latent * E[1]

      # Internal latent stages
      for (i in 2:k_latent) {
        dE[i] <- r_latent * E[i - 1] - r_latent * E[i]
      }

      # First infectious stage
      dI[1] <- r_latent * E[k_latent] - r_inf * I[1]

      # Internal infectious stages
      for (i in 2:k_inf) {
        dI[i] <- r_inf * I[i - 1] - r_inf * I[i]
      }

      # Recovery
      dR <- r_inf * I[k_inf]

      # Susceptible dynamics
      dS <- -beta * S * sum(I) / S0

      # Combine all derivatives
      list(c(dS, dE, dI, dR))
    })
  }

  # Solve ODEs
  out <- ode(y = y0, times = times, func = SEIR)

  # Return output
  colnames(out)[1] <- "time"
  out_df <- as.data.frame(out)
  return(out_df)
}
