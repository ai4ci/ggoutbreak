#' SEIR Model with Time-Varying Reproduction Number R_t
#'
#' This function simulates an SEIR (Susceptible-Exposed-Infectious-Recovered) model
#' where the transmission rate (\code{beta}) varies over time based on a user-defined
#' effective reproduction number function \code{rt_fn(t)}.
#'
#' The latent period (time from infection to becoming infectious) is assumed to be
#' exponentially distributed. The infectious period is derived from the given
#' generation time distribution (assumed to be Gamma distributed) and the latent period.
#'
#' The function is suitable for modeling interventions (e.g., lockdowns, vaccination)
#' that cause \code{R_t} to change dynamically over time.
#'
#' @param mean_latent_period Mean time from infection to becoming infectious (E to I),
#'        assumed to be exponentially distributed.
#' @param generation_time_shape Shape parameter of the Gamma-distributed generation time.
#' @param generation_time_rate Rate parameter of the Gamma-distributed generation time.
#' @param rt_fn A function of time \code{t} that returns the effective reproduction number \code{R_t(t)}.
#' @param N Total population size. Defaults to 1,000,000.
#' @param I0 Initial number of infectious individuals. Defaults to 10.
#' @param t_max Maximum simulation time (in days or time units). Defaults to 100.
#'
#' @return A \code{data.frame} with columns:
#'   \item{time}{Time step}
#'   \item{S}{Number of susceptible individuals}
#'   \item{E}{Number of exposed (latent) individuals}
#'   \item{I}{Number of infectious individuals}
#'   \item{R}{Number of recovered individuals}
#'
#' @details
#' The model assumes:
#' - Latent period ~ Exponential(1 / \code{mean_latent_period})
#' - Infectious period ~ Exponential(gamma), where gamma = 1 / (mean_gen_time - mean_latent_period)
#' - Generation time ~ Gamma(generation_time_shape, generation_time_rate)
#' - Transmission rate beta(t) = R_t(t) * gamma
#'
#' The generation time is defined as the sum of the latent and infectious periods.
#' This function matches the **mean** of the generation time distribution but cannot
#' match the variance unless non-exponential distributions are used (e.g., multi-stage compartments).
#'
#' @examples
#' # Example: Lockdown after day 20
#' rt_fn <- function(t) ifelse(t < 20, 3.0, 0.8)
#'
#' seir_output <- seir_model(
#'   mean_latent_period = 5,
#'   generation_time_shape = 6,
#'   generation_time_rate = 1,
#'   rt_fn = rt_fn,
#'   t_max = 100
#' )
#'
#'
seir_model <- function(
  mean_latent_period = 5,
  generation_time_shape = 6,
  generation_time_rate = 1,
  rt_fn = function(t) 2.5,
  N = 1e6,
  I0 = 10,
  t_max = 100
) {
  # --- Step 1: Compute mean generation time from Gamma distribution ---
  mean_gen_time <- generation_time_shape / generation_time_rate

  # --- Step 2: Validate input: latent period must be less than generation time ---
  if (mean_latent_period >= mean_gen_time) {
    stop("mean_latent_period must be less than mean_gen_time")
  }

  # --- Step 3: Derive infectious period and gamma ---
  mean_inf_period <- mean_gen_time - mean_latent_period
  gamma <- 1 / mean_inf_period
  sigma <- 1 / mean_latent_period

  # --- Step 4: Initial conditions ---
  S0 <- N - I0
  E0 <- 0
  R0 <- 0
  y0 <- c(S = S0, E = E0, I = I0, R = R0)

  # --- Step 5: Time vector ---
  times <- seq(0, t_max, by = 1)

  # --- Step 6: Define the ODE system ---
  SEIR <- function(t, y, ...) {
    with(as.list(y), {
      # Get current R_t and compute beta
      Rt <- rt_fn(t)
      beta <- Rt * gamma

      # ODEs
      dS <- -beta * S * I / N
      dE <- beta * S * I / N - sigma * E
      dI <- sigma * E - gamma * I
      dR <- gamma * I

      list(c(dS, dE, dI, dR))
    })
  }

  # --- Step 7: Solve the ODEs ---
  out <- deSolve::ode(y = y0, times = times, func = SEIR)

  # --- Step 8: Convert output to data frame ---
  colnames(out)[2:5] <- c("S", "E", "I", "R")
  out_df <- as.data.frame(out)

  return(out_df)
}


multi_stage_seir <- function(
  S0 = 1e6,
  I0 = 10,
  mean_latent = 4,
  mean_inf = 6,
  shape_latent = 2,
  shape_inf = 3,
  beta = 0.5,
  t_max = 100
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
  I[1] <- I0
  R <- 0
  S <- S0 - I0
  y0 <- c(S = S, E = E, I = I, R = R)

  # Time vector
  times <- seq(0, t_max, by = 1)

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
