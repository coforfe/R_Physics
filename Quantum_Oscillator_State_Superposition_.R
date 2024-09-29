# Conditional installation of required libraries
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gganimate)) install.packages("gganimate")      # For animation
if(!require(gifski)) install.packages("gifski")            # For saving GIFs
if(!require(tidyr)) install.packages("tidyr")              # For data manipulation
if(!require(dplyr)) install.packages("dplyr")              # For data manipulation

# Load the libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyr)
library(dplyr)

# Define parameters
m <- 1       # Mass of the particle (kg)
omega <- 1   # Angular frequency (rad/s)
hbar <- 1    # Reduced Planck's constant

# Position range
x <- seq(-5, 5, length.out = 1000)

# Function to compute Hermite polynomials using recursion
hermite <- function(n, x) {
  # Base cases
  if (n == 0) {
    return(rep(1, length(x)))
  } else if (n == 1) {
    return(2 * x)
  } else {
    # Recursive computation
    H_n_minus_1 <- hermite(n - 1, x)
    H_n_minus_2 <- hermite(n - 2, x)
    return(2 * x * H_n_minus_1 - 2 * (n - 1) * H_n_minus_2)
  }
}

# Function to calculate the wavefunction of the quantum harmonic oscillator
psi_n <- function(n, x) {
  # Calculate the normalization constant
  norm_const <- 1 / sqrt(2^n * factorial(n)) * (m * omega / (pi * hbar))^(1/4)
  
  # Calculate the Hermite polynomial
  xi <- sqrt(m * omega / hbar) * x
  hermite_poly <- hermite(n, xi)
  
  # Calculate the wavefunction
  psi <- norm_const * exp(-xi^2 / 2) * hermite_poly
  return(psi)
}

# Energy levels to compute
n_levels <- 2  # Compute for n = 0 and n = 1 for superposition

# Coefficients for superposition (e.g., equal superposition)
coefficients <- c(1, 1)  # You can adjust these coefficients
coefficients <- coefficients / sqrt(sum(abs(coefficients)^2))  # Normalize

# Time range
time <- seq(0, 10, length.out = 200)  # From t = 0 to t = 10

# Create a grid of positions and times
grid <- expand.grid(x = x, time = time)

# Function to calculate the time-dependent wavefunction for superposition
psi_total <- function(x, t, coefficients, n_levels) {
  psi <- rep(0, length(x))
  for (n in 0:(n_levels - 1)) {
    E_n <- hbar * omega * (n + 0.5)  # Energy eigenvalues
    psi_n_t <- psi_n(n, x) * coefficients[n + 1] * exp(-1i * E_n * t / hbar)
    psi <- psi + psi_n_t
  }
  return(Re(psi * Conj(psi)))  # Probability density
}

# Calculate probability density for each position and time
grid$Probability_Density <- mapply(function(x_val, t_val) {
  psi_total(x_val, t_val, coefficients, n_levels)
}, grid$x, grid$time)

# Plot and animate the probability density over time
p <- ggplot(grid, aes(x = x, y = Probability_Density)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Time Evolution of Probability Density",
       x = "Position (x)",
       y = expression(paste("|", psi(x, t), "|^2"))) +
  theme_minimal() +
  transition_time(time) +
  ease_aes('linear') +
  labs(subtitle = "Time: {frame_time} units")

# Render and save the animation
animate(p, renderer = gifski_renderer("quantum_harmonic_oscillator.gif"), fps = 20, duration = 10, width = 800, height = 600)