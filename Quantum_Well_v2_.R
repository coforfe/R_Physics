# Conditional installation of required libraries
required_packages <- c("ggplot2", "tidyr", "dplyr", "gganimate", "gifski")
installed_packages <- rownames(installed.packages())

for(p in required_packages){
  if(!p %in% installed_packages){
    install.packages(p)
  }
}

# Load the libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(gganimate)
library(gifski)

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
n_levels <- 4  # Compute for n = 0 to 3

# Coefficients for superposition (e.g., equal superposition)
coefficients <- rep(1, n_levels)  # Equal coefficients
coefficients <- coefficients / sqrt(sum(abs(coefficients)^2))  # Normalize

# Time range for animation
time <- seq(0, 10, length.out = 200)  # From t = 0 to t = 10

# Create a grid of positions and times
grid <- expand.grid(x = x, time = time)

# Function to calculate the time-dependent probability density for superposition
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

# Calculate the potential energy
V_x <- 0.5 * m * omega^2 * x^2
potential <- data.frame(x = x, V_x = V_x)

# Determine scaling factors for visualization
max_V <- max(potential$V_x)
max_P <- max(grid$Probability_Density)

scale_factor <- max_V * 0.8 / max_P  # Scale probability density to fit in plot

# Add scaled probability density to grid
grid$Probability_Density_Scaled <- grid$Probability_Density * scale_factor

# Prepare data for plotting
plot_data <- grid %>%
  select(x, time, Probability_Density_Scaled) %>%
  mutate(Probability_Density_Scaled = Probability_Density_Scaled)

# Plot and animate the probability density over time with potential well
p <- ggplot() +
  # Plot the potential energy
  geom_line(data = potential, aes(x = x, y = V_x), color = "black", size = 1.2, linetype = "dashed") +
  # Plot the scaled probability density
  geom_line(data = plot_data, aes(x = x, y = Probability_Density_Scaled), color = "blue", size = 1) +
  # Labels and titles
  labs(title = "Time Evolution of Probability Density in Quantum Harmonic Oscillator",
       subtitle = "Time: {round(frame_time, 2)} units",
       x = "Position (x)",
       y = "Scaled Probability Density",
       caption = "Dashed Line: Potential Energy V(x)") +
  theme_minimal() +
  # Animation settings
  transition_time(time) +
  ease_aes('linear')

# Render and save the animation
animate(p, renderer = gifski_renderer("quantum_harmonic_oscillator_with_potential.gif"),
        fps = 20, duration = 10, width = 800, height = 600)