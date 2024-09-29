# Conditional installation of required libraries
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")      # For data manipulation
if(!require(dplyr)) install.packages("dplyr")      # For data manipulation

# Load the libraries
library(ggplot2)
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
n_levels <- 4  # Compute the first 4 energy levels (n = 0 to 3)

# Create a data frame to store the wavefunctions
wavefunctions <- data.frame(x = x)

# Calculate wavefunctions for each energy level
for (n in 0:(n_levels - 1)) {
  wavefunctions[[paste0("n", n)]] <- psi_n(n, x)
}

# Reshape data for plotting
wavefunctions_long <- wavefunctions %>%
  pivot_longer(cols = starts_with("n"),
               names_to = "Energy_Level",
               values_to = "Psi") %>%
  mutate(Energy_Level = factor(Energy_Level, 
                               levels = paste0("n", 0:(n_levels - 1)),
                               labels = paste("n =", 0:(n_levels - 1))))

# Plot the wavefunctions
ggplot(wavefunctions_long, aes(x = x, y = Psi, color = Energy_Level)) +
  geom_line(size = 1) +
  labs(title = "Quantum Harmonic Oscillator Wavefunctions",
       x = "Position (x)",
       y = expression(paste("Wavefunction ", psi[n](x))),
       color = "Energy Level") +
  theme_minimal()

# Calculate probability densities
wavefunctions_long <- wavefunctions_long %>%
  mutate(Probability_Density = Psi^2)

# Plot the probability densities
ggplot(wavefunctions_long, aes(x = x, y = Probability_Density, color = Energy_Level)) +
  geom_line(size = 1) +
  labs(title = "Quantum Harmonic Oscillator Probability Densities",
       x = "Position (x)",
       y = expression(paste("|", psi[n](x), "|^2")),
       color = "Energy Level") +
  theme_minimal()