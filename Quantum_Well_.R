# Conditional installation of required libraries
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")      # For data manipulation
if(!require(dplyr)) install.packages("dplyr")      # For data manipulation

# Load the libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Define parameters
L <- 1        # Width of the potential well (meters)
m <- 1        # Mass of the particle (kg)
hbar <- 1     # Reduced Planck's constant

# Position range
x <- seq(0, L, length.out = 1000)

# Function to calculate the wavefunction for a particle in an infinite potential well
psi_n_well <- function(n, x, L) {
  sqrt(2/L) * sin(n * pi * x / L)
}

# Energy levels
n_levels <- 4  # Compute the first 4 energy levels (n = 1 to 4)

# Create a data frame to store the wavefunctions
wavefunctions_well <- data.frame(x = x)

# Calculate wavefunctions for each energy level
for (n in 1:n_levels) {
  wavefunctions_well[[paste0("n", n)]] <- psi_n_well(n, x, L)
}

# Reshape data for plotting
wavefunctions_long_well <- wavefunctions_well %>%
  pivot_longer(cols = starts_with("n"),
               names_to = "Energy_Level",
               values_to = "Psi") %>%
  mutate(Energy_Level = factor(Energy_Level, 
                               levels = paste0("n", 1:n_levels),
                               labels = paste("n =", 1:n_levels)))

# Plot the wavefunctions
ggplot(wavefunctions_long_well, aes(x = x, y = Psi, color = Energy_Level)) +
  geom_line(size = 1) +
  labs(title = "Wavefunctions in an Infinite Potential Well",
       x = "Position (x)",
       y = expression(paste("Wavefunction ", psi[n](x))),
       color = "Energy Level") +
  theme_minimal()

# Calculate probability densities
wavefunctions_long_well <- wavefunctions_long_well %>%
  mutate(Probability_Density = Psi^2)

# Plot the probability densities
ggplot(wavefunctions_long_well, aes(x = x, y = Probability_Density, color = Energy_Level)) +
  geom_line(size = 1) +
  labs(title = "Probability Densities in an Infinite Potential Well",
       x = "Position (x)",
       y = expression(paste("|", psi[n](x), "|^2")),
       color = "Energy Level") +
  theme_minimal()