# Install and load required packages
required_packages <- c("ggplot2", "tidyr", "dplyr", "gganimate", "gifski")

# Install any packages that are not already installed
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

# Define physical parameters
L <- 1        # Width of the potential well (meters)
m <- 1        # Mass of the particle (kg)
hbar <- 1     # Reduced Planck's constant

# Define the position range, extending beyond the well to visualize the walls
x <- seq(-0.5, L + 0.5, length.out = 1000)  # From -0.5 to L+0.5 meters

# Function to calculate the wavefunction for a particle in an infinite potential well
psi_n_well <- function(n, x, L) {
  # Initialize the wavefunction with zeros
  psi <- rep(0, length(x))
  
  # Identify positions inside the well
  inside <- which(x > 0 & x < L)
  
  # Calculate the wavefunction inside the well using the analytical solution
  psi[inside] <- sqrt(2/L) * sin(n * pi * x[inside] / L)
  
  return(psi)
}

#---- Alternatives
# n_levels <- 5  # For example, including n = 1 to 5
# coefficients <- rep(1, n_levels)
# coefficients <- coefficients / sqrt(sum(abs(coefficients)^2))  # Normalize

# n_levels <- 3  # For example, n = 1, 2, 3
# coefficients <- c(1, 2, 1)  # Unequal coefficients for n = 1, 2, 3
# coefficients <- coefficients / sqrt(sum(abs(coefficients)^2))  # Normalize
#-----

# #----- Default
# # Define the number of energy levels to include in the superposition
n_levels <- 3  # For example, n = 1, 2, 3
# 
# # Define coefficients for the superposition (e.g., equal superposition)
coefficients <- rep(1, n_levels)
coefficients <- coefficients / sqrt(sum(abs(coefficients)^2))  # Normalize the coefficients
# #-----

# Define the time range for the animation
time <- seq(0, 10, length.out = 200)  # From t = 0 to t = 10 units of time

# Create a grid of all combinations of x and time
grid <- expand.grid(x = x, time = time)

# Function to calculate the total probability density for a superposition of states
psi_total <- function(x, t, coefficients, n_levels, L, m, hbar) {
  psi <- rep(0+0i, length(x))  # Initialize with complex numbers
  
  for (n in 1:n_levels) {
    E_n <- (n^2 * pi^2 * hbar^2) / (2 * m * L^2)  # Energy eigenvalues
    psi_n_t <- psi_n_well(n, x, L) * coefficients[n] * exp(-1i * E_n * t / hbar)
    psi <- psi + psi_n_t
  }
  
  return(Re(psi * Conj(psi)))  # Return the probability density
}

# Calculate the probability density for each combination of x and time
grid$Probability_Density <- mapply(function(x_val, t_val) {
  psi_total(x_val, t_val, coefficients, n_levels, L, m, hbar)
}, grid$x, grid$time)

# Define the potential energy: 0 inside the well, 100 outside to simulate infinity
V_x <- ifelse(grid$x < 0 | grid$x > L, 100, 0)  # Assign V(x) = 100 outside the well

# Create a data frame for the potential to plot the walls and base
potential <- data.frame(x = x, V_x = ifelse(x >= 0 & x <= L, 0, 100))

# Determine the maximum values for scaling
max_V <- max(potential$V_x)
max_P <- max(grid$Probability_Density)

# Define a scaling factor to fit the probability density within the plot range
scale_factor <- max_V * 0.8 / max_P  # Scale so that the probability density fits below the potential walls

# Add the scaled probability density to the grid data frame
grid$Probability_Density_Scaled <- grid$Probability_Density * scale_factor

# Prepare the data for plotting
plot_data <- grid %>%
  select(x, time, Probability_Density_Scaled)

# Define the positions of the potential walls for plotting
# We use vertical lines at x=0 and x=L and a horizontal line at y=0
walls <- data.frame(
  x = c(0, L),
  y = c(0, 0)
)

# Create the plot with ggplot2 and gganimate
p <- ggplot() +
  # Draw the potential walls as continuous black lines
  geom_vline(xintercept = c(0, L), color = "black", size = 1.2) +
  geom_segment(aes(x = 0, y = 0, xend = L, yend = 0), color = "black", size = 1.2) +
  # Draw the scaled probability density as a blue line
  geom_line(data = plot_data, aes(x = x, y = Probability_Density_Scaled), color = "blue", size = 1) +
  # Add labels and titles
  labs(title = "Time Evolution of Probability Density in an Infinite Square Well",
       subtitle = "Time: {round(frame_time, 2)} units",
       x = "Position (x)",
       y = "Scaled Probability Density",
       caption = "Black Lines: Walls and Base of the Potential Well") +
  theme_minimal() +
  # Configure the animation to reveal over time
  transition_time(time) +
  ease_aes('linear') +
  # Set the y-axis limits for better visualization
  ylim(0, max(potential$V_x) * 1.2)

# Render and save the animation as a GIF
animate(p, renderer = gifski_renderer("infinite_square_well_animation.gif"),
        fps = 20, duration = 10, width = 800, height = 600)


