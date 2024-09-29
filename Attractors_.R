# Conditionally install and load required libraries
required_packages <- c("ggplot2", "deSolve", "gridExtra")

# Function to check and install missing packages
install_if_missing <- function(packages) {
  for(package in packages){
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# Install and load the required packages
install_if_missing(required_packages)

# Load the libraries
library(ggplot2)
library(deSolve)
library(gridExtra)

# Function to generate the Lorenz attractor
lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dx <- sigma * (y - x)
    dy <- x * (rho - z) - y
    dz <- x * y - beta * z
    list(c(dx, dy, dz))
  })
}

# Parameters and initial state for the Lorenz attractor
parameters_lorenz <- c(sigma = 10, rho = 28, beta = 8 / 3)
state_lorenz <- c(x = 1, y = 1, z = 1)
times <- seq(0, 50, by = 0.01)

# Solve the Lorenz system
out_lorenz <- ode(y = state_lorenz, times = times, func = lorenz, parms = parameters_lorenz)
out_lorenz <- as.data.frame(out_lorenz)

# Create plot for the Lorenz attractor
plot_lorenz <- ggplot(out_lorenz, aes(x = x, y = z)) +
  geom_path(color = "blue") +
  ggtitle("Lorenz Attractor") +
  theme_minimal()

# Function to generate the Rössler attractor
rossler <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dx <- -y - z
    dy <- x + a * y
    dz <- b + z * (x - c)
    list(c(dx, dy, dz))
  })
}

# Parameters and initial state for the Rössler attractor
parameters_rossler <- c(a = 0.2, b = 0.2, c = 5.7)
state_rossler <- c(x = 1, y = 1, z = 1)

# Solve the Rössler system
out_rossler <- ode(y = state_rossler, times = times, func = rossler, parms = parameters_rossler)
out_rossler <- as.data.frame(out_rossler)

# Create plot for the Rössler attractor
plot_rossler <- ggplot(out_rossler, aes(x = x, y = z)) +
  geom_path(color = "red") +
  ggtitle("Rössler Attractor") +
  theme_minimal()

# Display both plots side by side
grid.arrange(plot_lorenz, plot_rossler, ncol = 2)