# Conditional installation of required libraries
if(!require(deSolve)) install.packages("deSolve")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")

# Load the libraries
library(deSolve)
library(ggplot2)
library(plotly)

# Define the Lorenz system of differential equations
lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Equations of the Lorenz system
    dx <- sigma * (y - x)
    dy <- x * (rho - z) - y
    dz <- x * y - beta * z
    # Return the rate of change
    list(c(dx, dy, dz))
  })
}

# Set the parameters of the system
parameters <- c(sigma = 10, rho = 28, beta = 8/3)

# Set the initial state of the system
state <- c(x = 1, y = 1, z = 1)

# Define the time sequence for which to solve the system
times <- seq(0, 50, by = 0.01)

# Solve the system of differential equations
out <- ode(y = state, times = times, func = lorenz, parms = parameters)

# Convert the output to a data frame
out_df <- as.data.frame(out)

# Plot the Lorenz attractor in 2D using ggplot2 (X vs Z)
ggplot(out_df, aes(x = x, y = z, color = time)) +
  geom_path() +
  labs(title = "Lorenz Attractor (X vs Z Plane)",
       x = "X",
       y = "Z",
       color = "Time") +
  theme_minimal()

# Create an interactive 3D plot of the Lorenz attractor using plotly
plot_ly(out_df, x = ~x, y = ~y, z = ~z, color = ~time, type = "scatter3d", mode = "lines") %>%
  layout(title = "Lorenz Attractor",
         scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z")))


