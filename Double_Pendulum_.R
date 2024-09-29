# Conditional installation of required libraries
if(!require(deSolve)) install.packages("deSolve")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")

# Load the libraries
library(deSolve)
library(ggplot2)
library(plotly)

# Define the double pendulum system of differential equations
double_pendulum <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Equations of motion derived from Lagrangian mechanics
    delta <- theta2 - theta1
    
    den1 <- (m1 + m2) * l1 - m2 * l1 * cos(delta)^2
    den2 <- (l2 / l1) * den1
    
    dtheta1 <- omega1
    dtheta2 <- omega2
    
    domega1 <- (m2 * l1 * omega1^2 * sin(delta) * cos(delta) +
                  m2 * g * sin(theta2) * cos(delta) +
                  m2 * l2 * omega2^2 * sin(delta) -
                  (m1 + m2) * g * sin(theta1)) / den1
    
    domega2 <- (-m2 * l2 * omega2^2 * sin(delta) * cos(delta) +
                  (m1 + m2) * g * sin(theta1) * cos(delta) -
                  (m1 + m2) * l1 * omega1^2 * sin(delta) -
                  (m1 + m2) * g * sin(theta2)) / den2
    
    # Return the rate of change
    list(c(dtheta1, domega1, dtheta2, domega2))
  })
}

# Set the parameters of the system
parameters <- c(m1 = 1,    # Mass of the first pendulum bob (kg)
                m2 = 1,    # Mass of the second pendulum bob (kg)
                l1 = 1,    # Length of the first rod (m)
                l2 = 1,    # Length of the second rod (m)
                g  = 9.81) # Acceleration due to gravity (m/s^2)

# Set the initial state of the system
state <- c(theta1 = pi / 2,   # Initial angle of first pendulum (rad)
           omega1 = 0,        # Initial angular velocity of first pendulum (rad/s)
           theta2 = pi / 2,   # Initial angle of second pendulum (rad)
           omega2 = 0)        # Initial angular velocity of second pendulum (rad/s)

# Define the time sequence for which to solve the system
times <- seq(0, 20, by = 0.01)

# Solve the system of differential equations
out <- ode(y = state, times = times, func = double_pendulum, parms = parameters)

# Convert the output to a data frame
out_df <- as.data.frame(out)

# Calculate the (x, y) positions of the pendulum bobs
out_df$x1 <- parameters["l1"] * sin(out_df$theta1)
out_df$y1 <- -parameters["l1"] * cos(out_df$theta1)

out_df$x2 <- out_df$x1 + parameters["l2"] * sin(out_df$theta2)
out_df$y2 <- out_df$y1 - parameters["l2"] * cos(out_df$theta2)

# Plot the trajectory of the second pendulum bob in 2D using ggplot2
ggplot(out_df, aes(x = x2, y = y2, color = time)) +
  geom_path() +
  labs(title = "Double Pendulum Trajectory",
       x = "X Position (m)",
       y = "Y Position (m)",
       color = "Time (s)") +
  theme_minimal()

# Create an interactive 3D plot of the pendulum's phase space using plotly
plot_ly(out_df, x = ~theta1, y = ~theta2, z = ~omega1, color = ~time, type = "scatter3d", mode = "lines") %>%
  layout(title = "Double Pendulum Phase Space",
         scene = list(xaxis = list(title = "Theta1 (rad)"),
                      yaxis = list(title = "Theta2 (rad)"),
                      zaxis = list(title = "Omega1 (rad/s)")))