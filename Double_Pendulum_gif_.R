# Conditional installation of required libraries
if(!require(deSolve)) install.packages("deSolve")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gganimate)) install.packages("gganimate")
if(!require(gifski)) install.packages("gifski")  # For saving the animation as a GIF

# Load the libraries
library(deSolve)
library(ggplot2)
library(gganimate)
library(gifski)

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
times <- seq(0, 20, by = 0.02)  # Smaller time step for smoother animation

# Solve the system of differential equations
out <- ode(y = state, times = times, func = double_pendulum, parms = parameters)

# Convert the output to a data frame
out_df <- as.data.frame(out)

# Calculate the (x, y) positions of the pendulum bobs
out_df$x1 <- parameters["l1"] * sin(out_df$theta1)
out_df$y1 <- -parameters["l1"] * cos(out_df$theta1)

out_df$x2 <- out_df$x1 + parameters["l2"] * sin(out_df$theta2)
out_df$y2 <- out_df$y1 - parameters["l2"] * cos(out_df$theta2)

# Prepare data for animation
library(tidyr)
anim_data <- out_df %>%
  select(time, x1, y1, x2, y2) %>%
  pivot_longer(cols = c(x1, y1, x2, y2), 
               names_to = c(".value", "bob"), 
               names_pattern = "([xy])(\\d)") %>%
  mutate(bob = factor(bob, levels = c("1", "2")))

# Plot and animate the double pendulum motion using ggplot2 and gganimate
p <- ggplot(anim_data, aes(x = x, y = y, group = bob)) +
  geom_line(data = subset(anim_data, bob == "1"), size = 1, color = "black", group = 1) +
  geom_line(data = subset(anim_data, bob == "2"), size = 1, color = "black", group = 1) +
  geom_point(size = 4, color = "red") +
  coord_equal() +
  labs(title = "Double Pendulum Simulation",
       x = "X Position (m)",
       y = "Y Position (m)",
       subtitle = "Time: {round(frame_time, 2)} s") +
  theme_minimal() +
  transition_time(time) +
  ease_aes('linear')

# Render and save the animation
animate(p, renderer = gifski_renderer(), fps = 50, duration = 20, width = 600, height = 600)
anim_save("double_pendulum.gif")