# Conditional installation of required libraries
if(!require(deSolve)) install.packages("deSolve")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gganimate)) install.packages("gganimate")
if(!require(gifski)) install.packages("gifski")  # For saving the animation as a GIF
if(!require(tidyr)) install.packages("tidyr")    # For data manipulation

# Load the libraries
library(deSolve)
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyr)
library(dplyr)

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
out_df <- out_df %>%
  mutate(x1 = parameters["l1"] * sin(theta1),
         y1 = -parameters["l1"] * cos(theta1),
         x2 = x1 + parameters["l2"] * sin(theta2),
         y2 = y1 - parameters["l2"] * cos(theta2))

# Prepare data for animation
anim_data <- out_df %>%
  select(time, x1, y1, x2, y2) %>%
  pivot_longer(cols = starts_with("x"),
               names_to = c("coord", "bob"),
               names_pattern = "(x)(\\d)") %>%
  rename(x = value) %>%
  pivot_longer(cols = starts_with("y"),
               names_to = c("coord2", "bob2"),
               names_pattern = "(y)(\\d)") %>%
  rename(y = value) %>%
  filter(bob == bob2) %>%
  select(time, bob, x, y) %>%
  mutate(bob = factor(bob, levels = c("1", "2")))

# Plot and animate the double pendulum motion with trails using ggplot2 and gganimate
p <- ggplot(anim_data, aes(x = x, y = y, group = bob, color = bob)) +
  # Draw trails
  geom_path(size = 0.7, alpha = 0.6) +
  # Draw the rods and bobs
  geom_segment(aes(x = 0, y = 0, xend = x, yend = y),
               data = anim_data %>% filter(bob == "1"), size = 1, color = "black") +
  geom_segment(aes(x = x1, y = y1, xend = x, yend = y),
               data = anim_data %>% filter(bob == "2") %>%
                 left_join(out_df %>% select(time, x1, y1), by = "time"),
               size = 1, color = "black") +
  geom_point(size = 4) +
  scale_color_manual(values = c("red", "blue"), labels = c("Bob 1", "Bob 2")) +
  coord_equal() +
  labs(title = "Double Pendulum Simulation with Trails",
       x = "X Position (m)",
       y = "Y Position (m)",
       subtitle = "Time: {round(frame_along, 2)} s",
       color = "Bob") +
  theme_minimal() +
  transition_reveal(time)

# Render and save the animation
animate(p, renderer = gifski_renderer(), fps = 50, duration = 20, width = 600, height = 600)
anim_save("double_pendulum_with_trails.gif")