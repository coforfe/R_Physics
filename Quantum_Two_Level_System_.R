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
omega_0 <- 1    # Transition frequency (rad/s)
Omega <- 0.5     # Rabi frequency (rad/s)
delta <- 0       # Detuning (rad/s)

# Time range
time <- seq(0, 10, length.out = 500)

# Initialize data frame
data_two_level <- data.frame(time = time)

# Calculate probabilities
data_two_level$P0 <- (Omega^2 / (Omega^2 + delta^2)) * (1 + cos(sqrt(Omega^2 + delta^2) * time))
data_two_level$P1 <- 1 - data_two_level$P0

# Reshape data for plotting
data_two_level_long <- data_two_level %>%
  pivot_longer(cols = starts_with("P"),
               names_to = "State",
               values_to = "Probability") %>%
  mutate(State = factor(State, 
                        levels = c("P0", "P1"),
                        labels = c("Ground State", "Excited State")))

# Plot and animate the probabilities over time
p <- ggplot(data_two_level_long, aes(x = time, y = Probability, color = State)) +
  geom_line(size = 1.5) +
  labs(title = "Two-Level Quantum System Dynamics",
       x = "Time (s)",
       y = "Probability",
       color = "State") +
  theme_minimal() +
  transition_reveal(time) +
  labs(subtitle = "Time: {frame_along} s")

# Render and save the animation
animate(p, renderer = gifski_renderer("two_level_system.gif"), fps = 30, duration = 10, width = 800, height = 600)