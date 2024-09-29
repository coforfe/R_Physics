# Conditionally install and load required libraries
required_packages <- c("ggplot2", "deSolve", "gganimate", "transformr", "gifski", "png", "gridExtra")

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
library(gganimate)
library(transformr)
library(gifski)
library(png)
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
times <- seq(0, 50, by = 0.05)  # Increased step size for faster animations

# Solve the Lorenz system
out_lorenz <- ode(y = state_lorenz, times = times, func = lorenz, parms = parameters_lorenz)
out_lorenz <- as.data.frame(out_lorenz)

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

# Create a cumulative frame identifier for animation
out_lorenz$frame <- 1:nrow(out_lorenz)
out_rossler$frame <- 1:nrow(out_rossler)

# Create animation for the Lorenz attractor
plot_lorenz_anim <- ggplot(out_lorenz, aes(x = x, y = z)) +
  geom_path(color = "blue", alpha = 0.5) +
  geom_point(color = "blue") +
  transition_reveal(frame) +
  ggtitle("Lorenz Attractor") +
  theme_minimal() +
  labs(x = "X", y = "Z") +
  theme(plot.title = element_text(hjust = 0.5))

# Create animation for the Rössler attractor
plot_rossler_anim <- ggplot(out_rossler, aes(x = x, y = z)) +
  geom_path(color = "red", alpha = 0.5) +
  geom_point(color = "red") +
  transition_reveal(frame) +
  ggtitle("Rössler Attractor") +
  theme_minimal() +
  labs(x = "X", y = "Z") +
  theme(plot.title = element_text(hjust = 0.5))

# Render the animations
# Note: Rendering animations can take some time depending on the system's performance

# Save Lorenz attractor animation as GIF
anim_lorenz <- animate(plot_lorenz_anim, renderer = gifski_renderer(loop = TRUE), duration = 10, fps = 30, width = 600, height = 600)
anim_save("lorenz_attractor.gif", animation = anim_lorenz)

# Save Rössler attractor animation as GIF
anim_rossler <- animate(plot_rossler_anim, renderer = gifski_renderer(loop = TRUE), duration = 10, fps = 30, width = 600, height = 600)
anim_save("rossler_attractor.gif", animation = anim_rossler)

# Optionally, display both animations side by side in RStudio's Viewer or an HTML output
# Note: Displaying animated GIFs within R plots is not straightforward.
# Instead, you can view the saved GIFs in your default image viewer or web browser.

# If using RMarkdown or similar, you can embed the GIFs as follows:
# For demonstration, here is how you might display them in RStudio's Viewer pane.

# Create temporary HTML to display GIFs side by side
html_content <- '
<html>
  <body style="display: flex; justify-content: center; align-items: center;">
    <div style="margin-right: 10px;">
      <h3>Lorenz Attractor</h3>
      <img src="lorenz_attractor.gif" width="600" height="600">
    </div>
    <div>
      <h3>Rössler Attractor</h3>
      <img src="rossler_attractor.gif" width="600" height="600">
    </div>
  </body>
</html>
'

# Write the HTML content to a temporary file
temp_html <- tempfile(fileext = ".html")
writeLines(html_content, temp_html)

# Open the HTML file in the default web browser
browseURL(temp_html)

# Alternatively, if you are using RStudio, you can use the Viewer pane:
# library(htmltools)
# browsable(
#   tagList(
#     tags$h3("Lorenz Attractor"),
#     tags$img(src = "lorenz_attractor.gif", width = "600", height = "600"),
#     tags$h3("Rössler Attractor"),
#     tags$img(src = "rossler_attractor.gif", width = "600", height = "600")
#   )
# )