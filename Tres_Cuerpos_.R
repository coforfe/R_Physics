# Descripción:
# Este código simula el movimiento de tres cuerpos bajo la influencia de la gravedad mutua.
# Se utilizan las ecuaciones de movimiento de Newton y se resuelven numéricamente
# utilizando el método de Runge-Kutta de cuarto orden.
# Los resultados se visualizan utilizando ggplot2 y se crea una animación con gganimate.

# Instalación condicional de paquetes necesarios
list.of.packages <- c("ggplot2", "gganimate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(gganimate)

# Parámetros
G <- 1  # Constante gravitacional
m1 <- 1  # Masa del cuerpo 1
m2 <- 1  # Masa del cuerpo 2
m3 <- 1  # Masa del cuerpo 3

# Condiciones iniciales
state <- c(
  x1 = -1, y1 = 0, vx1 = 0,  vy1 = 0.5,
  x2 = 1,  y2 = 0, vx2 = 0,  vy2 = -0.5,
  x3 = 0,  y3 = 1, vx3 = -0.5, vy3 = 0
)

# Tiempo
dt <- 0.01
tmax <- 20
time <- seq(0, tmax, dt)

# Función que calcula las derivadas
derivatives <- function(state) {
  x1 <- state[1]; y1 <- state[2]; vx1 <- state[3]; vy1 <- state[4]
  x2 <- state[5]; y2 <- state[6]; vx2 <- state[7]; vy2 <- state[8]
  x3 <- state[9]; y3 <- state[10]; vx3 <- state[11]; vy3 <- state[12]
  
  # Distancias
  r12 <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  r13 <- sqrt((x3 - x1)^2 + (y3 - y1)^2)
  r23 <- sqrt((x3 - x2)^2 + (y3 - y2)^2)
  
  # Fuerzas
  fx1 <- G * m1 * m2 * (x2 - x1) / r12^3 + G * m1 * m3 * (x3 - x1) / r13^3
  fy1 <- G * m1 * m2 * (y2 - y1) / r12^3 + G * m1 * m3 * (y3 - y1) / r13^3
  
  fx2 <- G * m2 * m1 * (x1 - x2) / r12^3 + G * m2 * m3 * (x3 - x2) / r23^3
  fy2 <- G * m2 * m1 * (y1 - y2) / r12^3 + G * m2 * m3 * (y3 - y2) / r23^3
  
  fx3 <- G * m3 * m1 * (x1 - x3) / r13^3 + G * m3 * m2 * (x2 - x3) / r23^3
  fy3 <- G * m3 * m1 * (y1 - y3) / r13^3 + G * m3 * m2 * (y2 - y3) / r23^3
  
  return(c(
    vx1, vy1, fx1 / m1, fy1 / m1,
    vx2, vy2, fx2 / m2, fy2 / m2,
    vx3, vy3, fx3 / m3, fy3 / m3
  ))
}

# Método de Runge-Kutta de cuarto orden
states <- data.frame(time = time, matrix(0, nrow = length(time), ncol = length(state)))
colnames(states)[-1] <- names(state)
states[1, -1] <- state

for (i in 2:length(time)) {
  s <- as.numeric(states[i - 1, -1])
  
  k1 <- derivatives(s)
  k2 <- derivatives(s + dt * k1 / 2)
  k3 <- derivatives(s + dt * k2 / 2)
  k4 <- derivatives(s + dt * k3)
  
  s_next <- s + dt * (k1 + 2 * k2 + 2 * k3 + k4) / 6
  
  states[i, -1] <- s_next
}

# Convertir datos para ggplot
library(tidyr)
library(dplyr)
data_long <- states %>%
  select(time, x1, y1, x2, y2, x3, y3) %>%
  gather(key = "variable", value = "value", -time) %>%
  separate(variable, into = c("coord", "body"), sep = 1) %>%
  spread(key = "coord", value = "value")

# Crear la animación
p <- ggplot(data_long, aes(x = x, y = y, color = body)) +
  geom_point(size = 3) +
  xlim(-2, 2) + ylim(-2, 2) +
  theme_minimal() +
  transition_time(time) +
  labs(title = 'Tiempo: {frame_time}')

# Renderizar la animación
animate(p, nframes = length(time), fps = 30)