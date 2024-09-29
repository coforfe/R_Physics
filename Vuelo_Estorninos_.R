# Instalar y cargar las librerías necesarias
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gganimate)) install.packages("gganimate")
if(!require(gifski)) install.packages("gifski")

library(ggplot2)
library(gganimate)
library(gifski)

# Configurar los parámetros iniciales y funciones auxiliares
set.seed(123)      # Para reproducibilidad
N <- 200           # Número de pájaros aumentado
timesteps <- 200   # Número de pasos de tiempo aumentado
width <- 100       # Ancho del área de simulación
height <- 100      # Alto del área de simulación
max_speed <- 2     # Velocidad máxima
visual_range <- 10 # Rango de visión (para comportamiento de bandada)
max_force <- 0.05  # Fuerza máxima de aceleración

# Inicializar posiciones y velocidades aleatorias
birds <- data.frame(
  id = 1:N,
  x = runif(N, 0, width),
  y = runif(N, 0, height),
  vx = runif(N, -max_speed, max_speed),
  vy = runif(N, -max_speed, max_speed)
)

# Función para limitar la velocidad
limit_speed <- function(vx, vy, max_speed) {
  speed <- sqrt(vx^2 + vy^2)
  factor <- ifelse(speed > max_speed, max_speed / speed, 1)
  list(vx = vx * factor, vy = vy * factor)
}

# Función para limitar la fuerza (aceleración)
limit_force <- function(ax, ay, max_force) {
  force <- sqrt(ax^2 + ay^2)
  factor <- ifelse(force > max_force, max_force / force, 1)
  list(ax = ax * factor, ay = ay * factor)
}

# Simular el movimiento de la bandada
# Lista para almacenar posiciones en cada tiempo
positions <- vector("list", timesteps)

# Bucle de simulación
for (t in 1:timesteps) {
  # Crear una copia para cálculos simultáneos
  birds_prev <- birds
  
  # Matrices para cálculos vectoriales
  dx_matrix <- outer(birds_prev$x, birds_prev$x, "-")
  dy_matrix <- outer(birds_prev$y, birds_prev$y, "-")
  distance_matrix <- sqrt(dx_matrix^2 + dy_matrix^2)
  
  for (i in 1:N) {
    # Encontrar vecinos dentro del rango de visión
    neighbors <- which(distance_matrix[i, ] < visual_range & distance_matrix[i, ] > 0)
    
    # Inicializar aceleraciones
    ax <- 0
    ay <- 0
    
    if (length(neighbors) > 0) {
      # Separación: evitar colisiones
      separation_x <- sum((birds_prev$x[i] - birds_prev$x[neighbors]) / distance_matrix[i, neighbors])
      separation_y <- sum((birds_prev$y[i] - birds_prev$y[neighbors]) / distance_matrix[i, neighbors])
      
      # Alineación: igualar velocidad con vecinos
      avg_vx <- mean(birds_prev$vx[neighbors])
      avg_vy <- mean(birds_prev$vy[neighbors])
      alignment_x <- avg_vx - birds_prev$vx[i]
      alignment_y <- avg_vy - birds_prev$vy[i]
      
      # Cohesión: moverse hacia el centro de los vecinos
      center_x <- mean(birds_prev$x[neighbors])
      center_y <- mean(birds_prev$y[neighbors])
      cohesion_x <- center_x - birds_prev$x[i]
      cohesion_y <- center_y - birds_prev$y[i]
      
      # Sumar fuerzas con pesos
      ax <- ax + (separation_x * 1.0) + (alignment_x * 0.5) + (cohesion_x * 0.01)
      ay <- ay + (separation_y * 1.0) + (alignment_y * 0.5) + (cohesion_y * 0.01)
    }
    
    # Aleatoriedad pequeña
    ax <- ax + runif(1, -0.02, 0.02)
    ay <- ay + runif(1, -0.02, 0.02)
    
    # Limitar la fuerza de aceleración
    limited_force <- limit_force(ax, ay, max_force)
    ax <- limited_force$ax
    ay <- limited_force$ay
    
    # Actualizar velocidades
    birds$vx[i] <- birds_prev$vx[i] + ax
    birds$vy[i] <- birds_prev$vy[i] + ay
    
    # Limitar velocidad
    limited_speed <- limit_speed(birds$vx[i], birds$vy[i], max_speed)
    birds$vx[i] <- limited_speed$vx
    birds$vy[i] <- limited_speed$vy
  }
  
  # Actualizar posiciones
  birds$x <- birds_prev$x + birds$vx
  birds$y <- birds_prev$y + birds$vy
  
  # Manejar bordes (efecto "wrap around")
  birds$x <- (birds$x + width) %% width
  birds$y <- (birds$y + height) %% height
  
  # Almacenar posiciones
  positions[[t]] <- data.frame(birds, time = t)
}

# Combinar todas las posiciones
positions_df <- do.call(rbind, positions)

# Crear y guardar la animación
# Crear la trama base
p <- ggplot(positions_df, aes(x = x, y = y)) +
  geom_point(color = "black", size = 1) +
  xlim(0, width) +
  ylim(0, height) +
  theme_void() +
  transition_time(time) +
  ease_aes('linear')

# Generar y guardar la animación en formato gif
animate(p, nframes = timesteps, fps = 20, width = 600, height = 600, renderer = gifski_renderer("estorninos.gif"))