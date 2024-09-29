# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(gganimate)
library(viridis)

# Función para generar los datos del flujo
generar_datos_flujo <- function(Ω) {
  R <- 1 # Radio de la tubería
  # Generar una cuadrícula de puntos en la sección transversal
  x_seq <- seq(-R, R, length.out = 50)
  y_seq <- seq(-R, R, length.out = 50)
  grid <- expand.grid(x = x_seq, y = y_seq)
  
  # Filtrar los puntos que están dentro de la tubería
  grid <- grid %>% filter(x^2 + y^2 <= R^2)
  
  # Calcular r y θ
  grid <- grid %>%
    mutate(
      r = sqrt(x^2 + y^2),
      θ = atan2(y, x)
    )
  
  # Calcular la velocidad angular ω(r)
  grid <- grid %>%
    mutate(
      ω = Ω * (1 - r / R)
    )
  
  # Calcular la velocidad tangencial v_theta
  grid <- grid %>%
    mutate(
      v_theta = r * ω
    )
  
  # Calcular las componentes de velocidad en coordenadas cartesianas
  grid <- grid %>%
    mutate(
      u = -v_theta * sin(θ),
      v = v_theta * cos(θ),
      velocidad = sqrt(u^2 + v^2),
      Ω = Ω # Añadimos Ω para usarlo en la animación
    )
  
  return(grid)
}

# Generar datos para una secuencia de valores de Ω
valores_Ω <- seq(0, 5, length.out = 30) # 30 valores desde 0 hasta 5
factor_escala <- 0.2 # Factor para ajustar el tamaño de las flechas

# Generar los datos combinados
datos_flujo <- do.call(rbind, lapply(valores_Ω, generar_datos_flujo))

# Crear el gráfico animado
p <- ggplot(datos_flujo, aes(x = x, y = y)) +
  geom_segment(aes(xend = x + u * factor_escala, yend = y + v * factor_escala, color = velocidad),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.5) +
  scale_color_viridis_c() +
  coord_fixed() +
  theme_minimal() +
  labs(title = 'Giro del fluido en la sección transversal',
       subtitle = 'Velocidad angular Ω = {closest_state}',
       x = 'x',
       y = 'y',
       color = 'Velocidad') +
  transition_states(Ω, transition_length = 2, state_length = 1) +
  ease_aes('linear')

# Guardar la animación como GIF
animate(p, nframes = 100, fps = 10, width = 600, height = 600, renderer = gifski_renderer("flujo_animado.gif"))

