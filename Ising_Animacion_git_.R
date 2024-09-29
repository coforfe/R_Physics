# ============================
# Simulación y Visualización del Modelo de Ising en 2D
# ============================

# Cargar librerías necesarias
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("gganimate")) {
  install.packages("gganimate")
  library(gganimate)
}

if (!require("reshape2")) {
  install.packages("reshape2")
  library(reshape2)
}

# 1. Inicialización de la Red de Spins
initialize_lattice <- function(N) {
  # N: tamaño de la red (N x N)
  lattice <- matrix(sample(c(-1, 1), N * N, replace = TRUE), nrow = N, ncol = N)
  return(lattice)
}

# 2. Realización de un Paso de Metropolis
metropolis_step <- function(lattice, beta, J, h) {
  # lattice: matriz de spins
  # beta: 1 / (k_B * T) (usualmente k_B = 1)
  # J: constante de acoplamiento
  # h: campo magnético externo
  N <- nrow(lattice)
  for (n in 1:(N * N)) {
    # Seleccionar un spin al azar
    i <- sample(1:N, 1)
    j <- sample(1:N, 1)
    
    S <- lattice[i, j]
    # Calcular la suma de los spins vecinos con condiciones periódicas
    neighbors <- lattice[(i %% N) + 1, j] + 
      lattice[i, (j %% N) + 1] + 
      lattice[(i - 2) %% N + 1, j] + 
      lattice[i, (j - 2) %% N + 1]
    
    # Calcular el cambio de energía si se invierte el spin
    delta_E <- 2 * S * (J * neighbors + h)
    
    # Decidir si se acepta el cambio
    if (delta_E < 0 || runif(1) < exp(-delta_E * beta)) {
      lattice[i, j] <- -S
    }
  }
  return(lattice)
}

# 3. Simulación del Modelo de Ising y Captura de Configuraciones
simulate_ising_capture <- function(N, J, h, T_min, T_max, T_steps, equil_steps, mc_steps) {
  # N: tamaño de la red
  # J: constante de acoplamiento
  # h: campo magnético externo
  # T_min, T_max: rango de temperaturas
  # T_steps: número de pasos en la temperatura
  # equil_steps: pasos de equilibrio
  # mc_steps: pasos de Monte Carlo para captura
  
  temperaturas <- seq(T_min, T_max, length.out = T_steps)
  
  # Data frame para almacenar las configuraciones y temperaturas
  all_configs <- data.frame()
  
  # Inicializar la red
  lattice <- initialize_lattice(N)
  
  for (t in 1:length(temperaturas)) {
    T <- temperaturas[t]
    beta <- 1 / T
    
    # Pasos de equilibrio
    for (e in 1:equil_steps) {
      lattice <- metropolis_step(lattice, beta, J, h)
    }
    
    # Capturar la configuración actual
    # Convertir la matriz a un data frame para ggplot2
    config_df <- melt(lattice)
    colnames(config_df) <- c("X", "Y", "Spin")
    config_df$Temperatura <- T
    all_configs <- rbind(all_configs, config_df)
    
    cat("Temperatura:", round(T, 3), "capturada.\n")
  }
  
  return(all_configs)
}

# 4. Generación de la Animación
create_animation <- function(config_data, N, output_file = "ising_animation.gif") {
  # config_data: data.frame con columnas 'X', 'Y', 'Spin', 'Temperatura'
  # N: tamaño de la red
  # output_file: nombre del archivo de salida para la animación
  
  # Crear el gráfico estático con ggplot2
  p <- ggplot(config_data, aes(x = X, y = Y, fill = factor(Spin))) +
    geom_tile(color = "grey") +
    scale_fill_manual(values = c("-1" = "white", "1" = "black"), 
                      name = "Spin") +
    scale_y_reverse() +  # Para que el origen esté en la esquina superior izquierda
    coord_fixed() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none") +
    ggtitle('Temperatura: {round(frame_time, 2)}') +
    transition_time(Temperatura) +
    ease_aes('linear')
  
  # Renderizar la animación
  anim <- animate(p, nframes = length(unique(config_data$Temperatura)), 
                  fps = 2, width = 600, height = 600, renderer = gifski_renderer())
  
  # Guardar la animación
  anim_save(output_file, animation = anim)
  
  cat("Animación guardada como", output_file, "\n")
}

# 5. Ejemplo de Uso
# Parámetros de la simulación
# N <- 20            # Tamaño de la red (20x20)
N <- 40            # Tamaño de la red (20x20)
J <- 1             # Constante de acoplamiento
h <- 0             # Campo magnético externo
T_min <- 1.5       # Temperatura mínima
T_max <- 3.5       # Temperatura máxima
T_steps <- 20      # Número de puntos en la temperatura
equil_steps <- 1000  # Pasos de equilibrio
mc_steps <- 0         # No se realizan pasos adicionales después de equilibrar

# Ejecutar la simulación y capturar configuraciones
set.seed(123) # Para reproducibilidad
configuraciones <- simulate_ising_capture(N, J, h, T_min, T_max, T_steps, equil_steps, mc_steps)

# Crear y guardar la animación
create_animation(configuraciones, N, output_file = "ising_animation.gif")


