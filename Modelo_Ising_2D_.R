# ============================
# Modelo de Ising en R
# ============================

# Cargar librerías necesarias
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# 1. Inicialización de la Red de Spins
initialize_lattice <- function(N) {
  # N: tamaño de la red (N x N)
  lattice <- matrix(sample(c(-1, 1), N * N, replace = TRUE), nrow = N, ncol = N)
  return(lattice)
}

# 2. Cálculo de la Energía Total del Sistema
calculate_energy <- function(lattice, J, h) {
  # lattice: matriz de spins
  # J: constante de acoplamiento
  # h: campo magnético externo
  N <- nrow(lattice)
  energy <- 0
  for (i in 1:N) {
    for (j in 1:N) {
      S = lattice[i, j]
      # Vecinos con condiciones de frontera periódicas
      neighbors = lattice[(i %% N) + 1, j] + 
        lattice[i, (j %% N) + 1] + 
        lattice[(i - 2) %% N + 1, j] + 
        lattice[i, (j - 2) %% N + 1]
      energy <- energy - J * S * neighbors - h * S
    }
  }
  return(energy / 2) # Cada par contado dos veces
}

# 3. Realización de un Paso de Metropolis
metropolis_step <- function(lattice, beta, J, h) {
  # lattice: matriz de spins
  # beta: 1 / (k_B * T) (usualmente k_B = 1)
  # J: constante de acoplamiento
  # h: campo magnético externo
  N <- nrow(lattice)
  for (n in 1:(N*N)) {
    # Seleccionar un spin al azar
    i <- sample(1:N, 1)
    j <- sample(1:N, 1)
    
    S = lattice[i, j]
    # Calcular la suma de los spins vecinos con condiciones periódicas
    neighbors = lattice[(i %% N) + 1, j] + 
      lattice[i, (j %% N) + 1] + 
      lattice[(i - 2) %% N + 1, j] + 
      lattice[i, (j - 2) %% N + 1]
    
    # Calcular el cambio de energía si se invierte el spin
    delta_E = 2 * S * (J * neighbors + h)
    
    # Decidir si se acepta el cambio
    if (delta_E < 0 || runif(1) < exp(-delta_E * beta)) {
      lattice[i, j] <- -S
    }
  }
  return(lattice)
}

# 4. Simulación del Modelo de Ising
simulate_ising <- function(N, J, h, T_min, T_max, T_steps, equil_steps, mc_steps) {
  # N: tamaño de la red
  # J: constante de acoplamiento
  # h: campo magnético externo
  # T_min, T_max: rango de temperaturas
  # T_steps: número de pasos en la temperatura
  # equil_steps: pasos de equilibrio
  # mc_steps: pasos de Monte Carlo para promedio
  
  temperaturas <- seq(T_min, T_max, length.out = T_steps)
  magnetizaciones <- numeric(length(temperaturas))
  
  # Inicializar la red
  lattice <- initialize_lattice(N)
  
  for (t in 1:length(temperaturas)) {
    T <- temperaturas[t]
    beta <- 1 / T
    
    # Pasos de equilibrio
    for (e in 1:equil_steps) {
      lattice <- metropolis_step(lattice, beta, J, h)
    }
    
    # Pasos de Monte Carlo para promediar
    mag_sum <- 0
    for (m in 1:mc_steps) {
      lattice <- metropolis_step(lattice, beta, J, h)
      mag_sum <- mag_sum + abs(mean(lattice))
    }
    magnetizaciones[t] <- mag_sum / mc_steps
    cat("Temperatura:", round(T, 3), "Magnetización promedio:", round(magnetizaciones[t], 5), "\n")
  }
  
  return(data.frame(Temperatura = temperaturas, Magnetizacion = magnetizaciones))
}

# 5. Visualización de la Magnetización vs Temperatura
plot_magnetization <- function(data) {
  # data: data.frame con columnas 'Temperatura' y 'Magnetizacion'
  
  ggplot(data, aes(x = Temperatura, y = Magnetizacion)) +
    geom_line(color = "blue") +
    labs(title = "Magnetización vs Temperatura en el Modelo de Ising",
         x = "Temperatura (T)",
         y = "Magnetización Promedio") +
    theme_minimal()
}

# 6. Ejemplo de Uso
# Parámetros de la simulación
N <- 20            # Tamaño de la red (20x20)
J <- 1             # Constante de acoplamiento
h <- 0             # Campo magnético externo
T_min <- 1.5       # Temperatura mínima
T_max <- 3.5       # Temperatura máxima
T_steps <- 20      # Número de puntos en la temperatura
equil_steps <- 1000  # Pasos de equilibrio
mc_steps <- 5000     # Pasos de Monte Carlo para promedio

# Ejecutar la simulación
set.seed(123) # Para reproducibilidad
resultados <- simulate_ising(N, J, h, T_min, T_max, T_steps, equil_steps, mc_steps)

# Mostrar los resultados
print(resultados)

# Graficar los resultados
plot_magnetization(resultados)