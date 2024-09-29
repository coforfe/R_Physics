# ============================
# Modelo de Ising en 3D en R
# ============================

# Cargar librerías necesarias
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# 1. Inicialización de la Red de Spins en 3D
initialize_lattice_3D <- function(N) {
  # N: tamaño de la red (N x N x N)
  lattice <- array(sample(c(-1, 1), N^3, replace = TRUE), dim = c(N, N, N))
  return(lattice)
}

# 2. Cálculo de la Energía Total del Sistema en 3D
calculate_energy_3D <- function(lattice, J, h) {
  # lattice: array de spins
  # J: constante de acoplamiento
  # h: campo magnético externo
  N <- dim(lattice)[1]
  energy <- 0
  for (i in 1:N) {
    for (j in 1:N) {
      for (k in 1:N) {
        S <- lattice[i, j, k]
        # Vecinos con condiciones de frontera periódicas
        neighbors <- lattice[(i %% N) + 1, j, k] + 
          lattice[i, (j %% N) + 1, k] + 
          lattice[i, j, (k %% N) + 1] + 
          lattice[(i - 2) %% N + 1, j, k] + 
          lattice[i, (j - 2) %% N + 1, k] + 
          lattice[i, j, (k - 2) %% N + 1]
        energy <- energy - J * S * neighbors - h * S
      }
    }
  }
  return(energy / 2) # Cada par contado dos veces
}

# 3. Realización de un Paso de Metropolis en 3D
metropolis_step_3D <- function(lattice, beta, J, h) {
  # lattice: array de spins
  # beta: 1 / (k_B * T) (usualmente k_B = 1)
  # J: constante de acoplamiento
  # h: campo magnético externo
  N <- dim(lattice)[1]
  for (n in 1:(N^3)) {
    # Seleccionar un spin al azar
    i <- sample(1:N, 1)
    j <- sample(1:N, 1)
    k <- sample(1:N, 1)
    
    S <- lattice[i, j, k]
    # Calcular la suma de los spins vecinos con condiciones periódicas
    neighbors <- lattice[(i %% N) + 1, j, k] + 
      lattice[i, (j %% N) + 1, k] + 
      lattice[i, j, (k %% N) + 1] + 
      lattice[(i - 2) %% N + 1, j, k] + 
      lattice[i, (j - 2) %% N + 1, k] + 
      lattice[i, j, (k - 2) %% N + 1]
    
    # Calcular el cambio de energía si se invierte el spin
    delta_E <- 2 * S * (J * neighbors + h)
    
    # Decidir si se acepta el cambio
    if (delta_E < 0 || runif(1) < exp(-delta_E * beta)) {
      lattice[i, j, k] <- -S
    }
  }
  return(lattice)
}

# 4. Simulación del Modelo de Ising en 3D
simulate_ising_3D <- function(N, J, h, T_min, T_max, T_steps, equil_steps, mc_steps) {
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
  lattice <- initialize_lattice_3D(N)
  
  for (t in 1:length(temperaturas)) {
    T <- temperaturas[t]
    beta <- 1 / T
    
    # Pasos de equilibrio
    for (e in 1:equil_steps) {
      lattice <- metropolis_step_3D(lattice, beta, J, h)
    }
    
    # Pasos de Monte Carlo para promediar
    mag_sum <- 0
    for (m in 1:mc_steps) {
      lattice <- metropolis_step_3D(lattice, beta, J, h)
      mag_sum <- mag_sum + abs(mean(lattice))
    }
    magnetizaciones[t] <- mag_sum / mc_steps
    cat("Temperatura:", round(T, 3), "Magnetización promedio:", round(magnetizaciones[t], 5), "\n")
  }
  
  return(data.frame(Temperatura = temperaturas, Magnetizacion = magnetizaciones))
}

# 5. Visualización de la Magnetización vs Temperatura en 3D
plot_magnetization_3D <- function(data) {
  # data: data.frame con columnas 'Temperatura' y 'Magnetizacion'
  
  ggplot(data, aes(x = Temperatura, y = Magnetizacion)) +
    geom_line(color = "red") +
    labs(title = "Magnetización vs Temperatura en el Modelo de Ising 3D",
         x = "Temperatura (T)",
         y = "Magnetización Promedio") +
    theme_minimal()
}

# 6. Ejemplo de Uso
# Parámetros de la simulación
N <- 10             # Tamaño de la red (10x10x10)
J <- 1              # Constante de acoplamiento
h <- 0              # Campo magnético externo
T_min <- 2.0        # Temperatura mínima
T_max <- 5.0        # Temperatura máxima
T_steps <- 10       # Número de puntos en la temperatura
equil_steps <- 500   # Pasos de equilibrio
mc_steps <- 1000     # Pasos de Monte Carlo para promedio

# Ejecutar la simulación
set.seed(123) # Para reproducibilidad
resultados_3D <- simulate_ising_3D(N, J, h, T_min, T_max, T_steps, equil_steps, mc_steps)

# Mostrar los resultados
print(resultados_3D)

# Graficar los resultados
plot_magnetization_3D(resultados_3D)