# Cargar las librerías necesarias
library(ggplot2)
library(pracma)  # Usaremos 'hermite' de 'pracma' y 'trapz' para normalización

# ================================
# Pozo de Potencial Infinito
# ================================

# Definir la función de onda para el pozo de potencial infinito
psi_infinito <- function(x, n, L) {
  sqrt(2 / L) * sin(n * pi * x / L)
}

# Parámetros
L <- 1  # Longitud del pozo
n_values <- c(1, 2, 3)  # Niveles cuánticos
x_vals <- seq(0, L, length.out = 1000)  # Valores de x

# Crear un data frame para los datos de la gráfica
datos_infinito <- data.frame()

for (n in n_values) {
  psi_vals <- psi_infinito(x_vals, n, L)
  temp_data <- data.frame(x = x_vals, psi = psi_vals, n = as.factor(n))
  datos_infinito <- rbind(datos_infinito, temp_data)
}

# Generar la gráfica
ggplot(datos_infinito, aes(x = x, y = psi, color = n)) +
  geom_line() +
  labs(title = "Funciones de Onda en un Pozo de Potencial Infinito",
       x = "Posición x",
       y = expression(psi[n](x)),
       color = "Número Cuántico n") +
  theme_minimal()

# ================================
# Pozo de Potencial Finito
# ================================

# Definir constantes
hbar <- 1  # Constante reducida de Planck
m <- 1     # Masa de la partícula

# Definir la función de onda para el pozo de potencial finito
psi_finito <- function(x, n, L, V0) {
  # Cálculo de la energía
  En <- (n^2 * pi^2 * hbar^2) / (2 * m * L^2)
  # Verificar que la energía sea menor que V0
  if (En >= V0) {
    return(rep(NA, length(x)))
  }
  # Cálculo de los parámetros
  k <- sqrt(2 * m * En) / hbar
  alpha <- sqrt(2 * m * (V0 - En)) / hbar
  
  # Función de onda
  psi <- numeric(length(x))
  for (i in 1:length(x)) {
    xi <- x[i]
    if (xi < 0) {
      psi[i] <- exp(alpha * xi)
    } else if (xi >= 0 && xi <= L) {
      psi[i] <- sin(k * xi)
    } else {
      psi[i] <- exp(-alpha * (xi - L))
    }
  }
  # Normalizar la función de onda
  psi <- psi / sqrt(trapz(x, psi^2))
  return(psi)
}

# Parámetros
L <- 1    # Longitud del pozo
V0 <- 50  # Profundidad del pozo
n_values <- c(1, 2, 3)
x_vals <- seq(-1, 2, length.out = 1000)  # Valores de x extendidos

# Crear un data frame para los datos de la gráfica
datos_finito <- data.frame()

for (n in n_values) {
  psi_vals <- psi_finito(x_vals, n, L, V0)
  temp_data <- data.frame(x = x_vals, psi = psi_vals, n = as.factor(n))
  datos_finito <- rbind(datos_finito, temp_data)
}

# Generar la gráfica
ggplot(datos_finito, aes(x = x, y = psi, color = n)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Funciones de Onda en un Pozo de Potencial Finito",
       x = "Posición x",
       y = expression(psi[n](x)),
       color = "Número Cuántico n") +
  theme_minimal()

# ================================
# Oscilador Armónico Cuántico
# ================================

# Definir la función de onda para el oscilador armónico cuántico
psi_armonico <- function(x, n, m, omega) {
  hbar <- 1  # Constante reducida de Planck
  xi <- sqrt(m * omega / hbar) * x
  n_int <- as.integer(n)  # Aseguramos que 'n' es un entero
  coeficiente <- (1 / sqrt(2^n_int * factorial(n_int))) * (m * omega / (pi * hbar))^(1/4)
  hermite_vals <- hermite(n_int, xi)  # Usamos 'hermite' de 'pracma'
  psi <- coeficiente * exp(-xi^2 / 2) * hermite_vals
  return(psi)
}

# Parámetros
m <- 1
omega <- 1
n_values <- c(0, 1, 2)  # Niveles cuánticos (n comienza desde 0)
x_vals <- seq(-5, 5, length.out = 1000)

# Crear un data frame para los datos de la gráfica
datos_armonico <- data.frame()

for (n in n_values) {
  psi_vals <- psi_armonico(x_vals, n, m, omega)
  temp_data <- data.frame(x = x_vals, psi = psi_vals, n = as.factor(n))
  datos_armonico <- rbind(datos_armonico, temp_data)
}

# Generar la gráfica
ggplot(datos_armonico, aes(x = x, y = psi, color = n)) +
  geom_line() +
  labs(title = "Funciones de Onda del Oscilador Armónico Cuántico",
       x = "Posición x",
       y = expression(psi[n](x)),
       color = "Número Cuántico n") +
  theme_minimal()

