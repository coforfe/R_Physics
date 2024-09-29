# Definir constantes
G <- 6.674e-11        # Constante de gravitación universal (Nm²/kg²)
c <- 3e8              # Velocidad de la luz (m/s)

# Función para calcular la velocidad de escape
velocidad_escape <- function(M, R) {
  sqrt((2 * G * M) / R)
}

# Función para calcular el radio de Schwarzschild
radio_schwarzschild <- function(M) {
  (2 * G * M) / (c^2)
}

# Función para generar el gráfico
graficar_agujero_negro <- function(M) {
  # Definir un rango de radios desde 1 km hasta 10 veces el radio de Schwarzschild
  Rs <- radio_schwarzschild(M)
  R_min <- Rs * 0.1
  R_max <- Rs * 10
  R <- seq(R_min, R_max, length.out = 1000)
  
  # Calcular la velocidad de escape para cada radio
  ve <- velocidad_escape(M, R)
  
  # Crear el gráfico
  plot(R / 1000, ve / 1e6, type = "l", lwd = 2, col = "blue",
       xlab = "Radio (km)",
       ylab = "Velocidad de Escape (millones de m/s)",
       main = "Condición para la Formación de un Agujero Negro")
  
  # Añadir la línea de la velocidad de la luz
  abline(h = c / 1e6, col = "red", lwd = 2, lty = 2)
  
  # Añadir una línea vertical para el radio de Schwarzschild
  abline(v = Rs / 1000, col = "darkgreen", lwd = 2, lty = 3)
  
  # Añadir leyendas
  legend("topright",
         legend = c("Velocidad de Escape", "Velocidad de la Luz", "Radio de Schwarzschild"),
         col = c("blue", "red", "darkgreen"),
         lwd = 2, lty = c(1, 2, 3))
  
  # Mostrar el radio de Schwarzschild en el gráfico
  text(Rs / 1000, c / 1e6, labels = paste0("Rs = ", round(Rs / 1000, 2), " km"),
       pos = 4, col = "darkgreen")
}

# Ejemplo: Masa del Sol
masa_sol <- 1.988e30
graficar_agujero_negro(masa_sol)

# Ejemplo adicional: 10 masas solares
masa_10_sol <- 10 * 1.988e30
graficar_agujero_negro(masa_10_sol)

