# -------------------------------------------------------------
# Cálculo y representación del analema.
# -------------------------------------------------------------

# --- 1. Instalar y cargar las librerías necesarias ---

# Instalar las librerías (si no están instaladas)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")

# Cargar las librerías
library(ggplot2)
library(lubridate)

# --- 2. Definir las funciones para calcular la declinación solar y la ecuación del tiempo ---

# 2.1 Función para calcular la declinación solar (en grados)
calcular_declinacion_solar <- function(dia_del_anio) {
  # Convertir el día del año a ángulo en radianes
  gamma <- 2 * pi / 365 * (dia_del_anio - 1)
  
  # Calcular la declinación solar usando una aproximación
  declinacion <- (0.006918 -
                    0.399912 * cos(gamma) +
                    0.070257 * sin(gamma) -
                    0.006758 * cos(2 * gamma) +
                    0.000907 * sin(2 * gamma) -
                    0.002697 * cos(3 * gamma) +
                    0.00148 * sin(3 * gamma)) * (180 / pi)  # Convertir a grados
  
  return(declinacion)
}

# 2.2 Función para calcular la ecuación del tiempo (en minutos)
calcular_ecuacion_del_tiempo <- function(dia_del_anio) {
  # Convertir el día del año a ángulo en radianes
  gamma <- 2 * pi / 365 * (dia_del_anio - 1)
  
  # Calcular la ecuación del tiempo usando una aproximación
  EoT <- (229.18 * (0.000075 +
                      0.001868 * cos(gamma) -
                      0.032077 * sin(gamma) -
                      0.014615 * cos(2 * gamma) -
                      0.040849 * sin(2 * gamma)))  # En minutos
  
  return(EoT)
}

# --- 3. Calcular la declinación solar y la ecuación del tiempo para cada día del año ---

# Crear una secuencia de fechas para el año (año bisiesto o no)
anio <- 2024  # Puedes cambiar el año si lo deseas

if (leap_year(anio)) {
  num_dias <- 366
} else {
  num_dias <- 365
}

dias_del_anio <- 1:num_dias

# Calcular la declinación solar y la ecuación del tiempo para cada día
declinaciones <- sapply(dias_del_anio, calcular_declinacion_solar)
ecuacion_tiempo <- sapply(dias_del_anio, calcular_ecuacion_del_tiempo)

# Crear un data frame con los resultados
datos_analema <- data.frame(
  dia = dias_del_anio,
  declinacion = declinaciones,
  ecuacion_tiempo = ecuacion_tiempo
)

# --- 4. Representar el analema ---

# Crear el gráfico del analema
ggplot(datos_analema, aes(x = ecuacion_tiempo, y = declinacion)) +
  geom_path(color = 'blue') +
  geom_point(color = 'red', size = 1) +
  labs(title = paste('Analema Solar en el año', anio),
       x = 'Ecuación del Tiempo (minutos)',
       y = 'Declinación Solar (grados)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = ecuacion_tiempo[1], y = declinaciones[1], label = "1 Ene", hjust = 1.2, color = "darkgreen") +
  annotate("text", x = ecuacion_tiempo[round(num_dias/2)], y = declinaciones[round(num_dias/2)], label = "1 Jul", hjust = -0.1, color = "darkgreen")

