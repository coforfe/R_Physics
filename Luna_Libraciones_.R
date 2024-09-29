# -------------------------------------------------------------
# Cálculo y representación de las libraciones lunares.
# -------------------------------------------------------------

# --- 1. Instalar y cargar las librerías necesarias ---

# Instalar las librerías (si no están instaladas)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")

# Cargar las librerías
library(ggplot2)
library(lubridate)
library(dplyr)

# --- 2. Definir funciones para calcular las libraciones lunares ---

# 2.1 Función para calcular el Día Juliano (JD)
calcular_dia_juliano <- function(fecha) {
  anio <- year(fecha)
  mes <- month(fecha)
  dia <- day(fecha)
  hora <- hour(fecha)
  minuto <- minute(fecha)
  segundo <- second(fecha)
  
  # Convertir la fecha a UTC
  fecha_utc <- with_tz(fecha, tzone = "UTC")
  
  # Calcular la fracción del día
  fraccion_dia <- (hour(fecha_utc) + (minute(fecha_utc) + second(fecha_utc) / 60) / 60) / 24
  
  # Ajustar mes y año si el mes es enero o febrero
  if (mes <= 2) {
    anio <- anio - 1
    mes <- mes + 12
  }
  
  # Calcular los componentes necesarios
  A <- floor(anio / 100)
  B <- 2 - A + floor(A / 4)
  
  # Calcular el Día Juliano
  JD <- floor(365.25 * (anio + 4716)) +
    floor(30.6001 * (mes + 1)) +
    dia + fraccion_dia + B - 1524.5
  
  return(JD)
}

# 2.2 Función para calcular las libraciones en longitud y latitud
calcular_libraciones <- function(JD) {
  T <- (JD - 2451545.0) / 36525
  
  # Elementos orbitales medios de la Luna (en grados)
  L_prime <- (218.3164591 + 481267.88134236 * T) %% 360  # Longitud media de la Luna
  D <- (297.8502042 + 445267.1115168 * T) %% 360         # Elongación media del Sol
  M <- (357.5291092 + 35999.0502909 * T) %% 360          # Anomalía media del Sol
  M_prime <- (134.9634114 + 477198.8676313 * T) %% 360   # Anomalía media de la Luna
  F <- (93.2720993 + 483202.0175273 * T) %% 360          # Distancia media desde el nodo ascendente
  
  # Convertir a radianes
  deg_to_rad <- pi / 180
  L_prime_rad <- L_prime * deg_to_rad
  D_rad <- D * deg_to_rad
  M_rad <- M * deg_to_rad
  M_prime_rad <- M_prime * deg_to_rad
  F_rad <- F * deg_to_rad
  
  # Cálculo de la libración en longitud (en grados)
  rho <- (-0.0278 * sin(M_prime_rad) +
            0.0222 * sin(F_rad) -
            0.0066 * sin(M_prime_rad - 2 * F_rad) +
            0.0007 * sin(M_prime_rad + 2 * F_rad) -
            0.0005 * sin(2 * M_prime_rad)) * (180 / pi)
  
  # Cálculo de la libración en latitud (en grados)
  sigma <- (0.0267 * sin(M_prime_rad) +
              0.0033 * sin(M_prime_rad - 2 * F_rad) -
              0.0004 * sin(M_prime_rad + 2 * F_rad) -
              0.0003 * sin(2 * M_prime_rad)) * (180 / pi)
  
  return(list(libracion_longitud = rho, libracion_latitud = sigma))
}

# --- 3. Generar una secuencia de fechas para analizar las libraciones ---

# 3.1 Definir el período de tiempo
fecha_inicial <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
fecha_final <- as.POSIXct("2024-12-31 23:59:59", tz = "UTC")

# Crear una secuencia de fechas cada día
fechas <- seq(from = fecha_inicial, to = fecha_final, by = "1 day")

# --- 4. Calcular las libraciones para cada fecha ---

# Crear un data frame para almacenar los resultados
datos_libraciones <- data.frame(
  fecha = fechas,
  libracion_longitud = numeric(length(fechas)),
  libracion_latitud = numeric(length(fechas))
)

# Calcular las libraciones para cada fecha
for (i in seq_along(fechas)) {
  JD <- calcular_dia_juliano(fechas[i])
  libraciones <- calcular_libraciones(JD)
  
  datos_libraciones$libracion_longitud[i] <- libraciones$libracion_longitud
  datos_libraciones$libracion_latitud[i] <- libraciones$libracion_latitud
}

# --- 5. Representar gráficamente las libraciones ---

# 5.1 Gráfico de libración en longitud a lo largo del año
ggplot(datos_libraciones, aes(x = fecha, y = libracion_longitud)) +
  geom_line(color = 'blue') +
  labs(title = 'Libración en Longitud de la Luna en 2024',
       x = 'Fecha',
       y = 'Libración en Longitud (grados)') +
  theme_minimal()

# 5.2 Gráfico de libración en latitud a lo largo del año
ggplot(datos_libraciones, aes(x = fecha, y = libracion_latitud)) +
  geom_line(color = 'red') +
  labs(title = 'Libración en Latitud de la Luna en 2024',
       x = 'Fecha',
       y = 'Libración en Latitud (grados)') +
  theme_minimal()

# 5.3 Gráfico combinado de libración en longitud y latitud
ggplot(datos_libraciones) +
  geom_line(aes(x = fecha, y = libracion_longitud, color = 'Libración en Longitud')) +
  geom_line(aes(x = fecha, y = libracion_latitud, color = 'Libración en Latitud')) +
  scale_color_manual(values = c('Libración en Longitud' = 'blue', 'Libración en Latitud' = 'red')) +
  labs(title = 'Libraciones de la Luna en 2024',
       x = 'Fecha',
       y = 'Libración (grados)',
       color = 'Tipo de Libración') +
  theme_minimal()

# 5.4 Representación de las libraciones en el plano
ggplot(datos_libraciones, aes(x = libracion_longitud, y = libracion_latitud)) +
  geom_point(color = 'purple', alpha = 0.7) +
  labs(title = 'Combinación de Libraciones de la Luna en 2024',
       x = 'Libración en Longitud (grados)',
       y = 'Libración en Latitud (grados)') +
  theme_minimal()

