# -------------------------------------------------------------
# Cálculo y representación en 2D de la trayectoria del Sol
# vista desde Madrid en una fecha específica.
# -------------------------------------------------------------

# --- 1. Instalar y cargar las librerías necesarias ---

# Instalar las librerías (si no están instaladas)
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggplot2")) install.packages("ggplot2")

# Cargar las librerías
library(lubridate)
library(ggplot2)

# --- 2. Funciones para calcular la posición del Sol ---

# 2.1 Función para calcular el Día Juliano (JD) vectorizado
calcular_dia_juliano_vectorizado <- function(fecha) {
  anio <- year(fecha)
  mes <- month(fecha)
  dia <- day(fecha)
  hora <- hour(fecha)
  minuto <- minute(fecha)
  segundo <- second(fecha)
  
  # Convertir la fecha a UTC
  fecha_utc <- with_tz(fecha, tzone = "UTC")
  
  # Calcular la fracción del día
  fraccion_dia <- (hora + (minuto + segundo / 60) / 60) / 24
  
  # Ajustar mes y año si el mes es enero o febrero
  idx <- mes <= 2
  anio[idx] <- anio[idx] - 1
  mes[idx] <- mes[idx] + 12
  
  # Calcular los componentes necesarios
  A <- floor(anio / 100)
  B <- 2 - A + floor(A / 4)
  
  # Calcular el Día Juliano
  JD <- floor(365.25 * (anio + 4716)) +
    floor(30.6001 * (mes + 1)) +
    dia + fraccion_dia + B - 1524.5
  
  return(JD)
}

# 2.2 Función para calcular la posición del Sol (vectorizada)
calcular_posicion_sol <- function(JD) {
  T <- (JD - 2451545.0) / 36525
  
  # Longitud geométrica media del Sol (en grados)
  L0 <- 280.46646 + 36000.76983 * T + 0.0003032 * T^2
  L0 <- L0 %% 360
  
  # Anomalía media del Sol (en grados)
  M <- 357.52911 + 35999.05029 * T - 0.0001537 * T^2
  
  # Ecuación del centro
  C <- (1.914602 - 0.004817 * T - 0.000014 * T^2) * sin(M * pi / 180) +
    (0.019993 - 0.000101 * T) * sin(2 * M * pi / 180) +
    0.000289 * sin(3 * M * pi / 180)
  
  # Longitud eclíptica aparente del Sol (en grados)
  lambda <- L0 + C
  
  # Oblicuidad de la eclíptica (en grados)
  epsilon0 <- 23 + 26 / 60 + 21.448 / 3600 -
    (46.8150 * T + 0.00059 * T^2 - 0.001813 * T^3) / 3600
  
  # Ascensión recta y declinación del Sol
  lambda_rad <- lambda * pi / 180
  epsilon_rad <- epsilon0 * pi / 180
  alpha <- atan2(cos(epsilon_rad) * sin(lambda_rad), cos(lambda_rad))
  delta <- asin(sin(epsilon_rad) * sin(lambda_rad))
  
  # Convertir ascensión recta de radianes a grados
  alpha_deg <- alpha * 180 / pi
  alpha_deg <- (alpha_deg + 360) %% 360
  
  # Declinación en grados
  delta_deg <- delta * 180 / pi
  
  return(list(alpha = alpha_deg, delta = delta_deg))
}

# 2.3 Función para calcular el acimut y la altura del Sol (vectorizada)
calcular_acimut_altura_sol <- function(alpha_deg, delta_deg, fecha, latitud, longitud) {
  # Convertir latitud y longitud a grados decimales
  latitud_deg <- latitud
  longitud_deg <- longitud
  
  # Tiempo Sideral en Greenwich (en grados)
  JD <- calcular_dia_juliano_vectorizado(fecha)
  T <- (JD - 2451545.0) / 36525
  theta0 <- 280.46061837 + 360.98564736629 * (JD - 2451545.0) +
    0.000387933 * T^2 - T^3 / 38710000
  theta0 <- theta0 %% 360
  
  # Tiempo Sideral Local (en grados)
  TSL <- theta0 + longitud_deg
  
  # Ángulo Horario (H) en grados
  H <- TSL - alpha_deg
  H <- (H + 360) %% 360
  H_rad <- H * pi / 180
  
  # Convertir latitud y declinación a radianes
  lat_rad <- latitud_deg * pi / 180
  delta_rad <- delta_deg * pi / 180
  
  # Cálculo de la altura (elevación) y acimut
  sin_altura <- sin(delta_rad) * sin(lat_rad) + cos(delta_rad) * cos(lat_rad) * cos(H_rad)
  altura_rad <- asin(sin_altura)
  
  sin_acimut <- -cos(delta_rad) * cos(lat_rad) * sin(H_rad)
  cos_acimut <- sin(delta_rad) - sin(altura_rad) * sin(lat_rad)
  acimut_rad <- atan2(sin_acimut, cos_acimut)
  acimut_rad <- (acimut_rad + 2 * pi) %% (2 * pi)
  
  # Convertir a grados
  altura_deg <- altura_rad * 180 / pi
  acimut_deg <- acimut_rad * 180 / pi
  
  return(list(acimut = acimut_deg, altura = altura_deg))
}

# --- 3. Calcular la trayectoria del Sol para una fecha específica ---

# 3.1 Definir la fecha y la localización

# Fecha específica en hora local de Madrid
fecha_especifica <- as.POSIXct("2024-06-21", tz = "Europe/Madrid")

# Crear secuencia de tiempos cada 10 minutos durante el día
tiempos <- seq(from = fecha_especifica, by = "10 min", length.out = 144)

# Convertir a UTC
tiempos_utc <- with_tz(tiempos, tzone = "UTC")

# 3.2 Definir la localización de Madrid
latitud_madrid <- 40.4168   # Latitud en grados norte
longitud_madrid <- -3.7038  # Longitud en grados este (negativa hacia el oeste)

# --- 4. Calcular la posición del Sol para cada tiempo ---

# Calcular el Día Juliano
JD <- calcular_dia_juliano_vectorizado(tiempos_utc)

# Calcular la posición del Sol
posicion_sol <- calcular_posicion_sol(JD)

# Calcular el acimut y la altura
pos_horizontal <- calcular_acimut_altura_sol(posicion_sol$alpha, posicion_sol$delta, tiempos_utc, latitud_madrid, longitud_madrid)

# --- 5. Preparar los datos para la representación en 2D ---

# Crear un data frame con los resultados
datos_sol <- data.frame(
  tiempo = tiempos,
  acimut = pos_horizontal$acimut,
  altura = pos_horizontal$altura
)

# Filtrar datos cuando el Sol está por encima del horizonte
datos_sol_visibles <- datos_sol[datos_sol$altura > 0, ]

# --- 6. Representar la trayectoria del Sol en 2D ---

# 6.1 Gráfico de altura vs. acimut (trayectoria en el horizonte)

ggplot(datos_sol_visibles, aes(x = acimut, y = altura)) +
  geom_path(color = 'orange') +
  geom_point(color = 'orange') +
  labs(title = 'Trayectoria del Sol vista desde Madrid el 21 de junio de 2024',
       x = 'Acimut (grados)',
       y = 'Altura (grados)') +
  theme_minimal()

# 6.2 Gráfico de altura vs. tiempo (evolución de la altura a lo largo del día)

ggplot(datos_sol, aes(x = tiempo, y = altura)) +
  geom_line(color = 'red') +
  labs(title = 'Altura del Sol a lo largo del día en Madrid el 21 de junio de 2024',
       x = 'Tiempo',
       y = 'Altura (grados)') +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours")

