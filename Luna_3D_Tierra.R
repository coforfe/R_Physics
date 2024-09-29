# -------------------------------------------------------------
# Cálculo y representación en 3D de la trayectoria de la Luna
# vista desde Madrid el 27 de septiembre de 2024, incluyendo
# una esfera que representa la Tierra.
# -------------------------------------------------------------

# --- 1. Instalar y cargar las librerías necesarias ---

# Instalar las librerías (si no están instaladas)
if (!require("plotly")) install.packages("plotly")
if (!require("lubridate")) install.packages("lubridate")
if (!require("pracma")) install.packages("pracma")  # Para generar la esfera

# Cargar las librerías
library(plotly)
library(lubridate)
library(pracma)

# --- 2. Definir las funciones para calcular la posición de la Luna ---

# 2.1 Función para calcular el Día Juliano (JD) vectorizado
calcular_dia_juliano_vectorizado <- function(anio, mes, dia, hora = 0, minuto = 0, segundo = 0) {
  # Convertir todos los inputs a vectores de la misma longitud
  max_length <- max(length(anio), length(mes), length(dia), length(hora), length(minuto), length(segundo))
  anio <- rep(anio, length.out = max_length)
  mes <- rep(mes, length.out = max_length)
  dia <- rep(dia, length.out = max_length)
  hora <- rep(hora, length.out = max_length)
  minuto <- rep(minuto, length.out = max_length)
  segundo <- rep(segundo, length.out = max_length)
  
  # Ajustar mes y año si el mes es enero o febrero
  idx <- mes <= 2
  anio[idx] <- anio[idx] - 1
  mes[idx] <- mes[idx] + 12
  
  # Calcular la fracción del día
  fraccion_dia <- (hora + (minuto + segundo / 60) / 60) / 24
  
  # Calcular los componentes necesarios
  A <- floor(anio / 100)
  
  # Determinar si la fecha es posterior al inicio del calendario gregoriano
  es_gregoriano <- ( (anio > 1582) |
                       (anio == 1582 & mes > 10) |
                       (anio == 1582 & mes == 10 & dia >= 15) )
  es_juliano <- ( (anio < 1582) |
                    (anio == 1582 & mes < 10) |
                    (anio == 1582 & mes == 10 & dia <= 4) )
  
  B <- numeric(length(anio))
  B[es_gregoriano] <- 2 - A[es_gregoriano] + floor(A[es_gregoriano] / 4)
  B[es_juliano] <- 0
  
  # Verificar fechas inexistentes (entre el 5 y el 14 de octubre de 1582)
  fechas_inexistentes <- !(es_gregoriano | es_juliano)
  if (any(fechas_inexistentes)) {
    stop("Una o más fechas ingresadas no existen en el calendario (entre el 5 y el 14 de octubre de 1582).")
  }
  
  # Calcular el Día Juliano
  JD <- floor(365.25 * (anio + 4716)) +
    floor(30.6001 * (mes + 1)) +
    dia + fraccion_dia + B - 1524.5
  
  return(JD)
}

# 2.2 Función para calcular la posición geocéntrica de la Luna
calcular_posicion_luna <- function(JD) {
  # Tiempo en siglos julianos desde J2000.0
  T <- (JD - 2451545.0) / 36525
  
  # Variables fundamentales (en grados)
  L1 <- 218.3164477 + 481267.88123421 * T -
    0.0015786 * T^2 + T^3 / 538841 - T^4 / 65194000
  D  <- 297.8501921 + 445267.1114034 * T -
    0.0018819 * T^2 + T^3 / 545868 - T^4 / 113065000
  M  <- 357.5291092 + 35999.0502909 * T -
    0.0001536 * T^2 + T^3 / 24490000
  M1 <- 134.9633964 + 477198.8675055 * T +
    0.0087414 * T^2 + T^3 / 69699 - T^4 / 14712000
  F  <- 93.2720950 + 483202.0175233 * T -
    0.0036539 * T^2 - T^3 / 3526000 + T^4 / 863310000
  
  # Reducir al rango 0° - 360°
  L1 <- L1 %% 360
  D  <- D %% 360
  M  <- M %% 360
  M1 <- M1 %% 360
  F  <- F %% 360
  
  # Convertir a radianes
  deg_to_rad <- pi / 180
  L1_rad <- L1 * deg_to_rad
  D_rad  <- D * deg_to_rad
  M_rad  <- M * deg_to_rad
  M1_rad <- M1 * deg_to_rad
  F_rad  <- F * deg_to_rad
  
  # Términos para la longitud (en grados)
  # Incluimos algunos términos principales
  A1 <- sin(2 * D_rad) * -1.274
  A2 <- sin(2 * M1_rad) * 0.658
  A3 <- sin(2 * D_rad - M1_rad) * -0.186
  A4 <- sin(M_rad) * -0.059
  A5 <- sin(2 * D_rad - 2 * M1_rad) * -0.057
  A6 <- sin(2 * D_rad + M1_rad) * 0.053
  A7 <- sin(2 * M1_rad - 2 * D_rad) * -0.046
  A8 <- sin(M1_rad - M_rad) * 0.041
  A9 <- sin(M1_rad + M_rad) * -0.035
  A10 <- sin(2 * D_rad - M_rad) * -0.031
  
  suma_longitud <- A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
  
  # Longitud eclíptica de la Luna (en grados)
  lambda <- L1 + suma_longitud
  
  # Convertir a radianes
  lambda_rad <- lambda * deg_to_rad
  
  # Latitud eclíptica (en grados)
  B1 <- sin(F_rad) * 5.128
  beta <- B1
  beta_rad <- beta * deg_to_rad
  
  # Distancia a la Luna en kilómetros (simplificado)
  delta <- 385000.56  # Distancia media a la Luna
  
  return(list(lambda = lambda_rad, beta = beta_rad, delta = delta))
}

# 2.3 Función para convertir coordenadas eclípticas a ecuatoriales
ecliptica_a_ecuatorial <- function(lambda, beta, JD) {
  # Tiempo en siglos julianos desde J2000.0
  T <- (JD - 2451545.0) / 36525
  
  # Oblicuidad media de la eclíptica (en grados)
  epsilon <- 23 + 26 / 60 + 21.448 / 3600 -
    (46.8150 * T + 0.00059 * T^2 - 0.001813 * T^3) / 3600
  epsilon_rad <- epsilon * pi / 180  # Convertir a radianes
  
  # Cálculo de la ascensión recta (alpha) y declinación (delta)
  sin_alpha <- sin(lambda) * cos(epsilon_rad) - tan(beta) * sin(epsilon_rad)
  cos_alpha <- cos(lambda)
  alpha <- atan2(sin_alpha, cos_alpha)
  
  delta <- asin(sin(beta) * cos(epsilon_rad) + cos(beta) * sin(epsilon_rad) * sin(lambda))
  
  # Normalizar alpha entre 0 y 2*pi
  alpha <- alpha %% (2 * pi)
  
  return(list(alpha = alpha, delta = delta))
}

# 2.4 Función para calcular el acimut y la altura desde una localización geográfica
calcular_acimut_altura <- function(alpha, delta, JD, latitud, longitud) {
  # Convertir longitud y latitud a radianes
  latitud_rad <- latitud * pi / 180
  longitud_rad <- longitud * pi / 180
  
  # Tiempo Sideral en Greenwich (en grados)
  T <- (JD - 2451545.0) / 36525
  theta0 <- 280.46061837 + 360.98564736629 * (JD - 2451545.0) +
    0.000387933 * T^2 - T^3 / 38710000
  theta0 <- theta0 %% 360
  theta0_rad <- theta0 * pi / 180
  
  # Tiempo Sideral Local (en radianes)
  TSL <- theta0_rad + longitud_rad
  
  # Ángulo Horario (H)
  H <- (TSL - alpha + pi) %% (2 * pi) - pi  # Normalizar entre -pi y pi
  
  # Cálculo de la altura (elevación) y acimut
  sin_altura <- sin(delta) * sin(latitud_rad) + cos(delta) * cos(latitud_rad) * cos(H)
  altura <- asin(sin_altura)
  
  sin_acimut <- -cos(delta) * cos(latitud_rad) * sin(H)
  cos_acimut <- sin(delta) - sin(altura) * sin(latitud_rad)
  acimut <- atan2(sin_acimut, cos_acimut)
  acimut <- (acimut + 2 * pi) %% (2 * pi)  # Normalizar entre 0 y 2*pi
  
  # Convertir a grados
  altura_deg <- altura * 180 / pi
  acimut_deg <- acimut * 180 / pi
  
  return(list(acimut = acimut_deg, altura = altura_deg))
}

# 2.5 Función principal para calcular la posición de la Luna desde una localización geográfica
posicion_luna_observador <- function(anio, mes, dia, hora = 0, minuto = 0, segundo = 0, latitud, longitud) {
  # Calcular el Día Juliano
  JD <- calcular_dia_juliano_vectorizado(anio, mes, dia, hora, minuto, segundo)
  
  # Calcular las coordenadas eclípticas de la Luna
  pos_luna <- calcular_posicion_luna(JD)
  
  # Convertir a coordenadas ecuatoriales
  pos_ecuatorial <- ecliptica_a_ecuatorial(pos_luna$lambda, pos_luna$beta, JD)
  
  # Calcular el acimut y la altura desde la ubicación del observador
  pos_horizontal <- calcular_acimut_altura(pos_ecuatorial$alpha, pos_ecuatorial$delta, JD, latitud, longitud)
  
  # Retornar todos los resultados
  return(list(acimut = pos_horizontal$acimut,
              altura = pos_horizontal$altura,
              alpha = pos_ecuatorial$alpha,
              delta = pos_ecuatorial$delta,
              lambda = pos_luna$lambda,
              beta = pos_luna$beta))
}

# --- 3. Generar la trayectoria de la Luna para el 27 de septiembre de 2024 ---

# 3.1 Definir la fecha y la localización

# Fecha inicial y final en hora local de Madrid
fecha_inicial <- as.POSIXct("2024-09-27 00:00:00", tz = "Europe/Madrid")
fecha_final <- as.POSIXct("2024-09-27 23:59:59", tz = "Europe/Madrid")

# Crear secuencia de tiempos cada 10 minutos
tiempos <- seq(from = fecha_inicial, to = fecha_final, by = "10 min")

# Convertir a UTC (Tiempo Universal Coordinado)
tiempos_utc <- with_tz(tiempos, tzone = "UTC")

# 3.2 Extraer componentes de fecha y hora
anios <- year(tiempos_utc)
meses <- month(tiempos_utc)
dias <- day(tiempos_utc)
horas <- hour(tiempos_utc)
minutos <- minute(tiempos_utc)
segundos <- second(tiempos_utc)

# 3.3 Definir la localización de Madrid
latitud_madrid <- 40.4168   # Latitud en grados norte
longitud_madrid <- -3.7038  # Longitud en grados este (negativa hacia el oeste)

# --- 4. Calcular la posición de la Luna para cada tiempo ---

# Calcular la posición de la Luna para cada momento
posiciones <- posicion_luna_observador(anios, meses, dias, horas, minutos, segundos, latitud_madrid, longitud_madrid)

# --- 5. Preparar los datos para la representación en 3D ---

# Convertir acimut y altura a radianes
acimut_rad <- posiciones$acimut * pi / 180
altura_rad <- posiciones$altura * pi / 180

# Coordenadas cartesianas en una esfera de radio unitario (trayectoria de la Luna)
x_luna <- cos(altura_rad) * sin(acimut_rad)
y_luna <- cos(altura_rad) * cos(acimut_rad)
z_luna <- sin(altura_rad)

# Crear un data frame con los resultados
trayectoria_luna <- data.frame(
  tiempo = tiempos,
  x = x_luna,
  y = y_luna,
  z = z_luna,
  acimut = posiciones$acimut,
  altura = posiciones$altura
)

# --- 6. Generar los datos para la esfera que representa la Tierra ---

# Definir una función para generar una esfera
generar_esfera <- function(radio = 1, centro = c(0, 0, 0), n = 50) {
  # Crear una malla de puntos esféricos
  u <- seq(0, 2 * pi, length.out = n)
  v <- seq(0, pi, length.out = n)
  u_matrix <- matrix(rep(u, each = n), nrow = n, ncol = n)
  v_matrix <- matrix(rep(v, times = n), nrow = n, ncol = n)
  
  # Calcular coordenadas cartesianas
  x <- radio * sin(v_matrix) * cos(u_matrix) + centro[1]
  y <- radio * sin(v_matrix) * sin(u_matrix) + centro[2]
  z <- radio * cos(v_matrix) + centro[3]
  
  return(list(x = x, y = y, z = z))
}

# Generar la esfera de la Tierra con radio 1 (mismo que la trayectoria lunar)
esfera_tierra <- generar_esfera(radio = 1, centro = c(0, 0, 0), n = 50)

# --- 7. Representar la trayectoria de la Luna y la Tierra en 3D ---

# Crear el gráfico 3D interactivo
fig <- plot_ly()

# Añadir la esfera de la Tierra
fig <- fig %>% add_surface(x = esfera_tierra$x, y = esfera_tierra$y, z = esfera_tierra$z,
                           colorscale = list(c(0,1), c("green","blue")),
                           showscale = FALSE,
                           opacity = 0.5,
                           hoverinfo = 'none')

# Añadir la trayectoria de la Luna
fig <- fig %>% add_trace(data = trayectoria_luna, x = ~x, y = ~y, z = ~z, type = 'scatter3d',
                         mode = 'lines+markers',
                         line = list(width = 2, color = 'white'),
                         marker = list(size = 2, color = 'white'),
                         hoverinfo = 'text',
                         text = ~paste('Tiempo:', format(tiempo, "%Y-%m-%d %H:%M:%S"),
                                       '<br>Acimut:', round(acimut, 2), '°',
                                       '<br>Altura:', round(altura, 2), '°'))

# Configurar el diseño del gráfico
fig <- fig %>% layout(title = 'Trayectoria de la Luna vista desde Madrid el 27 de septiembre de 2024',
                      scene = list(
                        xaxis = list(title = 'X', showgrid = FALSE, zeroline = FALSE),
                        yaxis = list(title = 'Y', showgrid = FALSE, zeroline = FALSE),
                        zaxis = list(title = 'Z', showgrid = FALSE, zeroline = FALSE),
                        aspectmode = 'data',
                        bgcolor = 'black'
                      ),
                      paper_bgcolor = 'black',
                      plot_bgcolor = 'black')

# Mostrar el gráfico
fig


