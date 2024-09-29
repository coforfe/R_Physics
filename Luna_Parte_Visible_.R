# -------------------------------------------------------------
# Cálculo y representación de las fases de la Luna.
# -------------------------------------------------------------

# --- 1. Instalar y cargar las librerías necesarias ---

# Instalar las librerías (si no están instaladas)
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggforce")) install.packages("ggforce")  # Para funciones gráficas avanzadas

# Cargar las librerías
library(lubridate)
library(ggplot2)
library(ggforce)

# --- 2. Definir las funciones para calcular la fase de la Luna ---

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
  fraccion_dia <- (hora + (minuto + segundo / 60) / 60) / 24
  
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

# 2.2 Función para calcular la edad de la Luna en días desde la última Luna Nueva
calcular_edad_lunar <- function(JD) {
  # Día Juliano de una Luna Nueva conocida (6 de enero de 2000)
  JD_base <- 2451549.5
  
  # Longitud sinódica media de la Luna (29.530588853 días)
  ciclo_sinodico <- 29.530588853
  
  # Calcular la edad de la Luna
  edad <- (JD - JD_base) %% ciclo_sinodico
  
  return(edad)
}

# 2.3 Función para calcular la fase de la Luna y el porcentaje iluminado
calcular_fase_lunar <- function(edad) {
  # Calcular el porcentaje iluminado
  porcentaje_iluminado <- (1 - cos(2 * pi * edad / 29.530588853)) / 2 * 100
  
  # Determinar la fase basada en la edad
  if (edad < 1 || edad >= 29) {
    fase <- "Luna Nueva"
  } else if (edad >= 1 && edad < 7.38) {
    fase <- "Creciente"
  } else if (edad >= 7.38 && edad < 8.38) {
    fase <- "Cuarto Creciente"
  } else if (edad >= 8.38 && edad < 14.77) {
    fase <- "Gibosa Creciente"
  } else if (edad >= 14.77 && edad < 15.77) {
    fase <- "Luna Llena"
  } else if (edad >= 15.77 && edad < 22.15) {
    fase <- "Gibosa Menguante"
  } else if (edad >= 22.15 && edad < 23.15) {
    fase <- "Cuarto Menguante"
  } else if (edad >= 23.15 && edad < 29) {
    fase <- "Menguante"
  }
  
  return(list(fase = fase, porcentaje = porcentaje_iluminado))
}

# --- 3. Generar una secuencia de fechas para analizar las fases de la Luna ---

# 3.1 Definir la fecha inicial y final
fecha_inicial <- as.POSIXct("2024-09-01 00:00:00", tz = "Europe/Madrid")
fecha_final <- as.POSIXct("2024-09-30 23:59:59", tz = "Europe/Madrid")

# Crear una secuencia de fechas cada día
fechas <- seq(from = fecha_inicial, to = fecha_final, by = "1 day")

# --- 4. Calcular las fases de la Luna para cada fecha ---

# Crear un data frame para almacenar los resultados
datos_fases <- data.frame(
  fecha = fechas,
  fase = character(length(fechas)),
  porcentaje_iluminado = numeric(length(fechas)),
  stringsAsFactors = FALSE
)

# Calcular las fases y porcentajes
for (i in seq_along(fechas)) {
  JD <- calcular_dia_juliano(fechas[i])
  edad <- calcular_edad_lunar(JD)
  fase_info <- calcular_fase_lunar(edad)
  
  datos_fases$fase[i] <- fase_info$fase
  datos_fases$porcentaje_iluminado[i] <- fase_info$porcentaje
}

# --- 5. Representar gráficamente las fases de la Luna ---

# 5.1 Función para dibujar la Luna en una fase específica
dibujar_luna <- function(porcentaje_iluminado, fecha) {
  # Crear un data frame para el círculo de la Luna
  luna <- data.frame(x = 0, y = 0, r = 1)
  
  # Crear el gráfico base
  p <- ggplot() +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void() +
    coord_fixed()
  
  # Dibujar el círculo de la Luna
  p <- p + geom_circle(aes(x0 = x, y0 = y, r = r), data = luna, fill = "black", color = NA)
  
  # Calcular la fracción iluminada
  fraccion <- porcentaje_iluminado / 100
  
  # Determinar si es creciente o menguante
  if (fraccion <= 0.5) {
    # Creciente
    p <- p + geom_rect(aes(xmin = 0, xmax = sqrt(1 - (seq(-1, 1, length.out = 1000))^2),
                           ymin = seq(-1, 1, length.out = 1000),
                           ymax = seq(-1 + 2/1000, 1 + 2/1000, length.out = 1000)),
                       fill = "white", color = NA)
    p <- p + geom_rect(aes(xmin = -sqrt(1 - (seq(-1, 1, length.out = 1000))^2) * (1 - 2 * fraccion),
                           xmax = 0,
                           ymin = seq(-1, 1, length.out = 1000),
                           ymax = seq(-1 + 2/1000, 1 + 2/1000, length.out = 1000)),
                       fill = "white", color = NA)
  } else {
    # Menguante
    p <- p + geom_rect(aes(xmin = -sqrt(1 - (seq(-1, 1, length.out = 1000))^2),
                           xmax = 0,
                           ymin = seq(-1, 1, length.out = 1000),
                           ymax = seq(-1 + 2/1000, 1 + 2/1000, length.out = 1000)),
                       fill = "white", color = NA)
    p <- p + geom_rect(aes(xmin = 0,
                           xmax = sqrt(1 - (seq(-1, 1, length.out = 1000))^2) * (2 * fraccion - 1),
                           ymin = seq(-1, 1, length.out = 1000),
                           ymax = seq(-1 + 2/1000, 1 + 2/1000, length.out = 1000)),
                       fill = "white", color = NA)
  }
  
  # Añadir título con la fecha y porcentaje iluminado
  p <- p + ggtitle(paste(format(fecha, "%Y-%m-%d"), "-", round(porcentaje_iluminado, 1), "% iluminado"))
  
  return(p)
}

# 5.2 Generar los gráficos para cada fecha
lista_graficos <- list()

for (i in seq_along(fechas)) {
  porcentaje_iluminado <- datos_fases$porcentaje_iluminado[i]
  fecha <- datos_fases$fecha[i]
  
  p <- dibujar_luna(porcentaje_iluminado, fecha)
  
  lista_graficos[[i]] <- p
}

# 5.3 Mostrar algunos gráficos como ejemplo
# Por ejemplo, mostrar las fases cada 7 días
indices_mostrar <- seq(1, length(fechas), by = 7)

for (i in indices_mostrar) {
  print(lista_graficos[[i]])
}

# --- 6. Representar la evolución del porcentaje iluminado a lo largo del mes ---

ggplot(datos_fases, aes(x = fecha, y = porcentaje_iluminado)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  labs(title = 'Porcentaje iluminado de la Luna en septiembre de 2024',
       x = 'Fecha',
       y = 'Porcentaje iluminado (%)') +
  theme_minimal() +
  scale_x_datetime(date_labels = "%d-%b", date_breaks = "5 days")


