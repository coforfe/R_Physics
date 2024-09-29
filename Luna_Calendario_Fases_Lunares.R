# -------------------------------------------------------------
# Generación de un calendario de fases lunares para un mes específico.
# -------------------------------------------------------------

# --- 1. Instalar y cargar las librerías necesarias ---

# Instalar las librerías (si no están instaladas)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("lubridate")) install.packages("lubridate")
if (!require("cowplot")) install.packages("cowplot")

# Cargar las librerías
library(ggplot2)
library(gridExtra)
library(lubridate)
library(cowplot)

# --- 2. Definir funciones para calcular la fase de la Luna ---

# 2.1 Función para calcular el Día Juliano (JD)
calcular_dia_juliano <- function(fecha) {
  anio <- year(fecha)
  mes <- month(fecha)
  dia <- day(fecha)
  
  # Ajustar mes y año si el mes es enero o febrero
  if (mes <= 2) {
    anio <- anio - 1
    mes <- mes + 12
  }
  
  A <- floor(anio / 100)
  B <- 2 - A + floor(A / 4)
  
  JD <- floor(365.25 * (anio + 4716)) +
    floor(30.6001 * (mes + 1)) +
    dia + B - 1524.5
  
  return(JD)
}

# 2.2 Función para calcular la edad de la Luna en días desde la última Luna Nueva
calcular_edad_lunar <- function(JD) {
  # Día Juliano de referencia para una Luna Nueva conocida
  JD_base <- 2451550.1  # Luna Nueva el 6 de enero de 2000
  
  # Longitud sinódica media de la Luna (29.530588853 días)
  ciclo_sinodico <- 29.530588853
  
  # Calcular la edad de la Luna
  edad <- (JD - JD_base) %% ciclo_sinodico
  
  return(edad)
}

# 2.3 Función para calcular la fase de la Luna y el porcentaje iluminado
calcular_fase_lunar <- function(edad) {
  # Calcular el porcentaje iluminado
  porcentaje_iluminado <- (1 - cos(2 * pi * edad / 29.530588853)) / 2
  
  # Determinar la fase basada en la edad
  if (edad < 1.84566) {
    fase <- "Luna Nueva"
  } else if (edad < 5.53699) {
    fase <- "Creciente Iluminante"
  } else if (edad < 9.22831) {
    fase <- "Cuarto Creciente"
  } else if (edad < 12.91963) {
    fase <- "Gibosa Iluminante"
  } else if (edad < 16.61096) {
    fase <- "Luna Llena"
  } else if (edad < 20.30228) {
    fase <- "Gibosa Menguante"
  } else if (edad < 23.99361) {
    fase <- "Cuarto Menguante"
  } else if (edad < 27.68493) {
    fase <- "Creciente Menguante"
  } else {
    fase <- "Luna Nueva"
  }
  
  return(list(fase = fase, porcentaje = porcentaje_iluminado))
}

# --- 3. Generar la secuencia de fechas para el mes específico ---

# 3.1 Definir el año y el mes para el calendario
anio <- 2024
mes <- 9  # Septiembre

# 3.2 Crear una secuencia de fechas para el mes
fecha_inicio <- as.Date(paste(anio, mes, "01", sep = "-"))
fecha_fin <- as.Date(paste(anio, mes + 1, "01", sep = "-")) - 1
fechas <- seq(from = fecha_inicio, to = fecha_fin, by = "1 day")

# --- 4. Calcular las fases de la Luna para cada fecha ---

# Crear un data frame para almacenar los resultados
datos_fases <- data.frame(
  fecha = fechas,
  dia = day(fechas),
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

# --- 5. Generar imágenes de las fases lunares ---

# 5.1 Función para dibujar la Luna en una fase específica
dibujar_fase_lunar <- function(porcentaje_iluminado, dia) {
  porcentaje <- porcentaje_iluminado
  
  # Crear un data frame para el círculo de la Luna
  luna <- data.frame(x = 0, y = 0, r = 1)
  
  # Crear el gráfico base
  p <- ggplot() +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void() +
    coord_fixed()
  
  # Dibujar el círculo de la Luna (disco)
  p <- p + geom_circle(aes(x0 = x, y0 = y, r = r), data = luna, fill = "black", color = NA)
  
  # Determinar si es creciente o menguante
  if (porcentaje <= 0.5) {
    # Creciente
    semilla <- data.frame(x = seq(-1, 1, length.out = 1000))
    semilla$y1 <- sqrt(1 - semilla$x^2)
    semilla$y2 <- -sqrt(1 - semilla$x^2)
    semilla <- semilla[semilla$x >= 0, ]
    
    p <- p + geom_polygon(aes(x = c(semilla$x, rev(semilla$x)), y = c(semilla$y1, rev(semilla$y2))),
                          fill = "white", color = NA)
    p <- p + geom_polygon(aes(x = c(semilla$x * (1 - 2 * porcentaje), rev(semilla$x * (1 - 2 * porcentaje))),
                              y = c(semilla$y1, rev(semilla$y2))),
                          fill = "black", color = NA)
  } else {
    # Menguante
    semilla <- data.frame(x = seq(-1, 1, length.out = 1000))
    semilla$y1 <- sqrt(1 - semilla$x^2)
    semilla$y2 <- -sqrt(1 - semilla$x^2)
    semilla <- semilla[semilla$x <= 0, ]
    
    p <- p + geom_polygon(aes(x = c(semilla$x, rev(semilla$x)), y = c(semilla$y1, rev(semilla$y2))),
                          fill = "white", color = NA)
    p <- p + geom_polygon(aes(x = c(semilla$x * (2 * porcentaje - 1), rev(semilla$x * (2 * porcentaje - 1))),
                              y = c(semilla$y1, rev(semilla$y2))),
                          fill = "black", color = NA)
  }
  
  # Añadir el número del día
  p <- p + annotate("text", x = 0, y = -1.2, label = dia, size = 5)
  
  return(p)
}

# 5.2 Generar los gráficos para cada día
lista_graficos <- list()

for (i in seq_along(fechas)) {
  porcentaje_iluminado <- datos_fases$porcentaje_iluminado[i]
  dia <- datos_fases$dia[i]
  
  p <- dibujar_fase_lunar(porcentaje_iluminado, dia)
  
  lista_graficos[[i]] <- p
}

# --- 6. Organizar los gráficos en formato de calendario ---

# 6.1 Crear una matriz para organizar los días en semanas
dias_semana <- wday(fecha_inicio, label = FALSE)  # 1 = Domingo, 7 = Sábado
num_dias <- length(fechas)
num_semanas <- ceiling((dias_semana - 1 + num_dias) / 7)

# Crear una lista de listas para organizar las semanas
calendario <- vector("list", num_semanas)
indice_dia <- 1

for (semana in 1:num_semanas) {
  dias_en_semana <- vector("list", 7)
  for (dia_sem in 1:7) {
    if ((semana == 1 && dia_sem < dias_semana) || indice_dia > num_dias) {
      # Día vacío
      dias_en_semana[[dia_sem]] <- NULL
    } else {
      # Añadir el gráfico correspondiente
      dias_en_semana[[dia_sem]] <- lista_graficos[[indice_dia]]
      indice_dia <- indice_dia + 1
    }
  }
  calendario[[semana]] <- dias_en_semana
}

# 6.2 Organizar los gráficos utilizando grid.arrange
graficos_filas <- list()

for (semana in calendario) {
  fila <- do.call(cowplot::plot_grid, c(semana, nrow = 1, align = 'h'))
  graficos_filas <- c(graficos_filas, list(fila))
}

calendario_completo <- do.call(cowplot::plot_grid, c(graficos_filas, ncol = 1))

# 6.3 Añadir título al calendario
titulo <- ggdraw() + draw_label(paste("Calendario de fases lunares -", month(fecha_inicio, label = TRUE, abbr = FALSE), anio), fontface = 'bold', size = 20)

calendario_final <- plot_grid(titulo, calendario_completo, ncol = 1, rel_heights = c(0.1, 1))

# Mostrar el calendario
print(calendario_final)

