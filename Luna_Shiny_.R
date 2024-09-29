# -------------------------------------------------------------
# Aplicación Shiny: Astronomía Interactiva
# -------------------------------------------------------------

# --- 1. Cargar las librerías necesarias ---
library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(plotly)
library(shinythemes)
library(ggforce)
library(cowplot)

# --- 2. Definir la interfaz de usuario (UI) ---
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Aplicación Astronómica Interactiva"),
  tabsetPanel(
    tabPanel("Fases de la Luna",
             sidebarLayout(
               sidebarPanel(
                 h4("Selecciona el mes y el año"),
                 selectInput("mes", "Mes:", choices = month.name, selected = "Enero"),
                 numericInput("anio", "Año:", value = 2024, min = 1900, max = 2100)
               ),
               mainPanel(
                 plotOutput("calendario_lunar")
               )
             )
    ),
    tabPanel("Posición del Sol",
             sidebarLayout(
               sidebarPanel(
                 h4("Configuración"),
                 dateInput("fecha_sol", "Fecha:", value = Sys.Date()),
                 numericInput("latitud_sol", "Latitud:", value = 40.4168),
                 numericInput("longitud_sol", "Longitud:", value = -3.7038)
               ),
               mainPanel(
                 plotOutput("posicion_sol")
               )
             )
    ),
    tabPanel("Posición de la Luna desde Madrid",
             sidebarLayout(
               sidebarPanel(
                 h4("Configuración"),
                 dateInput("fecha_luna", "Fecha:", value = Sys.Date()),
                 sliderInput("hora_luna", "Hora:", min = 0, max = 23, value = 12, step = 1)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Gráfico 2D", plotOutput("posicion_luna_2d")),
                   tabPanel("Gráfico 3D", plotlyOutput("posicion_luna_3d"))
                 )
               )
             )
    )
  )
)

# --- 3. Definir la lógica del servidor ---
server <- function(input, output) {
  
  # --- 3.1 Fases de la Luna ---
  output$calendario_lunar <- renderPlot({
    # Obtener el mes y año seleccionados
    anio <- input$anio
    mes <- match(input$mes, month.name)
    
    # Generar el calendario lunar para el mes y año seleccionados
    # Crear la secuencia de fechas para el mes
    fecha_inicio <- as.Date(paste(anio, mes, "01", sep = "-"))
    if (mes == 12) {
      fecha_fin <- as.Date(paste(anio + 1, "1", "01", sep = "-")) - 1
    } else {
      fecha_fin <- as.Date(paste(anio, mes + 1, "01", sep = "-")) - 1
    }
    fechas <- seq(from = fecha_inicio, to = fecha_fin, by = "1 day")
    
    # Funciones para calcular las fases de la Luna
    calcular_dia_juliano <- function(fecha) {
      anio <- year(fecha)
      mes <- month(fecha)
      dia <- day(fecha)
      
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
    
    calcular_edad_lunar <- function(JD) {
      JD_base <- 2451550.1  # Luna Nueva el 6 de enero de 2000
      ciclo_sinodico <- 29.530588853
      edad <- (JD - JD_base) %% ciclo_sinodico
      return(edad)
    }
    
    calcular_fase_lunar <- function(edad) {
      porcentaje_iluminado <- (1 - cos(2 * pi * edad / 29.530588853)) / 2
      
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
    
    dibujar_fase_lunar <- function(porcentaje_iluminado, dia) {
      porcentaje <- porcentaje_iluminado
      
      luna <- data.frame(x = 0, y = 0, r = 1)
      
      p <- ggplot() +
        xlim(-1, 1) +
        ylim(-1.5, 1) +
        theme_void() +
        coord_fixed()
      
      p <- p + geom_circle(aes(x0 = x, y0 = y, r = r), data = luna, fill = "black", color = NA)
      
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
      
      p <- p + annotate("text", x = 0, y = -1.2, label = dia, size = 5, color = "white")
      
      p <- p + theme(panel.background = element_rect(fill = "black"),
                     plot.background = element_rect(fill = "black"))
      
      return(p)
    }
    
    datos_fases <- data.frame(
      fecha = fechas,
      dia = day(fechas),
      fase = character(length(fechas)),
      porcentaje_iluminado = numeric(length(fechas)),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(fechas)) {
      JD <- calcular_dia_juliano(fechas[i])
      edad <- calcular_edad_lunar(JD)
      fase_info <- calcular_fase_lunar(edad)
      
      datos_fases$fase[i] <- fase_info$fase
      datos_fases$porcentaje_iluminado[i] <- fase_info$porcentaje
    }
    
    lista_graficos <- list()
    
    for (i in seq_along(fechas)) {
      porcentaje_iluminado <- datos_fases$porcentaje_iluminado[i]
      dia <- datos_fases$dia[i]
      
      p <- dibujar_fase_lunar(porcentaje_iluminado, dia)
      
      lista_graficos[[i]] <- p
    }
    
    dias_semana <- wday(fecha_inicio, week_start = 1)
    num_dias <- length(fechas)
    num_semanas <- ceiling((dias_semana - 1 + num_dias) / 7)
    
    calendario <- vector("list", num_semanas)
    indice_dia <- 1
    
    for (semana in 1:num_semanas) {
      dias_en_semana <- vector("list", 7)
      for (dia_sem in 1:7) {
        if ((semana == 1 && dia_sem < dias_semana) || indice_dia > num_dias) {
          dias_en_semana[[dia_sem]] <- ggplot() + theme_void()
        } else {
          dias_en_semana[[dia_sem]] <- lista_graficos[[indice_dia]]
          indice_dia <- indice_dia + 1
        }
      }
      calendario[[semana]] <- plot_grid(plotlist = dias_en_semana, ncol = 7)
    }
    
    calendario_completo <- plot_grid(plotlist = calendario, ncol = 1)
    
    titulo <- ggdraw() + draw_label(paste("Calendario de fases lunares -", input$mes, anio),
                                    fontface = 'bold', size = 20, hjust = 0.5)
    
    calendario_final <- plot_grid(titulo, calendario_completo, ncol = 1, rel_heights = c(0.1, 1))
    
    print(calendario_final)
  })
  
  # --- 3.2 Posición del Sol ---
  output$posicion_sol <- renderPlot({
    # Obtener los valores de entrada
    fecha <- as.Date(input$fecha_sol)
    latitud <- input$latitud_sol
    longitud <- input$longitud_sol
    
    # Generar una secuencia de horas durante el día
    horas <- seq(0, 23, by = 1)
    tiempos <- as.POSIXct(paste(fecha, sprintf("%02d:00:00", horas)), tz = "UTC")
    
    # Calcular la posición del Sol para cada hora
    # Funciones para calcular la declinación solar y el ángulo horario
    calcular_declinacion_solar <- function(dia_del_anio) {
      gamma <- 2 * pi / 365 * (dia_del_anio - 1)
      declinacion <- (0.006918 -
                        0.399912 * cos(gamma) +
                        0.070257 * sin(gamma) -
                        0.006758 * cos(2 * gamma) +
                        0.000907 * sin(2 * gamma) -
                        0.002697 * cos(3 * gamma) +
                        0.00148 * sin(3 * gamma))
      return(declinacion)
    }
    
    calcular_angulo_horario <- function(hora_local, longitud) {
      hora_solar = (hora_local - 12) * 15  # Cada hora equivale a 15 grados
      angulo_horario = hora_solar + longitud
      return(angulo_horario * pi / 180)
    }
    
    dia_del_anio <- yday(fecha)
    declinacion <- calcular_declinacion_solar(dia_del_anio)
    angulos_horarios <- calcular_angulo_horario(horas, longitud)
    
    # Calcular la altura y el acimut del Sol
    altura <- asin(sin(latitud * pi / 180) * sin(declinacion) +
                     cos(latitud * pi / 180) * cos(declinacion) * cos(angulos_horarios)) * 180 / pi
    
    acimut <- acos((sin(declinacion) - sin(altura * pi / 180) * sin(latitud * pi / 180)) /
                     (cos(altura * pi / 180) * cos(latitud * pi / 180))) * 180 / pi
    
    # Ajustar el acimut según la hora
    acimut[horas > 12] <- 360 - acimut[horas > 12]
    
    datos_sol <- data.frame(hora = horas, altura = altura, acimut = acimut)
    
    # Generar el gráfico
    ggplot(datos_sol, aes(x = hora, y = altura)) +
      geom_line(color = 'orange', size = 1) +
      geom_point(color = 'red', size = 2) +
      labs(title = paste('Altura del Sol el', format(fecha, "%Y-%m-%d")),
           x = 'Hora',
           y = 'Altura (grados)') +
      theme_minimal()
  })
  
  # --- 3.3 Posición de la Luna desde Madrid ---
  
  # 3.3.1 Gráfico 2D
  output$posicion_luna_2d <- renderPlot({
    # Obtener la fecha y hora seleccionadas
    fecha <- as.Date(input$fecha_luna)
    hora <- input$hora_luna
    
    # Combinar fecha y hora
    fecha_hora <- as.POSIXct(paste(fecha, sprintf("%02d:00:00", hora)), tz = "Europe/Madrid")
    
    # Calcular la posición de la Luna usando el paquete suncalc
    if (!require("suncalc")) install.packages("suncalc")
    library(suncalc)
    
    latitud <- 40.4168
    longitud <- -3.7038
    
    datos_luna <- getMoonPosition(date = fecha_hora, lat = latitud, lon = longitud)
    
    altura <- datos_luna$altitude * 180 / pi
    acimut <- datos_luna$azimuth * 180 / pi + 180  # Ajuste para convertir de -180:180 a 0:360
    
    # Generar el gráfico 2D
    ggplot(data.frame(acimut = acimut, altura = altura), aes(x = acimut, y = altura)) +
      geom_point(color = 'gray', size = 5) +
      xlim(0, 360) +
      ylim(0, 90) +
      labs(title = paste('Posición de la Luna el', format(fecha_hora, "%Y-%m-%d %H:%M")),
           x = 'Acimut (grados)',
           y = 'Altura (grados)') +
      theme_minimal()
  })
  
  # 3.3.2 Gráfico 3D
  output$posicion_luna_3d <- renderPlotly({
    # Obtener la fecha y hora seleccionadas
    fecha <- as.Date(input$fecha_luna)
    hora <- input$hora_luna
    
    # Combinar fecha y hora
    fecha_hora <- as.POSIXct(paste(fecha, sprintf("%02d:00:00", hora)), tz = "Europe/Madrid")
    
    # Calcular la posición de la Luna usando el paquete suncalc
    if (!require("suncalc")) install.packages("suncalc")
    library(suncalc)
    
    latitud <- 40.4168
    longitud <- -3.7038
    
    datos_luna <- getMoonPosition(date = fecha_hora, lat = latitud, lon = longitud)
    
    altura <- datos_luna$altitude
    acimut <- datos_luna$azimuth + pi  # Ajuste para convertir de -pi:pi a 0:2pi
    
    # Convertir a coordenadas cartesianas
    x <- cos(altura) * sin(acimut)
    y <- cos(altura) * cos(acimut)
    z <- sin(altura)
    
    # Generar el gráfico 3D
    plot_ly(x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers',
            marker = list(size = 5, color = 'gray')) %>%
      layout(title = paste('Posición de la Luna el', format(fecha_hora, "%Y-%m-%d %H:%M")),
             scene = list(
               xaxis = list(title = 'X'),
               yaxis = list(title = 'Y'),
               zaxis = list(title = 'Z')
             ))
  })
  
}

# --- 4. Ejecutar la aplicación Shiny ---
shinyApp(ui = ui, server = server)