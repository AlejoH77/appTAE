library(shiny)
library(dplyr)
library(rgdal)
library(leaflet)
library(party)
library(plyr)
library(lubridate)
library(DT)

datos<-read.csv("db_22-06-2019_Complete.csv")
#no se muestra la columna #1
datos<-datos[,-1]
#cambio el tipo de dato a Date
datos<-mutate(datos, FECHA = as.Date(FECHA))
semanas <- data.frame(semana = week(datos$FECHA))
datos<-cbind(datos, semanas)
comunas <- unique(datos$COMUNA)
Sbarrios <- unique(select(datos, COMUNA, BARRIO))

#Lectura del shapefile
barrios_med = readOGR("mapa_barrios_categoria/barriosMedellin/Barrio_Vereda.shp",layer="Barrio_Vereda")
datos_barrios <- barrios_med@data
names (datos_barrios)[3] = "BARRIO"
datos_barrios$BARRIO <- iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")
datos_barrios$BARRIO <- iconv(datos_barrios$BARRIO, to="ASCII//TRANSLIT")
#Lectura de las categorias de agrupamiento
barrios_categorias <- read.csv("mapa_barrios_categoria/Categorias.csv", header = TRUE , sep = ";")
barrios_categorias$BARRIO <- iconv(barrios_categorias$BARRIO, to="ASCII//TRANSLIT")

#Unión de los barrios con su categoria
datos_listos <- join(datos_barrios, barrios_categorias)

#Interfaz grafica
ui <- fluidPage(title = "Accident Factory",
  theme = "estilos.css",
  navbarPage(title=div(img(src="logo.png", style = "width: 80px;")),
             tabPanel("Principal", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                      withTags(
                        div(class="container center",
                            h1("Accident Factory", align="center", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif;  font-size:4;font-weight: 1000; color:darkorange;"),
                            div(class="row",
                                div(class="col-md-10  col-md-offset-1 videoWrapper",
                                    iframe(src = "https://www.youtube.com/embed/LOZkDdE8jr0", height="40%", width="80%")
                                )
                            )
                        )
                      ),
                      fluidRow(
                        column(2,
                               p('')
                        ),
                        column(8, 
                               p('La accidentalidad es un problema que se presenta en todas la ciudades del mundo, y poder reducirla
                               es un problema que se ha venido tratando desde distintos ángulos y perspectivas, teniendo en cuenta 
                               que esta es una variable que esta dependiendo de muchos factores y posiblemente muchos de ellos imposibles de controlar. 
                               Así, nuestro trabajo al calcular y conocer el número de accidentes por comuna y barrio en la ciudad de Medellín 
                               en un período de tiempo determinado (diario, semanal y mensual) puede ser de gran ayuda para conocer los 
                               puntos más críticos de accidentalidad en la ciudad y evitarlos encontrando caminos alternos donde podamos 
                               viajar con más tranquilidad, ayudándonos adicionalmente del análisis de agrupamiento realizado donde se 
                               conocen los barrios con mayor similitud en cuanto a accidentalidad.', style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif;  font-size: 2em; text-align: justify;" )
                        ),
                        column(2,
                               p('')
                        )
                      ),
                      div(id='footer', align='center', style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif;  font-size: 2em; text-align: justify;",
                          h4('Realizado por: ', style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif;font-size: 2em; text-align: justify;"),'Alex Contreras, Alexis Valencia, Alejandro Herrera, Mateo Ochoa,
                        Lucas Munoz, Santiago Cadavid', hr())
                      
             ),
             navbarMenu('Historico',
                        tabPanel("Todos los datos", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                                 fluidRow(
                                   column(4,
                                          dateRangeInput("dates", "Rango de fechas",
                                                         start = "2014-01-01",
                                                         end = "2018-12-31",
                                                         min = "2014-01-01",
                                                         max = "2018-12-31",
                                                         separator = " - ")
                                   ),
                                   column(4, 
                                          selectInput("comu",
                                                      "Comuna:",
                                                      c("All",
                                                        unique(as.character(datos$COMUNA))
                                                      )
                                          )
                                   ),
                                   column(4, 
                                          uiOutput('columns')
                                   )
                                 ),
                                 
                                 DT::dataTableOutput('table')
                        ),
                        tabPanel("Mapa", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                                 # Sidebar to demonstrate various slider options ----
                                 div(id='divtit', align='center', h3('Historico por zonas')),
                                 p('En este mapa interactivo usted podrá visualizar el número de accidentes por zonas acorde a una
                                 ventana de tiempo definida; las zonas irán cambiando de acuerdo al zoom que se realice en el 
                                 mapa. Cada zona se representa con un círculo que tiene un color y un número asociado.',
                                   strong('El número expresa cuántos accidentes han ocurrido '), 'y el color expresa
                                 la ocurrencia de accidentes así: ',br(),
                                   strong('- Color Verde:'),
                                   strong(span(' Bajo', style = "color:green")),br(),
                                   strong('- Color Amarillo:'),
                                   strong(span(' Medio', style = "color:gold")),br(),
                                   strong('- Color Naranja:'),
                                   strong(span(' Alto', style = "color:darkorange"))),
                                 fluidRow(
                                   column(12, align="center",
                                          dateRangeInput("Mapdates", label = h4("Ventana de tiempo"),
                                                         start = "2014-01-01",
                                                         end = "2014-12-01",
                                                         min = "2014-01-01",
                                                         max = "2018-12-31",
                                                         separator = " - ")
                                   )
                                 ),
                                 leafletOutput("mymap")
                        ),
                        tabPanel("Agrupación",
                                 # Sidebar to demonstrate various slider options ----
                                 div(id='divtit', align='center', h3('Análisis de Agrupamiento')),
                                 p('Se realiza un agrupamiento a nivel de barrios mostrando patrones de accidentalidad similares en un mapa
                                   En este caso usamos la técnica de agrupamiento de K-MEDIAS utilizando los barrios como unidades de medida
                                   y la cantidad de los diferentes tipos de accidentes como características.
                                   Se determinó que el número de grupos adecuado es de 4. Para la caracterización de los grupos en materia de
                                   accidentalidad tendremos las siguientes 4 categorías:',br(),
                                   strong('-Son barrios con índices de accidentalidad que tienden a ser altos, pero no lo suficiente como para considerarlos muy peligrosos, serán barrios coloreados con naranjado: '),
                                   strong(span(' CATEGORIA MEDIA ALTA', style = "color:darkorange")),br(),
                                   strong('-Los barrios pertenecientes a esta categoría se caracterizan por ser lugares que se consideran muy peligrosos, aquí, los promedios de accidentalidad son los mas altos en cada una de las clases y serán vistos de color rojo: '),
                                   strong(span('CATEGORIA ALTA', style = "color:red")),br(),
                                   strong('-Los barrios pertenecientes a esta categoría se caracterizan por ser lugares que se consideran tranquilos. Los promedios de accidentalidad son los mejores, es decir, los más bajos y se identificarán con el color verde: '),
                                   strong(span(' CATEGORIA BAJA', style = "color:green")),br(),
                                   strong('-Son barrios con índices de accidentalidad relativamente bajos, pero no tanto como para considerarlos bastante tranquilos, estos barrios serán identificados con color amarillo: '),
                                   strong(span('CATEGORIA MEDIA BAJA', style = "color:gold"))),
                                 leafletOutput("mymapcategory")
                        )                        
             ),
             navbarMenu('Prediccion',
                        tabPanel("Por Comuna", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                                 
                                  div(id='divtitM', align='center', h2('PredicciÃ³n por comuna')),
                                  fluidRow(
                                    column(4,
                                           dateRangeInput("PredComdates", "Ventana de tiempo",
                                                          start = "2014-01-01",
                                                          end = "2014-12-01",
                                                          min = "2014-01-01",
                                                          max = "2018-12-31",
                                                          separator = " - ")
                                    ),
                                    column(4, 
                                           selectInput("comuPred",
                                                       "Comuna:",
                                                       unique(as.character(datos$COMUNA))
                                           )
                                    ),
                                    column(4, 
                                           selectInput("escala",
                                                       "Escala:",
                                                       c("Mes", "Semana", "Dia")
                                           )
                                    )
                                  ),
                                  fluidRow(
                                    column(12, align="center",
                                           actionButton("predecirCom", "Predecir")
                                    )
                                  ),
                                 DT::dataTableOutput('tablePredCom'),
                                 div(id='NotaBarr', align='center', strong('Importante: '),p('Los dias faltantes en la prediccion cuando se seleccione la escala "Dia"
                                                                      son los dias en los que no ocurren accidentes', style = "color:red"))
                                 
                        ),
                        tabPanel("Por Barrio", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",

                                       div(id='divtitM', align='center', h2('PredicciÃ³n por barrio')),
                                       fluidRow(
                                         column(4,
                                                dateRangeInput("PredBarrdates", "Ventana de tiempo",
                                                               start = "2014-01-01",
                                                               end = "2014-12-01",
                                                               min = "2014-01-01",
                                                               max = "2018-12-31",
                                                               separator = " - ")
                                         ),
                                         column(4, 
                                                selectInput("barrioPred",
                                                            "Barrio:",
                                                            unique(as.character(datos$BARRIO))
                                                )
                                         ),
                                         column(4, 
                                                selectInput("escalaBarr",
                                                            "Escala:",
                                                            c("Mes", "Semana", "Dia")
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         column(12, align="center",
                                                actionButton("predecirBarr", "Predecir")
                                         )
                                       ),
                                 DT::dataTableOutput('tablePredBarr'),
                                 div(id='NotaBarr', align='center', strong('Importante: '),p('Los dias faltantes en la prediccion cuando se seleccione la escala "Dia"
                                                                      son los dias en los que no ocurren accidentes', style = "color:red"))
                                 )
             )
      )
)

#logica de la app
server <- function(input, output, session) {
  
  output$columns <- renderUI({
    barrioos <- select(filter(datos, COMUNA == input$comu), BARRIO)
    selectInput('barr', 'Barrios:', c('All',barrioos))
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- filter(datos, FECHA >= input$dates[1]  & FECHA <= input$dates[2])
    
    if(input$comu != "All"){
      data <- data[data$COMUNA == input$comu,]
    }
    if(input$barr != "All"){
      data <- data[data$BARRIO == input$barr,]
    }
    data
  }))
  
  output$tablePred <- DT::renderDataTable(DT::datatable({
    
  }))
  
  output$mymap <- renderLeaflet({
    ndatos <- filter(datos, FECHA >= input$Mapdates[1]  & FECHA <= input$Mapdates[2])
    coordenadas <- data.frame("latitude"= ndatos$LATITUD, "longitude" = ndatos$LONGITUD)
    leaflet(coordenadas) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions()
    )
  })
  
  output$mymapcategory <- renderLeaflet({
    
    #Asignación de colores a la categoria
    datos_listos$CATEGORIA <- gsub(1,"orange", datos_listos$CATEGORIA )
    datos_listos$CATEGORIA <- gsub(2,"red", datos_listos$CATEGORIA )
    datos_listos$CATEGORIA <- gsub(3,"green", datos_listos$CATEGORIA )
    datos_listos$CATEGORIA <- gsub(4,"yellow", datos_listos$CATEGORIA )
    barrios_med@data$NOMBRE <- iconv(barrios_med@data$NOMBRE, to="ASCII//TRANSLIT")
    barrios_med@data <- datos_listos
    m=leaflet(barrios_med) 
    m=addTiles(m)
    m=addPolygons(m,popup=barrios_med@data$BARRIO,color=barrios_med@data$CATEGORIA)
  })
  
  
  #Tabla prediccion por comuna
  
  predCom <- eventReactive(input$predecirCom, {
    comaPred <- subset(datos, COMUNA == input$comuPred  & FECHA >= input$PredComdates[1]  & FECHA <= input$PredComdates[2])
    if(input$escala == "Mes"){
      modeloComunaMes <- cforest(N_ACC_MES_COMUNA~PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                     data = comaPred,
                     controls = cforest_unbiased(ntree = 100, mtry = 5))
      p <- predict(modeloComunaMes, comaPred, OOB = TRUE,
                   type = "response")
      pred <- strtoi(p)
      marco <- data.frame(Anio = comaPred$PERIODO, Mes = comaPred$MES, Predicciones = pred)
      res <- unique(marco)
      res <- res[!is.na(res$Predicciones),]
      res <- res[order(res$Anio, res$Mes),]
    }
    if(input$escala == "Semana"){
      modeloComunaSem <- cforest(N_ACC_SEMANA_COMUNA~DIA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                     data = comaPred,
                     controls = cforest_unbiased(ntree = 100, mtry = 5))
      pMES <- predict(modeloComunaSem, comaPred, OOB = TRUE,
                      type = "response")
      predMES <- round(pMES)
      marcoMES <- data.frame(Anio = comaPred$PERIODO, semana = comaPred$semana, Predicciones = predMES)
      res <- unique(marcoMES)
      #Selecciona un solo registro de cada semana
      res <- res %>%
        group_by(Anio,semana) %>%
        slice(1)
      res <- res[order(res$Anio, res$semana),]
    }
    if(input$escala == "Dia"){
      modeloComunaDia <- cforest(N_ACC_DIA_COMUNA~DIA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                     data = comaPred,
                     controls = cforest_unbiased(ntree = 100, mtry = 5))
      pDIA <- predict(modeloComunaDia, comaPred, OOB = TRUE,
                      type = "response")
      pDIA <- round(pDIA)
      marcoDIA <- data.frame(Anio = comaPred$PERIODO, Mes = comaPred$MES, Dia = comaPred$DIA, Predicciones = pDIA)
      #Selecciona un solo registro de cada día
      res <- marcoDIA %>%
        group_by(Dia,Mes,Anio) %>%
        slice(1)
      res <- res[order(res$Anio, res$Mes, res$Dia),]
    }
    res
  })
  
  output$tablePredCom <- DT::renderDataTable(DT::datatable({
    predCom()
  }))
  
  #FIN
  
  #Tabla prediccion por barrio
  
  predBarr <- eventReactive(input$predecirBarr, {
    barraPred <- subset(datos, BARRIO == input$barrioPred)
    if(input$escalaBarr == "Mes"){
      modeloBarrioMes <- cforest(N_ACC_MES_BARRIO~COMUNA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                                 data = barraPred,
                                 controls = cforest_unbiased(ntree = 150, mtry = 4))
      
      pMesBarrio <- predict(modeloBarrioMes, barraPred, OOB = TRUE,
                            type = "response")
      pMesBarrio <- round(pMesBarrio)
      marco <- data.frame(FECHA = barraPred$FECHA, Anio = barraPred$PERIODO, Mes = barraPred$MES, Predicciones = pMesBarrio)
      marco <- subset(marco, FECHA >= input$PredBarrdates[1]  & FECHA <= input$PredBarrdates[2])
      marco <- marco[,-1]
      res <- marco %>%
        group_by(Anio,Mes) %>%
        slice(1)
      res <- res[order(res$Anio, res$Mes),]
    }
    if(input$escalaBarr == "Semana"){
      modeloBarrioSem <- cforest(N_ACC_SEMANA_BARRIO~COMUNA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                     data = barraPred,
                     controls = cforest_unbiased(ntree = 100, mtry = 4))
      pSemBarrio <- predict(modeloBarrioSem, barraPred, OOB = TRUE,
                            type = "response")
      pSemBarrio <- round(pSemBarrio)
      marco <- data.frame(FECHA = barraPred$FECHA, Anio = barraPred$PERIODO, semana = barraPred$semana, Predicciones = pSemBarrio)
      marco <- subset(marco, FECHA >= input$PredBarrdates[1]  & FECHA <= input$PredBarrdates[2])
      marco <- marco[,-1]
      res <- marco %>%
        group_by(Anio,semana) %>%
        slice(1)
      res <- res[order(res$Anio, res$semana),]
    }
    if(input$escalaBarr == "Dia"){
      modeloBarrioDia <- cforest(N_ACC_DIA_BARRIO~DIA+COMUNA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                     data = barraPred,
                     controls = cforest_unbiased(ntree = 100, mtry = 4))
      pDiaBarrio <- predict(modeloBarrioDia, barraPred, OOB = TRUE,
                            type = "response")
      pDiaBarrio <- round(pDiaBarrio)
      marco <- data.frame(FECHA = barraPred$FECHA, Anio = barraPred$PERIODO, Mes = barraPred$MES, Dia =  barraPred$DIA, Predicciones = pDiaBarrio)
      marco <- subset(marco, FECHA >= input$PredBarrdates[1]  & FECHA <= input$PredBarrdates[2])
      marco <- marco[,-1]
      res <- marco %>%
        group_by(Dia,Mes,Anio) %>%
        slice(1)
      res <- res[order(res$Anio, res$Mes, res$Dia),]
    }
    res
  })
  
  output$tablePredBarr <- DT::renderDataTable(DT::datatable({
    predBarr()
  }))
}


shinyApp(ui = ui, server = server)
