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
barrios_med = readOGR("./mapa_barrios_categoria/Barrios de Medellín/Barrio_Vereda.shp",layer="Barrio_Vereda")
datos_barrios <- barrios_med@data
names (datos_barrios)[3] = "BARRIO"
datos_barrios$BARRIO <- iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")
datos_barrios$BARRIO <- iconv(datos_barrios$BARRIO, to="ASCII//TRANSLIT")
#Lectura de las categorias de agrupamiento
barrios_categorias <- read.csv("./mapa_barrios_categoria/Categorias.csv", header = TRUE , sep = ";")
barrios_categorias$BARRIO <- iconv(barrios_categorias$BARRIO, to="ASCII//TRANSLIT")

#Interfaz grafica
ui <- fluidPage(
  
  #css
  theme = "estilos.css",
  
  # Titulo App ----
  #titlePanel("Prediccion de accidentes"),
  
  navbarPage("Prediccion de accidentes",
             tabPanel("Home", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                      withTags(
                        div(class="container center",
                            h1("Accident Factory", align="center", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif;  font-size:4;font-weight: 1000; color:darkorange;"),
                            div(class="row",
                                div(class="col-md-10  col-md-offset-1 videoWrapper",
                                    iframe(src = "https://www.youtube.com/embed/G1-HDDDK87M", height="40%", width="80%")
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
                        Lucas Muñoz, Santiago Cadavid', hr())
                      
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
                                 p('En este mapa interactivo usted podrá visualizar el numero de accidentes por zonas acorde a una
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
                        tabPanel("Agrupación", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                                 # Sidebar to demonstrate various slider options ----
                                 div(id='divtit', align='center', h3('Análisis de Agrupamiento')),
                                 p('Se realiza un agrupamiento a nivel de barrios mostrando patrones de accidentalidad similares en un mapa
                                   En este caso usamos la técnica de agrupamiento de K-MEDIAS utilizando los barrios como unidades de medida
                                   y la cantidad de los diferentes tipos de accidentes como características.
                                   Se determinó que el número de grupos adecuado es de 4. Para la caracterización de los grupos en materia de
                                   accidentalidad tendremos las siguientes 4 categorías:',br(),
                                   strong('- Barrios con índices de accidentalidad tienden a ser altos, pero no lo suficiente como para considerarlos muy peligrosos:'),
                                   strong(span(' CATEGORIA MEDIA ALTA', style = "color:darkorange")),br(),
                                   strong('- Barrios pertenecientes a esta categoría se caracterizan por ser lugares que se consideran muy peligrosos:'),
                                   strong(span('CATEGORIA ALTA', style = "color:red")),br(),
                                   strong('-Los barrios pertenecientes a esta categoría se caracterizan por ser lugares que se consideran tranquilos:'),
                                   strong(span(' CATEGORIA BAJA', style = "color:green")),br(),
                                   strong('- Barrios con índices de accidentalidad relativamente bajos, pero no tanto como para considerarlos bastante tranquilos:'),
                                   strong(span('CATEGORIA MEDIA BAJA', style = "color:gold"))),
                                 leafletOutput("mymapcategory")
                        )                        
             ),
             tabPanel("Predicción", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                      
                      fluidRow(
                        column(6,
                               div(id='divtitM', align='center', h2('Predicción por comuna')),
                               div(id='divtitM', align='center', h3('Predictor por Mes')),
                               fluidRow(
                                 column(4,
                                        selectInput("comuMESPred",
                                                    "Comuna:",
                                                    unique(as.character(datos$COMUNA))
                                        )
                                 ),
                                 column(4, 
                                        selectInput("anoMESPred",
                                                    "Año:",
                                                    unique(as.character(datos$PERIODO))
                                        )
                                 ),
                                 column(4, 
                                        selectInput("mesPred",
                                                    "Mes:",
                                                    unique(as.character(datos$MES))
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(12, align="center",
                                        actionButton("showMes", "Predecir")
                                 )
                               ),
                               div(id='divtitS', align='center', h3('Predictor por Semana')),
                               fluidRow(
                                 column(4,
                                        selectInput("comuSEMPred",
                                                    "Comuna:",
                                                    unique(as.character(datos$COMUNA))
                                        )
                                 ),
                                 column(4, 
                                        selectInput("anoSEMPred",
                                                    "Año:",
                                                    unique(as.character(datos$PERIODO))
                                        )
                                 ),
                                 column(4, 
                                        selectInput("semPred",
                                                    "Semana:",
                                                    unique(as.character(datos$semana))
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(12, align="center",
                                        actionButton("showSem", "Predecir")
                                 )
                               ),
                               div(id='divtitD', align='center', h3('Predictor por Día')),
                               fluidRow(
                                 column(3,
                                        selectInput("comuDIAPred",
                                                    "Comuna:",
                                                    unique(as.character(datos$COMUNA))
                                        )
                                 ),
                                 column(3, 
                                        selectInput("anoDIAPred",
                                                    "Año:",
                                                    unique(as.character(datos$PERIODO))
                                        )
                                 ),
                                 column(3, 
                                        selectInput("mesDIAPred",
                                                    "Mes:",
                                                    unique(as.character(datos$MES))
                                        )
                                 ),
                                 column(3, 
                                        selectInput("diaPred",
                                                    "Dia:",
                                                    unique(as.character(datos$DIA))
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(12, align="center",
                                        actionButton("showDia", "Predecir")
                                 )
                               )
                               ),
                        column(6,
                               div(id='divtitM', align='center', h2('Predicción por barrio')),
                               div(id='divtitM', align='center', h3('Predictor por Mes')),
                               fluidRow(
                                 column(4,
                                        selectInput("barrioMESPred",
                                                    "Barrio:",
                                                    unique(as.character(datos$BARRIO))
                                        ) 
                                 ),
                                 column(4, 
                                        selectInput("anoMESPredBarrio",
                                                    "Año:",
                                                    unique(as.character(datos$PERIODO))
                                        )
                                 ),
                                 column(4, 
                                        selectInput("mesPredBarrio",
                                                    "Mes:",
                                                    unique(as.character(datos$MES))
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(12, align="center",
                                        actionButton("showMesBarrio", "Predecir")
                                 )
                               ),
                               div(id='divtitS', align='center', h3('Predictor por Semana')),
                               fluidRow(
                                 column(4,
                                        selectInput("barrioSEMPred",
                                                    "Barrio:",
                                                    unique(as.character(datos$BARRIO))
                                        )
                                 ),
                                 column(4, 
                                        selectInput("anoSEMPredBarrio",
                                                    "Año:",
                                                    unique(as.character(datos$PERIODO))
                                        )
                                 ),
                                 column(4, 
                                        selectInput("semPredBarrio",
                                                    "Semana:",
                                                    unique(as.character(datos$semana))
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(12, align="center",
                                        actionButton("showSemBarrio", "Predecir")
                                 )
                               ),
                               div(id='divtitD', align='center', h3('Predictor por Día')),
                               fluidRow(
                                 column(3,
                                        selectInput("barrioDIAPred",
                                                    "Barrio:",
                                                    unique(as.character(datos$BARRIO))
                                        )
                                 ),
                                 column(3, 
                                        selectInput("anoDIAPredBarrio",
                                                    "Año:",
                                                    unique(as.character(datos$PERIODO))
                                        )
                                 ),
                                 column(3, 
                                        selectInput("mesDIAPredBarrio",
                                                    "Mes:",
                                                    unique(as.character(datos$MES))
                                        )
                                 ),
                                 column(3, 
                                        selectInput("diaPredBarrio",
                                                    "Dia:",
                                                    unique(as.character(datos$DIA))
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(12, align="center",
                                        actionButton("showDiaBarrio", "Predecir")
                                 )
                               )
                              )
                        
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
    #Unión de los barrios con su categoria
    datos_listos <- join(datos_barrios, barrios_categorias)
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
  
  observeEvent(input$showMes, {
    comaPred <- subset(datos, COMUNA == input$comuMESPred  & PERIODO == input$anoMESPred & MES == input$mesPred)
    m19 <- cforest(N_ACC_MES_COMUNA~PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                   data = comaPred,
                   controls = cforest_unbiased(ntree = 100, mtry = 5))
    #m19
    p <- predict(m19, comaPred, OOB = TRUE,
                 type = "response")
    pred <- strtoi(p)
    marco <- data.frame(Año = comaPred$PERIODO, Mes = comaPred$MES, Predicciones = pred)
    res <- unique(marco)
    res <- res[!is.na(res$Predicciones),]
    showModal(modalDialog(title = paste(paste(paste("Predicción para el año ", comaPred$PERIODO), " en el mes "), comaPred$MES), 
                          paste("Número de accidentes: ", res$Predicciones),
                          easyClose = TRUE
    )
    )
  })
  
  observeEvent(input$showSem, {
    comaPredMES <- subset(datos, COMUNA == input$comuSEMPred & PERIODO == input$anoSEMPred & semana == input$semPred)
    mmm <- cforest(N_ACC_SEMANA_COMUNA~DIA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                   data = comaPredMES,
                   controls = cforest_unbiased(ntree = 100, mtry = 5))
    #mmm
    pMES <- predict(mmm, comaPredMES, OOB = TRUE,
                    type = "response")
    predMES <- strtoi(pMES)
    marcoMES <- data.frame(Año = comaPredMES$PERIODO, semana = comaPredMES$semana, Predicciones = predMES)
    resMES <- unique(marcoMES)
    resMES <- resMES[!is.na(resMES$Predicciones),]
    showModal(modalDialog(title = paste(paste(paste("Predicción para la semana ", input$semPred), " del año "), input$anoSEMPred), 
                          paste("Número de accidentes: ", resMES$Predicciones),
                          easyClose = TRUE
    )
    )
  })
  
  observeEvent(input$showDia, {
    comaPredDIA <- subset(datos, COMUNA == input$comuDIAPred & PERIODO == input$anoDIAPred & MES == input$mesDIAPred)
    mmm <- cforest(N_ACC_DIA_COMUNA~DIA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                   data = comaPredDIA,
                   controls = cforest_unbiased(ntree = 100, mtry = 5))
    pDIA <- predict(mmm, comaPredDIA, OOB = TRUE,
                    type = "response")
    marcoDIA <- data.frame(Año = comaPredDIA$PERIODO, Mes = comaPredDIA$MES, Dia = comaPredDIA$DIA, Predicciones = pDIA)
    marcoDIA <- subset(marcoDIA, Dia == input$diaPred)
    resDIA <- mean(marcoDIA$N_ACC_DIA_COMUNA)
    if(is.nan(resDIA)){
      resDIA<-"No ocurriran accidentes"
    }
    showModal(modalDialog(title = paste(paste(paste(paste(paste("Predicción para el día ", input$diaPred), " del mes "), input$mesDIAPred), "del año "), input$anoDIAPred), 
                          paste("Número de accidentes: ", resDIA),
                          easyClose = TRUE
    )
    )
  })
  
  observeEvent(input$showMesBarrio, {
    comaPredMesBarrio <- subset(datos, BARRIO == input$barrioMESPred)
    
    modeloBarrioMes <- cforest(N_ACC_MES_BARRIO~COMUNA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                               data = comaPredMesBarrio,
                               controls = cforest_unbiased(ntree = 150, mtry = 4))
    
    pMesBarrio <- predict(modeloBarrioMes, comaPredMesBarrio, OOB = TRUE,
                          type = "response")
    
    marco <- data.frame(Año = comaPredMesBarrio$PERIODO, Mes = comaPredMesBarrio$MES, Predicciones = pMesBarrio)
    marco <- subset(marco, Año == input$anoMESPredBarrio & Mes == input$mesPredBarrio)
    resMesBarrio <- mean(marco$N_ACC_MES_BARRIO)
    if(is.na(resMesBarrio)){
      resMesBarrio<-"No ocurriran accidentes"
    }
    showModal(modalDialog(title = paste(paste(paste("Predicción para el año ", comaPredMesBarrio$PERIODO), " en el mes "), comaPredMesBarrio$MES), 
                          paste("Número de accidentes: ", resMesBarrio),
                          easyClose = TRUE
    )
    )
  })
  
  observeEvent(input$showSemBarrio, {
    comaPredMES <- subset(datos, BARRIO == input$barrioSEMPred)
    mmm <- cforest(N_ACC_SEMANA_BARRIO~COMUNA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                   data = comaPredMES,
                   controls = cforest_unbiased(ntree = 100, mtry = 4))
    
    pMES <- predict(mmm, comaPredMES, OOB = TRUE,
                    type = "response")
    marcoMES <- data.frame(Año = comaPredMES$PERIODO, semana = comaPredMES$semana, Predicciones = pMES)
    marcoMES <- subset(marcoMES, Año == input$anoSEMPredBarrio & semana == input$semPredBarrio)
    resMES <- mean(marcoMES$N_ACC_SEMANA_BARRIO)
    if(is.na(resMES)){
      resMES<-"No ocurriran accidentes"
    }
    showModal(modalDialog(title = paste(paste(paste("Predicción para la semana ", input$semPredBarrio), " del año "), input$anoSEMPredBarrio), 
                          paste("Número de accidentes: ", resMES),
                          easyClose = TRUE
    )
    )
  })
  
  observeEvent(input$showDiaBarrio, {
    comaPredDIA <- subset(datos, BARRIO == input$barrioDIAPred)
    mmm <- cforest(N_ACC_DIA_BARRIO~DIA+COMUNA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                   data = comaPredDIA,
                   controls = cforest_unbiased(ntree = 100, mtry = 4))
    pDIA <- predict(mmm, comaPredDIA, OOB = TRUE,
                    type = "response")
    marcoDIA <- data.frame(Año = comaPredDIA$PERIODO, Mes = comaPredDIA$MES, Dia = comaPredDIA$DIA, Predicciones = pDIA)
    marcoDIA <- subset(marcoDIA, Año == input$anoDIAPredBarrio & Mes == input$mesDIAPredBarrio & Dia == input$diaPred)
    resDIA <- mean(marcoDIA$N_ACC_DIA_COMUNA)
    if(is.na(resDIA)){
      resDIA<-"No ocurriran accidentes"
    }
    showModal(modalDialog(title = paste(paste(paste(paste(paste("Predicción para el día ", input$diaPredBarrio), " del mes "), input$mesDIAPredBarrio), "del año "), input$anoDIAPredBarrio), 
                          paste("Número de accidentes: ", resDIA),
                          easyClose = TRUE
    )
    )
  })
}


shinyApp(ui = ui, server = server)
