library(shiny)
library(dplyr)
library(leaflet)

datos<-read.csv("db_lat_long_11-06-2019.csv")
#no se muestra la columna #1
datos<-datos[,-1]
#cambio el tipo de dato a Date
datos<-mutate(datos, FECHA = as.Date(FECHA))
comunas <- unique(datos$COMUNA)
Sbarrios <- unique(select(datos, COMUNA, BARRIO))
barrios <- NULL


#Interfaz grafica
ui <- fluidPage(
  
  #css
  theme = "estilos.css",
  
  # Titulo App ----
  #titlePanel("Prediccion de accidentes"),
  
  navbarPage("Prediccion de accidentes",
             tabPanel("Home", 
                      withTags(
                        div(class="container center",
                            h1("Bienvenidos", align="center"),
                            div(class="row",
                                div(class="col-md-10  col-md-offset-1 videoWrapper",
                                    iframe(src = "https://www.youtube.com/embed/G1-HDDDK87M", height="50%", width="100%")
                                )
                            )
                        )
                        
                      )
                      
             ),
             navbarMenu('Historico',
                        tabPanel("Todos los datos",
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
                        tabPanel("Mapa",
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
                                 strong(span(' Medio', style = "color:yellow")),br(),
                                 strong('- Color Amarillo:'),
                                 strong(span(' Alto', style = "color:orange"))),
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
                        tabPanel("Resumen",
                                 dateRangeInput("entrada", label = h4("Rango de fechas"),
                                                start = "2013-01-01",
                                                end = "2019-01-01",
                                                separator = " - "),
                                 selectInput("comunas", "Comunas", comunas),
                                 selectInput("Ibarrios", "Barrios", ""),
                                 #uiOutput('Ibarrios'),
                                 actionButton("show", "Show modal dialog")
                        )
             ),
             navbarMenu('Menu1',
                        tabPanel("SubMenu1",
                                 "hola"
                        )
             )
  )
  
)

#logica de la app
server <- function(input, output, session) {
  
  output$columns <- renderUI({
    barrioos <- select(filter(datos, COMUNA == input$comu), BARRIO)
    #selectInput('barr', 'Columns', names(barrioos$BARRIO))
    selectInput('barr', 'Barrios', c('All',barrioos))
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
  
  output$mymap <- renderLeaflet({
    ndatos <- filter(datos, FECHA >= input$Mapdates[1]  & FECHA <= input$Mapdates[2])
    coordenadas <- data.frame("latitude"= ndatos$LATITUD, "longitude" = ndatos$LONGITUD)
    leaflet(coordenadas) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions()
    )
  })
  
  observeEvent(input$show, {
    showModal(modalDialog(title = "Important message", 
                          paste("Comuna seleccionada: ", input$comunas),
                          easyClose = TRUE
                          )
              )
    })
  
  #output$fecha <- renderPrint({ class(as.integer(format(input$dates[1], "%Y"))) })
  
  #output$radio <- renderPrint({ input$radio })
  
  
}


shinyApp(ui = ui, server = server)