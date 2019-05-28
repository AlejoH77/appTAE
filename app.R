library(shiny)
library(dplyr)

datos<-read.csv("TAE_201905272108.csv")
datos<-datos[,-1]
#cambio el tipo de dato a Date
datos<-mutate(datos, FECHA = as.Date(FECHA))


#Interfaz grafica
ui <- fluidPage(
  
  #css
  theme = "estilos.css",
  
  # Titulo App ----
  #titlePanel("Prediccion de accidentes"),
  
  navbarPage("Predicción de accidentes",
             tabPanel("Home", 
                      withTags(
                        div(class="container center",
                            h1("Aquí va el video", align="center"),
                            div(class="row",
                                div(class="col-md-10  col-md-offset-1 videoWrapper",
                                    iframe(src = "https://www.youtube.com/embed/G1-HDDDK87M", height="50%", width="100%")
                                )
                            )
                        )
                        
                      )
                      
             ),
             tabPanel("Histórico",
                      sidebarLayout(
                        
                        # Sidebar to demonstrate various slider options ----
                        sidebarPanel(
                          
                          # Input: Simple integer interval ----
                          #sliderInput("ano", "Año:",
                           #           min = 2014, max = 2018,
                            #          value = 2015),
                          
                          # Input: Specification of range within an interval ----
                          #sliderInput("mes", "Mes:",
                           #           min = 1, max = 12,
                            #          value = c(2,5)),
                          
                          #radioButtons("radio", label = h3("Año"),
                           #            choices = list("2014" = 2014, "2015" = 2015, "2016" = 2016, "2017" = 2017), 
                            #           selected = 2015),
                          
                          dateRangeInput("dates", label = h3("Rango de fechas"))
                        ),
                        
                        # Main panel para mostrar las salidas ----
                        mainPanel(
                          
                          # Output: Table summarizing the values entered ----
                          #tableOutput("values"),
                          
                          
                          #fluidRow(verbatimTextOutput("fecha"))
                          
                          dataTableOutput('table')
                          
                          
                          #fluidRow(verbatimTextOutput("radio"))
                          
                        ),
                        
                        
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
server <- function(input, output) {
  
  output$table <- renderDataTable(filter(datos, FECHA >= input$dates[1]  & FECHA <= input$dates[2]))
  
  #output$fecha <- renderPrint({ class(as.integer(format(input$dates[1], "%Y"))) })
  
  #output$radio <- renderPrint({ input$radio })
  
}


shinyApp(ui = ui, server = server)