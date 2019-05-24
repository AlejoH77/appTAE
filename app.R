library(shiny)

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
                                 h1("Aqui va el video", align="center"),
                                 div(class="row",
                                          div(class="col-md-10  col-md-offset-1 videoWrapper",
                                                   iframe(src = "https://www.youtube.com/embed/G1-HDDDK87M", height="50%", width="100%")
                                          )
                                 )
                        )
                        
                      )
                      
             ),
             tabPanel("Historico",
                      sidebarLayout(
                        
                        # Sidebar to demonstrate various slider options ----
                        sidebarPanel(
                          
                          # Input: Simple integer interval ----
                          sliderInput("ano", "Ano:",
                                      min = 2014, max = 2018,
                                      value = 2015),
                          
                          # Input: Specification of range within an interval ----
                          sliderInput("mes", "Mes:",
                                      min = 1, max = 12,
                                      value = c(2,5))
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Table summarizing the values entered ----
                          tableOutput("values")
                          
                        )
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
  
  
}


shinyApp(ui = ui, server = server)