library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(knitr)
library(kableExtra)


ui <- fluidPage(
  style = "font-family: 'Roboto', sans-serif; font-size: 11px;",
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$title("CONETEC | Mnisterio de Salud de la Nación"),
    includeCSS("www/styles.css"),
    tags$link(rel = "shortcut icon", href="https://www.argentina.gob.ar/profiles/argentinagobar/themes/argentinagobar/argentinagobar_theme/favicon.ico"),
    tags$style(HTML("
        .custom-div {
                    display: flex;
                    align-items: center;
                    justify-content: flex-start;
                    background-color: #50535C; /* Blue background */
                    border-radius: 10px; /* Rounded corners */
                    padding: 10px; /* Some padding */
                    color: white; /* White text */
                    width: 300px; /* Fixed width */
                    height: 50px; /* Fixed height */
                    margin: 15px; /* Margin top and bottom for spacing */
                    position: relative; /* Needed for absolute positioning inside */
                    }

   

        .custom-div-text {
                    
                    margin-left: 20px;
                    padding-left: 20px;
                    flex-grow: 1;
                    font-family: 'Roboto', sans-serif;
                    font-weight: 700;
                    font-size: 14px !important; /* Usa !important para asegurar que se aplique */
                  }

    "))
  ),
  navbarPage(tags$a(href = "https://www.argentina.gob.ar/salud",
                    target = "_blank", 
                    tags$img(id = "logo",
                             src = "logo.png", 
                             class = "logo",
                             alt = "Logo")), collapsible = TRUE, 
             tabPanel("Inicio", 
                      fluidRow(
                        column(width = 2,),
                        
                        column(width = 8,tags$div(
                          HTML("
                                    <h4 class='title'>Evaluaciones de tecnologías sanitarias</h4>           
                                               
                                    <p class='paragraph'>Las evaluaciones de tecnologías sanitarias (ETS) son un proceso crítico y sistemático que se utiliza en el campo de la atención médica para evaluar y analizar de manera exhaustiva las tecnologías, intervenciones, tratamientos, dispositivos médicos y procedimientos utilizados en la atención de la salud. Estas evaluaciones tienen como objetivo principal proporcionar información basada en la evidencia que ayude a los profesionales de la salud, a los responsables de la toma de decisiones y a los pacientes a tomar decisiones informadas sobre la adopción, uso y financiamiento de las tecnologías sanitarias.
                                    </p>
                                    "))
                      )),
                      
                      column(width = 2,)
             ),
             tabPanel("Epidemiológico",
                      fluidRow(
                        # Columna para los filtros
                        column(width = 4, 
                               numericInput('year', 'Año de inicio', value = 2023, width = '100%'),
                               numericInput('population', 'Población', value = 46654581, width = '100%'),
                               numericInput('incidence_rate', 'Tasa de incidencia c/100.000', value = 0.000113, width = '100%'),
                               numericInput('percentage_carcinoma', '% de pacientes con Carcinoma de Células Renales de Células Claras', value = 85, width = '100%'),
                               numericInput('percentage_advanced', '% de pacientes con enfermedad localmente avanzada o metastásica', value = 10.48, width = '100%'),
                               numericInput('recurrence_rate_nephrectomy', 'Tasa de recurrencia anual de estadios tempranos I-III sometidos a nefrectomía', value = 12.9, width = '100%'),
                               numericInput('recurrence_rate_no_nephrectomy', 'Tasa de recurrencia anual de estadios tempranos I-III no sometidos a nefrectomía', value = 8.1, width = '100%'),
                               numericInput('recurrence_rate_early_stages', 'Tasa de recurrencia anual de estadios tempranos I-III sometidos a nefrectomía', value = 3.0, width = '100%'),
                               numericInput('eligible_patients', 'Pacientes elegibles para realizar tratamiento de primera línea', value = 87.5, width = '100%')
                               # Más inputs si es necesario
                        ),
                        
                        # Columna para la tabla
                        column(width = 8, 
                               dataTableOutput("epidemiologico_tabla1"),
                               plotOutput("epidemiologico_grafico1")
                        )
                      )
               
             ),
             tabPanel("Cuotas de mercado",
                      fluidRow(
                        column(width = 3, 
                               
                               # Include your custom div her
                               
                               
                               # Include your custom div here
                               tags$div(class = "custom-div",
                                        tags$div(class = "custom-div-circle"),  # Circle part
                                        tags$div(class = "custom-div-text", "Escenario sin inmunoterapia")  # The text you want to display with the specific class
                               ),
                               
                               tags$h3("Market Share en %", style = "font-weight: bold; color: #50535C;font-size: 13px;"),
                               DT::dataTableOutput("cuotademercado_tablaeditable1"),
                               
                               
                          
                               tags$h3("Market Share en cantidades", style = "font-weight: bold; color: #50535C;font-size: 13px;"),
                               DT::dataTableOutput("cuotademercado_tabla3"),
    
                                
                      
                              ),
                        
                        column(width = 3, 
                               
                               
                               tags$div(class = "custom-div",
                                        tags$div(class = "custom-div-circle"),  # Circle part
                                        tags$div(class = "custom-div-text", "Escenario con inmunoterapia")),  # The text you want to display with the specific class
                                
                               
                               tags$h3("Market Share en %", style = "font-weight: bold; color: #50535C;font-size: 13px;"),
                               DT::dataTableOutput("cuotademercado_tablaeditable2"),
                               
                               
                               
                               tags$h3("Market Share en cantidades", style = "font-weight: bold; color: #50535C;font-size: 13px;"),
                               DT::dataTableOutput("cuotademercado_tabla2")
                  
                               
                        ),
                        
                        
                        column(width = 4.5, 
                               div(style = "margin-top: 20px;margin-bottom: 20px;",  # Ajustar según sea necesario
                                   uiOutput("cuotademercado_tabla1")
                               ),
                               plotOutput("cuotademercado_grafico2"),
                               plotOutput("cuotademercado_grafico1")
                        )
                      )
             ),
             tabPanel("Costos",
                      tabsetPanel(
                        tabPanel("Resumen costos mensuales",
                                 column(width = 9,
                                        DTOutput("costos_tablaeditabla2")
                                 )
                        ),
                        tabPanel("Costos asocisados",
                                 
                                column(width = 9,
                                 tags$h3("Costos mensuales asociados al seguimiento habitual", style = "font-weight: bold; color: #50535C;font-size: 13px;"),
                                        DTOutput("costos_tablaeditable2"),
                                 div(style = "height:35px;"),#este div es para separar los objetos
                                 tags$h3("Costos asociados al tratamiento de eventos adversos", style = "font-weight: bold; color: #50535C;font-size: 13px;"),
                                        DTOutput('costos_tablaeditable3'),
                                 div(style = "height:35px;"),#este div es para separar los objetos
                                 tags$h3("Líneas condicionadas de tratamiento subsiguiente", style = "font-weight: bold; color: #50535C;font-size: 13px;"),
                                 DTOutput("costos_tablaeditable4"),
                                 div(style = "height:35px;"),#este div es para separar los objetos
                                 
                                )
                        ),
                        tabPanel("Costos de adquisición",
                                 
                                 fluidRow(
                                   column(width = 4,
                                          div(style = "height:50px;"),
                                          uiOutput("costos_tabla1")
                                   ),
                                   column(width = 8,
                                          div(style = "height:50px;"),#este div es para separar los objetos
                                          div(class = "row", 
                                              div(class = "col-md-6",
                                                  selectInput("costos_opciones_precio", "Elija tipo de precios:", choices = c("Precio de salida de laboratorio (PSL)", "Precio de venta al público (PVP)"))
                                              ),
                                              div(class = "col-md-6",
                                                  sliderInput("slider", "Descuento general", min = 0, max = 100, value = 0)
                                              )
                                          ),
                                          DTOutput("costos_tablaeditable1")
                                   )
                                   
                                 )
                                 
                        )
                        # Más tabs si es necesario
                      )
             ),
             tabPanel("Eficacia",
                      tabsetPanel(
                        tabPanel("Datos",
                                 column(width = 6,
                                        tags$h3("Sobrevida global", style = "font-weight: bold; color: #50535C;font-size: 13px;"),
                                        plotOutput("eficacia_grafico1")
                                 ),
                                 
                                 column(width = 6,
                                        tags$h3("Sobrevida libre de progresión", style = "font-weight: bold; color: #50535C;font-size: 13px;"),
                                        plotOutput("eficacia_grafico2")
                                 )
                        ),
                        tabPanel("Datos",
                                 column(width = 10,
                                        DTOutput("eficacia_tablaeditable1")
                                 )
                        )
                      )
             ),
             tabPanel("Resultados",
                      DTOutput("resultados_tabla_1"),
                      DTOutput("resultados_tabla_2"),
                      DTOutput("resultados_tabla_3")
             ),
             tabPanel("Umbrales",
             
             ),
             tags$footer(imageOutput("marca"))
  )
  
)
