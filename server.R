library(shiny)
library(readxl)
library(DT)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(stringi)
library(openxlsx)
library(leaflet)
library(knitr)
library(kableExtra)



#funcióin para calcular el flujo de pacientes. los parametros son la tabla de probabilidades (data frame) de dos columnas con primero la probabilidad de sobrevida global y despues la probabilidad de vida sin pogresión, el segundo parametro es la población objetivo inicial)


funcion_flujo_pacientes_por_periodo <- function(tabla_pobabilidades,n) {
  
  initial_data_flujo <- data.frame(
    period = c(1),
    patients = c(n),
    progresados = c(0),
    muertos = c(0),
    on_off_evento = c(0)
  )
  
  for(e in c(1:(nrow(tabla_pobabilidades)-1))){
    
    
    progresados <- (1-tabla_pobabilidades[e,2])*n - (n - tabla_pobabilidades[e,1]*n)
    
    patients <-  tabla_pobabilidades[e,2]*n
    
    muertos <- n - tabla_pobabilidades[e,1]*n
    
    on_off_evento <-progresados-initial_data_flujo[[e,3]] 
    
    initial_data_flujo[e+1,] <- c(e+1, patients, progresados, muertos, on_off_evento)
    
  }
  
  
  return(initial_data_flujo)
}


#flunción para determinar el flujo de costos

flujo_costos  <- function(flujos_anio1 ,flujos_anio2,flujos_anio3,costos){
  
  df_mil <- data.frame(
    
    flujo_pacientes1 = flujos_anio1[,2],
    flujo_eventos1   = flujos_anio1[,3],
    flujo_pacientes2 = flujos_anio2[,2],
    flujo_eventos2   = flujos_anio2[,3],
    flujo_pacientes3 = flujos_anio3[,2],
    flujo_eventos3   = flujos_anio3[,3]
  )
  
  
  df_mil$costos_adqui_1 = costos[[1,2]]*df_mil$flujo_pacientes1
  df_mil$costos_segui_1 = costos[[1,3]]*df_mil$flujo_pacientes1
  df_mil$costos_event_1 = costos[[1,4]]*df_mil$flujo_pacientes1
  df_mil$costos_subsi_1 = costos[[1,5]]*df_mil$flujo_eventos1
  
  df_mil$costos_adqui_2 = costos[[1,2]]*df_mil$flujo_pacientes2
  df_mil$costos_segui_2 = costos[[1,3]]*df_mil$flujo_pacientes2
  df_mil$costos_event_2 = costos[[1,4]]*df_mil$flujo_pacientes2
  df_mil$costos_subsi_2 = costos[[1,5]]*df_mil$flujo_eventos2
  
  df_mil$costos_adqui_3 = costos[[1,2]]*df_mil$flujo_pacientes3
  df_mil$costos_segui_3 = costos[[1,3]]*df_mil$flujo_pacientes3
  df_mil$costos_event_3 = costos[[1,4]]*df_mil$flujo_pacientes3
  df_mil$costos_subsi_3 = costos[[1,5]]*df_mil$flujo_eventos3
  
  
  df_anio1 <- df_mil[,c(7,8,9,10)]
  df_anio2 <- df_mil[,c(11,12,13,14)]
  df_anio3 <- df_mil[,c(15,16,17,18)]
  
  
  df_anio2 <- df_anio2[c(1:24),]
  vacio_anio2 <- data.frame(
    costos_adqui_2 = c(0,0,0,0,0,0,0,0,0,0,0,0),
    costos_segui_2 = c(0,0,0,0,0,0,0,0,0,0,0,0),
    costos_event_2 = c(0,0,0,0,0,0,0,0,0,0,0,0),
    costos_subsi_2 = c(0,0,0,0,0,0,0,0,0,0,0,0)
  )
  
  df_anio2 <- rbind(vacio_anio2,df_anio2)
  
  
  df_anio3 <- df_anio3[c(1:12),]
  vacio_anio3 <- data.frame(
    costos_adqui_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    costos_segui_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    costos_event_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    costos_subsi_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  )
  
  df_anio3 <- rbind(vacio_anio3,df_anio3)
  
  df_costos <- cbind(df_anio1,df_anio2,df_anio3)
  
  # Agregar una columna que sea la suma de todas las anteriores para cada fila
  df_costos$total_adqui = rowSums(df_costos[,c(1,5,9)])
  df_costos$total_segui = rowSums(df_costos[,c(2,6,10)])
  df_costos$total_event = rowSums(df_costos[,c(3,7,11)])
  df_costos$total_subsi = rowSums(df_costos[,c(4,8,12)])
  
  df_costos <- df_costos[,c(13:16)]
  
  # Crear un nuevo dataframe para almacenar sumas totales por año
  suma_costos_anios <- data.frame(
    suma_adqui_anio1 = sum(df_costos[1:12, "total_adqui"], na.rm = TRUE),
    suma_adqui_anio2 = sum(df_costos[13:24, "total_adqui"], na.rm = TRUE),
    suma_adqui_anio3 = sum(df_costos[25:36, "total_adqui"], na.rm = TRUE),
    suma_segui_anio1 = sum(df_costos[1:12, "total_segui"], na.rm = TRUE),
    suma_segui_anio2 = sum(df_costos[13:24, "total_segui"], na.rm = TRUE),
    suma_segui_anio3 = sum(df_costos[25:36, "total_segui"], na.rm = TRUE),
    suma_event_anio1 = sum(df_costos[1:12, "total_event"], na.rm = TRUE),
    suma_event_anio2 = sum(df_costos[13:24, "total_event"], na.rm = TRUE),
    suma_event_anio3 = sum(df_costos[25:36, "total_event"], na.rm = TRUE),
    suma_subsi_anio1 = sum(df_costos[1:12, "total_subsi"], na.rm = TRUE),
    suma_subsi_anio2 = sum(df_costos[13:24, "total_subsi"], na.rm = TRUE),
    suma_subsi_anio3 = sum(df_costos[25:36, "total_subsi"], na.rm = TRUE)
  )
  
  
  return(suma_costos_anios)
  
}


#esta es la misma función que antes pero con una modificación para nivolumab + Ipim.. ESTA MARCADA EN LA FUNCIÓN LA MODIFICACIÓN


flujo_costos_vespecial  <- function(flujos_anio1 ,flujos_anio2,flujos_anio3,costos,costo_nivolubam){
  
  df_mil <- data.frame(
    
    flujo_pacientes1 = flujos_anio1[,2],
    flujo_eventos1   = flujos_anio1[,3],
    flujo_pacientes2 = flujos_anio2[,2],
    flujo_eventos2   = flujos_anio2[,3],
    flujo_pacientes3 = flujos_anio3[,2],
    flujo_eventos3   = flujos_anio3[,3]
  )
  
  
  df_mil$costos_adqui_1 = c(costos[[1,2]]*df_mil$flujo_pacientes1[1:3],costo_nivolubam*df_mil$flujo_pacientes1[4:24],c(0,0,0,0,0,0,0,0,0,0,0,0))#ACA CAMBIA
  df_mil$costos_segui_1 = costos[[1,3]]*df_mil$flujo_pacientes1
  df_mil$costos_event_1 = costos[[1,4]]*df_mil$flujo_pacientes1
  df_mil$costos_subsi_1 = costos[[1,5]]*df_mil$flujo_eventos1
  
  df_mil$costos_adqui_2 = c(costos[[1,2]]*df_mil$flujo_pacientes2[1:3],costo_nivolubam*df_mil$flujo_pacientes2[4:24],c(0,0,0,0,0,0,0,0,0,0,0,0))#ACA CAMBIA
  df_mil$costos_segui_2 = costos[[1,3]]*df_mil$flujo_pacientes2
  df_mil$costos_event_2 = costos[[1,4]]*df_mil$flujo_pacientes2
  df_mil$costos_subsi_2 = costos[[1,5]]*df_mil$flujo_eventos2
  
  df_mil$costos_adqui_3 = c(costos[[1,2]]*df_mil$flujo_pacientes3[1:3],costo_nivolubam*df_mil$flujo_pacientes3[4:24],c(0,0,0,0,0,0,0,0,0,0,0,0))#ACA CAMBIA
  df_mil$costos_segui_3 = costos[[1,3]]*df_mil$flujo_pacientes3
  df_mil$costos_event_3 = costos[[1,4]]*df_mil$flujo_pacientes3
  df_mil$costos_subsi_3 = costos[[1,5]]*df_mil$flujo_eventos3
  
  
  df_anio1 <- df_mil[,c(7,8,9,10)]
  df_anio2 <- df_mil[,c(11,12,13,14)]
  df_anio3 <- df_mil[,c(15,16,17,18)]
  
  
  df_anio2 <- df_anio2[c(1:24),]
  vacio_anio2 <- data.frame(
    costos_adqui_2 = c(0,0,0,0,0,0,0,0,0,0,0,0),
    costos_segui_2 = c(0,0,0,0,0,0,0,0,0,0,0,0),
    costos_event_2 = c(0,0,0,0,0,0,0,0,0,0,0,0),
    costos_subsi_2 = c(0,0,0,0,0,0,0,0,0,0,0,0)
  )
  
  df_anio2 <- rbind(vacio_anio2,df_anio2)
  
  
  df_anio3 <- df_anio3[c(1:12),]
  vacio_anio3 <- data.frame(
    costos_adqui_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    costos_segui_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    costos_event_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    costos_subsi_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  )
  
  df_anio3 <- rbind(vacio_anio3,df_anio3)
  
  df_costos <- cbind(df_anio1,df_anio2,df_anio3)
  
  # Agregar una columna que sea la suma de todas las anteriores para cada fila
  df_costos$total_adqui = rowSums(df_costos[,c(1,5,9)])
  df_costos$total_segui = rowSums(df_costos[,c(2,6,10)])
  df_costos$total_event = rowSums(df_costos[,c(3,7,11)])
  df_costos$total_subsi = rowSums(df_costos[,c(4,8,12)])
  
  df_costos <- df_costos[,c(13:16)]
  
  # Crear un nuevo dataframe para almacenar sumas totales por año
  suma_costos_anios <- data.frame(
    suma_adqui_anio1 = sum(df_costos[1:12, "total_adqui"], na.rm = TRUE),
    suma_adqui_anio2 = sum(df_costos[13:24, "total_adqui"], na.rm = TRUE),
    suma_adqui_anio3 = sum(df_costos[25:36, "total_adqui"], na.rm = TRUE),
    suma_segui_anio1 = sum(df_costos[1:12, "total_segui"], na.rm = TRUE),
    suma_segui_anio2 = sum(df_costos[13:24, "total_segui"], na.rm = TRUE),
    suma_segui_anio3 = sum(df_costos[25:36, "total_segui"], na.rm = TRUE),
    suma_event_anio1 = sum(df_costos[1:12, "total_event"], na.rm = TRUE),
    suma_event_anio2 = sum(df_costos[13:24, "total_event"], na.rm = TRUE),
    suma_event_anio3 = sum(df_costos[25:36, "total_event"], na.rm = TRUE),
    suma_subsi_anio1 = sum(df_costos[1:12, "total_subsi"], na.rm = TRUE),
    suma_subsi_anio2 = sum(df_costos[13:24, "total_subsi"], na.rm = TRUE),
    suma_subsi_anio3 = sum(df_costos[25:36, "total_subsi"], na.rm = TRUE)
  )
  
  
  return(suma_costos_anios)
  
}




server <- function(input, output, session) {
  
  # Función para calcular los valores y crear el dataframe
  calcularValores <- reactive({
    

# Epidemiologico ----------------------------------------------------------

    
    # Asegúrate de que estas variables capturan los valores actuales de los inputs
    year <- input$year
    population <- input$population
    tasa_incidencia <- input$incidence_rate
    porcentaje_pacientes_con_carcinoma <- input$percentage_carcinoma
    porcentaje_pacientes_avanzada <- input$percentage_advanced
    pacientes_elegibles <- input$eligible_patients
    recurrencia_nephrectomy_1 <- input$recurrence_rate_nephrectomy
    recurrencia_nephrectomy_2 <- input$recurrence_rate_no_nephrectomy
    recurrencia_nephrectomy_3 <- input$recurrence_rate_early_stages
    
    #nombres para las filas
    nombre_poblacion <- "Población"
    nombre_tasa_incidencia <- "Tasa de incidencia c/100.000"
    nombre_porcentaje_pacientes_con_carcinoma <- "% de pacientes con Carcinoma de Células Renales de Células Claras"
    nombre_porcentaje_pacientes_avanzada <- "% de pacientes con enfermedad localmente avanzada o metastásica"
    nombre_recurrencia_nephrectomy_1 <- "Tasa de recurrencia anual de estadios tempranos I-III sometidos a nefrectomía (1er año)"
    nombre_recurrencia_nephrectomy_2 <- "Tasa de recurrencia anual de estadios tempranos I-III sometidos a nefrectomía (2do año)"
    nombre_recurrencia_nephrectomy_3 <- "Tasa de recurrencia anual de estadios tempranos I-III sometidos a nefrectomía (3er año)"
    nombre_pacientes_elegibles <- "Pacientes elegibles para realizar tratamiento de primera línea"
    
    # Aquí iría todo tu código para calcular los valores...
    
    year2 <- year+1
    year3 <- year+2
    
    population2 <- round(population*0.008853579+population,0)
    population3 <- round(population2*0.017116367+population2,0)
    
    
    year_valor1 <- population * tasa_incidencia
    year_valor2 <- year_valor1 * (porcentaje_pacientes_con_carcinoma / 100)
    year_valor3 <- year_valor2 * (porcentaje_pacientes_avanzada / 100)
    year_valor4 <- year_valor3 * (pacientes_elegibles / 100)
    year_valor5 <- ((year_valor2 - year_valor3) * (recurrencia_nephrectomy_1 / 100))
    
    year2_valor1 <- population2 * tasa_incidencia
    year2_valor2 <- year2_valor1 * (porcentaje_pacientes_con_carcinoma / 100)
    year2_valor3 <- year2_valor2 * (porcentaje_pacientes_avanzada / 100)
    year2_valor4 <- year2_valor3 * (pacientes_elegibles / 100)
    year2_valor5 <- ((year2_valor2 - year2_valor3) * (recurrencia_nephrectomy_1 / 100))
    year2_valor6 <- ((year_valor2 - year_valor3)-((year_valor2 - year_valor3) * recurrencia_nephrectomy_1/100)) * recurrencia_nephrectomy_2/100
    
    year3_valor1 <- population3 * tasa_incidencia
    year3_valor2 <- year3_valor1 * (porcentaje_pacientes_con_carcinoma / 100)
    year3_valor3 <- year3_valor2 * (porcentaje_pacientes_avanzada / 100)
    year3_valor4 <- year3_valor3 * (pacientes_elegibles / 100)
    year3_valor5 <- ((year3_valor2 - year3_valor3) * (recurrencia_nephrectomy_1 / 100))
    year3_valor6 <- ((year2_valor2 - year2_valor3)-((year2_valor2 - year2_valor3) * recurrencia_nephrectomy_1/100)) * recurrencia_nephrectomy_2/100
    year3_valor7 <- ((year_valor2 - year_valor3)-((year_valor2 - year_valor3) * recurrencia_nephrectomy_1/100)-year2_valor6) * recurrencia_nephrectomy_3/100
    

    # Convertir los años a caracteres para usarlos como nombres de columna
    year_col_name <- as.character(year)
    year2_col_name <- as.character(year2)
    year3_col_name <- as.character(year3)
    
    df <- data.frame(
      Variable = c(
        nombre_poblacion, 
        nombre_tasa_incidencia, 
        nombre_porcentaje_pacientes_con_carcinoma, 
        nombre_porcentaje_pacientes_avanzada, 
        nombre_pacientes_elegibles, 
        nombre_recurrencia_nephrectomy_1, 
        nombre_recurrencia_nephrectomy_2, 
        nombre_recurrencia_nephrectomy_3
      ),
      Valor = c(
        population, 
        tasa_incidencia, 
        porcentaje_pacientes_con_carcinoma, 
        porcentaje_pacientes_avanzada, 
        pacientes_elegibles, 
        recurrencia_nephrectomy_1, 
        recurrencia_nephrectomy_2, 
        recurrencia_nephrectomy_3
      ),
      col3 = c(NA,NA,NA,NA,NA,NA,NA,NA),
      col4 = c(NA,NA,NA,NA,NA,NA,NA,NA),
      col5 = c(NA,NA,NA,NA,NA,NA,NA,NA)
    )
    
    # Nombrar las columnas con los años
    names(df)[3] <- year_col_name
    names(df)[4] <- year2_col_name
    names(df)[5] <- year3_col_name
    
    # Añadir los datos correspondientes a los años
    df[year_col_name] <- c(population, year_valor1, year_valor2, year_valor3, year_valor4, year_valor5, NA, NA)
    df[year2_col_name] <- c(population2, year2_valor1, year2_valor2, year2_valor3, year2_valor4, year2_valor5, year2_valor6, NA)
    df[year3_col_name] <- c(population3, year3_valor1, year3_valor2, year3_valor3, year3_valor4, year3_valor5, year3_valor6, year3_valor7)
    
    
    
    # Devolver el dataframe
    df
  })
  
  # Ejemplo de cómo usar estos valores en otro lugar

  
  output$epidemiologico_tabla1 <- DT::renderDT({
    df <- calcularValores()  # Llama a la función reactiva para obtener el df actualizado
    
    datatable(df, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('excel', 'pdf'),
      paging = FALSE,  # Deshabilita la paginación
      info = FALSE,
      pageLength = 10,  # Puedes ajustar el número de filas mostradas por página
      columnDefs = list(list(visible = FALSE, targets = 0))  # Ocultar la primera columna (índices)
    ))
    
    })
  
  output$epidemiologico_grafico1 <- renderPlot({
    
    df2 <- calcularValores()
    
    pobl_ele1 <- df2[[5,3]] + df2[[6,3]]
    pobl_ele2 <- df2[[5,4]] + df2[[6,4]] + df2[[7,4]]
    pobl_ele3 <- df2[[5,5]] + df2[[6,5]] + df2[[7,5]] + df2[[8,5]]
    
    year <- as.numeric(colnames(df2)[[3]])
    year2 <- year + 1
    year3 <- year + 2
    
    year<- as.character(year)
    year2<- as.character(year2)
    year3<- as.character(year3)
    
    df_graf1 <- data.frame(
      anio  = c(year,year2,year3),
      valor = c(pobl_ele1,pobl_ele2,pobl_ele3)
    )
    
    ggplot(df_graf1, aes(x = anio, y = valor)) +
      geom_col(color = "#50535C", fill = "#50535C", width = 0.6) +
      geom_text(aes(label = valor), vjust = -0.3, color = "black") +  # Añade etiquetas de texto
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      labs(title = "", x = "", y = "Población Elegible")
    
  }, width = function() { 350 }, height = function() { 275 })
  

# Cuota_de_mercado --------------------------------------------------------
  #tabla 1
  
  tabla_poblacion_obj_por_año <- reactive({
    df <- calcularValores() # Asegúrate de que esta función devuelva el dataframe df2
    
    
    pobl_ele1 <- df[[5,3]] + df[[6,3]]
    pobl_ele2 <- df[[5,4]] + df[[6,4]] + df[[7,4]]
    pobl_ele3 <- df[[5,5]] + df[[6,5]] + df[[7,5]] + df[[8,5]]
    
    year <- as.numeric(colnames(df)[[3]])
    year2 <- year + 1
    year3 <- year + 2
    
    year<- as.character(year)
    year2<- as.character(year2)
    year3<- as.character(year3)
    
    tot <- "Total"
    
    df2 <- data.frame(
      a  = pobl_ele1,
      b  = pobl_ele2,
      c  = pobl_ele3,
      Total = pobl_ele1 + pobl_ele2 + pobl_ele3
    )
    names(df2)[1] <- year
    names(df2)[2] <- year2
    names(df2)[3] <- year3
    
    df2
    
  })
  
  output$cuotademercado_tabla1 <- renderUI({
    
    
    tabla <- kable(tabla_poblacion_obj_por_año(), "html", escape = FALSE) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover"),
        full_width = FALSE, 
        position = "center", # Esto centrará la tabla
        font_size = 16 # Esto aumentará el tamaño de la fuente
      ) %>%
      add_header_above(c("Población objetivo por año" = 4), background = "#59abcf", color = "white", extra_css = "border-top-left-radius: 10px; border-top-right-radius: 10px;") %>% # Añadir el título con fondo y color deseado
      row_spec(0, background = "#59abcf", color = "white", bold = TRUE) %>%
      row_spec(1, extra_css = "border-bottom-left-radius: 10px; border-bottom-right-radius: 10px;")
    
    
    # Devuelve un elemento UI para renderizar HTML
    htmltools::HTML(as.character(tabla))
  })
  
  #tabla editable 1
  
  # Define una variable reactiva al inicio del server para almacenar los datos
  values <- reactiveValues()
  values$initial_data <- read.csv("initialdata_cuotademercado_tablaeditable1.csv")
  
  #para que los años coincidan
  
  
  
  # Renderiza la tabla DT editable
  output$cuotademercado_tablaeditable1 <- renderDT({
    
    df4 <- calcularValores()
    
    names(values$initial_data)[2] <- names(df4)[3]
    names(values$initial_data)[3] <- names(df4)[4]
    names(values$initial_data)[4] <- names(df4)[5]
    
    datatable(
      values$initial_data,
      editable = TRUE,
      options = list(
        dom = 't',
        paging = FALSE,  # Deshabilita la paginación
        info = FALSE,
        pageLength = 10,  # Puedes ajustar el número de filas mostradas por página
        columnDefs = list(list(visible = FALSE, targets = 0))
      )
    )
  }, server = FALSE)
  
  # Observe changes in the table and update reactive values
  observeEvent(input$cuotademercado_tablaeditable1_cell_edit, {
    info <- input$cuotademercado_tablaeditable1_cell_edit
    str(info)  # For debugging; prints the edit information to the R console
    
    # Extract the new value and coordinates of the edited cell
    new_value <- as.numeric(str_replace(info$value, "%", ""))
    row <- info$row
    col <- info$col
    
    # Update the data in reactive values
    isolate({
      values$initial_data[row, col] <- new_value
      
      # Optionally, save the updated data frame back to CSV
      write.csv(values$initial_data, "initialdata_cuotademercado_tablaeditable1.csv", row.names = FALSE, quote = TRUE)
    })
  })
  
  # Renderiza el gráfico
  output$cuotademercado_grafico2 <- renderPlot({
    # Usa los datos actualizados desde reactiveValues para crear el gráfico
    df_long <- values$initial_data %>% pivot_longer( cols = c(2, 3, 4), names_to = "Year", values_to = "Percentage")
    
    color_palette <- c("#50535C", "#4a6eb0", "#114c5f", "#f2eecf","#0799b6", "#9cd2d3")
    
    
    # Creamos el gráfico de área proporcional
   
  ggplot(df_long, aes(x = Year, y = Percentage, fill = Tratamientos, group = Tratamientos)) +
  geom_area(position = 'fill') +
  scale_y_continuous(labels = scales::percent, name = "Porcentaje del total") +  # Elimina la etiqueta del eje Y
  scale_x_discrete(name = "") +  # Ajusta el eje X para tratar 'Year' como discreto
    scale_fill_manual(values = color_palette) + 
  theme_minimal() +
  labs(
    title = "Distribución porcentual de los tratamientos\na lo largo de los años (sin inmunoterapia)"  # Título de dos líneas
  ) +
  theme(
    legend.position = "none",  # Elimina la leyenda
    panel.grid.major = element_blank(),  # Elimina la cuadrícula principal
    panel.grid.minor = element_blank(),  # Elimina la cuadrícula secundaria
    axis.text.x = element_text(color = "#50535c"),
    axis.title.y = element_text(color = "#50535c"),# Color del texto del eje X en gris claro
 # Asegúrate de que el título del eje X también sea gris claro
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = "#50535C"),  # Estilo del título centrado
    plot.margin = margin(1, 1, 1, 1, "cm")  # Ajustar los márgenes del gráfico si es necesario
  )

    
    
    
    }, width = function() { 600 }, height = function() { 275 })
  
  
  poblacion_cantidades_sin_inmunoterapia <- reactive({
   
    #traigo los datos de la población objetivo
    
     
    datos_poblacion <- tabla_poblacion_obj_por_año()
    
    treatments <- c("Sunitinib",
                    "Pembrolizumab + Axitinib",
                    "Pembrolizumab + Lenvatinib",
                    "Nivolumab + Cabozantinib",
                    "Nivolumab + Ipilimumab",
                    "Avelumab + Axitinib")
    
    
    tabla_cantidades1 <- data.frame(
      Tratamientos = treatments,
      `2023` = numeric(length(treatments)),
      `2024` = numeric(length(treatments)),
      `2025` = numeric(length(treatments))
    )
    
    if(is.null(values$initial_data)){cuot_mercado<-read.csv("initialdata_cuotademercado_tablaeditable1.csv")}else{cuot_mercado <- values$initial_data}
    
    
    # Corrige el bucle for para actualizar correctamente los valores
    for(e in seq_along(treatments)){
      
      tabla_cantidades1[e, 2] <- cuot_mercado[e, names(cuot_mercado) == "2023"] * datos_poblacion[[1,1]] / 100
      tabla_cantidades1[e, 3] <- cuot_mercado[e, names(cuot_mercado) == "2024"] * datos_poblacion[[1,2]] / 100
      tabla_cantidades1[e, 4] <- cuot_mercado[e, names(cuot_mercado) == "2025"] * datos_poblacion[[1,3]] / 100
    }
    
    names(tabla_cantidades1)[2] <- names(datos_poblacion)[1]
    names(tabla_cantidades1)[3] <- names(datos_poblacion)[2]
    names(tabla_cantidades1)[4] <- names(datos_poblacion)[3]
    
    write.csv(tabla_cantidades1, "initialdata_cuotademercado_tabla1.csv", row.names = FALSE, quote = TRUE)
    
    return(tabla_cantidades1)
    
  })
  
  
  

  
  
  output$cuotademercado_tabla3 <- renderDT({
    
    tabla_cantidades1 <- poblacion_cantidades_sin_inmunoterapia()
    columnas_numericas <- sapply(tabla_cantidades1, is.numeric)
    
    datatable(
      tabla_cantidades1,
      editable = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        info = FALSE,
        pageLength = 10,
        columnDefs = list(list(visible = FALSE, targets = 0))
      )
    ) %>%
      DT::formatCurrency(columns = which(columnas_numericas), currency = '', mark = '',dec.mark = ',' ,digits = 0)
  }, server = FALSE)
 
  
  #tabla editable 2
  
  # Define una variable reactiva al inicio del server para almacenar los datos
  values2 <- reactiveValues()
  values2$initial_data <- read.csv("initialdata_cuotademercado_tablaeditable2.csv")
  
  #para que los años coincidan
  
  
  
  # Renderiza la tabla DT editable
  output$cuotademercado_tablaeditable2 <- renderDT({
    
    df4 <- calcularValores()
    
    names(values2$initial_data)[2] <- names(df4)[3]
    names(values2$initial_data)[3] <- names(df4)[4]
    names(values2$initial_data)[4] <- names(df4)[5]
    
    datatable(
      values2$initial_data,
      editable = TRUE,
      options = list(
        dom = 't',
        paging = FALSE,  # Deshabilita la paginación
        info = FALSE,
        pageLength = 10,  # Puedes ajustar el número de filas mostradas por página
        columnDefs = list(list(visible = FALSE, targets = 0))
      )
    )
  }, server = FALSE)
  
  # Observe changes in the table and update reactive values
  observeEvent(input$cuotademercado_tablaeditable2_cell_edit, {
    info <- input$cuotademercado_tablaeditable2_cell_edit
    str(info)  # For debugging; prints the edit information to the R console
    
    # Extract the new value and coordinates of the edited cell
    new_value <- as.numeric(str_replace(info$value, "%", ""))
    row <- info$row
    col <- info$col
    
    # Update the data in reactive values
    isolate({
      values2$initial_data[row, col] <- new_value
      
      # Optionally, save the updated data frame back to CSV
      write.csv(values2$initial_data, "initialdata_cuotademercado_tablaeditable2.csv", row.names = FALSE, quote = TRUE)
    })
  })
  
  # Renderiza el gráfico
  output$cuotademercado_grafico1 <- renderPlot({
    # Usa los datos actualizados desde reactiveValues para crear el gráfico
    df_long2 <- values2$initial_data %>% pivot_longer( cols = c(2, 3, 4), names_to = "Year", values_to = "Percentage")
    
    # Definir los colores a usar en el gráfico
    color_palette <- c("#50535C", "#4a6eb0", "#114c5f", "#f2eecf","#0799b6", "#9cd2d3")
    
    ggplot(df_long2, aes(x = Year, y = Percentage, fill = Tratamientos, group = Tratamientos)) +
      geom_area(position = 'fill') +
      scale_fill_manual(values = color_palette) +  # Asigna la paleta de colores a los tratamientos
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      labs(
        title = "Distribución porcentual de los tratamientos\na lo largo de los años (con inmunoterapia)",
        y = "Porcentaje del Total", x = ""
      ) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(color = "#50535C"),
        plot.title = element_text(hjust = 0.5, color = "#50535C", face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( margin = margin(t = -5)),
        axis.text.y = element_text(margin = margin(r = 5)),
        axis.title.y = element_text(color = "#50535C"),
        plot.margin = margin(1, 1, 1, 1, "cm")
      ) +
      guides(fill = guide_legend(title = NULL, nrow = 2, byrow = TRUE, keywidth = 1.5, keyheight = 1))
    
  }, width = function() { 600 }, height = function() { 325})
  
  
  
  
  
  
  poblacion_cantidades_con_inmunoterapia <- reactive({
    
    #traigo los datos de la población objetivo
    
    
    datos_poblacion <- tabla_poblacion_obj_por_año()
    
    treatments <- c("Sunitinib",
                    "Pembrolizumab + Axitinib",
                    "Pembrolizumab + Lenvatinib",
                    "Nivolumab + Cabozantinib",
                    "Nivolumab + Ipilimumab",
                    "Avelumab + Axitinib")
    
    
    tabla_cantidades1 <- data.frame(
      Tratamientos = treatments,
      `2023` = numeric(length(treatments)),
      `2024` = numeric(length(treatments)),
      `2025` = numeric(length(treatments))
    )
    
    if(is.null(values2$initial_data)){cuot_mercado<-read.csv("initialdata_cuotademercado_tablaeditable1.csv")}else{cuot_mercado <- values2$initial_data}
    
    # Corrige el bucle for para actualizar correctamente los valores
    for(e in seq_along(treatments)){
      
      tabla_cantidades1[e, 2] <- cuot_mercado[e, names(cuot_mercado) == "2023"] * datos_poblacion[[1,1]] / 100
      tabla_cantidades1[e, 3] <- cuot_mercado[e, names(cuot_mercado) == "2024"] * datos_poblacion[[1,2]] / 100
      tabla_cantidades1[e, 4] <- cuot_mercado[e, names(cuot_mercado) == "2025"] * datos_poblacion[[1,3]] / 100
    }
    
    names(tabla_cantidades1)[2] <- names(datos_poblacion)[1]
    names(tabla_cantidades1)[3] <- names(datos_poblacion)[2]
    names(tabla_cantidades1)[4] <- names(datos_poblacion)[3]
    
    write.csv(tabla_cantidades1, "initialdata_cuotademercado_tabla2.csv", row.names = FALSE, quote = TRUE)
    
    
    tabla_cantidades1
    
  })
  
  
  
  
  
  
  output$cuotademercado_tabla2 <- renderDT({
    
    tabla_cantidades1 <- poblacion_cantidades_con_inmunoterapia()
    columnas_numericas <- sapply(tabla_cantidades1, is.numeric)
    
    datatable(
      tabla_cantidades1,
      editable = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        info = FALSE,
        pageLength = 10,
        columnDefs = list(list(visible = FALSE, targets = 0))
      )
    ) %>%
      DT::formatCurrency(columns = which(columnas_numericas), currency = '', mark = '',dec.mark = ',' ,digits = 0)
  }, server = FALSE)
  
# Costos ------------------------------------------------------------------
  
  ## Pestaña tabla resumen----------------------------
  
  
  
  #pestaña tabla resumen
  
  resumen_costos_react  <- reactive({
  
  
  
  tabla_costos <- resultData()
  tabla_costosasociados <- initial_data3_react()
  tabla_costoseventos <- initial_data4_react()
  tabla_costostratsub <- initial_data5_react()
  
  tabla_costostratsub <- tabla_costostratsub[,-1]
  
  
  
  
  tabla_resumen <- data.frame(
    a = c("Sunitinib", "Pembrolizumab + Axitinib", "Pembrolizumab + Lenvatinib", "Nivolumab + Cabozantinib", "Nivolumab + Ipilimumab", "Avelumab + Axitinib"),
    b = c( tabla_costos[[10,4]],
           tabla_costos[[2,4]] + tabla_costos[[9,4]],
           tabla_costos[[5,4]] + tabla_costos[[9,4]],
           tabla_costos[[3,4]] + tabla_costos[[6,4]],
           tabla_costos[[4,4]] + tabla_costos[[7,4]],
           tabla_costos[[1,4]] + tabla_costos[[2,4]]),
    c = c(sum(tabla_costosasociados[,2] * tabla_costosasociados[,3]),
          sum(tabla_costosasociados[,2] * tabla_costosasociados[,4]),
          sum(tabla_costosasociados[,2] * tabla_costosasociados[,5]),
          sum(tabla_costosasociados[,2] * tabla_costosasociados[,6]),
          sum(tabla_costosasociados[,2] * tabla_costosasociados[,7]),
          sum(tabla_costosasociados[,2] * tabla_costosasociados[,8])),
    d = c(sum(tabla_costoseventos[,2] * tabla_costoseventos[,3]/100),
          sum(tabla_costoseventos[,2] * tabla_costoseventos[,4]/100),
          sum(tabla_costoseventos[,2] * tabla_costoseventos[,5]/100),
          sum(tabla_costoseventos[,2] * tabla_costoseventos[,6]/100),
          sum(tabla_costoseventos[,2] * tabla_costoseventos[,7]/100),
          sum(tabla_costoseventos[,2] * tabla_costoseventos[,8]/100)),
    e = c(sum(tabla_costostratsub[1,][c(1:7)]/100 * tabla_costos[,4][c(2,3,5,8,10,6,11)],tabla_costostratsub[1,][c(8)]/100*123000),
          sum(tabla_costostratsub[2,][c(1:7)]/100 * tabla_costos[,4][c(2,3,5,8,10,6,11)],tabla_costostratsub[2,][c(8)]/100*123000),
          sum(tabla_costostratsub[3,][c(1:7)]/100 * tabla_costos[,4][c(2,3,5,8,10,6,11)],tabla_costostratsub[3,][c(8)]/100*123000),
          sum(tabla_costostratsub[4,][c(1:7)]/100 * tabla_costos[,4][c(2,3,5,8,10,6,11)],tabla_costostratsub[4,][c(8)]/100*123000),
          sum(tabla_costostratsub[5,][c(1:7)]/100 * tabla_costos[,4][c(2,3,5,8,10,6,11)],tabla_costostratsub[5,][c(8)]/100*123000),
          sum(tabla_costostratsub[6,][c(1:7)]/100 * tabla_costos[,4][c(2,3,5,8,10,6,11)],tabla_costostratsub[6,][c(8)]/100*123000))
  )
  
  nuevos_nombres <- c("Tratamientos", "Adquisición", "Seguimiento / Monitoreo", "Eventos Adversos", "Tratamiento subsiguiente")
  
  # Asignando los nuevos nombres al data frame tabla_resumen
  names(tabla_resumen) <- nuevos_nombres
  
  tabla_resumen
  })
  
  
  
  output$costos_tablaeditabla2 <- renderDT({
    
    tabla_resumen <- resumen_costos_react()
    
    datatable(tabla_resumen, editable = TRUE, options = list(autoWidth = F,dom = 't', paging = FALSE, info = FALSE, pageLength = 10, columnDefs = list(list(visible = FALSE, targets = 0))))
  })
  
  
  ## Pestaña costos asociados ---------------------------
  
  
  #tabla 1 de costos asociados
  
  output$costos_tablaeditable2 <- renderDT({
    
    initial_data3 <- read.csv("initialdata_costos_tablaeditable2.csv", check.names = FALSE)
    
    datatable(initial_data3, editable = TRUE, options = list(dom = 't', paging = FALSE, info = FALSE, pageLength = 10, columnDefs = list(list(visible = FALSE, targets = 0))))
  })
  
  
  observeEvent(input$costos_tablaeditable2_cell_edit, {
    info2 <- input$costos_tablaeditable2_cell_edit
    str(info2) # Para depuración
    
    # Actualiza el dataframe y la variable reactiva
    updated_data <- editData(initial_data3_react(), info2)
    initial_data3_react(updated_data) # Actualizar la variable reactiva
    write.csv(updated_data, "initialdata_costos_tablaeditable2.csv", row.names = FALSE)
  })
  
  
  initial_data3_react <- reactiveVal(read.csv("initialdata_costos_tablaeditable2.csv", check.names = FALSE))
  
  #tabla 2 de costos asociados
  
  
  output$costos_tablaeditable3 <- renderDT({
    
    initial_data4 <- read.csv("initialdata_costos_tablaeditable3.csv", check.names = FALSE)
    
    datatable(initial_data4, editable = TRUE, options = list(dom = 't', paging = FALSE, info = FALSE, pageLength = 10, columnDefs = list(list(visible = FALSE, targets = 0))))
  })
  
  
  observeEvent(input$costos_tablaeditable3_cell_edit, {
    info3 <- input$costos_tablaeditable3_cell_edit
    str(info3) # Para depuración
    
    # Actualiza el dataframe y la variable reactiva
    updated_data <- editData(initial_data4_react(), info3)
    initial_data4_react(updated_data) # Actualizar la variable reactiva
    write.csv(updated_data, "initialdata_costos_tablaeditable3.csv", row.names = FALSE)
  })
  
  
  initial_data4_react <- reactiveVal(read.csv("initialdata_costos_tablaeditable3.csv", check.names = FALSE))
  
  ## Pestaña costos de adquisición ---------------------
  
  output$costos_tablaeditable1 <- renderDT({
    initial_data2 <- read.csv("initialdata_costos_tablaeditable1.csv", check.names = FALSE)
    
    datatable(initial_data2, editable = TRUE, options = list(dom = 't', paging = FALSE, info = FALSE, pageLength = 10, columnDefs = list(list(visible = FALSE, targets = 0))))
  })
  
  observeEvent(input$costos_tablaeditable1_cell_edit, {
    info <- input$costos_tablaeditable1_cell_edit
    str(info) # Para depuración
    
    # Actualiza el dataframe y la variable reactiva
    updated_data <- editData(initial_data2_react(), info)
    initial_data2_react(updated_data) # Actualizar la variable reactiva
    write.csv(updated_data, "initialdata_costos_tablaeditable1.csv", row.names = FALSE)
  })
  
  initial_data2_react <- reactiveVal(read.csv("initialdata_costos_tablaeditable1.csv", check.names = FALSE))
  # Observar cambios y realizar cálculos
  resultData <- reactive({
    data <- initial_data2_react()
    
    descuento_gral <- input$slider
    dropdownValue <- input$costos_opciones_precio
    
    if(is.null(dropdownValue)){dropdownValue <- "Precio de salida de laboratorio (PSL)"}
    
    if(dropdownValue == "Precio de salida de laboratorio (PSL)"){factor <- 1.7545}else{factor <- 1}
    
    nombres_simplificados <- c("tratamientos", "mg_por_unidad", "unidades_por_presentacion",
                               "costo_presentacion", "descuento_individual",
                               "mg_por_dosis",  "dosis_por_mes",
                               "adherencia")
    colnames(data) <- nombres_simplificados
    
    
    data <- data %>%
      mutate(costo_por_mg = (((costo_presentacion * (1 - descuento_individual/100)) / factor) / (unidades_por_presentacion * mg_por_unidad)) * (1 - descuento_gral/100)) %>%
      mutate(costo_dosis = costo_por_mg* mg_por_dosis) %>%
      mutate(costo_mensual = costo_dosis*dosis_por_mes*adherencia/100) %>%
      select(tratamientos, costo_por_mg, costo_dosis, costo_mensual)
    colnames(data) <- c("Tratamientos","Costo por mg", "Costo por dosis", "Costo mensual")
    
    

    data# Devuelve un dataframe para mostrar en la tabla de resultados
  })
  
  # Tabla de resultados (no editable)
  
  
  output$costos_tabla1 <- renderUI({
    # Obtenemos los datos
    data_calculada <- resultData()
    
    # Creamos la tabla con kable y la personalizamos con kableExtra
    tabla2 <- kable(data_calculada, format = "html", escape = FALSE) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                    full_width = F, font_size = 12) %>%  # Controlar el tamaño del texto de toda la tabla
      row_spec(0, bold = TRUE, color = "white", background = "#59abcf") %>%  # Primera fila con configuración específica
      row_spec(c(1:nrow(data_calculada)), background = "white") %>%  # Primera fila con configuración específica
      
      add_header_above(c("Costos" = 4), background = "#59abcf", color = "white", extra_css = "border-top-left-radius: 10px; border-top-right-radius: 10px;") %>% # Añadir el título con fondo y color deseado
      column_spec(1,color = "white", background = "#59abcf")
    # Devuelve un elemento UI para renderizar HTML
    HTML(as.character(tabla2))
  })
  
  
  
  
  #tabla 3 de costos asociados
  
  output$costos_tablaeditable4 <- renderDT({
    
    initial_data5 <- read.csv("initialdata_costos_tablaeditable4.csv", check.names = FALSE)
    
    datatable(initial_data5, editable = TRUE, options = list(dom = 't', paging = FALSE, info = FALSE, pageLength = 10, columnDefs = list(list(visible = FALSE, targets = 0))))
  })
  
  
  observeEvent(input$costos_tablaeditable4_cell_edit, {
    info4 <- input$costos_tablaeditable4_cell_edit
    str(info4) # Para depuración
    
    # Actualiza el dataframe y la variable reactiva
    updated_data <- editData(initial_data5_react(), info4)
    initial_data5_react(updated_data) # Actualizar la variable reactiva
    write.csv(updated_data, "initialdata_costos_tablaeditable4.csv", row.names = FALSE, quote = FALSE)
  })
  
  
  initial_data5_react <- reactiveVal(read.csv("initialdata_costos_tablaeditable4.csv", check.names = FALSE))
  
  
  
  # Eficacia ----------------------------------------------------------------
  
  output$eficacia_tablaeditable1 <- renderDT({
    
    initial_data_eficacia <-  initial_data6_react()
    
    datatable(initial_data_eficacia, editable = TRUE, options = list(autoWidth = F,dom = 't', paging = FALSE, info = FALSE, pageLength = 10, columnDefs = list(list(visible = FALSE, targets = 0))))
    
  })
  
  
  
  
  observeEvent(input$eficacia_tablaeditable1_cell_edit, {
    info5 <- input$eficacia_tablaeditable1_cell_edit
    str(info5) # Para depuración
    
    # Actualiza el dataframe y la variable reactiva
    updated_data <- editData(initial_data6_react(), info5)
    initial_data6_react(updated_data) # Actualizar la variable reactiva
    write.csv(updated_data, "initialdata_eficacia_tablaeditable1.csv", row.names = FALSE, quote = FALSE)
  })
  
  initial_data6_react <- reactiveVal(read.csv("initialdata_eficacia_tablaeditable1.csv", check.names = FALSE))
  
  output$eficacia_grafico1 <- renderPlot({
    
    data_plot <- initial_data6_react()
    

    data_plot_sg <- data_plot[,c(1,3,5,7,9,11)]
    

    # Crear una secuencia de 36 meses
    
    mes <- c(1:36)
    
    data_plot_sg <- cbind(data_plot_sg,mes)
    
    data_plot_sg <- pivot_longer(data_plot_sg, 
                                   cols = -one_of('mes'), # asumiendo que 'Months' es la columna de tiempo
                                   names_to = "Treatment", 
                                   values_to = "Value")
    
    
    # Definir la paleta de colores personalizada
    color_palette <- c("#50535C", "#4a6eb0", "#114c5f", "#f2eecf","#0799b6", "#9cd2d3")
    
    # Crear el gráfico con ggplot
    ggplot(data_plot_sg, aes(x = mes, y = Value, group = Treatment, color = Treatment)) +
      geom_line(size = 1) + # Hacer las líneas más gruesas
      scale_color_manual(values = color_palette) + # Usar la paleta de colores personalizada
      scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.5,0.75,1))+
      scale_x_continuous(
        breaks = c(12, 24, 36),  # Puntos donde queremos las etiquetas
        labels = c("Año 1", "Año 2", "Año 3")  # Etiquetas para esos puntos
      ) +
      labs(x = "", y = "", color = NULL) + # Eliminar el título de la leyenda
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom", # Posicionar la leyenda en la parte inferior
        legend.direction = "horizontal", # Dirección horizontal de los elementos de la leyenda
        legend.box = "vertical", # Caja vertical para contener las filas de la leyenda
        legend.box.background = element_rect(colour = "transparent"), # Fondo transparente para la caja de la leyenda
        legend.title = element_blank(), # Eliminar el título de la leyenda
        axis.text.x = element_text(angle = 90, vjust = 0.5) # Etiquetas del eje x verticales para mejor legibilidad
      ) +
      guides(
        color = guide_legend(nrow = 2, title.position = "top") # Configurar la leyenda para tener dos filas y sin título
      )
    
  }, width = function() { 600 }, height = function() { 325})
    
    output$eficacia_grafico2 <- renderPlot({
      
      data_plot <- initial_data6_react()
      
      data_plot <- read.csv("initialdata_eficacia_tablaeditable1.csv", check.names = FALSE)
      
      data_plot_slp <- data_plot[,c(2,4,6,8,10,12)]
      
      # Crear una secuencia de 36 meses
      
      mes <- c(1:36)
      
      data_plot_slp <- cbind(data_plot_slp,mes)
      
      data_plot_slp <- pivot_longer(data_plot_slp, 
                                   cols = -one_of('mes'), # asumiendo que 'Months' es la columna de tiempo
                                   names_to = "Treatment", 
                                   values_to = "Value")
      
      
      # Definir la paleta de colores personalizada
      color_palette <- c("#50535C", "#4a6eb0", "#114c5f", "#f2eecf","#0799b6", "#9cd2d3")
      
      # Crear el gráfico con ggplot
      ggplot(data_plot_slp, aes(x = mes, y = Value, group = Treatment, color = Treatment)) +
        geom_line(size = 1) + # Hacer las líneas más gruesas
        scale_color_manual(values = color_palette) + # Usar la paleta de colores personalizada
        scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.5,0.75,1))+
        scale_x_continuous(
          breaks = c(12, 24, 36),  # Puntos donde queremos las etiquetas
          labels = c("Año 1", "Año 2", "Año 3")  # Etiquetas para esos puntos
        ) +
        labs(x = "", y = "", color = NULL) + # Eliminar el título de la leyenda
        theme_minimal() +
        theme(
          legend.position = "bottom", # Posicionar la leyenda en la parte inferior
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.direction = "horizontal", # Dirección horizontal de los elementos de la leyenda
          legend.box = "vertical", # Caja vertical para contener las filas de la leyenda
          legend.box.background = element_rect(colour = "transparent"), # Fondo transparente para la caja de la leyenda
          legend.title = element_blank(), # Eliminar el título de la leyenda
          axis.text.x = element_text(angle = 90, vjust = 0.5) # Etiquetas del eje x verticales para mejor legibilidad
        ) +
        guides(
          color = guide_legend(nrow = 2, title.position = "top") # Configurar la leyenda para tener dos filas y sin título
        )
  
  }, width = function() { 600 }, height = function() { 325})
    

# resultados ------------------------------------------------------
    
  ##resultados sin inmunoterapia--------------------------
    
    ### Sunitinib ----------------------------------------
    
    
    resultados_gral_sunitinib <- reactive({
    
    #COSAS QUE TENGO QUE TRAER
      
     #tabla para ponderar escenarios (PENDIENTE)
      pobl_por_anio <- poblacion_cantidades_sin_inmunoterapia()
      pobl_por_anio <- pobl_por_anio[1,c(2,3,4)]
     
     # probabilidades eficacia
     
     prob <- initial_data6_react()
     
     
     #resumen de costos mensuales
     
     
     costos_res <- resumen_costos_react()
     colnames(costos_res) <- c("trat","adqui","segui","event","subsi")
     
     
     #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
     
  #FUNCIONES
     
     flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(1,2)], pobl_por_anio[1,1])
     flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(1,2)], pobl_por_anio[1,2])
     flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(1,2)], pobl_por_anio[1,3])
     
     flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[1,])
    
     })
    
    ### Pembrolizumab + Axitinib -------------------------
    
    resultados_gral_pembro_axitinib <- reactive({
    
      #COSAS QUE TENGO QUE TRAER
      

      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_sin_inmunoterapia()
      
      pobl_por_anio <- pobl_por_anio[2,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()

      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(3,4)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(3,4)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(3,4)], pobl_por_anio[1,3])
      
      flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[2,])
      
    
    })

    
    ### Pembrolizumab + Lenvatinib -------------------------
    
    resultados_gral_pembro_lenvatinib <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      
      
      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_sin_inmunoterapia()
      
      pobl_por_anio <- pobl_por_anio[3,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(5,6)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(5,6)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(5,6)], pobl_por_anio[1,3])
      
      flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[3,])
      
      
    })
    
    
    
    ### Nivolumab + Cabozantinib-------------------------
    
    resultados_gral_nivolu_cabozanti <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      

      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_sin_inmunoterapia()
      
      pobl_por_anio <- pobl_por_anio[4,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(7,8)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(7,8)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(7,8)], pobl_por_anio[1,3])
      
      flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[4,])
      
      
    })
    
    
    
    
    
    
    output$resultados_tabla_1 <- renderDT({
      
      tablaaa <- resultados_gral_nivolu_cabozanti()
      

      datatable(tablaaa, editable = TRUE, options = list(autoWidth = F,dom = 't', paging = FALSE, info = FALSE, pageLength = 10, columnDefs = list(list(visible = FALSE, targets = 0))))
    })
    
    
    
    ### Nivolumab + Ipilimumab-------------------------
    
    resultados_gral_nivolu_ipilimu <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      

      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_sin_inmunoterapia()
      
      pobl_por_anio <- pobl_por_anio[5,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      
      #resumen costo adquisición mensual por droga individual
      
      tabla_costos_ind <- resultData()
      costo_ind <- tabla_costos_ind[[6,4]]
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(9,10)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(9,10)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(9,10)], pobl_por_anio[1,3])
      
      flujo_costos_vespecial(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[5,],costo_ind)
      
      
    })
    
    
    
    ### Avelumab + Axitinib-------------------------
    
    resultados_gral_avelu_axiti <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      

      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_sin_inmunoterapia()
      
      pobl_por_anio <- pobl_por_anio[6,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(11,12)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(11,12)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(11,12)], pobl_por_anio[1,3])
      

    flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[6,])
      
     
    })
    
    ##resultados con inmunoterapia--------------------------
    
    ### Sunitinib ----------------------------------------
    
    
    resultados_gral_sunitinib2 <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_con_inmunoterapia()
      pobl_por_anio <- pobl_por_anio[1,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      colnames(costos_res) <- c("trat","adqui","segui","event","subsi")
      
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(1,2)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(1,2)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(1,2)], pobl_por_anio[1,3])
      
      flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[1,])
      
    })
    
    ### Pembrolizumab + Axitinib -------------------------
    
    resultados_gral_pembro_axitinib2 <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      
      
      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_con_inmunoterapia()
      pobl_por_anio <- pobl_por_anio[2,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(3,4)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(3,4)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(3,4)], pobl_por_anio[1,3])
      
      flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[2,])
      
      
    })
    
    
    ### Pembrolizumab + Lenvatinib -------------------------
    
    resultados_gral_pembro_lenvatinib2 <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      
      
      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_con_inmunoterapia()
      pobl_por_anio <- pobl_por_anio[3,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(5,6)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(5,6)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(5,6)], pobl_por_anio[1,3])
      
      flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[3,])
      
      
    })
    
    
    
    ### Nivolumab + Cabozantinib-------------------------
    
    resultados_gral_nivolu_cabozanti2 <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      
      
      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_con_inmunoterapia()
      pobl_por_anio <- pobl_por_anio[4,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(7,8)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(7,8)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(7,8)], pobl_por_anio[1,3])
      
      flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[4,])
      
      
    })
    
    
    
    
    
    
    output$resultados_tabla_1 <- renderDT({
      
      tablaaa <- resultados_gral_nivolu_cabozanti()
      
      
      datatable(tablaaa, editable = TRUE, options = list(autoWidth = F,dom = 't', paging = FALSE, info = FALSE, pageLength = 10, columnDefs = list(list(visible = FALSE, targets = 0))))
    })
    
    
    
    ### Nivolumab + Ipilimumab-------------------------
    
    resultados_gral_nivolu_ipilimu2 <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      
      
      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_con_inmunoterapia()
      pobl_por_anio <- pobl_por_anio[5,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      
      #resumen costo adquisición mensual por droga individual
      
      tabla_costos_ind <- resultData()
      costo_ind <- tabla_costos_ind[[6,4]]
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(9,10)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(9,10)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(9,10)], pobl_por_anio[1,3])
      
      flujo_costos_vespecial(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[5,],costo_ind)
      
      
    })
    
    
    
    ### Avelumab + Axitinib-------------------------
    
    resultados_gral_avelu_axiti2 <- reactive({
      
      #COSAS QUE TENGO QUE TRAER
      
      
      
      #tabla para ponderar escenarios (PENDIENTE)
      
      pobl_por_anio <- poblacion_cantidades_con_inmunoterapia()
      pobl_por_anio <- pobl_por_anio[6,c(2,3,4)]
      
      # probabilidades eficacia
      
      prob <- initial_data6_react()
      
      
      #resumen de costos mensuales
      
      
      costos_res <- resumen_costos_react()
      
      #saco el flujo de pacientes con la función los costos los podría PODRÍA AGREGAR A LA FUNCIÓN
      
      #FUNCIONES
      
      flujo_anio_uno  <-  funcion_flujo_pacientes_por_periodo(prob[,c(11,12)], pobl_por_anio[1,1])
      flujo_anio_dos  <-  funcion_flujo_pacientes_por_periodo(prob[,c(11,12)], pobl_por_anio[1,2])
      flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob[,c(11,12)], pobl_por_anio[1,3])
      
      
      flujo_costos(flujo_anio_uno,flujo_anio_dos,flujo_anio_tres,costos_res[6,])
      
      
    })
    
    
    output$resultados_tabla_1 <- renderDT({
      
      
      tabl1 <- resultados_gral_sunitinib()
      tabl2 <- resultados_gral_pembro_axitinib()
      tabl3 <- resultados_gral_pembro_lenvatinib()
      tabl4 <- resultados_gral_nivolu_cabozanti()
      tabl5 <- resultados_gral_nivolu_ipilimu()
      tabl6 <- resultados_gral_avelu_axiti()
      
      
      tablaaa <- rbind(tabl1,tabl2,tabl3,tabl4,tabl5,tabl6)
      columnas_numericas <- sapply( tablaaa, is.numeric)
      
      datatable(
        tablaaa,
        editable = TRUE,
        options = list(
          dom = 't',
          paging = FALSE,
          info = FALSE,
          pageLength = 10,
          columnDefs = list(list(visible = FALSE, targets = 0))
        )
      ) %>%
        DT::formatCurrency(columns = which(columnas_numericas), currency = '', mark = '',dec.mark = ',' ,digits = 0)
    }, server = FALSE)
    
    output$resultados_tabla_2 <- renderDT({
      
      
      tabl1 <- resultados_gral_sunitinib2()
      tabl2 <- resultados_gral_pembro_axitinib2()
      tabl3 <- resultados_gral_pembro_lenvatinib2()
      tabl4 <- resultados_gral_nivolu_cabozanti2()
      tabl5 <- resultados_gral_nivolu_ipilimu2()
      tabl6 <- resultados_gral_avelu_axiti2()
      
      
      tablaaa <- rbind(tabl1,tabl2,tabl3,tabl4,tabl5,tabl6)
      columnas_numericas <- sapply( tablaaa, is.numeric)
      
      datatable(
        tablaaa,
        editable = TRUE,
        options = list(
          dom = 't',
          paging = FALSE,
          info = FALSE,
          pageLength = 10,
          columnDefs = list(list(visible = FALSE, targets = 0))
        )
      ) %>%
        DT::formatCurrency(columns = which(columnas_numericas), currency = '$', mark = '.',dec.mark = ',' ,digits = 0)
    }, server = FALSE)
    
    output$resultados_tabla_1 <- renderDT({
      
      
      tabl1 <- resultados_gral_sunitinib()
      tabl2 <- resultados_gral_pembro_axitinib()
      tabl3 <- resultados_gral_pembro_lenvatinib()
      tabl4 <- resultados_gral_nivolu_cabozanti()
      tabl5 <- resultados_gral_nivolu_ipilimu()
      tabl6 <- resultados_gral_avelu_axiti()
      
      
      tablaaa <- rbind(tabl1,tabl2,tabl3,tabl4,tabl5,tabl6)
      columnas_numericas <- sapply( tablaaa, is.numeric)
      
      datatable(
        tablaaa,
        editable = TRUE,
        options = list(
          dom = 't',
          paging = FALSE,
          info = FALSE,
          pageLength = 10,
          columnDefs = list(list(visible = FALSE, targets = 0))
        )
      ) %>%
        DT::formatCurrency(columns = which(columnas_numericas), currency = '$', mark = '.',dec.mark = ',' ,digits = 0)
    }, server = FALSE)
    
    
        
    
    
    
}

