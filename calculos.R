

# Epidemiológico ----------------------------------------------------------

#creo los nombres de las variables
nombre_poblacion <- "Población"
nombre_tasa_incidencia <- "Tasa de incidencia c/100.000"
nombre_porcentaje_pacientes_con_carcinoma <- "% de pacientes con Carcinoma de Células Renales de Células Claras"
nombre_porcentaje_pacientes_avanzada <- "% de pacientes con enfermedad localmente avanzada o metastásica"
nombre_recurrencia_nephrectomy_1 <- "Tasa de recurrencia anual de estadios tempranos I-III sometidos a nefrectomía (1er año)"
nombre_recurrencia_nephrectomy_2 <- "Tasa de recurrencia anual de estadios tempranos I-III sometidos a nefrectomía (2do año)"
nombre_recurrencia_nephrectomy_3 <- "Tasa de recurrencia anual de estadios tempranos I-III sometidos a nefrectomía (3er año)"
nombre_pacientes_elegibles <- "Pacientes elegibles para realizar tratamiento de primera línea"

year <- 2023
population <- 46654581
tasa_incidencia <- 0.000113
porcentaje_pacientes_con_carcinoma <- 85
porcentaje_pacientes_avanzada <- 10.48
pacientes_elegibles <- 87.5
recurrencia_nephrectomy_1 <- 12.9
recurrencia_nephrectomy_2 <- 8.1
recurrencia_nephrectomy_3 <- 3.0


year2 <- year+1
year3 <- year+2

population2 <- population*0.008853579+population
population3 <- population2*0.017116367+population2


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

year3_valor1 <- population2 * tasa_incidencia
year3_valor2 <- year3_valor1 * (porcentaje_pacientes_con_carcinoma / 100)
year3_valor3 <- year3_valor2 * (porcentaje_pacientes_avanzada / 100)
year3_valor4 <- year3_valor3 * (pacientes_elegibles / 100)
year3_valor5 <- ((year3_valor2 - year3_valor3) * (recurrencia_nephrectomy_1 / 100))
year3_valor6 <- ((year2_valor2 - year2_valor3)-((year2_valor2 - year2_valor3) * recurrencia_nephrectomy_1/100)) * recurrencia_nephrectomy_2/100
year3_valor7 <- ((year2_valor2 - year2_valor3)-((year2_valor2 - year2_valor3) * recurrencia_nephrectomy_1/100)-year3_valor6) * recurrencia_nephrectomy_3/100

year_valor1 <- round(year_valor1, 0)
year_valor2 <- round(year_valor2, 0)
year_valor3 <- round(year_valor3, 0)
year_valor4 <- round(year_valor4, 0)
year_valor5 <- round(year_valor5, 0)

year2_valor1 <- round(year2_valor1, 0)
year2_valor2 <- round(year2_valor2, 0)
year2_valor3 <- round(year2_valor3, 0)
year2_valor4 <- round(year2_valor4, 0)
year2_valor5 <- round(year2_valor5, 0)
year2_valor6 <- round(year2_valor6, 0)

year3_valor1 <- round(year3_valor1, 0)
year3_valor2 <- round(year3_valor2, 0)
year3_valor3 <- round(year3_valor3, 0)
year3_valor4 <- round(year3_valor4, 0)
year3_valor5 <- round(year3_valor5, 0)
year3_valor6 <- round(year3_valor6, 0)
year3_valor7 <- round(year3_valor7, 0)


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
    round(population,0), 
    round(tasa_incidencia,6), 
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



# cuota_de_mercado --------------------------------------------------------

treatments <- c("Sunitinib",
                "Pembrolizumab + Axitinib",
                "Pembrolizumab + Lenvatinib",
                "Nivolumab + Cabozantinib",
                "Nivolumab + Ipilimumab",
                "Avelumab + Axitinib")

year <-"2023"
year2<-"2024"
year3 <-"2025"

initial_data <- data.frame(
  Tratamientos = treatments,
  yearasd  = c(100, 0, 0, 2, 0, 6),
  year2asd  = c(100, 0, 0, 0, 0, 0),
  year3asd = c(100, 0, 0, 0, 0, 0)
)

names(initial_data)[2] <- year
names(initial_data)[3] <- year2
names(initial_data)[4] <- year3
write.csv(initial_data, "initialdata_cuotademercado_tablaeditable1.csv", row.names = FALSE, quote = FALSE)

initial_data%>% pivot_longer(cols = c("Tratamientos"), names_to = "Year", values_to = "Percentage")




# Costos ------------------------------------------------------------------


# Install and load necessary package


# Manually input the data from the image
data <- data.frame(
  a = c("Consulta Oncología", "Laboratorio", "TAC torax", "TAC torax, abdomen y pelvis", "RMN de abdomen y pelvis", "Centellograma Oseo", "PET/TC"),
  b = c(4750, 3245, 7512, 20751, 8578, 9235, 31755),
  c = c(1.33, 1.33, 0.25, 0.00, 0.25, 0.33, 0.00),
  d = c(1.33, 1.33, 0.25, 0.00, 0.25, 0.33, 0.00),
  e = c(1.33, 1.33, 0.25, 0.00, 0.25, 0.33, 0.00),
  f = c(1.00, 1.00, 0.25, 0.00, 0.25, 0.33, 0.00),
  g = c(1.00, 1.00, 0.25, 0.00, 0.25, 0.33, 0.00),
  h = c(1.00, 1.00, 0.25, 0.00, 0.25, 0.33, 0.00)
)

colnames(data) = c("Recursos", "Costo unitario","Sunitinib", "Pembrolizumab + Axitinib", "Pembrolizumab + Lenvatinib", "Nivolumab + Cabozantinib", "Nivolumab + Ipilimumab", "Avelumab + Axitinib")


# Save the DataFrame to a CSV file
write.csv(data, "initialdata_costos_tablaeditable2.csv", row.names = FALSE)



data2 <- data.frame(
  a = c("Fatiga", "Hiporexia", "Diarrea", "Hipotiroidismo", "Rash", "Hipertensión Arterial", "Sme. Mano Pie", "Alteración del Hepatograma"),
  b = c(50415.97, 159264.71, 1052017.15, 397279.02, 652793.86, 124361.42, 160930.07, 168792.87),
  c = c(6.60, 0.00, 4.70, 0.20, 0.50, 19.30, 3.80, 3.10),
  d = c(2.80, 0.00, 9.10, 0.20, 0.20, 22.10, 5.10, 13.30),
  e = c(4.30, 4.00, 9.70, 1.40, 3.70, 27.60, 4.00, 0.00),
  f = c(3.00, 0.00, 6.50, 1.00, 2.00, 12.50, 8.00, 10.00),
  g = c(4.00, 0.00, 4.00, 1.00, 1.00, 1.00, 0.00, 10.00),
  h = c(4.00, 0.00, 10.00, 1.00, 0.00, 29.00, 7.00, 7.00)
)

colnames(data2) = c("Eventos adversos", "Costo unitario","Sunitinib", "Pembrolizumab + Axitinib", "Pembrolizumab + Lenvatinib", "Nivolumab + Cabozantinib", "Nivolumab + Ipilimumab", "Avelumab + Axitinib")



write.csv(data2, "initialdata_costos_tablaeditable3.csv", row.names = FALSE)



data3 <- data.frame(
  Tratamiento = c("Sunitinib", "Pembrolizumab + Axitinib", "Pembrolizumab + Lenvatinib", "Nivolumab + Cabozantinib", "Nivolumab + Ipilimumab", "Avelumab + Axitinib"),
  Axitinib = c(25, 0, 20, 8, 18, 34),
  Cabozantinib = c(16, 8, 5, 2, 16, 10),
  Lenvatinib = c(3, 2, 0, 2, 5, 3),
  Pazopanib = c(7, 3, 4, 3, 19, 2),
  Sunitinib = c(0, 7, 8, 7, 24, 3),
  Nivolumab = c(35, 2, 0, 4, 0, 3),
  Everolimus = c(14, 4, 3, 2, 10, 4),
  Cuidados_Paliativos = c(36, 80, 45, 67, 40, 80)
)


tabla_costostratsub <- data3
# Save the DataFrame to a CSV file
write.csv(data3, "initialdata_costos_tablaeditable4.csv", row.names = FALSE, quote = FALSE)




# Definiendo los datos de cada columna
a <- c("Avelumab", "Axitinib", "Cabozantinib", "Ipilimumab", "Lenvatinib", "Nivolumab", "Nivolumab", "Pazopanib", "Pembrolizumab", "Sunitinib", "Everolimus")
b <- c(2056.56, 4120.71, 549.24, 30189.85, 2542.20, 7668.14, 7667.91, 36.22, 13811.01, 779.85, 4680.30)
c <- c(1645250.73, 41207.12, 21969.62, 3018985.18, 50843.96, 3680708.62, 613432.96, 28975.26, 2762201.16, 38992.56, 46803.02)
d <- c(3290501.5, 1236213.7, 659088.5, 1992530.2, 1423630.9, 3680708.6, 815865.8, 869257.9, 3637327.5, 1091791.6, 1404900.7)

# Creando el data frame
tabla_costos <- data.frame(a, b, c, d)

# Asignando nombres simples a las columnas
names(tabla_costos) <- c("a", "b", "c", "d")

# Mostrando el data frame creado
print(df)


#tabla resumen trat subsiguientes

str(tabla_costos)
str(tabla_costostratsub)

tabla_costostratsub <- tabla_costostratsub[,-1]

e = c(sum(c(24.9000,	16.2000,	2.7000,	7.1000,	0.0000,	35.0000,	13.9000,	35.5000)/100 * c( 1236213.693360, 	659088.486748, 1423630.914791, 	869257.908236, 	1091791.570248, 	 3680708.623539, 	 1404090.703904, 	 123000.000000)),
      sum(tabla_costostratsub[2,][c(1:8)]/100 * c(tabla_costos[,4][c(2,3,5,8,10,6,11)],123000)),
      sum(tabla_costostratsub[3,][c(1:8)]/100 * c(tabla_costos[,4][c(2,3,5,8,10,6,11)],123000)),
      sum(tabla_costostratsub[4,][c(1:8)]/100 * c(tabla_costos[,4][c(2,3,5,8,10,6,11)],123000)),
      sum(tabla_costostratsub[5,][c(1:8)]/100 * c(tabla_costos[,4][c(2,3,5,8,10,6,11)],123000)),
      sum(tabla_costostratsub[6,][c(1:8)]/100 * c(tabla_costos[,4][c(2,3,5,8,10,6,11)],123000))
      )
e





# flujo -------------------------------------------------------------------

#otra manera de sacar lo de los flujos


pobl_por_anio <- tabla_poblacion_obj_por_año()

prob <- initial_data6_react()

prob <- prob[,c(1,2)]

costos_res <- resumen_costos_react()
colnames(costos_res) <- c("trat","adqui","segui","event","subsi")

costos_sunitinib <- costos_res[1,]

#año 1

pobl_anio_uno <- pobl_por_anio[[1,1]]

flujo_anio_uno <-  funcion_flujo_pacientes_por_periodo(prob, 1000)


#año 2

pobl_anio_dos <- pobl_por_anio[[1,2]]

flujo_anio_dos <-  funcion_flujo_pacientes_por_periodo(prob, 1000)

#año 3

pobl_anio_tres <- pobl_por_anio[[1,3]]

flujo_anio_tres <-  funcion_flujo_pacientes_por_periodo(prob, 1000)


df_mil <- data.frame(
  
  anio_1 = flujo_anio_uno[,2],
  
  anio_2 = c(0,0,0,0,0,0,0,0,0,0,0,0,flujo_anio_dos[c(1:24),2]),
  
  anio_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,flujo_anio_dos[c(1:12),2])
)


df_miluno <- data.frame(
  
  anio_1 = flujo_anio_uno[,3],
  
  anio_2 = c(0,0,0,0,0,0,0,0,0,0,0,0,flujo_anio_dos[c(1:24),3]),
  
  anio_3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,flujo_anio_dos[c(1:12),3])
)



df_mil$total = df_mil$anio_1 + df_mil$anio_2 + df_mil$anio_3

df_mil$total_event = df_miluno$anio_1 + df_miluno$anio_2 + df_miluno$anio_3


df_mil$costos_adqui = costos_sunitinib[[1,2]]*df_mil$total
df_mil$costos_segui = costos_sunitinib[[1,3]]*df_mil$total
df_mil$costos_event = costos_sunitinib[[1,4]]*df_mil$total
df_mil$costos_subsi = costos_sunitinib[[1,5]]*df_mil$total_event
