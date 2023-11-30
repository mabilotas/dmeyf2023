# # Limpio la memoria
rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection

require("data.table")
require("lightgbm")
require(ggplot2)

# #-----------------------------------CONFIGURAR PARÁMETROS-------------------------------------------#
PARAM <- list()

# Nombre del experimento
PARAM$experimento <- "ES_18" #Ensemble Semillas

# Path donde se alojan las predicciones
PARAM$input$ensemble <- "./exp/KA8240_18/ensemble.csv"
PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

setwd("~/buckets/b1")

#-----------------------------------CARGO ARCHIVO CON PREDICCIONES----------------------------------#
# Cargo el archivo
predicciones <- fread(PARAM$input$ensemble)
datos <- fread(PARAM$input$dataset)
datos_reales <- datos[datos$foto_mes == 202107, c("numero_de_cliente", "foto_mes", "clase_ternaria")]

# Tranformo la clase ternaria en binaria
datos_reales[, real := ifelse(clase_ternaria %in% c("BAJA+2"), 1L, 0L)]

rm(datos)


#---------------------------------CREAR DIRECTORIOS---------------------------------------------#
# Creo carpeta donde guardar los experimentos en caso de que no exista
dir.create("./exp/", showWarnings = FALSE)

# Creo carpeta donde guardar este experimento en caso de que no exista
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory de este experimento
setwd(paste0("./exp/", PARAM$experimento, "/"))

resultados_ganancia <- data.frame(semilla = character(), ganancia = numeric())



#----------------------------------------------------------------------------------------------------------#
#-------------------------------------CALCULO GANANCIAS SEMILLAS y SEMILLERIO-----------------------------------------#


#GANANCIAS SEMILLAS---------------------------------------------

# Defino dataset con columna de cantidad de envios/ estimulo
resultados_ganancia <- data.frame(envios = seq(5000, 20000, by = 500))

# Crear un bucle para calcular la ganancia para cada ensemble
for (i in 1:30) {
  
  total <- i
  
  # Selecciono la semilla
  col_proba <- paste0("prob_", i)
  ensemble <- predicciones[, .(numero_de_cliente, col_proba = get(col_proba))][order(-col_proba)]
  
  # Crear una columna para almacenar la ganancia para el ensemble actual
  resultados_ganancia[, paste0("ganancia_", i)] <- NA
  
  # Calcular la ganancia para diferentes cantidades de envíos
  cortes <- seq(5000, 20000, by = 500)
  for (envios in cortes) {
    ensemble$prediccion <- 0   
    ensemble$prediccion[1:envios] <- 1
    
    # Hago un join entre el dataset de predichos con los reales
    df <- merge(ensemble, datos_reales, by = "numero_de_cliente")
    
    # Ganancia individual
    df$ganancia_individual  = ifelse(df$prediccion == 1 & df$real == 1, 273000,
                                     ifelse(df$prediccion == 1 & df$real == 0, -7000, 0))
    # Sumar todas las ganancias individuales para obtener la ganancia total
    ganancia_total <- sum(df$ganancia_individual)
    
    resultados_ganancia[resultados_ganancia$envios == envios, paste0("ganancia_", i)] <- ganancia_total
  }
}

#GANANCIAS ENSEMBLE---------------------------------------------------------
# Selecciono el ensemble 
total <- i+1

ensemble <- predicciones[, .(numero_de_cliente, promedio = get("promedio"))][order(-promedio)]

# Crear una columna para almacenar la ganancia para el ensemble actual
resultados_ganancia[, paste0("ganancia_", total)] <- NA

# Calcular la ganancia para diferentes cantidades de envíos
cortes <- seq(5000, 20000, by = 500)
for (envios in cortes) {
  ensemble$prediccion <- 0   
  ensemble$prediccion[1:envios] <- 1
  
  # Hago un join entre el dataset de predichos con los reales
  df <- merge(ensemble, datos_reales, by = "numero_de_cliente")
  
  # Ganancia individual
  df$ganancia_individual  = ifelse(df$prediccion == 1 & df$real == 1, 273000,
                                   ifelse(df$prediccion == 1 & df$real == 0, -7000, 0))
  # Sumar todas las ganancias individuales para obtener la ganancia total
  ganancia_total <- sum(df$ganancia_individual)
  
  resultados_ganancia[resultados_ganancia$envios == envios, paste0("ganancia_", total)] <- ganancia_total
}

# guardo archivo
archivo_salida <- paste0(PARAM$experimento, "resultados_ganancia.csv")
fwrite(resultados_ganancia, file = archivo_salida, sep = ",")

cat("\n\nGanancias semillerio ha terminado\n")


