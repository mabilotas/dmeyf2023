# # Limpio la memoria
rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection

require("data.table")
require("lightgbm")
require(ggplot2)

# #-----------------------------------CONFIGURAR PARÁMETROS-------------------------------------------#
PARAM <- list()

# Nombre del experimento
PARAM$experimento <- "ES_01" #Ensemble Semillas

# Path donde se alojan las predicciones
PARAM$input$ensemble <- "./exp/KA8240_colaborativo/ensemble.csv"
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


#---------------------------------------------------------------------------------------------------------#
#----------------------------------CALCULO GANANCIAS BASELINE---------------------------------------------#

# Calculo la ganancia para cada semilla con envíos fijo en 11k
for (i in 1:30) {
  # Selecciono columna de la semilla i
  col_semilla <- paste0("prob_", i)
  # Ordenar por probabilidad descendente
  df_ordenado = predicciones[, .(numero_de_cliente, semilla_valor = get(col_semilla))][order(-semilla_valor)]
  
  
  # Binarizar la probabilidad
  df_ordenado$prediccion <- 0   
  df_ordenado$prediccion[1:12000] <- 1
  
  # Hacer un join con los datos reales
  df_resultado <- merge(df_ordenado, datos_reales, by = "numero_de_cliente")
  
  # Calcular la ganancia
  df_resultado$ganancia_individual  <- ifelse(df_resultado$prediccion == 1 & df_resultado$real == 1, 273000,
                                              ifelse(df_resultado$prediccion == 1 & df_resultado$real == 0, -7000, 0))
  
  ganancia <- sum(df_resultado$ganancia_individual)
  # Almacenar el resultado en el data frame de resultados
  resultados_ganancia <- rbind(resultados_ganancia, data.frame(semilla = col_semilla, ganancia = ganancia))
}

archivo_salida <- paste0(PARAM$experimento, "resultados_ganancia_baseline.csv")
print(archivo_salida)
fwrite(resultados_ganancia, file = archivo_salida, sep = ",")

cat("\n\nGanancias baseline terminado\n")


#----------------------------------------------------------------------------------------------------------#
#-------------------------------------CALCULO GANANCIAS SEMILLERIO-----------------------------------------#

# Defino dataset con columna de cantidad de envios/ estimulo
resultados_ganancia <- data.frame(envios = seq(5000, 20000, by = 500))

# Crear un bucle para calcular la ganancia para cada ensemble
for (i in 2:30) {
  # Selecciono el ensemble de interés
  col_proba <- paste0("promedio_", i)
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

# guardo archivo
archivo_salida <- paste0(PARAM$experimento, "resultados_ganancia.csv")
fwrite(resultados_ganancia, file = archivo_salida, sep = ",")

cat("\n\nGanancias semillerio ha terminado\n")


#------------------------------------------------------------------------------------------------------#
#--------------DISTRIBUCION DE ENSEMBLES DEL SEMILLERIO------------------------------------------------#

# Genero 50 ensembles de 20 semillas, tomo muestras con reposición del conjunto de 20 semillas original

# Configuración de parámetros
num_ensembles <- 50
sample_size <- 20  # Tamaño de la submuestra, ajusta según tus necesidades

# Crear un bucle para calcular los ensembles
for (i in 1:num_ensembles) {
  # Muestreo con reposición de las semillas
  sampled_seeds <- sample(1:30, size = sample_size, replace = TRUE)
  
  # Seleccionar las columnas correspondientes a las semillas muestreadas
  columns_sampled <- c(paste0("prob_", sampled_seeds))
  
  # Calcular el promedio de las predicciones para los 50 modelos
  predicciones[, paste0("proba_ensemble_", i)] <- rowMeans(predicciones[, ..columns_sampled])
  
}

ensemble_resultados <- predicciones[, c("numero_de_cliente", tail(names(predicciones), 99)), with = FALSE]


#----------------------------------CALCULO GANANCIAS------------------------------------------------#

# Defino dataset con columna de cantidad de envíos/estímulo
resultados_ganancia <- data.frame(ensemble = seq(1, 50, by = 1), ganancia = numeric(50))

# Crear un bucle para calcular la ganancia para cada ensemble
for (i in 1:50) {
  # Selecciono el ensemble de interés
  col_proba <- paste0("proba_ensemble_", i)
  ensemble <- predicciones[, .(numero_de_cliente, col_proba = get(col_proba))][order(-col_proba)]
  
  ensemble$prediccion <- 0   
  ensemble$prediccion[1:12000] <- 1
  
  # Hago un join entre el dataset de predichos con los reales
  df <- merge(ensemble, datos_reales, by = "numero_de_cliente")
  
  # Ganancia individual
  df$ganancia_individual <- ifelse(df$prediccion == 1 & df$real == 1, 273000,
                                   ifelse(df$prediccion == 1 & df$real == 0, -7000, 0))
  # Sumar todas las ganancias individuales para obtener la ganancia total
  ganancia_total <- sum(df$ganancia_individual)
  
  # Almaceno la ganancia en el dataframe resultados_ganancia
  resultados_ganancia[i, "ganancia"] <- ganancia_total
}

#guardo en un archivo
archivo_salida <- paste0(PARAM$experimento, "resultados_ganancia_ensembles.csv")
fwrite(resultados_ganancia, file = archivo_salida, sep = ",")

cat("\n\nEl programa ha terminado\n")