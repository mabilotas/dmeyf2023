# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA8240_Kaggle_18"

PARAM$input$dataset <- "./datasets/competencia_03_NA_noMIN_noMAX.csv.gz"

# meses donde se entrena el modelo.
# roll forward un mes
PARAM$input$training <- c(201906, 201907, 201908, 201909, 201910, 201911, 201912, 202001, 202002,
                          202011, 202012, 202101, 202102, 202103, 202104, 202105, 202106, 202107)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$undersampling <- 0.1
PARAM$trainingstrategy$semilla_azar <- 102191 # Aqui poner su  primer  semilla

semillas <- c(106853, 191071, 337511, 400067, 991751, 
              100019, 200183, 300193, 400213, 500267,
              600291, 700297, 800313, 900343, 100361,
              200381, 300397, 400411, 500417, 600437,
              106867, 191083, 337529, 400087, 991777,
              100043, 200197, 300221, 400231, 500287)




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))
#--------------------------------------
#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

#--------------------------------------

#-----HAGO EL UNDERSAMPLING----------------

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
set.seed(PARAM$trainingstrategy$semilla_azar)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]


# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, 
                   ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
                          ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)


# genero el modelo

#Pongo el numero de envios
envios<-12000 #ajustar según lo calculado en 202107

#creo la tabla de entrega
tb_semillas <- dataset[dataset$foto_mes == PARAM$input$future, c("numero_de_cliente", "foto_mes")]

for (i in 1:30) {
  
  PARAM$finalmodel$semilla <- semillas[i]
  
  # hiperparametros intencionalmente NO optimos
  PARAM$finalmodel$optim$num_iterations <- 20
  PARAM$finalmodel$optim$learning_rate <- 1
  PARAM$finalmodel$optim$feature_fraction <- 0.4
  PARAM$finalmodel$optim$num_leaves <- 40
  PARAM$finalmodel$optim$min_data_in_leaf <- 5000
  
  
  
  # Hiperparametros FIJOS de  lightgbm
  PARAM$finalmodel$lgb_basicos <- list(
    boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE, # para reducir warnings
    verbosity = -100,
    max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
    min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
    lambda_l1 = 0.0, # lambda_l1 >= 0.0
    lambda_l2 = 0.0, # lambda_l2 >= 0.0
    max_bin = 31L, # lo debo dejar fijo, no participa de la BO
    
    bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
    pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
    neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
    is_unbalance = FALSE, #
    scale_pos_weight = 1.0, # scale_pos_weight > 0.0
    
    drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
    max_drop = 50, # <=0 means no limit
    skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
    
    extra_trees = FALSE, # Magic Sauce
    
    seed = PARAM$finalmodel$semilla
  )
  
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
                      PARAM$finalmodel$optim)
  
  
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )
  
  #--------------------------------------
  
  
  # aplico el modelo a los datos sin clase
  dapply <- dataset[foto_mes == PARAM$input$future]
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # agrego las predicciones de cada semilla
  
  tb_semillas[[paste0("prob_", i)]] <- prediccion
  
  # grabo las probabilidad del modelo
  fwrite(tb_semillas,
         file = "probabilidades.csv",
         sep = ","
  )
  
  print(paste0("Iteracion ",i, " finalizada"))
  
}


cat("\n\nPrediccion finalizada\n")



#------------------ENSEMBLE DE SEMILLAS--------------------------------------------------------------------


# Selecciona las columnas de probabilidad
columnas_prob <- grep("^prob_", names(tb_semillas), value = TRUE)

tb_semillas <- copy(tb_semillas)  # Hacer una copia superficial
tb_semillas[, Predicted := rowMeans(.SD, na.rm = TRUE), .SDcols = columnas_prob]

# ordeno por probabilidad descendente
setorder(tb_semillas, -Predicted)

cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  tb_semillas[, Predicted := 0L]
  tb_semillas[1:envios, Predicted := 1L]
  
  fwrite(tb_semillas[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ","
  )
}



cat("\n\nEl programa ha terminado\n")


