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
PARAM$experimento <- "KA8240_colaborativo_BO2_a"

PARAM$input$dataset <- "./datasets/competencia_03_FEH_colaborativos.csv.gz"

# meses donde se entrena el modelo.
# roll forward un mes
PARAM$input$training <- c(202101, 202102, 202103,202104,202105,202106)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo


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

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)


# genero el modelo

#creo la tabla de entrega

tb_entrega <- dataset[dataset$foto_mes == PARAM$input$future, c("numero_de_cliente", "foto_mes")]

for (i in 1:30) {
  
  PARAM$finalmodel$semilla <- semillas[i]
  
# hiperparametros intencionalmente NO optimos
  PARAM$finalmodel$optim$num_iterations <- 110
  PARAM$finalmodel$optim$learning_rate <- 1
  PARAM$finalmodel$optim$feature_fraction <- 0.972885098834825
  PARAM$finalmodel$optim$num_leaves <- 78
  PARAM$finalmodel$optim$min_data_in_leaf <- 2090
    
 
  
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

    extra_trees = TRUE, # Magic Sauce

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
  
  tb_entrega[[paste0("prob_", i)]] <- prediccion
  
  # grabo las probabilidad del modelo
  fwrite(tb_entrega,
    file = "prediccion.csv",
    sep = ","
  )
      
  print(paste0("Iteracion ",i, " finalizada"))
  
}

 #------------------ENSEMBLE DE SEMILLAS--------------------------------------------------------------------

cat("\n\nPrediccion finalizada\n")
  
# Seleccionar las columnas de probabilidad de cada semilla
columnas_prob <- grep("^prob_", names(tb_entrega), value = TRUE)

# Calcular el promedio acumulativo para cada conjunto de semillas incrementando de a 1 hasta llegar al total
for (i in seq_along(columnas_prob)) {
  tb_entrega[, paste0("promedio_", i) := rowMeans(.SD, na.rm = TRUE), .SDcols = columnas_prob[1:i]]
}
#guardar el archivo
fwrite(tb_entrega,
       file = "ensemble.csv",
       sep = ","
) 
cat("\n\nEl programa ha terminado\n")


