# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")


# defino los parametros 
PARAM <- list()

PARAM$input$dataset <- "./competencia_03_FEH_NA.csv.gz"

setwd("~/buckets/b1/datasets")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

#---------------------------------------------------
# Obtener las columnas que contienen "min_5"
min_5 <- grep("min_5", names(dataset), value = TRUE)

# Imprimir las columnas que serán eliminadas
print(min_5)

ncol(dataset)

# Eliminar las columnas encontradas
dataset[, (min_5) := NULL]

ncol(dataset)

#-----------------------------------------------------
# Obtener las columnas que contienen "max_5"
max_5 <- grep("max_5", names(dataset), value = TRUE)

# Imprimir las columnas que serán eliminadas
print(max_5)

# Eliminar las columnas encontradas
dataset[, (max_5) := NULL]

ncol(dataset)


# Grabo el dataset
fwrite(dataset,
       file = "competencia_03_FEH_NA_noMIN_noMAX.csv.gz",
       sep = ","
)
