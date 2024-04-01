# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("D:\\MAESTRIA AUSTRAL DATA SCIENCE\\MATERIAS\\2doSem\\Laboratorio 1\\datasets") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 700, # minima cantidad de registros para que se haga el split
        minbucket = 400, # tamaño minimo de una hoja
        maxdepth = 11
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
#fwrite(dapply[, list(numero_de_cliente, Predicted)],
     #   file = "./exp/KA2001/K101_001.csv",
     #   sep = ","
#)

ruta_carpeta <- "D:/exp_KA2001"  # Cambia el nombre de la carpeta si es necesario
# Crear la carpeta si no existe. 'recursive = TRUE' crea directorios anidados de ser necesario.
if(!dir.exists(ruta_carpeta)) {
  dir.create(ruta_carpeta, recursive = TRUE)
}
# Especificar el archivo de salida dentro de la nueva carpeta
archivo_salida <- file.path(ruta_carpeta, "K141_006.csv")
# Guardar los datos especificados en el archivo de salida
# Asegúrate de que 'dapply' sea tu data.table o dataframe y que contiene las columnas 'numero_de_cliente' y 'Predicted'
fwrite(dapply[, .(numero_de_cliente, Predicted)], file = archivo_salida, sep = ",")


