# para correr el Google Cloud

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA4540"
PARAM$input$dataset <- "D:/dataset_pequeno.csv"
PARAM$input$training <- c(202107) # meses donde se entrena el modelo
PARAM$input$future <- c(202109) # meses donde se aplica el modelo
PARAM$finalmodel$num_iterations <- 559
PARAM$finalmodel$learning_rate <- 0.0100746999
PARAM$finalmodel$feature_fraction <- 0.5144127527
PARAM$finalmodel$min_data_in_leaf <- 505
PARAM$finalmodel$num_leaves <- 44
PARAM$finalmodel$max_bin <- 31

#------------------------------------------------------------------------------
# Aqui empieza el programa
#setwd("~/buckets/b1")  # revisar luego

# cargo MI semilla, que esta en MI bucket
tabla_semillas <- fread( "D://mis_semillas.txt" )
ksemilla_azar <- tabla_semillas[ 1, semilla ]  #fila 1, columna semilla (el archivo tiene head 'semilla').

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE) 
# 329689, 155 variables (incluye clase_ternaria, con sus tres etiquetas baja+1, baja+2, continua y 0).

dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
# 329689, 156 variables. Se agrega columna 'clase01'con categorias (baja+2,baja+1) etiq 1L y ('continua','0') etiq 0L.

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))
# en campos_buenos esta el nombre de las columnas menos las columnas 'clase_ternaria' y 'clase01'(todas las columnas
# menos las etiquetas original y la creada)

dataset[, train := 0L] 
# 329689, 157 variables. El dataset anterior, mas variable 'train' con categoria '0' para la totalidad de los registros.

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dataset[foto_mes %in% PARAM$input$training, train := 1L]
# 329689, 157 variables. El dataset anterior filtra '202107'y le asigna a train la etiqueta 1.
# entonces los registros que quedan son todos los foto_mes 202107, y tienen 'train'= 1 para luego manipular.
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
# dir.create("./exp/", showWarnings = FALSE)
# dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
# setwd(paste0("./exp/", PARAM$experimento, "/"))

# dejo los datos en el formato que necesita LightGBM

dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)
# con CAMPOS_BUENOS se quitan clase_ternaria, clase01 , y train.
# prepara datos para correr LightGBM.   dtrain <- (data.matrix(), label)
# se entrena con dataset, train=1 (foto mes entrenam),campos_buenos (sin clase_ternaria, clase01 y train).
# label es datos mes de entrenamiento, y se predice en clase01.


# genero el modelo
# estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana!!!!!!!!!!!

modelo <- lgb.train(
  data = dtrain,
  param = list(
    objective = "binary",
    max_bin = PARAM$finalmodel$max_bin,
    learning_rate = PARAM$finalmodel$learning_rate,
    num_iterations = PARAM$finalmodel$num_iterations,
    num_leaves = PARAM$finalmodel$num_leaves,
    min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
    feature_fraction = PARAM$finalmodel$feature_fraction,
    seed = ksemilla_azar
  )
)

# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

#fwrite(tb_importancia,
 # file = archivo_importancia,
 # sep = "\t"
#)

# Convertir la importancia de las variables a data.table
tb_importancia <- as.data.table(lgb.importance(modelo))

# Definir el nombre del archivo de salida con la ruta completa
archivo_importancia <- "D:/impo.txt"

# Exportar la data.table a un archivo de texto en la ubicación especificada
fwrite(tb_importancia,
       file = archivo_importancia,
       sep = "\t"
)


# grabo a disco el modelo en un formato para seres humanos ... ponele ...
lgb.save(modelo, "modelo.txt" ) #guarda el modelo

#--------------------------------------

# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future] # PARAM$input$future <- c(202109)
# dapply filtra del dataset las filas del 212109. 
# 165093, 157 filas. 

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)
# predice con campos_buenos, aplicados a dapply que tiene datos del futuro.
# da las probabilidades de que el registro tenga etiqueta 1, que sea baja+1, o baja+2.
prediccion


# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

#tb_entrega
#numero_de_cliente foto_mes         prob
#<int>    <int>        <num>
#  1:          29183733   202109 0.0003797548
#2:          29184468   202109 0.0006397546




# grabo las probabilidad del modelo

fwrite(tb_entrega,
  file = "prediccion.txt",
  sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)
tb_entrega

# genero archivos con los  "envios" mejores
cortes <- seq(8000, 12000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]

  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
