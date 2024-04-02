
# Carga de librerías necesarias
require("data.table")
require("rpart")
require("rpart.plot")

# Establecer el directorio de trabajo
setwd("D:\\MAESTRIA AUSTRAL DATA SCIENCE\\MATERIAS\\2doSem\\Laboratorio 1\\datasets") 

# Carga del dataset
dataset <- fread("dataset_pequeno.csv")

# Creación de 'clase_binaria' y eliminación de 'clase_ternaria'
dataset[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "baja+2", "no_baja+2")]
dataset[, clase_ternaria := NULL]

# División del dataset para entrenamiento y aplicación
dtrain <- dataset[foto_mes == 202107]  # Conjunto de entrenamiento
dapply <- dataset[foto_mes == 202109]  # Conjunto para aplicar el modelo

# Generación del modelo con la columna 'clase_binaria'
modelo <- rpart(
  formula = "clase_binaria ~ .",
  data = dtrain,
  xval = 0,
  cp = -0.8, # Control de la complejidad del árbol
  minsplit = 1200, # Mínima cantidad de registros para realizar un split
  minbucket = 1000, # Tamaño mínimo de una hoja
  maxdepth = 8 # Profundidad máxima del árbol
)

# Gráfica del árbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

# Aplicación del modelo al conjunto de datos de aplicación
prediccion <- predict(modelo, newdata = dapply, type = "prob")

# Agregar a 'dapply' una columna con la probabilidad de ser 'baja+2'
dapply[, prob_baja2 := prediccion[, "baja+2"]]

# Decisión de a quién enviar estímulo basada en la probabilidad de 'baja+2'
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# Creación de directorios para guardar los resultados
dir.create("D:/exp_KA2001", recursive = TRUE)

# Especificar el archivo de salida dentro de la nueva carpeta
archivo_salida <- file.path("D:/exp_KA2001", "K141bi_006.csv")

# Guardar los datos en el archivo de salida
fwrite(dapply[, .(numero_de_cliente, Predicted)], file = archivo_salida, sep = ",")
