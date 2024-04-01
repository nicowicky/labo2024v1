# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(100297, 422573, 492641, 301039, 723721)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)

  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 1 # en Windows este valor debe ser 1
  )

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
setwd("D:\\MAESTRIA AUSTRAL DATA SCIENCE\\MATERIAS\\2doSem\\Laboratorio 1\\datasets") 
# cargo los datos
dataset <- fread("dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch.txt"

# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table( max_depth = integer(),
                              min_split = integer(),
                              ganancia_promedio = numeric() )


# itero por los loops anidados para cada hiperparametro

library(data.table)

# Asumiendo que tb_grid_search es un data.table donde guardarás los resultados
tb_grid_search <- data.table(max_depth=integer(), min_split=integer(), cp=numeric(), minbucket=integer(), ganancia_promedio=numeric())
contador = 0
for (vmax_depth in c(7,9,11)) {
  for (vmin_split in c(300,500,700,900,1100, 1300)) {
    for (vcp in c(-0.8,  -0.3)) {
      for (vminbucket in c(200,300,400,500)) {
        contador = contador + 1
        print(contador)
        # Configuración de los parámetros básicos, incluyendo los nuevos parámetros
        param_basicos <- list(
          "cp" = vcp,
          "minsplit" = vmin_split,
          "minbucket" = vminbucket,
          "maxdepth" = vmax_depth
        )
        
        # Llamada a la función ArbolesMontecarlo, asumiendo que esta función ya existe y hace lo que necesitas
        ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
        
        # Agregamos los resultados al data.table
        tb_grid_search <- rbindlist(list(tb_grid_search, 
                                         list(vmax_depth, vmin_split, vcp, vminbucket, ganancia_promedio)))
      }
    }
  }
}

# Asumiendo que necesitas imprimir o guardar tb_grid_search después
print(tb_grid_search)

archivo_salida <- "D:/resultado.csv"
# Usar fwrite para guardar tb_grid_search en archivo_salida
fwrite(tb_grid_search, file = archivo_salida, sep = ",", col.names = TRUE)
  # escribo la tabla a disco en cada vuelta del loop mas externo
  Sys.sleep(2)  # espero un par de segundos

}