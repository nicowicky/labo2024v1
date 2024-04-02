# Preparación del entorno
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection
require("data.table")
require("rpart")
require("parallel")
# Configuración inicial
PARAM <- list()
PARAM$semillas <- c(100297, 422573, 492641, 301039, 723721)
# Función para particionar el dataset estratificadamente
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa]
}

# Función para estimar la ganancia del modelo
ArbolEstimarGanancia <- function(semilla, param_basicos) {
  particionar(dataset, division = c(7, 3), agrupa = "clase_binaria", seed = semilla)
  
  modelo <- rpart("clase_binaria ~ .",
                  data = dataset[fold == 1], # 70% de los datos para entrenamiento
                  xval = 0,
                  control = param_basicos)
  
  prediccion <- predict(modelo, dataset[fold == 2], type = "prob")
  
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "baja+2"] > 0.025,
               ifelse(clase_binaria == "baja+2", 117000, -3000),
               0))
  ]
  
  return(ganancia_test / 0.3) # Ganancia normalizada
}

# Función para realizar Monte Carlo con diferentes semillas
ArbolesMontecarlo <- function(semillas, param_basicos) {
  ganancias <- mcmapply(ArbolEstimarGanancia,
                        semillas,
                        MoreArgs = list(param_basicos),
                        SIMPLIFY = FALSE,
                        mc.cores = 1)
  
  return(mean(unlist(ganancias)))
}

# Carga y preparación del dataset
setwd("D:\\MAESTRIA AUSTRAL DATA SCIENCE\\MATERIAS\\2doSem\\Laboratorio 1\\datasets") 
dataset <- fread("dataset_pequeno.csv")

dataset[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "baja+2", "no_baja+2")]
dataset[, clase_ternaria := NULL] # Eliminamos la columna original

# Configuración del grid search
tb_grid_search <- data.table(max_depth = integer(),
                             min_split = integer(),
                             cp = numeric(),
                             minbucket = integer(),
                             ganancia_promedio = numeric())

contador = 0
for (vmax_depth in c(9)) {
  for (vmin_split in c( 1200,1300,1400,1500,1600,1700,1800,1900,2000)) {
    for (vcp in c(-0.8,-0.5,-0.3)) {
      for (vminbucket in c(600,700,800,900, 1000)) {
        contador = contador + 1
        print(contador)
        param_basicos <- list(cp = vcp,
                              minsplit = vmin_split,
                              minbucket = vminbucket,
                              maxdepth = vmax_depth)
        
        ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
        
        tb_grid_search <- rbind(tb_grid_search, 
                                list(max_depth = vmax_depth, min_split = vmin_split, cp = vcp, minbucket = vminbucket, ganancia_promedio = ganancia_promedio))
      }
    }
  }
}

# Guardado de resultados
fwrite(tb_grid_search, file = "D:/resultado_grid_search1.csv", sep = ",", col.names = TRUE)
