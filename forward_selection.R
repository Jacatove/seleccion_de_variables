# Instalamos el Paquete BAS, en el cual encontramos el dataset a usar, Hald Cement Data.
install.packages('BAS')
library(BAS) # Usamos la librería.

# Cargamos y adjuntamos el dataset.
data(Hald)

# Miramos la estructura y el resumen del dataset.
str(Hald)

#################################################
###### Algoritmo de Regresión Paso a paso  ######
#################################################
# 0. Se inicializan variables.
# regresores_modelo: lista vacía para agregar los regresores al modelo.
# regresores_disponibles: lista con los regresores candidatos para el modelo.
# alpha: valor corte para determinar el valor crítico del t-estadístico.
# Y: respuesta.

# 1. Se halla la mayor correlación simple entre un regresor X_j y la respuesta Y
#  1.1. Se comparan la correlación entre todos los regresores en regresores_disponibles
#.      contra la respuesta Y.
# 2. Se hallan la mayores correlaciónes parciales
## 2.1. Se determina la mejor correlación parcial con un nuevo,
#.      dados los regresores del modelo, se reevalúan via el t-estadístico
# 3. 

# Definimos el valor Cutoff, (Alpha) para comprobar que el t-estadístico supera
# el valor preseleccionado T_IN (T-to-enter).
alpha <- 0.25
regresores_modelo <- list() # El modelo inicia sin regresores.
# names(Hald)  X1, X2, ..., Xn, Y.
# Extraemos las variables regresoras de los nombres de las columnas, y removemos
# la respuesta Y. Como names(Hald) retorna: [1] "X1" "X2" "X3" "X4" "Y", se
# remueve el último elemento de la lista.
regresores_disponibles <- names(Hald)[-length(names(Hald))]
Y <- Hald$Y # Observaciones de la Respuesta.

# 1) Mayor correlación simple entre los regresores y la respuesta.
mejor_regresor <- NULL # El regresor X_i con mejor correlación contra la respuesta Y.
mejor_correlacion <- 0

# Iteramos sobre las observaciones de las variables regresores.
for (regresor_disponible in regresores_disponibles) {
  X_i <- Hald[, regresor_disponible] # Valores del regresor Xi.
  cor_y_xi <- cor(Y, X_i) # Covarianza entre Xi y la respuesta Y.
  if (abs(cor_y_xi) > abs(mejor_correlacion)){
    mejor_correlacion <- cor_y_xi
    mejor_regresor <- regresor_disponible
  }
}

formula_string <- paste("Y ~", sprintf("Hald$%s", mejor_regresor))

# Regresión lineal.
model <- lm(as.formula(formula_string))

n <- nrow(Hald) # Número de observaciones.
df <- n - 2 # grados de libertad, n observaciones menos dos parámetros. 
valor_critico <- qt(1-alpha/2, df) # Valor Crítico
t_valor <- abs(summary(model)$coefficients[, "t value"][2])

# Aunque el regresor tenga la mejor correlación con la respuesta, solo se agrega
# al modelo si existe relación lineal entre el regresor y la respuesta.
if (t_valor > valor_critico) {
  # Se agrega el regresor al modelo.
  dim <- length(regresores_modelo)
  regresores_modelo[[dim+1]] <- mejor_regresor
  print(sprintf('Se agrega el Regresor %s al modelo.', mejor_regresor))
  # Se elimina el regresor de los regresores disponibles.
  regresores_disponibles <- regresores_disponibles[regresores_disponibles != mejor_regresor]
}


# 2) Correlaciones Parciales.

crear_formula <- function(regresores_modelo) {
  # Crea una cadena de texto para una fórmula de modelado lineal
  # Argumentos:
  #   regresores_modelo: Un vector de caracteres con los nombres de los regresores
  # Retorna:
  #   Una cadena de texto que representa la fórmula del modelo lineal.
  #
  # Ejemplo:
  #   crear_formula(c("X1", "X2")) devuelve "Y ~ Hald$X1 + Hald$X2"
  formula <- "Y ~"
  for (indice in seq_along(regresores_modelo)){
    if (indice == 1){
      formula <- paste(formula,  sprintf("Hald$%s", regresores_modelo[indice]))
    } else {
      formula <- paste(formula,  sprintf("+ Hald$%s", regresores_modelo[indice]))
    }
  }
  return(formula)
}

while (TRUE){
  formula_del_modelo <- crear_formula(regresores_modelo)
  mejor_t_stadistico <- 0
  mejor_regresor <- NULL
  for (regresor in regresores_disponibles) {
    # Evaluamos "Y ~ Hald$Xi + Hald$Xj" con i fijo y j variable. Caso del primer for dentro del while.
    formula_string_regresor <- paste(formula_del_modelo, sprintf("+ Hald$%s", regresor))
    modelin <- lm(as.formula(formula_string_regresor), x = TRUE)
    
    # Calculamos el t-estadístico.
    Beta <- modelin$coefficients
    X <-  modelin$x
    C <- solve(t(X) %*% X)
    SSres <- t(Y) %*% Y - t(Beta) %*% t(X) %*% Y
    num_parametros <- length(regresores_modelo) + 2 # Los regresores en el modelo + B0 y el de la iteración.
    sigma_estimada_cuadrado <- SSres/(n-num_parametros)
    t_estadistico <- Beta[num_parametros]/sqrt(sigma_estimada_cuadrado*C[num_parametros, num_parametros])
    
    if (abs(t_estadistico) > abs(mejor_t_stadistico)){
      mejor_t_stadistico <- t_estadistico
      mejor_regresor <- regresor
    }
    
  }
  
  df <- df - 1 # Se ha agregado un nuevo parámetro al modelo.
  valor_critico <- qt(1-alpha/2, df) # Valor Crítico
  
  if (mejor_t_stadistico > valor_critico) {
    # Se agrega el regresor al modelo.
    dim <- length(regresores_modelo)
    regresores_modelo[[dim+1]] <- mejor_regresor
    print(sprintf('Se agrega el Regresor %s al modelo.', mejor_regresor))
    # Se elimina el regresor de los regresores disponibles.
    regresores_disponibles <- regresores_disponibles[regresores_disponibles != mejor_regresor]
  } else {
    print(sprintf('El t-estadístico del regresor %s, no supera el valor crítico.', mejor_regresor))
    break
  }
}


print(paste("El modelo final es:", crear_formula((regresores_modelo))))