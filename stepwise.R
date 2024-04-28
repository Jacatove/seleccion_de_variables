################################################################################
###### Algoritmo de Regresión Paso a paso  #####################################
################################################################################
# 1. Se inicializan variables.
  # regresores_modelo: lista vacía para agregar los regresores al modelo.
  # regresores_disponibles: lista con los regresores candidatos para el modelo.
  # alpha_in: valor corte de entrada.
  # alpha_out: valor corte de salida.
  # Y: respuesta.

# 2. Se halla la mayor correlación simple con la respuesta Y.
# 2.1. Se compara la correlación entre todos los regresores en
  #    regresores_disponibles con Y para determinar el regresor (Xk) con mayor
  #    correlación con Y.
# 2.2 Se verifica si el regresor (Xk) es significante para el nivel alpha_in
  #   para agregarlo al modelo, de lo contrario se termina el algoritmo.

# 3. Se hallan la mayores correlaciónes parciales.
# 3.1. Se determina el regresor (Xj) con el mayor t-estadístico dados los
#      regresores ya en el modelo.
# 3.2 Se verifica si este regresor (Xj) es significante para el nivel alpha_in
#   para agregarlo al modelo.

# 4. Se reevalúa el modelo con la nueva variable agregada. Se determina el
#    regresor con el menor t-estadístico y si dejó de ser significante par el
#    nivel alpha_out, se remueve.

# Instalamos el Paquete BAS, en el cual encontramos el dataset a usar
# (Hald Cement Data) para explicar el algoritmo.
install.packages('BAS')
library(BAS) # Usamos la librería.
# Cargamos y adjuntamos el dataset.
data(Hald)

# 1] Se inicializan variables.
# Definimos las cotas de entrada y salida.
alpha_in <- 0.15
alpha_out <- 0.15
regresores_modelo <- list() # El modelo inicia sin regresores.
# Extraemos las variables regresoras de los nombres de las columnas, y removemos
# la respuesta Y. Como names(Hald) retorna: [1] "X1" "X2" "X3" "X4" "Y", se
# remueve el último elemento de la lista para sacar Y.
regresores_disponibles <- names(Hald)[-length(names(Hald))]
Y <- Hald$Y # Respuesta.

# 2] Se halla la mayor correlación simple con la respuesta Y.
regresor_candidato <- NULL # X_k con mejor correlación con la respuesta Y.
mejor_correlacion <- 0

for (regresor_disponible in regresores_disponibles) {
  X_i <- Hald[, regresor_disponible] # Vector con los valores del regresor Xi.
  cor_y_xi <- cor(Y, X_i) # Correlación entre Xi y la respuesta Y.
  if (abs(cor_y_xi) > abs(mejor_correlacion)){
    mejor_correlacion <- cor_y_xi
    regresor_candidato <- regresor_disponible
  }
}

formula_string <- paste("Y ~", sprintf("Hald$%s", regresor_candidato))

# Regresión lineal del regresor con mejor correlación con la respuesta Y.
modelo <- lm(as.formula(formula_string))

n <- nrow(Hald) # Número de observaciones.
df <- n - 2 # grados de libertad, n observaciones menos el parametro B0 y Bk.
abs_t_valor <- abs(summary(modelo)$coefficients[, "t value"][2])
T_in <- qt(1-alpha_in/2, df) # El valor crítico, la cota de entrada.

# Aunque el regresor tenga la mejor correlación con la respuesta, solo se agrega
# si satisface el criterio estipulado con la cota de entrada.
if (abs_t_valor > T_in) {
  # Se agrega el regresor al modelo en regresores_modelo.
  regresores_modelo[[length(regresores_modelo)+1]] <- regresor_candidato
  print(sprintf(
    'Se ha agregado el regresor %s al modelo pues |to| > T_in',
    regresor_candidato))
  print(sprintf('|to| = %f', abs_t_valor))
  print(sprintf('T_in = %f', T_in))
  # Se elimina el regresor de los regresores disponibles.
  regresores_disponibles <- regresores_disponibles[
    regresores_disponibles != regresor_candidato]
} else {
  print("Ningún regresor satisface el criterio estipulado.")
}

# 3] Se hallan la mayores correlaciónes parciales.
crear_formula <- function(regresores_modelo) {
  # Crea una cadena de texto para una fórmula de modelado lineal
  # Argumentos:
  #   regresores_modelo: Un vector de caracteres con los nombres de los
  #   regresores
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
  mayor_t_stadistico <- 0
  regresor_candidato <- NULL
  regresores_a_reevaluar <- NULL
  formula_del_modelo <- crear_formula(regresores_modelo)
  modelo <- NULL

  for (regresor in regresores_disponibles) {
    # En la primera iteración se evalua "Y ~ Hald$Xi + Hald$Xj" con i fijo y
    # j variable.
    formula_string_regresor <- paste(
      formula_del_modelo, sprintf("+ Hald$%s", regresor))
    modelo_regresor <- lm(as.formula(formula_string_regresor))
    # Extraemos el t-estadístico del regresor candidato.
    t_estadistico <- summary(
      modelo_regresor)$coefficients[, "t value"][sprintf('Hald$%s', regresor)]
    
    if (abs(t_estadistico) > abs(mayor_t_stadistico)){
      mayor_t_stadistico <- t_estadistico
      regresor_candidato <- regresor
      modelo <- modelo_regresor
    }

  }
  
  # Se actualizan los grados de libertad pues se va a agregar un nuevo
  # regresor al modelo.
  df <- df - 1
  T_in <- qt(1-alpha_in/2, df) # Cota de entrada.
  regresores_a_reevaluar <- regresores_modelo # Antes de que se agregue el nuevo

  if (abs(mayor_t_stadistico) > T_in) {
    # Se agrega el regresor al modelo.
    dim <- length(regresores_modelo)
    regresores_modelo[[dim+1]] <- regresor_candidato
    print(sprintf(
      'Se ha agregado el regresor %s al modelo pues |to| > T_in',
      regresor_candidato))
    print(sprintf('|to| = %f', abs(mayor_t_stadistico)))
    print(sprintf('T_in = %f', T_in))
    # Se elimina el regresor de los regresores disponibles.
    regresores_disponibles <- regresores_disponibles[
      regresores_disponibles != regresor_candidato]
  } else {
    print('Ninguno de los regresores disponibles supera el valor crítico.')
    print(regresores_disponibles)
    print(sprintf('T_in = %f', T_in))
    break
  }
  
  # 4]
  # Una vez agregado el nuevo regresor, se reevalúan los regresores via el
  # t-estadístico para remover el regresor con el menor t-estadístico siempre y
  # cuando dicho t-estadístico sea menor que la cota de salida (t_OUT).
  menor_t_stadistico <- Inf
  regresor_a_remover <- NULL
  for (regresor in regresores_a_reevaluar){
    t_estadistico_regresor <- summary(
      modelo)$coefficients[, "t value"][sprintf('Hald$%s', regresor)]

    if (abs(t_estadistico_regresor) < menor_t_stadistico){
      menor_t_stadistico <- t_estadistico_regresor
      regresor_a_remover <- regresor
    }
  }

  T_out <- qt(1-alpha_out/2, df) # Valor crítico, Cota de salida
  if (abs(menor_t_stadistico) < T_out){
    # Se elimina el regresor de los regresores del modelo
    print(sprintf(
      'Se ha eliminado el regresor %s del modelo pues |to| < T_out',
      regresor_a_remover))
    print(sprintf('|to| = %f', abs(menor_t_stadistico)))
    print(sprintf('T_out = %f', T_out))
    regresores_modelo <- regresores_modelo[
      regresores_modelo != regresor_a_remover]
    # Se agrega a los regresores disponibles.
    dim <- length(regresores_disponibles)
    regresores_disponibles[[dim+1]] <- regresor_a_remover
    df <- df + 1 # Se obtiene un grado de libertad al prescindir de un regresor.
  }
}

print(paste("La ecuación final es:", crear_formula((regresores_modelo))))