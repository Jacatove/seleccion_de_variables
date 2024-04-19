# Instalamos el Paquete BAS, en el cual encontramos el dataset a usa, Hald Cement Data.
install.packages('BAS')
library(BAS) # Usamos la librería.

# Cargamos y adjuntamos el dataset.
data(Hald)

# Miramos la estructura y el resumen del dataset.
str(Hald)
summary(Hald)

# 0) Definimos el valor Cutoff, Alpha para comprobar que el F estadístico F
# supere el valor preseleccionado, F_IN (F-to-enter).
alpha <- 0.25

# 1) Largest Simple Correlation with the response variable.
y <- Hald$Y # Observaciones de la Respuesta.
indice_regresor <- 0 # El regresor X_i con mejor correlación contra la respuesta Y.
num_regresores <- ncol(Hald) - 1 # Restamos la respuesta Y.
mejor_correlacion <- 0
# Iteramos sobre las observaciones de los regresores.
for (i in 1:num_regresores) {
  X_i <- Hald[, i] # Valores del regresor Xi.
  cor_y_xi <- cor(y, X_i) # Covarianza entre Xi y la respuesta Y.
  if (abs(cor_y_xi) > abs(mejor_correlacion)){
    mejor_correlacion <- cor_y_xi
    indice_regresor <- i
  }
}

df <- 11 # No sé por qué son 11...
valor_critico <- qt(alpha/2, df) # Valor Crítico
model <- lm(y ~ Hald$X4)

t_valor <- abs(summary(model)$coefficients[, "t value"][2])

if (t_valor > valor_critico) {
  print(sprintf('Se agrega el Regreso X_%d al modelo.', indice_regresor))
}




