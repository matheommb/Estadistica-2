install.packages("MPV")
library("tidyverse")
datos <- MPV::table.b7
str(datos)
#Vamos a quitar x4
datos <- datos[, -4]
str(datos)
#a
#calculando la matriz de varcov
var(datos)
cov(datos) #forma alternativa de calcular la matriz de varcov
cov(datos$y, datos$x5) #covarianza entre pares de variables

#b
#calculando la matriz de correlaciones
cor(datos)
cor(datos$y, datos$x5) #correlación entre pares de variables
#d
#excluyendo la covariable

X <- cbind(intercept=1, datos[, -5]) %>% 
  as.matrix()
y <- as.matrix(datos[, 5])

#e
#Computando las matrices solicitadas
mat1 <- t(X) %*% X
mat2 <- solve(mat1) #calculando la inversa
mat3 <- mat2 %*% (t(X) %*% y) #betas estimados
mat4 <- X %*% mat3 #ygorro
mat5 <- y - mat4 #residuales

#viendo las ultimas tres matrices
mat3 #coeficientes estimados de la regresion
mat4 #las predicciones (valor medio)
mat5 #residuales

#f
#Lo de arriba no se hace
mod <- lm(y ~ x1 + x2 + x3 + x5, data = datos)
#cuando uno ajusta en funcion de todas las covariables, puede escribir y ~ .
#mod <- lm(y~., data=datos)
coef(mod)
matrix(fitted(mod)) - mat4
matrix(residuals(mod)) - mat5
#Supuesto de media cero
sum(mat5)
sum(residuals(mod))
