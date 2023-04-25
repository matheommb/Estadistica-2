#Ejercicio 2
#Literal a

datos <- read.table("datos.txt", header = T)
str(datos)

#convirtiendo X2 a numerica (no es necesario)
datos$X2 <- as.numeric(datos$X2)
str(datos)

#Ajustando el modelo 
#mod1 <- lm(Y ~ X1 + X2 + X3 + X4, data = datos)
#cuando se ajusta en funcion de todas las covariables es mas comodo
#hacerlo de esta manera
mod1 <- lm(Y ~ ., data = datos)
#viendo el resumen del modelo
summary(mod1)

#el b) se encuentra en las observaciones
#esto es una ilustracion de sumas de cuadrados extra

#suponiendo beta_3 = 0
mod2 <- lm(Y ~ ., data = datos[, -4])
summary(mod2)

#Sumas de cuadrados
anova(mod1)
MSE_mod1 <- 29.28 #(MSE FULL)
SSE_mod1 <- 1434.53 #(SSE FULL)
anova(mod2)
SSE_mod2 <- 1707.18 #(SSE REDUCIDO)

Fo.a <- ((SSE_mod2 - SSE_mod1)/1)/MSE_mod1

#elevando el To al cuadrado
3.052^2 #teoricamente es igual al Fo.a

#c

#cargando lo que hay en un archivo externo
source("funciones.R")
#funciones del curso
myAnova(mod1)

#Anova como sumas de cuadrados extra
mod3 <- lm(Y ~ 1, data = datos) #Modelo reducido para la ANOVA
anova(mod3)
SSE_mod3 <- 1844.5 #(SSE reducido para la ANOVA)

Fo.c <- ((SSE_mod3 - SSE_mod1)/4)/MSE_mod1
Fo.c

rm(list = ls())
