library(tidyverse)

#Ejercicio 1
datos <- read.csv("Ecommerce_Customers.csv")
str(datos) #nos muestra la estructura de la base de datos
datos.modelo <- datos %>% 
  select(Avg..Session.Length:Length.of.Membership,
         Yearly.Amount.Spent) #seleccionando las variables

#Ejercicio 2
plot(datos.modelo)
# de aca nos damos cuenta de que la variable que mejor
# correlacion lineal tiene con la respuesta es
# duracion de la membresia
#base de datos final
datos.modelo.final <- datos.modelo %>% 
  select(Length.of.Membership, Yearly.Amount.Spent)

#Ejercicio 3
n <- nrow(datos.modelo.final) #el total de filas de la base de datos
set.seed(9012913)
filas <- sample(1:n, 0.8*n) #filas a seleccionar
sample()
#particionando la base de datos
datos.ajuste <- datos.modelo.final[filas, ] #datos para ajustar el modelo (80%)
datos.prediccion <- datos.modelo.final[-filas, ] #datos para predecir con el modelo (20% restante)

#ajustando el modelo
mod <- lm(Yearly.Amount.Spent ~ Length.of.Membership, data=datos.ajuste)

#vamos a ver el resumen del modelo
summary(mod)

#viendo la tabla anova
anova(mod)

#Ejercicio 6

#usando sumas de cuadrados
ybarra <- mean(yi.real) #media de los valores de y
SST <- sum((yi.real - ybarra)^2)
SSR <- sum((yi.hat - ybarra)^2)
SSE <- sum((yi.real - yi.hat)^2)
R.2.sums <- SSR/SST


#Ejercicio 7
#intervalo de confianza para la respuesta media
predict(mod, newdata = datos.ajuste, interval = "confidence", level = 0.95)[1:3, ]
#intervalo de prediccion
predict(mod, newdata = datos.prediccion, interval = "prediction", level = 0.95)[1:3, ]
