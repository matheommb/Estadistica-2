#el link donde estan los datos
url <- "https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo2"
datos <- read.table(url, sep = "\t", header = T)
source("funciones.R")

#Ejercicio 2
#a) ajustando el modelo de regresion
names(datos)
mod <- lm(Estatura ~ ., data = datos[, c(-1, -2,-4)])

#b)

modelo.full <- myAnova(mod)
mod2 <- lm(Estatura ~ ., data=datos[, c(-2, -1, -4, -3, -6)])
modelo.reducido <- myAnova(mod2)
Fo <- ((0.324568-0.301248)/2)/0.0062760
qf(0.05, 2, nrow(datos)-4, lower.tail = F) #si Fo menor que este valor, acepto hipotesis nula
pf(Fo, 2, nrow(datos)-4, lower.tail=F) #valor-P grande, me quedo con H0

#c) verifiquemos los supuestos del modelo y determinemos si hay

#puntos atipicos, de balanceo o de influencia
#puntos atipicos
library(MASS)
res.stand <- round(rstandard(mod), 4) #calculando los residuales estandarizados
res.stud <- round(studres(mod), 4) ##calculando los residuales estudentizados

#puntos de influencia
Cooks.D <- round(cooks.distance(mod), 4) #calculando las distancias de cook
Dffits <- round(dffits(mod), 4) #calculando los dffits

#puntos de balanceo
hii <- round(hatvalues(mod), 4)

diagnosticos <- data.frame(res.stud, Cooks.D, Dffits, hii)

diagnosticos
#Haciendo el analisis

#puntos atipicos
with(diagnosticos, 
     plot(res.stud, ylim = c(-3.5, 3.5), pch = 20,
          xlab = "Observación", ylab = "Residuales estudentizados",
          main = "Residuales estudentizados vs índice de las observaciones"))
abline(h = 3, col = "red")
abline(h = -3, col = "red")

#no hay puntos atipicos, pues ningun residual está por fuera del intervalo [-3, 3]

#puntos de influencia
#distancia de Cook
with(diagnosticos, 
     plot(Cooks.D, pch = 20, ylim = c(0, 1.5), 
          xlab = "Observación", ylab = "Distancias de Cook",
          main = "Distancias de Cook vs índice de las observaciones"))
abline(h = 1, col = "red")

#segun el criterio de las distancias de cook no hay puntos influenciales

#dffits
threshold <- 2*sqrt(4/26) #2*sqrt(p/n)
with(diagnosticos, 
     plot(Dffits, pch = 20, ylim = c(-2, 2), 
          xlab = "Observación", ylab = "Dffits",
          main = "Dffits vs índice de las observaciones"))
abline(h = threshold, col = "red")
abline(h = -threshold, col = "red")
diagnosticos[Dffits > threshold | Dffits < -threshold, ]
#observación 46 influencial

#puntos de balanceo

threshold.balanceo <- 2*(4/26) #2*(p/n) < 1
with(diagnosticos, 
     plot(hii, pch = 20, 
          ylim = c(0, 0.5),
          xlab = "Observación", ylab = "hii",
          main = "hii vs índice de las observaciones"))
abline(h = threshold.balanceo, col = "red")
diagnosticos[hii > threshold.balanceo, ]

#Se observa que 2 observaciones resultaron ser puntos de balanceo

#literal d
x <- as.matrix(cbind(intercept=1, datos[, c(-1, -2,-4, -5)]))
xo <- matrix(c(1, 69.2, 40.5, 16.5))
hoo <- t(xo) %*% solve(t(x) %*% x) %*% xo
hoo
max(hatvalues(mod))
nuevo <- data.frame(Peso=69.2, circun_cuello=40.5, circun_muneca=16.5)
predict(object=mod, newdata=nuevo, interval="confidence", level=0.95)

#literal e
Xo <- matrix(c(1, 64, 36, 15))
hoo <- t(Xo) %*% solve(t(x) %*% x) %*% Xo
hoo
max(hatvalues(mod))
nuevo2 <- data.frame(Peso=64, circun_cuello=36, circun_muneca=15)
predict(object=mod, newdata=nuevo2, interval="prediction", level=0.95)

XO <- matrix(c(1, 80, 47, 20))
hoo <- hoo <- t(XO) %*% solve(t(x) %*% x) %*% XO
hoo
max(hatvalues(mod))
#extrapolacion, no se predice

