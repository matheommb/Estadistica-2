#Ejercicio 2)

# install.packages("car")

library(car)
gen_dat <- function(n) { #funcion generadora de datos
  x1 <- runif(n=n, min=0, max=10)
  x2 <- x1 * 2 + rnorm(n=n, sd=0.01) # x2 es el doble de x1 + ruido
  y <- rnorm(n=n, mean= - 3 + 2 * x1 - 4 * x2, sd=2)
  data.frame(y, x1, x2)
}

#Generando numeros aleatorios y partiendo la base de datos en 2
set.seed(12345)
datos <- gen_dat(n=40)
datos1 <- datos[1:20, ]
datos2 <- datos[21:40, ]

#ajustando un modelo por base de datos
mod1 <- lm(y ~ ., datos1)
mod2 <- lm(y ~ ., datos2)

#viendo los coeficientes
coef(mod1)
coef(mod2)

#Factores de inflacion de varianza
#alternativa 1
car::vif(mod1)
car::vif(mod2)

#alternativa 2
source("~/Documents/Estadística 2/Taller7_Abr_17/solucion/funciones.R")

myCoefficients(lm.model = mod1, dataset = datos1)
myCoefficients(lm.model = mod2, dataset = datos2)


#Analizar el espectro
mod <- lm(y ~ ., datos)
myCollinDiag(lm.model = mod, center = F)
myCollinDiag(lm.model = mod, center = T)

#Ejercicio 3)
datosall <- MPV::earthquake
str(datosall)
modall <- lm(depth ~ ., data = datosall)
library(leaps)
tablall <- myAllRegTable(modall)
tablall[, 1:5] <- apply(X = tablall[, 1:5], MARGIN = 2, as.numeric)
tablall$Cp_p <- abs(tablall$Cp - (tablall$k + 1))
tablall
