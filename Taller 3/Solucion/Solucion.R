#Ejercicio 1
#funcion que simula los datos
gen_dat <- function(n, seed = 7){
  varianza <- 16
  set.seed(seed)
  x <- rep(runif(n=floor(n/2)+1, min=-5, max=6),2)[sample(2*floor(n/2)+2,n)]
  media <- 4 - 6 * x + 2 * x^2
  set.seed(seed^2)
  y <- rnorm(n=n, mean=media, sd=sqrt(varianza))
  marco_datos <- data.frame(y=y, x=x)
  return(marco_datos)
}
datos <- gen_dat(75) #generando los datos
#Ejercicio 2
mod <- lm(y ~ x, data = datos) #ajustando el modelo

#Ejercicio 3
summary(mod)
#ambos coeficientes son significativos

#Ejercicio 4
residuales <- residuals(mod) #extrayendo los residuales
mean(residuales) #la media de los residuales da 0

#Ejercicio 5
ajustados <- fitted(mod) #valores ajustados
plot(ajustados, residuales, pch=20) #hay no linealidad


#Ejercicio 6
#qqplot
qqnorm(residuales)
qqline(residuales)
#prueba de hipotesis
shapiro.test(residuales) #prueba de bondad de ajuste de normalidad

#conclusion, los residuales no son normales

#Ejercicio 7
install.packages("rsm") #instalando los paquetes necesarios
#graficando los datos
with(datos, plot(x, y), pch=20) #plot(datos.ajuste$x, datos.ajuste$y)

#cargando el paquete rsm
library(rsm)
mod.falta.ajuste <- rsm(y ~ FO(x), data = datos)
summary(mod.falta.ajuste)

#Ejercicio 8
datos.transform <- readxl::read_xlsx("decaimiento.xlsx")
with(datos.transform, plot(t, sustancia, pch=20))
#ajustando el modelo
with(datos.transform, plot(t, log(sustancia), pch=20))
mod.transform <- lm(log(sustancia) ~ t, data = datos.transform)
#viendo el summary
summary(mod.transform)

