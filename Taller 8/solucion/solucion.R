#Ejercicio 2
mtcars #Presentando la base de datos
str(mtcars) #Aca nos damos cuenta de que cyl es numerica y no categorica como deberia ser
#vamos a convertir cyl en categorica
datos <- mtcars
datos$cyl <- factor(datos$cyl)
str(datos)
unique(datos$cyl)
# vamos a ajustar el modelo
# mod <- lm(mpg ~ 1+ wt + cyl + wt*cyl, data = datos) esta es otra forma de escribirlo y dará lo mismo
mod <- lm(mpg ~ wt*cyl, data = datos)
summary(mod)

#Ejercicio 3
rock
library(leaps) #para poder usar las funciones del curso
source("funciones.R")
myAllRegTable(lm(area ~ ., data = rock))
mod.naive <- lm(area ~ 1, data = rock)
sse.naive <- sum(residuals(mod.naive)^2)
summary(mod.naive)
myAnova(mod.naive)
#Forward
#paso 1
(338543101 - 109513013)/(109513013/(48 - 2))
qf(0.95, 1, 48 - 2)

#paso 2
(109513013 - 76348142)/(76348142/(48 - 3))
qf(0.95, 1, 48 - 3)

#Backward
#paso1
(76348142 - 74326644)/(74326644/(48 - 4))
qf(0.95, 1, 48 - 4)

#paso2
(109513013 - 76348142)/(76348142/(48 - 3))

#Stepwise
#paso 1 es como en forward
#paso 2
(285283187 - 76348142)/(76348142/(48 - 3))
