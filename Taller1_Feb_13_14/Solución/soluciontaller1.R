#instalando el tidyverse
install.packages("tidyverse")
#cargando el tidyverse
library(tidyverse)

#Ejercicio 1
datos <- read.csv("android-games.csv")
#como obtengo los nombres de las variables?
names(datos)
#como se que valores hay en category?
unique(datos$category)
#filtrando la base de datos
datos_modelo <- datos %>%
  filter(total_ratings < 4121627,
         category == "GAME ACTION") %>% 
  select(total_ratings, five_star_rating)

#Ejercicio 2
#forma1
plot(datos_modelo$total_ratings, datos_modelo$five_star_rating,
     xlab="Total de calificaciones",
     ylab="Total de 5 estrellas",
     main="5 estrellas vs Calificaciones")

#forma2
(p <- ggplot(datos_modelo, aes(total_ratings, five_star_rating)) +
  geom_point() + #agregando la capa de puntos
  labs(x="Total de calificaciones",
       y="Total de 5 estrellas",
       title="5 estrellas vs Calificaciones")) #agregando la capa de los rotulos

#Ejercicio 3

#ajustando el modelo manualmente (no lo haga compa)
#definiendo x e y
x <- datos_modelo$total_ratings #covariable
y <- datos_modelo$five_star_rating #respuesta

n <- nrow(datos_modelo) #numero de datos
xbarra <- mean(x) #media muestral de x
ybarra <- mean(y) #media muestral de y
Sxx <- sum((x - xbarra)^2)
Syy <- sum((y - ybarra)^2)
Sxy <- sum((x-xbarra)*(y-ybarra))
beta1.man <- Sxy/Sxx #estimacion de la pendiente
beta0.man <- ybarra - beta1.man*xbarra #estimacion del intercepto
ygorro.man <- beta0.man + beta1.man*x #estimacion de la respuesta media
residuos.man <- y - ygorro.man #residuales
sigma2.man <- sum(residuos.man^2)/(n-2) #estimacion de sigma^2

#forma chida
mod <- lm(five_star_rating ~ total_ratings, data=datos_modelo)
summary(mod) #muestra el resumen del modelo
beta0 <- coef(mod)[1] #intercepto
beta1 <- coef(mod)[2] #pendiente

#Recta de regresion con ggplot
p +
  geom_smooth(method="lm", formula="y~x", se=F) #capa que agrega las regresiones

#con el basico
plot(datos_modelo$total_ratings, datos_modelo$five_star_rating,
     xlab="Total de calificaciones",
     ylab="Total de 5 estrellas",
     main="5 estrellas vs Calificaciones")
abline(mod)

#Ejercicio 4

#tiene sentido beta0? No tiene porque una app sin calificaciones
#no puede tener calificaciones de 5 estrellas
#que unidades tiene beta0? numero de votaciones con calificacion de 5 estrellas
#como carece de sentido no se interpreta
#p valor enorme (0.363), no se rechaza H0

#Ejercicio 5
#tiene sentido beta1? si
#que unidades tiene beta1? unidades de y / unidades de x
#interpretacion: por cada cien votos que reciba una app se espera en promedio
#que reciba 68 votos de 5 estrellas
#pvalor < 2*10^-16, significativo y mucho

#Ejercicio 6
alpha <- 0.05
#forma manual 
#para beta0
se.b0 <- sqrt(sigma2.man*sum(x^2)/(n*Sxx))
li.b0 <- beta0.man - qt(alpha/2, n-2, lower.tail = F)*se.b0
ls.b0 <- beta0.man + qt(alpha/2, n-2, lower.tail = F)*se.b0
c(li.b0, ls.b0)

#para beta1
se.b1 <- sqrt(sigma2.man/Sxx)
li.b1 <- beta1.man - qt(alpha/2, n-2, lower.tail = F)*se.b1
ls.b1 <- beta1.man + qt(alpha/2, n-2, lower.tail = F)*se.b1
c(li.b1, ls.b1)

#forma eficiente
confint(mod)
confint(mod, "(Intercept)", level = 0.95)
confint(mod, "total_ratings", level = 0.95)
