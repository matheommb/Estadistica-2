library(tidyverse)
datos <- MPV::table.b7
datos <- datos[, -4] %>%
  select(y, everything())
#a
var(datos)
cov(datos$y, datos$x1)
cov(datos)

#b
cor(datos)

#c (mostrar latex)

#d
datos.matriz <- as.matrix(datos)
X <- cbind("(intercepto)" = 1, datos.matriz[, -1])
y <- as.matrix(datos.matriz[, 1], ncol = 1)

#e
first <- t(X) %*% X
second <- solve(first)
third <- second %*% t(X) %*% y #betas
fourth <- X %*% third #ygorro
fifth <- y - fourth #residuales

#f
mod <- lm(y ~ x1+x2+x3+x5, data=datos)
