library('tidyverse')
datos <- read.table("./Bases/EquipoXX.txt", header = T)
datos <- datos[, c(2, 5, 1)] %>% 
  mutate(W = if_else(Y <= 4.1, 1, if_else(Y > 4.8, 3, 2)))

n <- nrow(datos)
N <- 8*n
alpha <- 0.05
cuantmas <- qt(1 - alpha/2, df = n-1)

rprint <- function(x) {
  sprintf(x, fmt = "%#.4f")
}

#Punto 1

#X1 mu y tau
rprint(meanX1 <- mean(datos$X1))
rprint(varX1 <- var(datos$X1))
rprint(varX1hat <- (1 - n/N)*varX1/n)
rprint(liX1 <- meanX1 - cuantmas*sqrt(varX1hat))
rprint(lsX1 <- meanX1 + cuantmas*sqrt(varX1hat))

rprint(N*meanX1)
rprint((N**2)*varX1hat)
rprint(N*liX1)
rprint(N*lsX1)

#X4 my y tau

rprint(meanX4 <- mean(datos$X4))
rprint(varX4 <- var(datos$X4))
rprint(varX4hat <- (1 - n/N)*varX4/n)
rprint(liX4 <- meanX4 - cuantmas*sqrt(varX4hat))
rprint(lsX4 <- meanX4 + cuantmas*sqrt(varX4hat))

rprint(N*meanX4)
rprint((N**2)*varX4hat)
rprint(N*liX4)
rprint(N*lsX4)

#Z
rprint(Zp <- mean(datos$W == 1))
rprint(varZp <- Zp*(1-Zp))
rprint(varZphat <- (1- n/N) * varZp/(n-1))
rprint(liZp <- Zp - cuantmas*sqrt(varZphat))
rprint(lsZp <- Zp + cuantmas*sqrt(varZphat))

rprint(N*Zp)
rprint((N**2)*varZphat)
rprint(N*liZp)
rprint(N*lsZp)

#Punto 2
alpha <- 0.1
rprint(cuant2 <- qnorm(1 - alpha/2))
rprint(deltaX1 <- (lsX1 - liX1)/4)
rprint(DX1 <- (deltaX1/cuant2)**2)
rprint(nX1 <- (N-1)*varX1/((N-1)*DX1 + (N-1)/N * varX1))
rprint(ceiling(nX1))

rprint(deltaZp <- (lsZp - liZp)/4)
rprint(DZp <- (deltaZp/cuant2)**2)
rprint(nZp <- N*varZp/((N-1)*DZp + varZp))
rprint(ceiling(nZp))

#Punto 3

L <- 3
alpha <- 0.05
cuantmae <- qt(1 - alpha/2, df = n - L)

(infomae <- datos %>% 
  group_by(W) %>% 
  summarise(ni = n(), Ni = 6*ni, cuantiles.estratos = qt(1 - alpha/2, df = ni - 1),
            meanX1 = mean(X1), meanX4 = mean(X4), varX1 = var(X1), varX4 = var(X4), varX1hat = (1 - ni/Ni)*varX1/ni, varX4hat = (1 - ni/Ni)*varX4/ni,
  liX1 = meanX1 - cuantiles.estratos*sqrt(varX1hat),
  lsX1 = meanX1 + cuantiles.estratos*sqrt(varX1hat),
  liX4 = meanX4 - cuantiles.estratos*sqrt(varX4hat),
  lsX4 = meanX4 + cuantiles.estratos*sqrt(varX4hat)) %>% 
    as.data.frame()
)

N <- with(infomae, sum(Ni))

rprint(meanX1 <- with(infomae, sum((Ni/N)*meanX1)))
rprint(varX1hat <- with(infomae, sum((Ni/N)**2*varX1hat)))
rprint(liX1 <- meanX1 - cuantmae*sqrt(varX1hat))
rprint(lsX1 <- meanX1 + cuantmae*sqrt(varX1hat))

rprint(meanX4 <- with(infomae, sum((Ni/N)*meanX4)))
rprint(varX4hat <- with(infomae, sum((Ni/N)**2*varX4hat)))
rprint(liX4 <- meanX4 - cuantmae*sqrt(varX4hat))
rprint(lsX4 <- meanX4 + cuantmae*sqrt(varX4hat))

rm(list = ls())
