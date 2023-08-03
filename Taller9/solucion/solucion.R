#Definiendo las cantidades necesarias
n <- 100; N <- 10000; ybar <- 12.5; S2 <- 1.252
#lee para mu
deltamu <- qt(0.05/2, n-1, lower.tail = F)*sqrt((1 - n/N)*S2/n)
#intervalo de confianza para mu
limu <- ybar - deltamu; lsmu <- ybar + deltamu 
ybar #estimacion puntual de mu
deltamu 
c(limu, lsmu) #intervalo de confianza

#Hagamos lo mismo para tau
tauhat <- N*ybar
deltatau <- N*deltamu
litau <- N*limu; lstau <- N*lsmu

tauhat
deltatau
c(litau, lstau)

#Calculemos el tamano de muestra
D <- (1/qnorm(0.05/2, lower.tail = F))^2
sigma2hat <- (N-1)*S2/N
nhat <- N*sigma2hat/((N-1)*D + sigma2hat)
nhat
ceiling(nhat)
#para lograr un lee a 1 galon por dia, se requieren al menos 5 unidades
#muestrales

#Ejercicio 3

#datos para el ejercicio
df <- data.frame(ID = 1:15, Gastos = c(48, 41, 34, 25, 32, 25, 36, 31, 30, 
                                       38, 31, 19, 26, 27, 22),
                 Nacionalidad = c(0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1 ,1))
#tamano de muestra y de la poblacion
N <- 1500; n <- nrow(df)
#media y varianza muestrales para los gastos diarios
ybar <- mean(df$Gastos); S2 <- var(df$Gastos)
deltamu <- qt(0.05/2, n-1, lower.tail = F)*sqrt((1-n/N)*S2/n)
limu <- ybar - deltamu; lsmu <- ybar + deltamu

#estimemos para tau
tauhat <- N*ybar
litau <- N*limu; lstau <- N*lsmu

tauhat
c(litau, lstau)

#Total de turistas extranjeros
phat <- mean(df$Nacionalidad)
deltap <- qt(0.05/2, n-1, lower.tail = F)sqrt((1-n/N)*phat(1-phat)/(n-1))
lip <- phat - deltap; lsp <- phat + deltap
c(lip, lsp)
#para A es solo multiplicar lo de p por N
Ahat <- N*phat
liA <- N*lip; lsA <- N*lsp

Ahat
c(liA, lsA) #ojo, deben de ser enteros, (596, 1404)

D <- (0.02/qnorm(0.05/2, lower.tail = F))^2
nhat <- (N*phat*(1-phat))/((N-1)D + phat(1-phat))
ceiling(nhat) #tamaño

rm(list=ls())

#punto 4

n <- 50; N <- 750; s2 <- 2.25; ybar <- 10.31

vary <- ((N-n)/N)*(s2/n)
t <- qt(0.05/2, n-1, lower.tail = F)

liy <- ybar-t*sqrt(vary); lsy <- ybar+t*sqrt(vary) 
c(liy, lsy)

#punto 5
N*ybar

#punto 6

c(N*liy, N*lsy)