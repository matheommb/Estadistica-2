# punto 1
Es1 <- c(97,67,42,125,25,92,105,86,27,43,45,59,53,21)
Es2 <- c(125,155,67,96,256,47,310,326,220,352,142,190)
Es3 <- c(142,256,310,440,495,510,320,396,196)
Es4 <- c(167,220,780,655,540)

media1 <- mean(Es1)
var1 <- (1-14/86)*(var(Es1)/14)
t <- qt(1-0.05/2, 12+1)
li <- media1 - t * sqrt(var1)
ls <- media1 + t * sqrt(var1)
c(li, ls)

media2 <- mean(Es2)
var2 <- (1-12/72)*(var(Es2)/12)
t <- qt(1-0.05/2, 11)
li <- media2 - t * sqrt(var2)
ls <- media2 + t * sqrt(var2)
c(li, ls)

# punto 2

media3 <- mean(Es3)
var3 <- (1-9/52)*(var(Es3)/9)

media4 <- mean(Es4)
var4 <- (1-5/30)*(var(Es4)/5)

medias <- c(media1, media2, media3, media4)
vars <- c(var1, var2, var3, var4)
Ns <- c(86, 72, 52, 30)

yst <- (1/sum(Ns))*sum(Ns*medias)
sest <- sqrt((1/sum(Ns)^2)*sum(Ns^2*vars))

t <- qt(1-0.05/2, 40-4)
li <- yst - t * sest
ls <- yst + t * sest
c(li, ls)

# punto 3

t <- qt(1-0.1/2, 40-4)
li <- sum(Ns)*yst - sum(Ns) * t * sest
ls <- sum(Ns)*yst + sum(Ns) * t * sest
c(li, ls)

# punto 4

pst <- (110/250)*0.15 + (70/250)*0.5 + (45/250)*0.52 + (25/250)*0.63
varpst <- (110/250)^2*((1-40/110)*(0.15*(1-0.15)/(40-1))) + (70/250)^2*((1-28/70)*(0.5*(1-0.5)/(28-1))) + (45/250)^2*((1-19/45)*(0.52*(1-0.52)/(19-1))) + (25/250)^2*((1-11/25)*(0.63*(1-0.63)/(11-1)))

t <- qt(1-0.05/2, 98-4)
li <- pst - t * sqrt(varpst)
ls <- pst + t * sqrt(varpst)
c(li, ls)

# punto 5
# a
psi1 <- 5/29
psi2 <- 9/29
psi3 <- 15/29

psi1*210
psi2*210
psi3*210

# b
psi1 <- 1/8
psi2 <- 1/4
psi3 <- 5/8

psi1*210
psi2*210
psi3*210

# punto 6

n = 1500/(0.3*10+0.2*25+0.25*50+0.25*5)

# punto 7

psi1 <- 100/347
psi2 <- 247/347

n <- 36.5687

psi1*n
psi2*n

11+27
