# Instalación y carga de paquetes necesarios (DEBE TENER CONEXIÓN A INTERNET)
if(!require(leaps)){install.packages("leaps"); library(leaps)}

# Carga de funciones de usuario para RLM
source("Functions.R")

# Lectura de la base de datos asignada 'EquipoXX.txt' a traves del comando file.choose()
base <- read.table(file.choose(), header = T)

# Matriz de gráficas de dispersión con boxplots y correlaciones de las variables
pairs(base, lower.panel = myPanel.cor, upper.panel = panel.smooth, diag.panel = myPanel.box, labels = names(base))

# Mínimos y Máximos de cada variable
data.frame(apply(base, 2, range), row.names = c('min', 'max'))

# Definición del modelo de RLM, Tabla ANOVA y Tabla de parámetros estimados
modelo <- lm(Y ~ ., base)
myAnova(modelo)
summary(modelo)$coefficients

# Tabla de todas las regresiones posibles
myAllRegTable(modelo)

# Gráfica y prueba de normalidad de Shapiro-Wilk
myQQnorm(modelo)

# Cálculo de residuales estudentizados y valores ajustados
res.stud <- round(rstandard(modelo), 4)
yhat <- round(modelo$fitted.values, 4)

# Gráfica de Residuales estudentizados vs. Valores ajustados
plot(yhat, res.stud, xlab = "Valores Ajustados", ylab = "Residuales Estudentizados")
abline(h = 0, lty = 2, lwd = 2, col = 2)

## Diagnósticos para identificar valores extremos

# Cálculo de errores estándar de los valores ajustados
se.yhat <- round(predict(modelo, se.fit = T)$se.fit, 4)
# Residuales crudos del modelo
residuals <- round(modelo$residuals, 4)
# Distancias de Cook
Cooks.D <- round(cooks.distance(modelo), 4)
# Valores de la diagonal de la matriz H
hii.value <- round(hatvalues(modelo), 4)
# Dffits
Dffits <- round(dffits(modelo), 4)
# Tabla de diagnósticos
data.frame(base, yhat, se.yhat, residuals, res.stud, Cooks.D, hii.value, Dffits)
