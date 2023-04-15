# Ejercicio 1
import pandas as pd

datos = pd.read_csv("android-games.csv")
# Como obtengo los nombres de las variables?
print(datos.columns)
# Como se que valores hay en category?
print(datos['category'].unique())
# Filtrando la base de datos
datos_modelo = datos[(datos['total_ratings'] < 4121627) & 
                     (datos['category'] == "GAME ACTION")][['total_ratings', 'five_star_rating']]

# Ejercicio 2
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import linregress

# Forma 1
plt.scatter(datos_modelo['total_ratings'], datos_modelo['five_star_rating'])
plt.xlabel('Total de calificaciones')
plt.ylabel('Total de 5 estrellas')
plt.title('5 estrellas vs Calificaciones')
plt.show()

# Forma 2
sns.scatterplot(data=datos_modelo, x='total_ratings', y='five_star_rating')
plt.xlabel('Total de calificaciones')
plt.ylabel('Total de 5 estrellas')
plt.title('5 estrellas vs Calificaciones')
plt.show()

# Ejercicio 3
import numpy as np

# Definiendo x e y
x = datos_modelo['total_ratings'] # covariable
y = datos_modelo['five_star_rating'] # respuesta

# Ajuste del modelo
slope, intercept, r_value, p_value, std_err = linregress(x, y)
print("Pendiente: ", slope)
print("Intercepto: ", intercept)
print("Coeficiente de correlación: ", r_value)
print("P-valor: ", p_value)

# Recta de regresión con seaborn
sns.regplot(data=datos_modelo, x='total_ratings', y='five_star_rating')
plt.xlabel('Total de calificaciones')
plt.ylabel('Total de 5 estrellas')
plt.title('5 estrellas vs Calificaciones')
plt.show()

# Ejercicio 4
# Tiene sentido el intercepto?
# No tiene sentido porque no hay juegos con 0 calificaciones que tengan 5 estrellas.
# Qué unidades tiene el intercepto?
# Número de juegos con 5 estrellas y 0 calificaciones.

# Ejercicio 5
# Tiene sentido la pendiente?
# Sí, la pendiente indica cuántas 5 estrellas adicionales se esperan por cada unidad adicional de calificaciones.
# Qué unidades tiene la pendiente?
# Unidades de 5 estrellas por unidad de calificaciones.

# Ejercicio 6
from scipy.stats import t

alpha = 0.05
n = len(x)
df = n - 2
t_value = t.ppf(1 - alpha/2, df)
se_slope = std_err
se_intercept = std_err * np.sqrt(np.mean(x**2))

# Intervalo de confianza para la pendiente
CI_slope = (slope - t_value*se_slope, slope + t_value*se_slope)
print("Intervalo de confianza para la pendiente: ", CI_slope)

# Intervalo de confianza para el intercepto
CI_intercept = (intercept - t_value*se_intercept, intercept + t_value*se_intercept)
print("Intervalo de confianza para el intercepto: ", CI_intercept)
