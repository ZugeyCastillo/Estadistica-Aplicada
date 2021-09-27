# Primera clase Series de tiempo

# Clasificación de series de tiempo de una serie

# EJEMPLO 1

data(lynx)
e1 <- lynx
e1
class(lynx) # Identificar si es una serie de tiempo
start(lynx) # Identificar el inicio de la serie de tiempo
end(lynx)   # Identificar el final de la serie de tiempo
time(lynx)  # Identificar el vector de tiempos de la serie de tiempo
plot(lynx)  # Vista de la serie de tiempo

# Sospechamos

# Revisión de cambios en la media ¿tiene tendencia o no?
# si tiene cambios es que hay tendencia
tl <- lm(lynx ~ time(lynx)) # modelo lineal para la tendencia
anova(tl) # o
summary(tl)
# Hipótesis
# Ho: la serie no muestra tendencia
# Ha: la serie muestra tendencial
# Rechazamos Ho si p-valor < alpha
# 0.4691 < 0.05 por lo tanto No R. Ho
# el modelo no es significativo
abline(tl, col = "magenta") # el albline es para lineas rectas y line es para lineas curvas
# la serie no muestra tendencia

# Revisión de cambios en varianza
library(tseries)
adf.test(lynx, alternative = "stationary")
# Ho: La serie no es estacionaria
# Ha: la serie es estacionaria
# Rechazamos Ho si P-valor < alpha
# 0.01 < 0.05
# Por lo tanto, R. Ho, la serie no tiene cambios en varianza, es decir, es estacionaria

# Clasificación de la serie
# La serie es estacionaria

# EJEMPLO 2

data(austres)
e2 <- austres
e2
class(austres) 
start(austres) 
end(austres)   
time(austres)  
plot(austres)

# Sospechamos tendencia alcista no estacionaria

# Revisión de cambios en la media ¿tiene tendencia o no?
# si tiene cambios es que hay tendencia
tlc <- lm(austres ~ time(austres)) # modelo lineal para la tendencia
anova(austres) # o
summary(austres)

# muestra tendencia lineal creciente

# Pronostico informal para 1994
plot(austres, xlim = c(1971,1994.5))
abline(tlc, col = "purple")
y.pro <- coef(tlc)[1]+coef(tlc)[2]*1994 # y. es y gorro
# x = 1994
points(1994, y.pro, pch = 8, col = "blue")

# Revisión de cambios en varianza
adf.test(austres, alternative = "stationary")
# Ho: La serie no es estacionaria
# Ha: la serie es estacionaria
# Rechazamos Ho si P-valor < alpha
# 0.3495 < 0.05
# Por lo tanto, No R. Ho, la serie tiene cambios en varianza, es decir, no es estacionaria

# Clasificación de la serie
# La serie no es estacionaria ni en media ni en varianza
