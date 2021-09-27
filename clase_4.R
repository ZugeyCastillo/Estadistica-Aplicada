# Segunda clase Series de tiempo

# EJEMPLO 3
# Crear serie de tiempo
setwd("C:/Users/DarioJaciel/Desktop/Zuky/Rstudio/Estadística apliacada")
tmp <- read.table("SerieTempMaxMty.csv", sep = ",", header = T)
library(tseries)
smty <- ts(tmp$Temp.Max, start = c(2017,10), end = c(2020,5), frequency =  12)
plot(smty)
start(smty)
time(smty)

# Sospechamos
# NOTA. si la gráfica no tiene tendencia entonces es estacionaria

# Revisamos cambios en media
tend <- lm(smty ~ time(smty))
summary(tend)
# Hipótesis
# Ho: la serie no muestra tendencia
# Ha: la serie muestra tendencial
# Rechazamos Ho si p-valor < alpha
# 0.6674 < 0.05 por lo tanto No R. Ho
# el modelo no es significativo, no hay tendencia lineal,
# la serie de tiempo es estacionaria en media
abline(tend, col = "magenta")
# Revisando tendencia no lineal
tm <- seq(from = 2017.75, to = 2020.4, by = 1/12)
v <- lm(smty ~ poly(tm, degree = 3))
summary(v)
# Hipótesis
# Ho: la serie no muestra tendencia
# Ha: la serie muestra tendencial
# Rechazamos Ho si p-valor < alpha
# 0.1545 < 0.05 por lo tanto No R. Ho
lines(tm, fitted.values(v), col = "blue")
# Vemos que no muestra tendencia cúbica

# Revisando cambios en la varianza
adf.test(smty, alternative = "stationary")
# Hipótesis
# Ho: la serie no estacionaria
# Ha: la serie es estacionaria
# Rechazamos Ho si p-valor < alpha
# 0.3582 < 0.05
# Por lo tanto, No R. Ho, la serie no muestra tendencia, no es estacionaria

# Clasificación de la serie
# Serie no estacionaria, debido a cambios en varianza

# EJEMPLO 4
data("airmiles")
airmiles
time(airmiles)
plot(airmiles)
# No estacionaria

# Revisamos cambios en la media
yt <- log(airmiles)
la <- lm(yt ~ time(airmiles))
summary(la) # el modelo es significativo
# Hipótesis
# Ho: la serie no muestra tendencia
# Ha: la serie muestra tendencial
# Rechazamos Ho si p-valor < alpha
# 2.2e-16 < 0.05 por lo tanto R. Ho
# Modelo de tendencia exponencial
B0 <- exp(coef(la)[1]) # transformación
# y estimada = 1.32e^-159 exp 1.913e^-01x
d <- B0*exp(coef(la)[2]*time(airmiles))
ta <- seq(from = 1937, to = 1960, by = 1) # tiempos
lines(ta, d, col = "purple")

# Revisamos el modelo cuadrático de tendencia
w <- lm(airmiles ~ poly(ta, degree = 2))
summary(w)
# Hipótesis
# Ho: la serie no muestra tendencia
# Ha: la serie muestra tendencial
# Rechazamos Ho si p-valor < alpha
# 2.2e-16 < 0.05 por lo tanto R. Ho
lines(ta, fitted.values(w), col = "green")

# Revisamos cambios en varianza
adf.test(airmiles, alternative = "stationary")
# Hipótesis
# Ho: la serie no estacionaria
# Ha: la serie es estacionaria
# Rechazamos Ho si p-valor < alpha
# 0.895 < 0.05
# Por lo tanto, No R. Ho, la serie no es estacionaria en varianza

# Clasificación de la serie
# Serie no estacionaria debido a cambios en media y varianza
