# Modelando series estacionarias
data(nottem)
nottem

# Clasificamos la serie (estacionaria o no estacionaria)

# Revisamos tendencia (sin tendencia o con tendencia: lineal o no lineal y
# creciente o decreciente)
tend <- lm(nottem ~ time(nottem))
summary(tend) # sin tendencia
# Hipótesis
# Ho: la serie no muestra tendencia
# Ha: la serie muestra tendencial
# Rechazamos Ho si p-valor < alpha
# 0.4446 < 0.05 por lo tanto No R. Ho
# Estacionaria en media

#H0: la serie es estacionaria
#Ha: la serie no es estacionaria
#P valor es 1.931e-14 < alfa 0.05

# Revisamos varianza
library(tseries)
adf.test(nottem, alternative = "stationary")
# Hipótesis
# Ho: la serie no estacionaria
# Ha: la serie es estacionaria
# Rechazamos Ho si p-valor < alpha
# 0.01 < 0.05 por lo tanto R. Ho
# Serie estacionaria al 95% de confianza

# Serie de tiempo estacionaria

# Revisamos correlogramas
acf(nottem)
pacf(nottem)

# Sugerencia de modelo ARMA(p,q)

# Modelado

# NOTA. En los ciclos solo se meten series estacionarias.

aic.mejor.arma <- Inf
for(j in 0:10){
  for(i in 0:10){
    aic2 <- AIC(arima(nottem, order = c(j,0,i), method = "ML"))
    if(aic2 < aic.mejor.arma){
      aic.mejor.arma <- aic2 # Guardamos valor AIC
      pq <- c(j,i) # Guardamos el orden
    }
  }
}
aic.mejor.arma
pq

# El modelo elegido es ARMA(9,10)
pe <- predict(airma(nottem, order = c(9,0,10), method = "ML"), n.ahead = 50)$pred
plot(nottem, xlim = c(1920,1945))
lines(pe, col = "magenta")

# Revisando series no estacionarias
data("austres")
plot(austres)
# Tendencia
l <- lm(austres ~ time(austres))
summary(l) # la serie muestra tendencia no estacionaria en media
# Hipótesis
# Ho: la serie no muestra tendencia
# Ha: la serie muestra tendencial
# Rechazamos Ho si p-valor < alpha
# 2.2e-16 < 0.05 por lo tanto R. Ho
# Varianza
adf.test(austres, alternative = "stationary")
# Hipótesis
# Ho: la serie no estacionaria
# Ha: la serie es estacionaria
# Rechazamos Ho si p-valor < alpha
# 0.3493 < 0.05 por lo tanto No R. Ho
# Serie no estacionaria en varianza

# Modelado de serie no estacionaria
plot(log(austres))
austres[1:5]
diff(austres)[1:5] # hace restas del anterior menos el primero
plot(diff(austres))

# Intentos
c1 <- diff(log(austres)) # c1 = correción 1
plot(c1, ylim = c(0.001,0.1))
# Revisando si funciona la corrección
# Tendencia de c1
tc <- lm(c1 ~ time(c1)) # tc = tendencia de la corrección
summary(c1)
abline(tc, col = "red")
tcub <- lm(c1 ~ poly(as.numeric(time(c1)), degree = 3))
summary(tcub)
# Hipótesis
# Ho: la serie no muestra tendencia
# Ha: la serie muestra tendencial
# Rechazamos Ho si p-valor < alpha
# 1.048e-08 < 0.05 por lo tanto R. Ho
lines(as.numeric(time(c1)), fitted.values(tcub))
# Sin tendencia estacionaria en media
# Varianza de c1
adf.test(c1, alternative = "stationary")
# Hipótesis
# Ho: la serie no estacionaria
# Ha: la serie es estacionaria
# Rechazamos Ho si p-valor < alpha
# 0.5592 < 0.05 por lo tanto No R. Ho
# La serie no es estacionaria

# c2, la segunda diferenciación
c2 <- diff(c1)
plot(c2, ylim = c(-0.5,0.5))

# Revisando que corrección 2
# Media
tlc2 <- lm(c2 ~ time(c2))
summary(c2) # no muestra tendencia
# La serie es estacionaria en media

# Varianza
adf.test(c2, alternative = "stationary")
# Hipótesis
# Ho: la serie no estacionaria
# Ha: la serie es estacionaria
# Rechazamos Ho si p-valor < alpha
# 0.01 < 0.05 por lo tanto R. Ho
# Estacionaria en varianza

# La serie c2 es estaionaria <- ya no se modela
# Modelando c2
# Autocorrelogramas
acf(c2)
pacf(c2) # sugerencia de ARMA(p,q)

# Buscando ARMA(p,q)
aic.mejor.arma <- Inf
for(j in 0:10){
  for(i in 0:10){
    aic2 <- AIC(arima(c2, order = c(j,0,i), method = "ML"))
    if(aic2 < aic.mejor.arma){
      aic.mejor.arma <- aic2 # Guardamos valor AIC
      pq <- c(j,i) # Guardamos el orden
    }
  }
}
aic.mejor.arma
pq

# El mejor para c2 es ARMA(2,3) con aic = -1036.92
arima(c2, order = c(2,0,3), method = "ML")
p23 <- predict(arima(c2, order = c(2,0,3), method = "ML"),n.ahead = 50)$pred
plot(c2, xlim = c(1972,2006))
lines(p23, col = "blue")

# El mejor para log(austres) es ARIMA(2,2,3)
arima(log(austres), order = c(2,2,3), method = "ML")
p223 <- predict(arima(log(austres), order = c(2,2,3), method = "ML"),n.ahead = 50)$pred
plot(log(austres), xlim = c(1970,2006), ylim = c(9.4,9.9))
lines(p223, col = "purple")

o <- exp(p223) # o = originales
plot(austres, xlim = c(1970,2006), ylim = c(13000,20000))
lines(o, col = "magenta")


# Autocorrelogramas se comportan como senos y cosenos