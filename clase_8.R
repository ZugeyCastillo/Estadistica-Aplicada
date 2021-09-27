# Modelo de series de tiempo

plot(co2)
plot(airmiles)

# Clasificación de la serie de tiempo
# No estacionaria ni en media ni en varianza

# Corrección (en caso necesario)
c1.m <- diff(log(airmiles))
plot(c1.m, ylim = c(0,2))

# Revisando el efecto de la correción

  # Media
  summary(lm(c1.m ~ time(c1.m)))
  # Varianza
  library(tseries)
  adf.test(c1.m, alternative = "stationary")
    # Estacionaria en variaza
    c2.m <- diff(c1.m)
    plot(c2.m, ylim = c(-1,1))
    # Revisando el efecto de la correción
      # Media
      summary(lm(c2.m ~ time(c2.m)))
      # Varianza
      adf.test(c2.m, alternative = "stationary")
      # c2.m es estacionaria

# Modelado de serie estacionaria o correción
acf(c2.m)
# Sugerencia modelo ARMA
pacf(c2.m)

# NOTA. En los ciclos solo se meten series estacionarias.

aic.mejor.arma <- Inf
for(j in 0:15){
  for(i in 0:15){
    aic2 <- AIC(arima(c2.m, order = c(j,0,i), method = "ML"))
    if(aic2 < aic.mejor.arma){
      aic.mejor.arma <- aic2 # Guardamos valor AIC
      pq <- c(j,i) # Guardamos el orden
    }
  }
}
aic.mejor.arma
pq
# El mejor es ARMA(15,0)

AIC(arima(c2.m, order = c(2,0,2), method = "ML")) # Solo valor AIC
AIC(arima(c2.m, order = c(15,0,0), method = "ML")) # Solo valor AIC
mejor <- arima(c2.m, order = c(15,0,0), method = "ML")
res15 <- residuals(mejor)
      
# Ruido blanco
  
  # Media al rededor del cero entre (-2,2)
  mean(res15)
  plot(res15, type = "p", ylim = c(-0.01,0.01))
  # Incorrelación
  acf(res15, ci = 0.998)
  pacf(res15, ci = 0.998)
  Box.test(res15) # buscamos valores pequeños
  # Hipótesis
  # Ho: los residuales son independientes
  # Ha: los residuales no son independientes
  # Rechazamos Ho si p-valor < alpha
  # 0.003912 < 0.05 por lo tanto R. Ho
  # Normalidad
  shapiro.test(res15)
  # Hipótesis
  # Ho: los residuales provienen de una distribución normal
  # Ha: los residuales no provienen de una distribución normal
  # Rechazamos Ho si p-valor < alpha
  # 0.001449 < 0.05 por lo tanto R. Ho
  mejor2 <- arima(c2.m, order = c(2,0,2), method = "ML")
  res22 <- residuals(mejor2)
  mean(res22)
  plot(res22, type = "p", ylim = c(-1,1))
  acf(res22)
  pacf(res22)
  Box.test(res22)
  # Hipótesis
  # Ho: los residuales son independientes
  # Ha: los residuales no son independientes
  # Rechazamos Ho si p-valor < alpha
  # 0.6966 < 0.05 por lo tanto No R. Ho
  shapiro.test(res22)
  # Hipótesis
  # Ho: los residuales provienen de una distribución normal
  # Ha: los residuales no provienen de una distribución normal
  # Rechazamos Ho si p-valor < alpha
  # 0.0454 < 0.05 por lo tanto R. Ho
  
  # Ruido blanco gaussiano para alpha = 0.01
  res.e <- res22/sqrt(var(res22))
  plot(res.e, type = "p", ylim = c(-3.5,3.5))
  abline(h = c(-3,3), col = "orange")
  # El valor del residual
  max(res.e)
  # el año en que se registró el atipico
  time(res.e)[res.e == max(res.e)]
  points(1946,3.166894, col = "red")
  
  # Predicciones
  # Modelo ARMA(2,2)
  p22 <- predict(mejor2, n.ahead = 10)$pred
  # Sobre la serie estacionaria
  plot(c2.m, xlim = c(1939,1970))
  lines(p22, col = "magenta")
  # La gráfica tiene un hueco porque la correción termina antes de que
  # comience la predicción. Esto se debe a la diferenciación
  p222 <- predict(arima(log(airmiles), order = c(2,2,2), method = "ML"),
                  n.ahead = 10)$pred
  plot(log(airmiles), xlim = c(1939,1970), ylim = c(6,12))
  lines(p222, col = "blue")
  # Serie original
  plot(airmiles, xlim = c(1939,1970), ylim = c(0,85000))
  # Exponencial de las predicciones
  m <- exp(p222)
  lines(m, col = "purple")
  p.o222 <- predict(arima(airmiles, order = c(2,2,2), method = "ML"),
                    n.ahead = 10)$pred
  lines(p.o222, col = "green")
  # CHECAR CUAL GRÁFICA ES LA BUENA: MORADA O VERDE

plot(co2)
# 1. Clasificar la serie (estacionaria o no estacionaria)
  # Tendencia (sin tendencia o con tendencia: lineal o no lineal y creciente
  # o decreciente)
  a <- lm(co2 ~ time(co2))
  summary(a)
  abline(a, col = "magenta")
  # Muestra tendencia creciente porque time(co2) estimate = 1.308
  
  # Varianza
  adf.test(co2, alternative = "stationary")
  # Serie no estacionaria por cambios en media y varianza. Se necesita
  # correción

# 2. Correcciones
  # Aplicación de correción
  i1 <- diff(log(co2)) # intento 1
  plot(i1)
  # Revisión del efecto de correción
  summary(lm(i1 ~ time(i1)))
  # Hipótesis
  # Ho: la serie no muestra tendencia
  # Ha: la serie muestra tendencial
  # Rechazamos Ho si p-valor < alpha
  # 0.7625 < 0.05 por lo tanto No R. Ho
  # No muestra tendencia
  adf.test(i1, alternative = "stationary")
  # Hipótesis
  # Ho: la serie no estacionaria
  # Ha: la serie es estacionaria
  # Rechazamos Ho si p-valor < alpha
  # 0.01 < 0.05 por lo tanto R. Ho
  # Serie estacionaria
  
# 3. Modelo
  aic.mejor.arma <- Inf
  for(j in 0:10){
    for(i in 0:10){
      aic2 <- AIC(arima(i1, order = c(j,0,i), method = "ML"))
      if(aic2 < aic.mejor.arma){
        aic.mejor.arma <- aic2 # Guardamos valor AIC
        pq <- c(j,i) # Guardamos el orden
      }
    }
  }
  aic.mejor.arma
  pq
  # El mejor es ARMA(,)
    
  mejor.i1 <- arima(i1, order = c(10,0,7), method = "ML")
  AIC(arima(i1, order = c(11,0,10), method = "ML")) # Solo valor AIC
  AIC(arima(i1, order = c(10,0,7), method = "ML")) # Solo valor AIC
  
# 4. Ruido blanco
  res107 <- residuals(mejor.i1)
  
  # Media al rededor del cero
  mean(res107)
  # Varianza constante
  plot(res107, type = "p")
  # Incorrelación
  acf(res107, ci = 0.999)
  pacf(res107, ci = 0.999)
  Box.test(res107)
  # Hipótesis
  # Ho: los residuales son independientes
  # Ha: los residuales no son independientes
  # Rechazamos Ho si p-valor < alpha
  # 0.8887 < 0.05 por lo tanto No R. Ho
  # Normalidad
  shapiro.test(res107)
  # Hipótesis
  # Ho: los residuales provienen de una distribución normal
  # Ha: los residuales no provienen de una distribución normal
  # Rechazamos Ho si p-valor < alpha
  # 0.849 < 0.05 por lo tanto No R. Ho
  
# Ruido blanco gaussiano

# 5. Predicciones
  p1110 <- predict(arima(i1, order = c(11,0,10), method = "ML"),
                   n.ahead = 50)$pred
  p107 <- predict(arima(i1, order = c(10,0,7), method = "ML"),
                  n.ahead = 50)$pred
  plot(i1, xlim = c(1960,2003))
  lines(p1110, col = "blue")
  lines(p107, col = "magenta")
  p1017 <- predict(arima(log(co2), order = c(10,1,7), method = "ML"),
                   n.ahead = 50)$pred
  plot(log(co2), xlim = c(1960,2003))
  lines(p1017, col = "red")
  # Predicciones en datos originales
  plot(co2, xlim = c(1960,2003))
  lines(exp(p1017), col = "blue")
  p.o <- predict(arima(co2, order = c(10,1,7), method = "ML"),
                 n.ahead = 50)$pred
  lines(p.o, col = "magenta")
  
  