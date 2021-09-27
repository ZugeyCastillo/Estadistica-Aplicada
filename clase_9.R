library(TSA)
library(tseries)

# A. Serie flow

data(flow)
plot(flow)
help(flow) # Serie de tiempo por meses

# Pasos para el análisis de una serie de tiempo

# 1. Clasificar la serie

  # Media
  # Modelo lineal
  a <- lm(flow ~ time(flow))
  summary(a)
  # Hipótesis
  # Ho: la serie no muestra tendencia (no es significativa)
  # Ha: la serie muestra tendencial (es significativa)
  # Rechazamos Ho si p-valor < alpha
  # 0.1111 < 0.05 por lo tanto No R. Ho, la serie no muestra tendencia
  # NOTA. El r^2 ajustado nos sirve para confirmar que no hay tendencia
  # NOTA. Si el modelo es no significativo quiere decir que la
  # serie es estacionaria
  # Por lo tanto, la serie es estacionaria en media
  abline(a, col = "magenta")

  # Varianza
  adf.test(flow, alternative = "stationary") # Prueba Dickey Fuller aumentada
  # Hipótesis
  # Ho: la serie no es estacionaria
  # Ha: la serie es estacionaria
  # Rechazamos Ho si p-valor < alpha
  # 0.01 < 0.05 por lo tanto R. Ho
  # Por lo tanto, la serie de tiempo es estacionaria en varianza
  
  # NOTA. Debe ser estacionaria ambas: en media y en varianza
  # En conclusión, la serie es estacionaria

# 2. Corregir
  
  # Dado que la serie es estacionaria NO se ocupa corregir
  # NOTA. Se hacen correciones en la serie cuando no es estacionaria
  # incluso cuando sólo una sea estacionaria
  
# 3. Modelado
  
  # NOTA. En los ciclos solo se meten series estacionarias.
  
  aic.mejor.arma <- Inf
  for(j in 0:10){
    for(i in 0:10){
      aic2 <- AIC(arima(flow, order = c(j,0,i), method = "ML"))
      if(aic2 < aic.mejor.arma){
        aic.mejor.arma <- aic2 # Guardamos valor AIC
        pq <- c(j,i) # Guardamos el orden
      }
    }
  }
  aic.mejor.arma
  pq
  
  # El modelo elegido es ARMA(7,9)
  AIC(arima(flow, order = c(7,0,9), method = "ML")) # Solo valor AIC
  mejor <- arima(flow, order = c(7,0,9), method = "ML")
  mejor # s.e. es el error de estimación
  # el mes que más afecta (o más reduce el valor) es el 6 con -0.9224 porque 
  # del cero es el que más se aleja
  # el mes que más afecta aumentando el valor es el 7 con 0.7165
  res79 <- residuals(mejor)
  
# 4. Revisar ruido blanco
  
  # Media al rededor del cero entre(-2,2)
  mean(res79)
  # Varianza constante
  plot(res79, type = "p") # Bucamos que no haya patrones
  
  # Correlación (correlograma parcial y ordinario)
  acf(res79)  # ordinario
  pacf(res79) # parcial
  # Las barritas son los grados de oscilación
  # Como los datos no pasan de los límites (franjas azules) los datos son
  # incorrelacionados (son independientes)
  
  Box.test(res79) # buscamos valores pequeños
  # Hipótesis
  # Ho: los residuales son independientes
  # Ha: los residuales no son independientes
  # Rechazamos Ho si p-valor < alpha
  # 0.8684 < 0.05 por lo tanto No R. Ho
  # Es decir, los residuales son independientes (no están relacionados)
  
  # Normalidad
  shapiro.test(res79)
  # Hipótesis
  # Ho: los residuales provienen de una distribución normal
  # Ha: los residuales no provienen de una distribución normal
  # Rechazamos Ho si p-valor < alpha
  # 2.2e-16 < 0.05 por lo tanto R. Ho
  # Es decir, los residuales no provienen de una distribución normal
  
# 5. Predicciones
  
  p79 <- predict(mejor, n.ahead = 16)$pred # 12 xq está bien pronosticar 1 año
  plot(flow, xlim = c(1958,2008)) # Agarrar un lapso que abarque más
  lines(p79, col = "magenta")
  
# B. Serie gold
  
data(gold)
plot(gold)
help(gold) # Serie de tiempo por dias hábiles 

# Pasos para el análisis de una serie de tiempo

# 1. Clasificar la serie

  # Media
  # Modelo cuadrático
  mc <- lm(gold ~ poly(as.numeric(time(gold)), degree = 2))
  summary(mc)
  # Hipótesis
  # Ho: la serie no muestra tendencia (no es significativa)
  # Ha: la serie muestra tendencial (es significativa)
  # Rechazamos Ho si p-valor < alpha
  # 2.2e-16 < 0.05 por lo tanto R. Ho, la serie muestra tendencia
  # NOTA. Si el modelo es no significativo quiere decir que la
  # serie es estacionaria
  # Por lo tanto, la serie no es estacionaria en media

  # Varianza
  adf.test(gold, alternative = "stationary") # Prueba Dickey Fuller aumentada
  # Hipótesis
  # Ho: la serie no es estacionaria
  # Ha: la serie es estacionaria
  # Rechazamos Ho si p-valor < alpha
  # 0.6665 < 0.05 por lo tanto No R. Ho
  # Por lo tanto, la serie de tiempo no es estacionaria en varianza
 
  # NOTA. Debe ser estacionaria ambas: en media y en varianza 
  # En conclusión, la serie no es estacionaria
  
# 2. Corregir
  
  # NOTA. Se hacen correciones en la serie cuando no es estacionaria
  # incluso cuando sólo una sea estacionaria
  # NOTA. Cuando el problema es solo en media se aplica diff(serie)
  # Cuando el problema es solo en varianza se aplica diff(serie)
  # Cuando el problema es en ambas se aplica diff(log(serie))
  # El problema es que no sean estacionarias.
  cor1 <- diff(log(gold)) # corrección 1
  plot(cor1, ylim = c(-0.02,0.02))
  
  # Revisando el efecto de la correción
  
  # Media
  # Modelo lineal
  summary(lm(cor1 ~ time(cor1)))
  # Hipótesis
  # Ho: la serie no muestra tendencia (no es significativa)
  # Ha: la serie muestra tendencial (es significativa)
  # Rechazamos Ho si p-valor < alpha
  # 0.1617 < 0.05 por lo tanto No R. Ho, la serie no muestra tendencia
  # NOTA. Si el modelo es no significativo quiere decir que la
  # serie es estacionaria
  # Por lo tanto, la serie es estacionaria en media
  
  # Varianza
  adf.test(cor1, alternative = "stationary")
  # Hipótesis
  # Ho: la serie no es estacionaria
  # Ha: la serie es estacionaria
  # Rechazamos Ho si p-valor < alpha
  # 0.01 < 0.05 por lo tanto R. Ho
  # Por lo tanto, la serie de tiempo es estacionaria en varianza
 
  # NOTA. Debe ser estacionaria ambas: en media y en varianza 
  # En conclusión, la serie es estacionaria
  
# 3. Modelado
  
  # NOTA. En los ciclos solo se meten series estacionarias.
  
  acf(cor1)
  # Sugerencia modelo ARMA
  pacf(cor1)
  
  aic.mejor.arma <- Inf
  for(j in 0:15){
    for(i in 0:15){
      aic2 <- AIC(arima(cor1, order = c(j,0,i), method = "ML"))
      if(aic2 < aic.mejor.arma){
        aic.mejor.arma <- aic2 # Guardamos valor AIC
        pq <- c(j,i) # Guardamos el orden
      }
    }
  }
  aic.mejor.arma
  pq
  # El mejor es ARMA(3,2)
  
  AIC(arima(cor1, order = c(3,0,2), method = "ML")) # Solo valor AIC
  mejor2 <- arima(cor1, order = c(3,0,2), method = "ML")
  res32 <- residuals(mejor2)
  
# 4. Ruido blanco
  
  # Media al rededor del cero entre(-2,2)
  mean(res32)
  # Varianza constante
  plot(res32, type = "p") # Bucamos que no haya patrones
  
  # Correlación (correlograma parcial y ordinario)
  acf(res32)  # ordinario
  pacf(res32) # parcial
  # Las barritas son los grados de oscilación
  # Como los datos no pasan de los límites (franjas azules) los datos son
  # incorrelacionados (son independientes)
  
  Box.test(res32) # buscamos valores pequeños
  # Hipótesis
  # Ho: los residuales son independientes
  # Ha: los residuales no son independientes
  # Rechazamos Ho si p-valor < alpha
  # 0.9937 < 0.05 por lo tanto No R. Ho
  # Es decir, los residuales son independientes (no están relacionados)
  
  # Normalidad
  shapiro.test(res32)
  # Hipótesis
  # Ho: los residuales provienen de una distribución normal
  # Ha: los residuales no provienen de una distribución normal
  # Rechazamos Ho si p-valor < alpha
  # 0.08207 < 0.05 por lo tanto No R. Ho
  # Es decir, los residuales provienen de una distribución normal

  # Solo cuando los datos provengan de una dist. normal:
  # Datos atípicos los que están fuera de (-3,3)
  re <- res32/sqrt(var(res32))
  plot(re, type = "p", ylim = c(-4,4), main = "Outliers")
  abline(h = c(-3,3), col = "orange")
  
  min(re) # es el dato atípico
  which(abs(re) > 3) # es el tiempo en el que está el outlier
  
  # NOTA. Identificar los datos atípicos me sirve para detectar
  # algún evento extraño que haya ocurrido en la línea del tiempo
  # o puede indicar que alguna medición se hizo mal
  
# 5. Predicciones
  
  # Modelo ARMA(3,2)
  pred32 <- predict(mejor2, n.ahead = 10)$pred
  # Sobre la serie estacionaria
  plot(cor1, xlim = c(0,270))
  lines(pred32, col = "magenta")
  
  # Predicciones sobre la serie de tiempo original
  pred.o <- predict(arima(gold, order = c(3,1,2), method = "ML"),
                    n.ahead = 10)$pred
  # Con una diferenciación
  plot(gold, xlim = c(0,270))
  lines(pred.o, col = "blue")
  
  lines(as.numeric(time(gold)),fitted.values(mc), col = "green")