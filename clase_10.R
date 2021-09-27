library(TSA)
library(tseries)

setwd("C:/Users/DarioJaciel/Desktop/Zuky/Rstudio/Estadística apliacada")
tmp <- read.table("SerieTempMaxMty.csv", sep = ",", header = T)
smty <- ts(tmp$Temp.Max, start = c(2017,10), end = c(2020,5), frequency =  12)
plot(smty)
start(smty)
time(smty)
help(smty) # Serie de tiempo mensual

# Pasos para el análisis de una serie de tiempo

# 1. Clasificar la serie

  # Media
  # Modelo lineal
  tend <- lm(smty ~ time(smty))
  summary(tend)
  # Hipótesis
  # Ho: la serie no muestra tendencia (no es significativa)
  # Ha: la serie muestra tendencial (es significativa)
  # Rechazamos Ho si p-valor < alpha
  # 0.6674 < 0.05 por lo tanto No R. Ho, la serie no muestra tendencia
  # NOTA. El r^2 ajustado nos sirve para confirmar que no hay tendencia
  # NOTA. Si el modelo es no significativo quiere decir que la
  # serie es estacionaria
  # Por lo tanto, la serie es estacionaria en media
  abline(tend, col = "magenta")
  
  # Varianza
  adf.test(smty, alternative = "stationary") # Prueba Dickey Fuller aumentada
  # Hipótesis
  # Ho: la serie no es estacionaria
  # Ha: la serie es estacionaria
  # Rechazamos Ho si p-valor < alpha
  # 0.3582 < 0.05 por lo tanto No R. Ho
  # Por lo tanto, la serie no es estacionaria en varianza
  
  # NOTA. Debe ser estacionaria ambas: en media y en varianza
  # En conclusión, la serie de tiempo no es estacionaria
  
# 2. Corregir
  
  # NOTA. Se hacen correciones en la serie cuando no es estacionaria
  # incluso cuando sólo una sea estacionaria
  # NOTA. Cuando el problema es solo en media se aplica diff(serie)
  # Cuando el problema es solo en varianza se aplica diff(serie)
  # Cuando el problema es en ambas se aplica diff(log(serie))
  # El problema es que no sean estacionarias.
  
  # corrección 1
  cn1 <- diff(smty)
  plot(cn1)
  
  # Revisando el efecto de la correción 1
  
    # Media
    # Modelo lineal
    summary(lm(cn1 ~ time(cn1)))
    # Hipótesis
    # Ho: la serie no muestra tendencia (no es significativa)
    # Ha: la serie muestra tendencial (es significativa)
    # Rechazamos Ho si p-valor < alpha
    # 0.9204 < 0.05 por lo tanto No R. Ho, la serie no muestra tendencia
    # NOTA. Si el modelo es no significativo quiere decir que la
    # serie es estacionaria
    # Por lo tanto, la serie es estacionaria en media
    
    # Varianza
    adf.test(cn1, alternative = "stationary")
    # Hipótesis
    # Ho: la serie no es estacionaria
    # Ha: la serie es estacionaria
    # Rechazamos Ho si p-valor < alpha
    # 0.1932 < 0.05 por lo tanto No R. Ho
    # Por lo tanto, la serie no es estacionaria en varianza
    
    # NOTA. Debe ser estacionaria ambas: en media y en varianza 
    # En conclusión, la serie de tiempo no es estacionaria
  
  # corrección 2
  cn2 <- diff(cn1)
  plot(cn2)
  
  # Revisando el efecto de la correción 2
  
    # Media
    # Modelo lineal
    summary(lm(cn2 ~ time(cn2)))
    # Hipótesis
    # Ho: la serie no muestra tendencia (no es significativa)
    # Ha: la serie muestra tendencial (es significativa)
    # Rechazamos Ho si p-valor < alpha
    # 0.9819 < 0.05 por lo tanto No R. Ho, la serie no muestra tendencia
    # NOTA. Si el modelo es no significativo quiere decir que la
    # serie es estacionaria
    # Por lo tanto, la serie es estacionaria en media
    
    # Varianza
    adf.test(cn2, alternative = "stationary")
    # Hipótesis
    # Ho: la serie no es estacionaria
    # Ha: la serie es estacionaria
    # Rechazamos Ho si p-valor < alpha
    # 0.01 < 0.05 por lo tanto R. Ho
    # Por lo tanto, la serie es estacionaria en varianza
    
    # NOTA. Debe ser estacionaria ambas: en media y en varianza 
    # En conclusión, la serie de tiempo es estacionaria

# 3. Modelado
    
  # NOTA. En los ciclos solo se meten series estacionarias.

  aic.mejor.arma <- Inf
  for(j in 0:10){
    for(i in 0:10){
      aic2 <- AIC(arima(cn2, order = c(j,0,i), method = "ML"))
      if(aic2 < aic.mejor.arma){
        aic.mejor.arma <- aic2 # Guardamos valor AIC
        pdq <- c(j,0,i) # Guardamos el orden
      }
    }
  }
  aic.mejor.arma
  pdq
  # El mejor es ARMA(1,1) DATOS CORREGIDOS cn2
  
  AIC(arima(cn2, order = c(1,0,1), method = "ML")) # Solo valor AIC
  mej <- arima(cn2, order = c(1,0,1), method = "ML")
  res11 <- residuals(mej)
  
# 4. Ruido blanco
  
  # Media al rededor del cero entre(-2,2)
  mean(res11)
  # Varianza constante
  plot(res11, type = "p") # Bucamos que no haya patrones
  
  # Correlación (correlograma parcial y ordinario)
  acf(res11)  # ordinario
  pacf(res11) # parcial
  # Las barritas son los grados de oscilación
  # Como los datos no pasan de los límites (franjas azules) los datos son
  # incorrelacionados (son independientes)
  
  Box.test(res11) # buscamos valores pequeños
  # Hipótesis
  # Ho: los residuales son independientes
  # Ha: los residuales no son independientes
  # Rechazamos Ho si p-valor < alpha
  # 0.9874 < 0.05 por lo tanto No R. Ho
  # Es decir, los residuales son independientes (no están relacionados)
  
  # Normalidad
  shapiro.test(res11)
  # Hipótesis
  # Ho: los residuales provienen de una distribución normal
  # Ha: los residuales no provienen de una distribución normal
  # Rechazamos Ho si p-valor < alpha
  # 0.1888 < 0.05 por lo tanto No R. Ho
  # Es decir, los residuales provienen de una distribución normal
  
  # Solo cuando los datos provengan de una dist. normal:
  # Datos atípicos los que están fuera de (-3,3)
  r <- res11/sqrt(var(res11)) # residuales/desviación est(residuales)
  plot(r, type = "p", ylim = c(-4,4), main = "Outliers")
  abline(h = c(-3,3), col = "orange")
  # Sin valores atípicos

# 5. Predicciones
  
  # Modelo ARMA(1,1)
  pred11 <- predict(mej, n.ahead = 12)$pred
  # Sobre la serie estacionaria
  plot(cn2, xlim = c(2018,2022), ylim = c(-10,10), main = "Correción
       2 - Predicciones")
  lines(pred11, col = "magenta")
  
  # Predicciones sobre la serie de tiempo original
  # Modelo ARIMA(1,2,1) DATOS ORIGINALES
  pred.ogn <- predict(arima(smty, order = c(1,2,1), method = "ML"),
                    n.ahead = 12)$pred
  # 2 porque hubo dos correcciones; dos diferenciaciones
  # El parámetro de las diferenciaciones es d, ARIMA(p,d,q)
  # Con una diferenciación
  
  # ¿Cuál sería la predicción para diciembre del 2020?
  pred.ogn # dice que 39.83544
  
  plot(smty, xlim = c(2018,2022), main = "Temperaturas máxmas en MTY -
       Predicciones")
  lines(pred.ogn, col = "blue")
  