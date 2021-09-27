# Modelado de series de tiempo estacionarias

# Consideremos la serie que ya quedó estacionaria
data(lynx)
help(lynx)
plot(lynx)
# Serie estacionaria de la clase 4

# Revisar autocorrelograma
library(tseries)
acf(lynx)   # autocorrelograma ordinario
pacf(lynx)  # autocorrelograma parcial

# Sospechamos de modelo ARMA con parámetros (p,q)

# 1. Encontrar el mejor AR(p)
ar(lynx) # el mejor es AR(8)
help(arima)
# ML = Máxima verosimilitud
arima(lynx, order = c(8,0,0), method = "ML") # AR(8)
arima(lynx, order = c(3,0,0), method = "ML") # AR(3)
# Buscamos valor pequeños en AIC
AIC(arima(lynx, order = c(8,0,0), method = "ML")) # Solo valor AIC
AIC(arima(lynx, order = c(3,0,0), method = "ML")) # Solo valor AIC
    
# 2. Encontrar el mejor MA(q)
arima(lynx, order = c(0,0,1), method = "ML") # MA(1)
arima(lynx, order = c(0,0,2), method = "ML") # MA(2)
arima(lynx, order = c(0,0,3), method = "ML") # MA(3)

# NOTA. En los ciclos solo se meten series estacionarias.

aic.mejor.ma <- Inf
for(i in 1:20){
  aic <- AIC(arima(lynx, order = c(0,0,i), method = "ML"))
  if(aic < aic.mejor.ma){
    aic.mejor.ma <- aic # Guardamos valor AIC
    q <- i # Guardamos el orden
  }
}
aic.mejor.ma
q
# El mejor es MA(10)

# 3. Encontrar el mejor ARMA(p,q)
arima(lynx, order = c(1,0,1), method = "ML") # ARMA(1,1)
arima(lynx, order = c(1,0,2), method = "ML") # ARMA(1,2)
arima(lynx, order = c(1,0,3), method = "ML") # ARMA(1,3)
arima(lynx, order = c(2,0,1), method = "ML") # ARMA(2,1)
arima(lynx, order = c(2,0,2), method = "ML") # ARMA(2,2)
arima(lynx, order = c(2,0,3), method = "ML") # ArMA(3,3)

# NOTA. En los ciclos solo se meten series estacionarias.

aic.mejor.arma <- Inf
for(j in 1:10){
  for(i in 1:10){
    aic2 <- AIC(arima(lynx, order = c(j,0,i), method = "ML"))
    if(aic2 < aic.mejor.arma){
      aic.mejor.arma <- aic2 # Guardamos valor AIC
      pq <- c(j,i) # Guardamos el orden
    }
  }
}
aic.mejor.arma
pq
# El mejor es ARMA(2,4)

# 4. Indicar el modelo que mejor describe mi serie
# Modelo ARMA(2,4)

# Predicciones de los 3 mejores
# Predict() imprime Predicciones y Error estandar de un modelo
p.ar <- predict(arima(lynx, order = c(8,0,0), method = "ML"), n.ahead = 50)$pred
p.ma <- predict(arima(lynx, order = c(0,0,10), method = "ML"), n.ahead = 50)$pred
p.arma <- predict(arima(lynx, order = c(2,0,4), method = "ML"), n.ahead = 50)$pred

# Gráfica predicciones
plot(lynx, xlim = c(1820,1985), main = "Mejores modelos: Adornos anuales de 
     lince canadiense 1821-1934")
lines(p.ar, col = "red")
lines(p.ma, col = "blue")
lines(p.arma, col = "green")
legend("topright",c("Modelo AR","Modelo MA","Modelo ARMA"),
       col = c("red","blue","green"), lty = 1)