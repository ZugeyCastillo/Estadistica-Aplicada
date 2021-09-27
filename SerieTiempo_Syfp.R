library(TSA)
library(tseries)

setwd("C:/Users/DarioJaciel/Desktop/Zuky/Rstudio/Estadística apliacada")
datos <- read.table("SerieTiempo_syfp_pyr.csv", sep = ",", header = T)
serie <- ts(datos$Valor, start = 1987, end = 2021, frequency =  4)
plot(serie, main = "Seguros y fondos de pensiones: 
     Primas y reservas", xlim = c(1987,2021),
     xlab = "Años", ylab = "Reserva")
start(serie)
end(serie)
time(serie)
help(serie) # Serie de tiempo trimestral

# Pasos para el análisis de una serie de tiempo

# 1. Clasificar la serie

  # Media
  # Modelo lineal
  lml <- lm(serie ~ time(serie))
  summary(lml)
  # Hipótesis
  # Ho: la serie no muestra tendencia (no es significativa)
  # Ha: la serie muestra tendencial (es significativa)
  # Rechazamos Ho si p-valor < alpha
  # 2.2e-16 < 0.05 por lo tanto R. Ho, la serie muestra tendencia
  # NOTA. Si el modelo es no significativo quiere decir que la
  # serie es estacionaria
  # Por lo tanto, la serie no es estacionaria en media
  abline(lml, col = "magenta")
  
  # Varianza
  adf.test(serie, alternative = "stationary") # Prueba Dickey Fuller aumentada
  # Hipótesis
  # Ho: la serie no es estacionaria
  # Ha: la serie es estacionaria
  # Rechazamos Ho si p-valor < alpha
  # 0.4038 < 0.05 por lo tanto No R. Ho
  # Por lo tanto, la serie no es estacionaria en varianza
  
  # En conclusión, la serie de tiempo no es estacionaria

# Dado que la serie es no estacionaria en este punto, podemos
# checar que metodo le funcionaria mejor

# Modelo Lineal
mlin<-lm(serie ~ time(serie), data = serie)
s1p <- summary(mlin) # R^2 ajustada = 0.7147
mean(s1p$residuals**2) # MSE = 17866032235
anova(mlin)['Residuals', 'Mean Sq']
plot(serie, main = "Modelo Lineal", type = "p", xlim = c(1986,2021),
     xlab = "Años", ylab = "Reserva")
abline(mlin, col = "blue")
legend("topleft","Modelo Lineal", col = "blue", lty = 1)

# Forma lineal: y gorro = β0+β1x
# y gorro = -42650352+21507x

# Modelo lineal asociado: y gorro = -42650352+21507x

# Prueba F Hipótesis
# Ho: β0=β1=0 (el modelo no es significativo).
# Ha: Al menos una betha es diferente de 0 (el modelo es significativo).

# Rechazamos Ho si p-valor < α
# 2.2e-16 < 0.05, por lo tanto R. Ho
# Al menos una betha es diferente de 0, el modelo es significativo.

# Modelo Cuadrático
mc <- lm(serie ~ poly(as.numeric(time(serie)), degree = 2))
s2p <- summary(mc) # R^2 ajustada = 0.738
mean(s2p$residuals**2) # MSE = 16285783758
anova(mc)['Residuals', 'Mean Sq']
plot(serie, main = "Modelo Cuadrático", type = "p",
     xlim = c(1986,2021), xlab = "Años", ylab = "Reserva")
tp.graf <- seq(from = 1987, to = 2021, by = 0.250)
lines(tp.graf, mc$fitted.values, col = "red")
legend("topleft","Modelo Cuadrático", col = "red", lty = 1)

# Forma lineal: y gorro = β0x^2 + β1x + β2
# y gorro = -4.547e+02x^2 + 1.844e+06x - 1.869e+09

# Sin modelo lineal asociado dado que el modelo cuadrático no es
# un modelo linealizable.

# Prueba F Hipótesis
# Ho: β0=β1=0 (el modelo no es significativo).
# Ha: Al menos una betha es diferente de 0 (el modelo es significativo).

# Rechazamos Ho si p-valor < α
# 2.2e-16 < 0.05, por lo tanto R. Ho
# Al menos una betha es diferente de 0, el modelo es significativo.

# Modelo Exponencial
mexp <- lm(log((serie), exp(1)) ~ time(serie), data = serie)
s3p <- summary(mexp)
exp(coefficients(mexp)[1])
mean(s3p$residuals**2) # MSE = 0.1054113
summary(mexp) # R^2 ajustada = 0.7703 
anova(mexp)['Residuals', 'Mean Sq']
plot(serie, main = "Modelo Exponencial", type = "p", 
     xlim = c(1986,2021), xlab = "Años", ylab = "Reserva")
tp.graf <- seq(from = 1987, to = 2021, by = 0.250)
lines(tp.graf, exp(fitted.values(mexp)), col = "green")
legend("topleft","Modelo Exponencial", col = "green", lty = 1)

# Forma lineal: y gorro = β0e^(β1x)
# e^(ln(β0)) -> e^(-1.083e+02) = 9.24501E-48
# y gorro = 9.24501E-48 * e^[(6.042e-02)*x]

# Modelo lineal asociado: y gorro = -1.083e+02+6.042e-02x 

# Prueba F Hipótesis
# Ho: β0=β1=0 (el modelo no es significativo).
# Ha: Al menos una betha es diferente de 0 (el modelo es significativo).

# Rechazamos Ho si p-valor < α
# 2.2e-16 < 0.05, por lo tanto R. Ho
# Al menos una betha es diferente de 0, el modelo es significativo.

# Modelo Logarítmico
mlog <- lm(serie ~ log10(time(serie)), data = serie)
s4p <- summary(mlog)
mean(s4p$residuals**2) # MSE = 17829098668
summary(mlog) # R^2 ajustada = 0.7153  
anova(mlog)['Residuals', 'Mean Sq']
plot(serie, main = "Modelo Logarítmico", type = "p", 
     xlim = c(1987,2021), xlab = "Años", ylab = "Reserva")
tp.graf <- seq(from = 1987, to = 2021, by = 0.250)
lines(tp.graf, fitted.values(mlog), col = "magenta")
legend("topleft","Modelo Logarítmico", col = "magenta", lty = 1)

# Forma lineal: y gorro = β0 + β1log(x)
# y gorro = -327367008 + 99281627 * log(x)

# Modelo lineal asociado: y gorro = -327367008+99281627x 

# Prueba F Hipótesis
# Ho: β0=β1=0 (el modelo no es significativo).
# Ha: Al menos una betha es diferente de 0 (el modelo es significativo).

# Rechazamos Ho si p-valor < α
# 2.2e-16 < 0.05, por lo tanto R. Ho
# Al menos una betha es diferente de 0, el modelo es significativo.

# Modelo Potencial
mp <- lm(log10(serie) ~ log10(time(serie)), data = serie)
s5p <- summary(mp)
mean(s5p$residuals**2) # MSE = 0.01977427
summary(mp) # R^2 ajustada = 0.7715
anova(mp)['Residuals', 'Mean Sq']
plot(serie, main = "Modelo Potencial", type = "p", 
     xlim = c(1987,2021), xlab = "Años", ylab = "Reserva")
tp.graf <- seq(from = 1987, to = 2021, by = 0.250)
lines(tp.graf, 10^(fitted.values(mp)), col = "yellow3")
legend("topleft","Modelo Potencial", col = "yellow3", lty = 1)

# Forma lineal: y gorro = 10^log(β0) * x^β1
# 10^log(β0) --> 10^-394.527 = 4.5614E-171
# y gorro = 4.5614E-171 * x^121.171

# Modelo lineal asociado: y gorro = -394.527+121.171x 

# Prueba F Hipótesis
# Ho: β0=β1=0 (el modelo no es significativo).
# Ha: Al menos una betha es diferente de 0 (el modelo es significativo).

# Rechazamos Ho si p-valor < α
# 2.2e-16 < 0.05, por lo tanto R. Ho
# Al menos una betha es diferente de 0, el modelo es significativo.

# Modelo Cúbico
mcub <- lm(serie ~ poly(as.numeric(time(serie)), degree = 3))
s6p <- summary(mcub) # R^2 ajustada = 0.787 
mean(s6p$residuals**2) # MSE = 13140195054
anova(mcub)['Residuals', 'Mean Sq']
plot(serie, main = "Modelo Cúbico", type = "p",
     xlim = c(1986,2021), xlab = "Años", ylab = "Reserva")
tp.graf <- seq(from = 1987, to = 2021, by = 0.250)
lines(tp.graf, mcub$fitted.values, col = "turquoise")
legend("topleft","Modelo Cúbico", col = "turquoise", lty = 1)

# Forma lineal: y gorro = β3x^3 + β2x^2 + β1x + β0
# y gorro = -656465x^3 - 465289x^2 + 2488882x + 450246

# Sin modelo lineal asociado dado que el modelo cuadrático no es
# un modelo linealizable.

# Prueba F Hipótesis
# Ho: β0=β1=0 (el modelo no es significativo).
# Ha: Al menos una betha es diferente de 0 (el modelo es significativo).

# Rechazamos Ho si p-valor < α
# 2.2e-16 < 0.05, por lo tanto R. Ho
# Al menos una betha es diferente de 0, el modelo es significativo.

# Gráfica comparativa de Modelos
plot(serie, main = "Grafica comparativa de modelos", type = "p", 
     xlim = c(1980,2021), xlab = "Años", ylab = "Reservas")
abline(mlin, col = "blue")
tp.graf <- seq(from = 1987, to = 2021, by = 0.250)
lines(tp.graf, mc$fitted.values, col = "red")
lines(tp.graf, exp(fitted.values(mexp)), col = "green")
lines(tp.graf, fitted.values(mlog), col = "magenta")
lines(tp.graf, 10^(fitted.values(mp)), col = "yellow3")
lines(tp.graf, mcub$fitted.values, col = "turquoise")
legend(x = "topleft", inset = 0.01, legend = 
         c("Modelo Lineal", "Modelo Cuadrático",
           "Modelo Exponencial", "Modelo Logarítmico",
           "Modelo Potencial", "Modelo Cúbico"),
       col = c("blue","red","green","magenta","yellow3","turquoise"),
       lty = c(1,1,1), cex = 0.7, title = "Modelos de tendencia")

# En conclusión, el modelo que más se apega al comportamiento de 
# la serie de tiempo es el Modelo Cúbico ya que su R^2 ajustada 
# es la de mayor valor de entre los seis modelos.
  
# 2. Corregir    
  
  # Dado que la serie no es estacionaria se ocupa corregir.
  # corrección 1
  c1 <- diff(log(serie))
  plot(c1, main = "Corrección 1 - Seguros y fondos 
       de pensiones: Primas y reservas", 
       xlim = c(1987,2021), xlab = "Años", ylab = "Reserva")
  
  # Revisando el efecto de la correción 1
  
  # Media
  # Modelo lineal
  lm <- lm(c1 ~ time(c1))
  summary(lm)
  # Hipótesis
  # Ho: la serie no muestra tendencia (no es significativa)
  # Ha: la serie muestra tendencial (es significativa)
  # Rechazamos Ho si p-valor < alpha
  # 0.2702 < 0.05 por lo tanto No R. Ho, la serie no muestra tendencia
  # NOTA. Si el modelo no es significativo quiere decir que la
  # serie es estacionaria
  # Por lo tanto, la serie es estacionaria en media
  abline(lm, col = "magenta")
  
  # Varianza
  adf.test(c1, alternative = "stationary")
  # Hipótesis
  # Ho: la serie no es estacionaria
  # Ha: la serie es estacionaria
  # Rechazamos Ho si p-valor < alpha
  # 0.01 < 0.05 por lo tanto R. Ho
  # Por lo tanto, la serie es estacionaria en varianza
  
  # En conclusión, la serie de tiempo es estacionaria
  
# 3. Modelado
  
  # NOTA. En los ciclos solo se meten series estacionarias.
  
  aic.mejor.arma <- Inf
  for(j in 0:10){
    for(i in 0:10){
      aic2 <- AIC(arima(c1, order = c(j,0,i), method = "ML"))
      if(aic2 < aic.mejor.arma){
        aic.mejor.arma <- aic2 # Guardamos valor AIC
        pdq <- c(j,0,i) # Guardamos el orden
      }
    }
  }
  aic.mejor.arma
  pdq
  # El mejor es ARMA(0,1) DATOS CORREGIDOS c1
  # El modelo se vuelve un MA(1)
  
  AIC(arima(c1, order = c(0,0,1), method = "ML")) # Solo valor AIC
  mejor <- arima(c1, order = c(0,0,1), method = "ML")
  res01 <- residuals(mejor)

# 4. Ruido blanco
  
  # Media al rededor del cero entre(-2,2)
  mean(res01)
  # Varianza constante
  plot(res01, type = "p", main = "Varianza constante",
       xlim = c(1987,2021), xlab = "Años", ylab = "Reserva")
  
  # Correlación
  acf(res01, main = "Correlograma ordinario")
  pacf(res01, main = "Correlograma parcial")
  # Como los datos no pasan de los límites (franjas azules) los datos son
  # incorrelacionados (son independientes)
  
  Box.test(res01)
  # Hipótesis
  # Ho: los residuales son independientes
  # Ha: los residuales no son independientes
  # Rechazamos Ho si p-valor < alpha
  # 0.994 < 0.05 por lo tanto No R. Ho
  # Es decir, los residuales son independientes (no están relacionados)
  
  # Normalidad
  shapiro.test(res01)
  # Hipótesis
  # Ho: los residuales provienen de una distribución normal
  # Ha: los residuales no provienen de una distribución normal
  # Rechazamos Ho si p-valor < alpha
  # 0.03234 < 0.05 por lo tanto R. Ho
  # Es decir, los residuales no provienen de una distribución normal
  
# 5. Predicciones
  
  # Modelo MA(0,1) DATOS CORREGIDOS c1
  pred01 <- predict(mejor, n.ahead = 6)$pred
  # Sobre la serie estacionaria
  plot(c1, main = "Corrección 1 - Predicciones", ylim = c(-0.45,0.45),
       xlim = c(1987,2022), xlab = "Años", ylab = "Reserva")
  lines(pred01, col = "magenta")
  legend("topleft","Modelo MA(1)", col = "magenta", lty = 1)
  
  # Predicciones sobre la serie de tiempo original
  # Modelo ARIMA(0,1,1) DATOS ORIGINALES serie
  pred.o <- predict(arima(serie, order = c(0,1,1), method = "ML"),
                      n.ahead = 6)$pred
  plot(serie, main = "Seguros y fondos de pensiones: 
     Primas y reservas - Predicciones", xlim = c(1987,2022),
       xlab = "Años", ylab = "Reserva")
  lines(pred.o, col = "green")
  legend("topleft","Modelo ARIMA(0,1,1)", col = "green", lty = 1)
  
  # Ecuación del Modelo ARIMA(0,1,1)
  arima(serie, order = c(0,1,1), method = "ML")
  # x't = c + 0.0024e't-1 + et'
