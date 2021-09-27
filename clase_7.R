# INCOMPLETO
# Revisión de ruido blanco

# de la serie estacionaria lynx ARMA(2,4)
elegido.l <- arima(lynx, order = c(2,0,4), method = "ML")
res24 <- residuals(elegido.l)
# Media al rededor del cero (que no pase del (-2,2))
mean(res24)
mean(res24)/var(res24) # menor que 2

# Varianza constante
plot(res24, type = "p")
abline(h=0)

# Independientes o incorrelacionados
acf(res24, ci = 0.9999)     # gráfica
pacf(res24, ci = 0.9999)    # residuales incorrelacionados
# Un alpha más chiquito hace una confianza más grande

Box.test(res24)
# Analítica
# Ho: los residuales son independientes
# Ha: los residuales no son independientes
# Rechazamos Ho si p-valor < alpha
# 0.97 < 0.05 por lo tanto No R. Ho

# Normales
shapiro.test(res24)
# Ho: los residuales provienen de una distribución normal
# Ha: los residuales no provienen de una distribución normal
# Rechazamos Ho si p-valor < alpha
# 5.831e-06 < 0.05 por lo tanto R. Ho

# residuales como ruido blanco no gaussiano

# de la serie no estacionaria austres
# serie estacionaria diff(diff(log(austres))) ARMA(2,3)
data(austres)
c2 <- diff(diff(log(austres)))
elegido.c2 <- arima(c2, order = c(2,0,3), method = "ML")
elegido.c1 <- arima(diff(log(austres)), order = c(2,1,3), method = "ML")
# 2,1,3 donde 1 <- diferenciado una vez

res23 <- residuals(elegido.c2)
# Media al rededor del cero (que no pase del (-2,2))
mean(res23)

# Varianza
plot(res23, type = "p", ylim = c(-0.01,0.01))
abline(h = 0)

# Correlación
acf(res23)
pacf(res23)
Box.test(res23)
# Analítica
# Ho: los residuales son independientes
# Ha: los residuales no son independientes
# Rechazamos Ho si p-valor < alpha
# 0.81 < 0.05 por lo tanto No R. Ho

# Normalidad
shapiro.test(res23)
# Ho: los residuales provienen de una distribución normal
# Ha: los residuales no provienen de una distribución normal
# Rechazamos Ho si p-valor < alpha
# 9.125e-05 < 0.05 por lo tanto R. Ho
