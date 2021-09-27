# Analisis de regresión lineal simple

data(women)
plot(women, main = "Grafica de disperción")
# y = weight, x = height

# Revisando modelo lineal
rl <- lm(women$weight ~ women$height) # lm es "lineal model"
# el modelo de recta estimado: peso estimado = 3.45, altura = -87.52
anova(rl) # tabla
# Ho: betha 1 = 0, la regresión lineal no es significativa
# Ha: betha 1 <> 0, la regresión lineal es significativa

# Por P-valor rechazamos Ho si
# P-valor = 1.091e-14 < alpha = 0.05
# Por lo tanto, R.Ho
# En conclusión, el modelo es significativo con 95% de confianza

# Revisando intervalos
confint(rl)
# Sin evidencia
# NOTA. Si el inervalo pasa por el cero, esto indica que hay regresión lineal
# al origen
# NOTA. Alpha grande = intervalo grande, alpha pequeño = intervalo pequeño

# Si hubiera regresión lineal al origen
rlo <- lm(women$weight ~ 0 + women$height)

# Revisando ajuste 
summary(rl)
summary(rl)$adj.r.squared
# R^2 ajustado = 0.9903
summary(rl)$sigma

# Gráfica base con linea de tendencia
plot(women, main = "Comparación modelo lineal vs modelo lineal al origen",
     xlim = c(0,73), ylim = c(0,170))
abline(rl, col = "green")   # linea de regresion lineal
abline(rlo, col = "purple") # linea de regresión lineal al origen
fitted.values(rl)
points(women$height[14],157.4333) # Pinta un punto
residuals(rl) # errores

# Revisando modelo exponencial
# y gorro = (betha sub cero)[e^ (betha sub 1)(x)]
# transformando
y. <- log(women$weight)
rla <- lm(y. ~ women$height)  # regresión lineal asociada
anova(rla) # tabla
summary(rla)
# R^2 ajustado = 0.9976
# modelo lineal asociado y. estimada = 3.27 + 0.025x
B0 <- exp(coefficients(rla)[1])
# y gorro = 26.44 e^ 0.025x Modelo exponencial
fitted.values(rla) # valores estimados, y gorro
y.est.exp <- B0*exp(coefficients(rla)[2]*women$height)

# Graficando modelo exponencial
plot(women, main = "Ajuste del modelo exponencial")
lines(women$height,y.est.exp, col = "magenta")

# Revisando modelo cuadrático
rc <- lm(women$weight ~ poly(women$height, degree = 2))
anovac(rc)
sumarry(rc)
# P-valor = 2.2e^-16
# Comparación modelo exponencial vs modelo cuadrático
plot(women, main = "Comparación modelo exponencial vs modelo cuadrático")
lines(women$height,y.est.exp, col = "magenta")
lines(women$height, fitted.values(rc), col = "blue")
# legend(Posición, c(titulos), c(colores), tipo de linea)
# Posición : bottomright, bottom, bottomleft, left, topleft, top, topright, right, center
# Tipo de linea: lty <- linea, pch <- punto
legend("bottomright",c("Modelo exponencial","Modelo cuadrático"),
       col = c("magenta","blue"), lty = 1)