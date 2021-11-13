library(survival)
library(flexsurv)

# Análisis de diabetes con modelos paramétricos

data(diabetic)
d2 <- diabetic[diabetic$trt == 1,]
# Se quitaron los ids sin tratamiento (trt = 0) ya que aunque no
# tuvieran tratamiento tenían datos en las demás variables como si
# hubieran tenido tratamiento. Esto no es congruente.

# Respuesta compuesta
rescom <- Surv(d2$time, d2$status)

mod1 <- coxph(rescom ~ d2$laser + d2$age + d2$eye
              + d2$risk)
summary(mod1)

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería age con 0.98935

mod2 <- coxph(rescom ~ d2$laser + d2$eye + d2$risk)
summary(mod2)

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería risk con 0.53484

mod3 <- coxph(rescom ~ d2$laser + d2$eye)
summary(mod3)

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería laser con 0.15896

mod4 <- coxph(rescom ~ d2$eye)
summary(mod4)

# Prueba general: Contrastes

# Hipótesis
# Ho: Los coeficientes del vector son iguales a cero.
# Ha: Al menos un coeficiente del vector difiere.
# Rechazamos Ho si p-valor < alpha

# 0.002, 0.003 y 0.002 < 0.05, por lo tanto R. Ho

# Análisis de supervivencia

# Estimador de Kaplan - Meier
sup.d2 <- survfit(mod4)
summary(sup.d2)
plot(sup.d2, main = "Estimación de supervivencia general",
     ylab = "Probabilidad de supervivencia", xlab = "Unidad de tiempo")

# Características
t <- sup.d2$time # sólo el tiempo
s <- sup.d2$surv # sólo la supervivencia

# Calcular función de riesgo
r <- rep(0, length(t))
for(i in 2:length(t)){
  r[i] <- (s[i-1]-s[i])/s[i]
}
ri <- r[-1]

# Modelo exponencial
plot(t[-1],ri, ylim = c(0,0.5), ylab = "Probabilidad de supervivencia",
     main = "Modelo exponencial", xlab = "Unidad de tiempo")
u <- lm(ri ~ t[-1])
summary(u)
abline(u, col = "green2")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 1.309e-06 < 0.05

# R^2 ajustada = 0.1168

# Modelo Weibull
# Versión 1: recta al origen -log(supervivencia) vs tiempo
plot(t, -log(s), ylab = "Probabilidad de supervivencia",
     main = "Modelo Weibull - Versión 1", xlab = "Unidad de tiempo")
v <- lm(-log(s) ~ t)
summary(v)
abline(v, col = "orange")
v2 <- lm(-log(s) ~ 0 + t)
summary(v2)
abline(v2, col = "purple")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.9783

# Modelo Weibull
# Versión 2: recta log(-log(supervivencia)) vs log(tiempo)
plot(log(t), log(-log(s)), ylab = "",
     main = "Modelo Weibull - Versión 2", xlab = "")
w <- lm(log(-log(s))[-1] ~ log(t)[-1])
summary(w)
abline(w, col = "red")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.9483

# Modelo Log Normal
# Versión 1: buscamos recta con inversa de normal estándar
plot(log(t), qnorm(1-s), ylab = "",
     main = "Modelo Lognormal - Versión 1", xlab = "")
k <- lm(qnorm(1-s)[-1] ~ log(t)[-1])
abline(k, col = "blue")
summary(k)

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.9776

# Modelo Log Normal
# Versión 2: buscamos recta con log((1-supervivencia)/supervivencia)
plot(log(t), log((1-s)/s), ylab = "",
     main = "Modelo Lognormal - Versión 2", xlab = "")
g <- lm(log((1-s)/s)[-1] ~ log(t)[-1])
summary(g)
abline(g, col = "turquoise3")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.9544

# Modelo Log Logístico
# Buscar recta en logit de supervivencia vs log(t)
# Función logit: logit(a) = log(a/(1-a)
lo <- log(s/(1-s))
plot(log(t), lo, ylab = "",
     main = "Modelo Loglogístico", xlab = "")
f <- lm(lo[-1] ~ log(t)[-1])
summary(f)
abline(f, col ="hotpink1")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.9544

# Ajuste de datos para el modelo sugerido

weibull <- survreg(rescom ~ d2$eye, dist = "weibull")
summary(weibull)

# Prueba de significancia

# Hipótesis
# Ho: β0=β1=β2=0, la regresión lineal no es significativa.
# Ha: Al menos un βj<>0 ∀ jϵ[0,1,2], la regresión lineal es significativa.

# Rechazamos Ho si p-valor < alpha
# 0.0012 < 0.05, por lo tanto R. Ho

logn <- survreg(rescom ~ d2$eye, dist = "lognormal")
summary(logn)

# Prueba de significancia

# Hipótesis
# Ho: β0=β1=β2=0, la regresión lineal no es significativa.
# Ha: Al menos un βj<>0 ∀ jϵ[0,1,2], la regresión lineal es significativa.

# Rechazamos Ho si p-valor < alpha
# 0.005 < 0.05, por lo tanto R. Ho

weibull2 <- flexsurv::flexsurvreg(rescom ~ d2$eye, dist = "weibull")
summary(weibull2)
# AIC = 634.5868

summary(weibull2)[1] # Ojo derecho
summary(weibull2)[2] # Ojo izquierdo
plot(weibull2, main = "Modelo Weibull - Ajuste de datos:
     Modelo de Cox", ylab = "Probabilidad de supervivencia",
     xlab = "Unidad de tiempo")
legend("bottomleft", c("Ojo derecho","Ojo Izquierdo","Supervivencia"), 
       col = c("black","black","red"), lty = 1)

# aplha = 0.7957 y lambda = 405.48
# Funciones características:
# riesgo: h(t) = (alpha)(lambda)(t)**(alpha - 1)
# supervivencia: s(t) = exp((-lambda)(t)**(aplha))
# función: f(t) = 

logn2 <- flexsurv::flexsurvreg(rescom ~ d2$eye, dist = "lognormal")
summary(logn2)
# AIC = 631.6413
plot(logn2, main = "Modelo Lognormal - Ajuste de datos:
     Modelo de Cox", ylab = "Probabilidad de supervivencia",
     xlab = "Unidad de tiempo")
legend("bottomleft", c("Ojo derecho","Ojo Izquierdo","Supervivencia"), 
       col = c("black","black","red"), lty = 1)

# aplha = 5.699 y lambda = 2.070
# Funciones características:
# riesgo: h(t) = (alpha)(lambda)(t)**(alpha - 1)
# supervivencia: s(t) = exp((-lambda)(t)**(aplha))
# función: f(t) = 

# El modelo elegido de forma parcial es el lognormal por tener menor
# AIC = 631.6413 con parámetros: función de riesgo, función de
# supervivencia y función de densidad.
