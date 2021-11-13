library(survival)
library(flexsurv)
library(Rcpp)

# Análisis de tratamiento de ratas con modelos paramétricos

data(rats)
help(rats)
summary(rats)
rats

# Respuesta compuesta
y1 <- Surv(rats$time, rats$status)
cox1 <- coxph(y1 ~ rats$sex + rats$litter + rats$rx)
summary(cox1)

# Prueba general
# Hipótesis
# Ho: Todos los coeficientes del vector son cero.
# Ha: Al menos un coeficiente del vector difiere de cero.
# Rechazamos Ho si p-valor < alpha
# 2e-11, 7e-06 y 9e-10 < 0.05, por lo tanto R. Ho

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería litter con 0.11315

cox2 <- coxph(y1 ~ rats$sex + rats$rx)
summary(cox2)

# Análisis de supervivencia

# Estimador de Kaplan - Meier
est.c2 <- survfit(cox2)
summary(est.c2)
plot(est.c2, main = "Estimación de supervivencia general",
     ylab = "Probabilidad de supervivencia", xlab = "Unidad de tiempo")

# Características
s.c2 <- est.c2$surv # Sólo supervivencia
t.c2 <- est.c2$time # Sólo tiempo

# Función de riesgo
r <- rep(0, length(s.c2))
for(i in 2:length(s.c2)){
  r[i] <- (s.c2[i-1]-s.c2[i])/s.c2[i]
}
ri

# Modelo exponencial
plot(t.c2[-1],ri[-1], ylab = "Probabilidad de supervivencia",
     main = "Modelo exponencial", xlab = "Unidad de tiempo")
u <- lm(ri[-1] ~ t.c2[-1])
summary(u)
abline(u, col = "green2")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 0.0152 < 0.05

# R^2 ajustada = 0.08931

# Modelo Weibull
# Versión 1: recta al origen -log(supervivencia) vs tiempo
plot(t.c2, -log(s.c2), ylab = "Probabilidad de supervivencia",
     main = "Modelo Weibull - Versión 1", xlab = "Unidad de tiempo",
     xlim = c(0,105))
v <- lm(-log(s.c2) ~ t.c2)
summary(v)
abline(v, col = "orange")
v2 <- lm(-log(s.c2) ~ 0 + t.c2)
summary(v2)
abline(v2, col = "purple")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.8599

# Modelo Weibull
# Versión 2: recta log(-log(supervivencia)) vs log(tiempo)
plot(log(t.c2), log(-log(s.c2)), ylab = "",
     main = "Modelo Weibull - Versión 2", xlab = "Logaritmo del tiempo")
w <- lm(log(-log(s.c2)[c(-1,-2)]) ~ log(t.c2)[c(-1,-2)])
summary(w)
abline(w, col = "red")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1<>0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.9824

# Modelo Log Logístico
# Buscar recta en logit de supervivencia vs log(t)
# Función logit: logit(a) = log(a/(1-a)
lo <- log(s.c2/(1-s.c2))
plot(log(t.c2), lo, ylab = "Función logísitca de la supervivencia",
     main = "Modelo Loglogístico", xlab = "Logaritmo del tiempo")
f <- lm(lo[c(-1,-2)] ~ log(t.c2)[c(-1,-2)])
summary(f)
abline(f, col ="hotpink1")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.9818

# Modelo Log Normal
# Versión 1: buscamos recta con inversa de normal estándar
plot(log(t.c2), qnorm(1-s.c2), ylab = "Función inversa Normal Est.",
     main = "Modelo Lognormal - Versión 1", xlab = "Logaritmo del tiempo")
k <- lm(qnorm(1-s.c2)[c(-1,-2)] ~ log(t.c2)[c(-1,-2)])
abline(k, col = "blue")
summary(k)

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.9727

# Modelo Log Normal
# Versión 2: buscamos recta con log((1-supervivencia)/supervivencia)
plot(log(t.c2), log((1-s.c2)/s.c2), ylab = "",
     main = "Modelo Lognormal - Versión 2", xlab = "Logaritmo del tiempo")
g <- lm(log((1-s.c2)/s.c2)[c(-1,-2)] ~ log(t.c2)[c(-1,-2)])
summary(g)
abline(g, col = "turquoise3")

# Hipótesis
# Ho: Betha1 = 0 (recta horizontal)
# Ha: Betha1 <> 0 (recta con inclinación)
# Rechazamos Ho si p-valor < aplha
# 2.2e-16 < 0.05

# R^2 ajustada = 0.9818

# Ajuste de datos para el modelo sugerido

w1 <- survreg(y1 ~ rats$sex + rats$rx, dist = "weibull")
summary(w1)

# Prueba de significancia

# Hipótesis
# Ho: β0=β1=β2=0, la regresión lineal no es significativa.
# Ha: Al menos un βj<>0 ∀ jϵ[0,1,2], la regresión lineal es significativa.

# Rechazamos Ho si p-valor < alpha
# 8.1e-12 < 0.05, por lo tanto R. Ho

logn1 <- survreg(y1 ~ rats$sex + rats$rx, dist = "lognormal")
summary(logn1)

# Prueba de significancia

# Hipótesis
# Ho: β0=β1=β2=0, la regresión lineal no es significativa.
# Ha: Al menos un βj<>0 ∀ jϵ[0,1,2], la regresión lineal es significativa.

# Rechazamos Ho si p-valor < alpha
# 7.3e-11 < 0.05, por lo tanto R. Ho

logl1 <- survreg(y1 ~ rats$sex + rats$rx, dist = "loglogistic")
summary(logl1)

# Prueba de significancia

# Hipótesis
# Ho: β0=β1=β2=0, la regresión lineal no es significativa.
# Ha: Al menos un βj<>0 ∀ jϵ[0,1,2], la regresión lineal es significativa.

# Rechazamos Ho si p-valor < alpha
# 2.4e-11 < 0.05, por lo tanto R. Ho

w2 <- flexsurv::flexsurvreg(y1 ~ rats$sex + rats$rx, dist = "weibull")
plot(w2, main = "Modelo Weibull - Ajuste de datos:
     Modelo de Cox", ylab = "Probabilidad de supervivencia",
     xlab = "Unidad de tiempo")
# AIC = 531.13

# aplha = 3.7646 y lambda = 144.2335
# Funciones características:
# riesgo: h(t) = (alpha)(lambda)(t)**(alpha - 1)
# supervivencia: s(t) = exp((-lambda)(t)**(aplha))
# función: f(t) = 

logn2 <- flexsurv::flexsurvreg(y1 ~ rats$sex + rats$rx, dist = "lognormal")
plot(logn2, main = "Modelo Lognormal - Ajuste de datos:
     Modelo de Cox", ylab = "Probabilidad de supervivencia",
     xlab = "Unidad de tiempo")
# AIC = 536.1961

# aplha = 4.9542 y lambda = 0.4960
# Funciones características:
# riesgo: h(t) = (alpha)(lambda)(t)**(alpha - 1)
# supervivencia: s(t) = exp((-lambda)(t)**(aplha))
# función: f(t) = 

logl2 <- flexsurv::flexsurvreg(y1 ~ rats$sex + rats$rx, dist = "llogis")
plot(logl2, main = "Modelo Loglogistico - Ajuste de datos:
     Modelo de Cox", ylab = "Probabilidad de supervivencia",
     xlab = "Unidad de tiempo")
# AIC = 533.401

# aplha = 4.0501 y lambda = 134.8195
# Funciones características:
# riesgo: h(t) = (alpha)(lambda)(t)**(alpha - 1)
# supervivencia: s(t) = exp((-lambda)(t)**(aplha))
# función: f(t) = 

# El modelo elegido de forma parcial es el weibull por tener menor
# AIC = 531.13 con parámetros: función de riesgo, función de
# supervivencia y función de densidad.