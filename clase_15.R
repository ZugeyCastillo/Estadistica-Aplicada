library(survival)

# Análisis de diabetes

setwd("C:/Users/DarioJaciel/Desktop/Zuky/Rstudio/Estadística apliacada")
diab <- read.table("Diabetes.csv", sep = ",", header = T)
diab

# NOTA. 1 <- presencia del evento
#       0 <- ausencia del evento

# Objetivos:
# 1. Revisar si mis variables estáb bien seleccionadas.
# 2. Variables significativas para la 
# 3. Revisar el modelo adecuado

# Estadística descriptiva

table(diab$censor, diab$sexo) # table(y, x)
table(diab$censor, diab$diabetes)
table(diab$censor, diab$edad)

# Cada variable (nombre de cada columna) tienen su categoría.
# Por ejemplo, la variable sexo tiene 2 categorías: H y M. LA
# variable diabetes no tiene categoría pero se le pueden hacer.

# Respuesta compuesta

r.comp <- Surv(diab$meses, diab$censor) # Surv(tiempo, status)
m1 <- coxph(r.comp ~ diab$sexo + diab$diabetes + diab$edad + 
             diab$IMC) # coxph(y ~ x)
summary(m1)

# Quitamos la variable con mayor probabilidad en Pr(>|z|), en
# este caso sería edad con 0.8317

# Modelo de coxph sin la variable edad
m2 <- coxph(r.comp ~ diab$sexo + diab$diabetes + 
             diab$IMC) # coxph(y ~ x)
summary(m2)

# Quitamos la variable con mayor probabilidad en Pr(>|z|), en
# este caso sería diabetes con 0.7575

# Modelo de coxph sin las variables edad y diabetes
m3 <- coxph(r.comp ~ diab$sexo + diab$IMC) # coxph(y ~ x)
summary(m3)

# Quitamos la variable con mayor probabilidad en Pr(>|z|), en
# este caso sería IMC con 0.2567

# Modelo de coxph sin las variables edad, diabetes y IMC
m4 <- coxph(r.comp ~ diab$sexo) # coxph(y ~ x)
summary(m4)

# Análisis de supervivencia

# Estimador de Kaplan - Meier
cox.diab <- survfit(m4)
summary(cox.diab)
plot(cox.diab, main = "Estimación de supervivencia general",
     ylab = "Probabilidad de supervivencia", xlab = "Meses")

# Análisis de residuales

mg <- resid(m4, type = "martingale")
plot(mg, main = "Residuales de martingala",
     ylab = "Valor del residual", xlab = "Indice del residual")
abline(h = 1, col = "orange") # Se revisa que esté entre (-infinito,1]
mg[mg > 1]

des <- resid(m4, type = "deviance")
plot(des, main = "Residuales de desviación", ylim = c(-3,3),
     ylab = "Valor del residual", xlab = "Indice del residual")
abline(h = c(-3,3), col = "green3")

pun <- resid(m4, type = "score")
plot(pun, main = "Residuales de puntaje",
     ylab = "Valor del residual", xlab = "Indice del residual")

scho <- resid(m4, type = "schoenfeld")
plot(diab$sexo[diab$censor == 1], scho, ylab = "Valor del residual",
     main = "Residuales de schoenfeld", xlab = "Indice del residual")
# En este gráfico sólo se toman en cuenta las fallas (éxitos)