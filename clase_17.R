library(survival)
data(cancer)
help(cancer)
cancer

summary(cancer) # Cuantos NA hay por variable
length(cancer$status)
ca2 <- na.omit(cancer) # quitando los items o renglones con NA
lenghth(ca2$time)
summary(ca2)
sum(is.na(cancer$wt.loss)) # Indica la suma de NA's
sapply(ca2, function(ca2) sum(is.na(ca2)))

# Respuesta compuesta

rc <- Surv(ca2$time, ca2$status == 2)
mode1 <- coxph(rc ~ ca2$age + ca2$sex + ca2$ph.ecog + ca2$ph.karno +
                ca2$pat.karno + ca2$meal.cal + ca2$wt.loss)
summary(mode1)

# Prueba general: Contrastes

# Hipótesis
# Ho: Los coeficientes del vector son iguales a cero.
# Ha: Al menos un coeficiente del vector difiere.
# Rechazamos Ho si p-valor < alpha
# 2e-04, 3e-04, 2e-04 < 0.10, por lo tanto R. Ho

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería meal.cal con 0.91298

# Modelo de coxph sin la variable meal.cal
mode2 <- coxph(rc ~ ca2$age + ca2$sex + ca2$ph.ecog + ca2$ph.karno +
                 ca2$pat.karno + ca2$wt.loss)
summary(mode2)

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería age con 0.35419

# Modelo de coxph sin la variable age
mode3 <- coxph(rc ~ ca2$sex + ca2$ph.ecog + ca2$ph.karno +
                 ca2$pat.karno + ca2$wt.loss)
summary(mode3)

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería pat.karno con 0.12008

# Modelo de coxph sin la variable pat.karno
mode4 <- coxph(rc ~ ca2$sex + ca2$ph.ecog + ca2$ph.karno + ca2$wt.loss)
summary(mode4)

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería wt.loss con 0.107975

# Modelo de coxph sin la variable wt.loss
mode5 <- coxph(rc ~ ca2$sex + ca2$ph.ecog + ca2$ph.karno)
summary(mode5)

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería ph.karno con 0.110009

# Modelo de coxph sin la variable ph.karno
mode6 <- coxph(rc ~ ca2$sex + ca2$ph.ecog)
summary(mode6)

# Análisis de supervivencia

# Estimador de Kaplan - Meier
cox.s <- survfit(mode6)
summary(cox.s)
plot(cox.s, main = "Estimación de supervivencia general",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo",
     col = "purple")

cox.su <- survfit(mode6)
summary(cox.su)
plot(cox.su, main = "Estimación de supervivencia general
sin la posición 41", ylab = "Probabilidad de supervivencia",
     xlab = "Tiempo", col = "purple")

# Análisis de residuales

mart <- resid(mode6, type = "martingale")
plot(mart, main = "Residuales de martingala",
     ylab = "Valor del residual", xlab = "Indice del residual")
abline(h = 1, col = "orange") # Se revisa que esté entre (-infinito,1]
mart[mart > 1]

pu <- resid(mode6, type = "score")
plot(pu, main = "Residuales de puntaje",
     ylab = "Valor del residual", xlab = "Indice del residual")

dee <- resid(mode6, type = "deviance")
plot(dee, main = "Residuales de desviación", ylim = c(-3.5,3.5),
     ylab = "Valor del residual", xlab = "Indice del residual")
abline(h = c(-3,3), col = "green3")
dee[abs(dee) > 3]

u <- ca2[-41,] # Posición 41 del ca2 sin considerar este item
rco <- Surv(u$time, u$status == 2)
mode6u <- coxph(rco ~ u$sex + u$ph.ecog)
summary(mode6u)
plot(resid(mode6u, type = "deviance"), ylim = c(-3.5,3.5),
     main = "Residuales de desviación sin el outlier",
     ylab = "Valor del residual", xlab= "Indice del residual")
abline(h = c(-3,3), col = "green3")

plot(resid(mode6u, type = "martingale"), ylab = "Valor del residual",
     xlab = "Indice del residual", main = "Residuales de martingala")
abline(h = 1, col = "orange")
plot(resid(mode6u, type = "score"), ylab = "Valor del residual",
     xlab = "Indice del residual", main = "Residuales de putaje")

so <- resid(mode6u, type = "schoenfeld")
plot(u$sex[u$status == 2],so[,1], ylab = "Valor del residual",
     main = "Residuales de schoenfeld - Pacientes Hombres",
     xlab = "Indice del residual")
plot(u$sex[u$status == 2],so[,2], ylab = "Valor del residual",
     main = "Residuales de schoenfeld - Pacientes Mujeres",
     xlab = "Indice del residual")
# En este gráfico sólo se toman en cuenta las fallas (éxitos)
