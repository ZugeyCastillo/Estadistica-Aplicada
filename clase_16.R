# Análisis de supervivencia

library(survival)
data(kidney)
help(kidney)
k <- kidney

# Se tienen 38 pacientes bajo estudio.

nambes(k)

# Solamente primeros registros
k2 <- kidney[seq(from = 1, to = length(kidney$id), by = 2),]

# Estadítica descriptiva

table(k2$status, k2$sex)
table(k2$status, k2$disease)

# Respuesta compuesta

resp.c <- Surv(k2$time, k2$status)
modelo1 <- coxph(resp.c ~ k2$age + k2$sex + k2$disease + k2$frail)
summary(modelo1)

# Prueba general: Contrastes

# Hipótesis
# Ho: Los coeficientes del vector son iguales a cero.
# Ha: Al menos un coeficiente del vector difiere.
# Rechazamos Ho si p-valor < alpha
# 3e-07, 1e-05, 3e-09 < 0.05, por lo tanto R. Ho

# Quitamos la variable con probabilidad mayor a alpha en
# Pr(>|z|), en este caso sería age con 0.4417

# Modelo de coxph sin la variable edad
modelo2 <- coxph(resp.c ~ k2$sex + k2$disease + k2$frail)
summary(modelo2)

# Actual GN/AN/PKD/Other
# Ahora buscamos AN/PKD/Other-GN
k2$disease[k2$diasease == "GN"] <- "Other"
k2$disease <- droplevels(k2$disease)

# Sin categoría GN
modelo22 <- coxph(resp.c ~ k2$sex + k2$disease + k2$frail)
summary(modelo22)

# Análisis de supervivencia

# Estimador de Kaplan - Meier
cox.k <- survfit(modelo22)
summary(cox.k)
plot(cox.k, main = "Estimación de supervivencia general",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo",
     conf.int = "none", col = "purple")
km.k <- survfit(resp.c ~ 1)
lines(km.k, conf.int = "none", col = "red")
legend("topright",c("Modelo de Cox", "Estimador Kaplan-Meier"),
       col = c("purple", "red"), lty = 1)

# Análisis de residuales

mag <- resid(modelo22, type = "martingale")
plot(mag, main = "Residuales de martingala", ylim = c(-2,1.5),
     ylab = "Valor del residual", xlab = "Indice del residual")
abline(h = 1, col = "orange") # Se revisa que esté entre (-infinito,1]
mag[mag > 1]

pje <- resid(modelo22, type = "score")
plot(pje, main = "Residuales de puntaje",
     ylab = "Valor del residual", xlab = "Indice del residual")

# Revisando todos los pares
plot(pje[,1],pje[,2])
plot(pje[,1],pje[,3])
plot(pje[,1],pje[,4])
plot(pje[,2],pje[,3])
plot(pje[,2],pje[,4])
plot(pje[,3],pje[,4])

dev <- resid(modelo22, type = "deviance")
plot(dev, main = "Residuales de desviación", ylim = c(-3,3),
     ylab = "Valor del residual", xlab = "Indice del residual")
abline(h = c(-3,3), col = "green3")
dev[abs(dev) > 3]

AN <- rep(0,38)
AN[k2$disease == "AN"] <- 1 # indica si es AN o no
sc <- resid(modelo22, type = "schoenfeld")
plot(AN[k2$status == 1], sc[,2], ylab = "Valor del residual",
     main = "Residuales de schoenfeld: 
Enfermedad tipo AN", xlab = "Indice del residual")
PKD <- rep(0,38)
PKD[k2$disease == "PKD"] <- 1 # indica si es PKD o no
plot(PKD[k2$status == 1],sc[,3], ylab = "Valor del residual",
     main = "Residuales de schoenfeld:
Enfermedad tipo PKD", xlab = "Indice del residual")
# En este gráfico sólo se toman en cuenta las fallas (éxitos)

