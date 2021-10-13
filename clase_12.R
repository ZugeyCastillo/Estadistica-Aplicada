# Análisis de supervivencia

library(survival)
data(leukemia)
help(leukemia)

# NOTA. 1 <- presencia del evento
#       0 <- ausencia del evento
 
# Estadística descriptiva
 
sum(leukemia$status) # Número de muertes registradas
fvsc <- factor(leukemia$status) # Proporción de fallas vs cencuras
levels(fvsc) <- c("Censuras", "Muertes")
pie(table(fvsc), main = "Proporción de censuras y muertes")
 
cvss <- factor(leukemia$x) # Proporción de pacientes que mantienen quimioterapia vs lo que no
levels(cvss) <- c("Mantiene quimioterapia", "Sin quimioterapia")
pie(table(cvss), main = "Proporción de pacientes con quimioterapia")

mean(leukemia$time[leukemia$status == 1]) # Tiempos promedio de falla
mean(leukemia$time[leukemia$status == 0]) # Tiempos promedio de censura

# Estimador de Kaplan - Meier(solo falla y tiempo)

obj <- Surv(leukemia$time, leukemia$status)
g <- survfit(obj ~ 1)
sort(obj)
summary(g)
plot(g, ylab = "Probabilidad de supervivencia", xlab = "Tiempo",
     main = "Supervivencia general de la
leucemia mielógena aguda")
summary(g, c(10,42,70))

# Comparativa con y sin quimioterapia
km.q <- survfit(obj ~ leukemia$x)
summary(km.q)
plot(km.q, col = c("magenta", "green"), xlab = "Tiempo",
    main = "Comparativa de pacientes",
    ylab = "Probabilidad de supervivencia")
legend("topright",c("Con quimioterapia", "Sin quimioterapia"), 
       col = c("magenta", "green"), lty = 1)

# Comparativa de funciones

survdiff(obj ~ leukemia$x, rho = 0)
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.

# aplha = 5%
# Rechazamos Ho si p-valor < aplha
# 0.07 < 0.05, por lo tanto No R. Ho

# aplha = 10%
# Rechazamos Ho si p-valor < aplha
# 0.07 < 0.10, por lo tanto R. Ho
# Las funciones de supervivencia difieren. Apoyados en el gráfico.
# El paciente que mantiene la quimioterapia presenta mayor supervivencia.

# Sólo con quimioterapia con su intervalo de confianza
summary(km.q[1])
plot(km.q[1], col = "magenta", main = "Pacientes con quimioterapia",
     xlab = "Tiempo", ylab = "Probabilidad de supervivencia")
# Sólo sin quimioterapia con su intervalo de confianza
summary(km.q[2])
plot(km.q[2], col = "green", main = "Pacientes sin quimioterapia",
     xlab = "Tiempo", ylab = "Probabilidad de supervivencia")

# Gráfica 3 en 1
par(mfrow = c(1,3)) # Partición del área del gráfico
plot(km.q, col = c("magenta", "green"), main = "Comparativa",
     ylab = "Probabilidad de supervivencia")
plot(km.q[1], col = "magenta", main = "Pacientes con
quimioterapia", xlab = "Unidades de tiempo", yaxt = "n", axes = TRUE)
axis(2, col.axis = "white")
plot(km.q[2], col = "green", main = "Pacientes sin
quimioterapia", xlim = c(0,160), yaxt = "n", axes = TRUE)
axis(2, col.axis = "white")
par(mfrow = c(1,1))