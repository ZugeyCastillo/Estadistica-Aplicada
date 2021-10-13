library(survival)

# Análisis de aviones

setwd("C:/Users/DarioJaciel/Desktop/Zuky/Rstudio/Estadística apliacada")
datosa <- read.table("Avion.csv", sep = ",", header = T)
datosa

# NOTA. 1 <- presencia del evento
#       0 <- ausencia del evento

# Objetivos:
# 1. A mayor carga (>media) menor supervivencia del avión
# 2. ¿Qué motor presenta mayor supervivencia?, es decir, no requiera
# ir al taller (ausencia del evento)

# Estadística descriptiva

m <- mean(datosa$CargaBombas)
median(datosa$CargaBombas)
boxplot(datosa$CargaBombas, ylab = "Carga de bombas", xlab = "Aviones",
        main = "Carga de bombas de aviones")

mot <- factor(datosa$TipoMotor) # Tipos de motores
summary(mot) # Cantidad de motores por cada tipo de motor
pie(table(mot), main = "Proporción por tipo de motor")

table(datosa$TipoMotor, datosa$falla)
# se sospecha que el motro cero es el mejor por tener mayor supervivencia

# Análisis de supervivencia

# Objetivo 1. A mayor carga (>media) menor supervivencia del avión

av <- Surv(datosa$Tiempo, datosa$falla)

# Estimador de Kaplan - Meier (solo falla y tiempo)
km.a <- survfit(av ~ 1)
summary(km.a)
plot(km.a, main = "Estimación de supervivencia general",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo")

# Estimador de Kaplan - Meier con carga como discriminante
w <- datosa$CargaBombas # Vector donde se asigne "pesado" y "ligero"
w[] <- "ligero"
w[datosa$CargaBombas >= m] <- "pesado"

carga <- survfit(av ~ w)
summary(carga)
plot(carga, col = c("orange2", "purple"), main = "Clasificación de aviones por carga",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo")
legend("bottomleft",c("Ligero", "Pesado"),
       col = c("orange2", "purple"), lty = 1)

# Comparativa de funciones

survdiff(av ~ w, rho = 0) # rho = 0 es que roce el cero
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.
# Rechazamos Ho si p-valor < aplha
# 0.001 < 0.05, por lo tanto R. Ho
# Existe evidencia estadística suficiente para decir, con 95% de
# confianza, que las funciones de supervivencia difieren.

# Apoyados de la gráfica decimos que la menor supervivencia la
# muestran los aviones con carga pesada.

# Objetivo 2. ¿Qué motor presenta mayor supervivencia?, es decir, no 
# requiera ir al taller (ausencia del evento)

# Estimador de Kaplan - Meier con tipo de motor como discriminante
km.mo <- survfit(av ~ datosa$TipoMotor)
summary(km.mo)
plot(km.mo, col = c("green", "blue", "magenta"), main = "Supervivencia por tipo de motor",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo")
legend("bottomleft", c("Motor 0", "Motor 1", "Motor 2"),
       col = c("green", "blue", "magenta"), lty = 1)

# Comparativa de funciones

survdiff(av ~ datosa$TipoMotor)
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Al menos una función de supervivencia difiere.
# Rechazamos Ho si p-valor < aplha
# 0.04 < 0.05, por lo tanto R. Ho
# Existe evidencia estadística suficiente para decir, con 95% de
# confianza, al menos una función de supervivencia difiere.

# Motor 0 vs Motor 1

m01 <- survfit(av[datosa$TipoMotor != "2"] ~ datosa$TipoMotor[datosa$TipoMotor != "2"])
summary(m01)
plot(m01, col = c("green", "blue"), main = "Comparación Motor 0 vs Motor 1",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo")
legend("bottomleft", c("Motor 0", "Motor 1"),
       col = c("green", "blue"), lty = 1)

# Comparativa de funciones

survdiff(av[datosa$TipoMotor != "2"] ~ datosa$TipoMotor[datosa$TipoMotor != "2"])
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.
# Rechazamos Ho si p-valor < aplha
# 0.5 < 0.10, por lo tanto No R. Ho
# Existe evidencia estadística suficiente para decir, con 90% de
# confianza, las funciones de supervivencia son iguales.

# Motor 1 vs Motor 2

m12 <- survfit(av[datosa$TipoMotor != "0"] ~ datosa$TipoMotor[datosa$TipoMotor != "0"])
summary(m12)
plot(m12, col = c("blue", "magenta"), main = "Comparación Motor 1 vs Motor 2",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo")
legend("bottomleft", c("Motor 1", "Motor 2"),
       col = c("blue", "magenta"), lty = 1)

# Comparativa de funciones

survdiff(av[datosa$TipoMotor != "0"] ~ datosa$TipoMotor[datosa$TipoMotor != "0"])
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.
# Rechazamos Ho si p-valor < aplha
# 0.03 < 0.10, por lo tanto R. Ho
# Existe evidencia estadística suficiente para decir, con 90% de
# confianza, las funciones de supervivencia difieren.

# Motor 0 vs Motor 2

m02 <- survfit(av[datosa$TipoMotor != "1"] ~ datosa$TipoMotor[datosa$TipoMotor != "1"])
summary(m02)
plot(m02, col = c("green", "magenta"), main = "Comparación Motor 0 vs Motor 2",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo")
legend("bottomleft", c("Motor 0", "Motor 2"),
       col = c("green", "magenta"), lty = 1)

# Comparativa de funciones

survdiff(av[datosa$TipoMotor != "1"] ~ datosa$TipoMotor[datosa$TipoMotor != "1"])
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.
# Rechazamos Ho si p-valor < aplha
# 0.05 < 0.10, por lo tanto R. Ho
# Existe evidencia estadística suficiente para decir, con 90% de
# confianza, las funciones de supervivencia difieren.

# El tipo de motor con menor supervivencia es el 2.

# Gráfica 4 en 1
par(mfrow = c(1,4))
plot(km.mo, col = c("green", "blue", "magenta"), main = "Comparativa
de motores",
     ylab = "Probabilidad de supervivencia")
plot(km.mo[1], col = "green", main = "Motor 0", xlab = "Tiempo",
     yaxt = "n", axes = TRUE)
axis(2, col.axis = "white")
plot(km.mo[2], col = "blue", main = "Motor 1", 
     yaxt = "n", axes = TRUE)
axis(2, col.axis = "white")
plot(km.mo[3], col = "magenta", main = "Motor 2", 
     yaxt = "n", axes = TRUE)
axis(2, col.axis = "white")
par(mfrow = c(1,1))