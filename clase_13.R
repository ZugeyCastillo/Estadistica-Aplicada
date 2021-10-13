library(survival)

# Análisis de cucarachas

setwd("C:/Users/DarioJaciel/Desktop/Zuky/Rstudio/Estadística apliacada")
datosc <- read.table("Cucarachas.csv", sep = ",", header = T)
datosc

# NOTA. 1 <- presencia del evento
#       0 <- ausencia del evento

# Objetivos:
# 1. Identificar el insecticida más efectivo, supervivencia menor
# 2. Del mejor, revisar cómo se desmepeña con cucarachas grandes 
# (con mayor peso)
# 3. ¿Sobreviven más las cucarachas con mayor peso con cualquier 
# insecticida?

# Estadística descriptiva

summary(datosc)
ins <- factor(datosc$GRUPO)
levels(ins) <- c("Insecticida A", "Insecticida B", "Insecticida C")
pie(table(ins), main = "Proporción por insecticida")

# Peso
boxplot(datosc$PESO, ylab = "Peso", xlab = "Grupo A, B y C en conjunto", 
        main = "Pesos de todas las cucarachas")
hist(datosc$PESO)
boxplot(datosc$PESO ~ datosc$GRUPO, ylab = "Peso", xlab = "Grupos",
        main = "Pesos de todas las cucarachas por grupo")

# Tiempo
boxplot(datosc$TIEMPO ~ datosc$GRUPO, ylab = "Tiempo",
        xlab = "Grupos", main = "Supervivencia por grupos")
boxplot(datosc$TIEMPO ~ datosc$ESTATUS, ylab = "Tiempo",
        xlab = "Probabilidad de supervivencia", main = "Supervivencia general")

# Tablas de contingencia
table(datosc$ESTATUS, datosc$GRUPO)

# Análisis de supervivencia

# Objetivo 1. Identificar el insecticida más efectivo, supervivencia
# menor

s <- Surv(datosc$TIEMPO, datosc$ESTATUS)

# Estimador de Kaplan - Meier (solo falla y tiempo)
km.t <- survfit(s ~ 1)
summary(km.t)
plot(km.t, main = "Estimación de supervivencia general",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo")

# Estimación de km con GRUPO como discriminante

km.g <- survfit(s ~ datosc$GRUPO)
summary(km.g)
plot(km.g, col = c("magenta","blue","green"), main = "Comparativa por grupos",
     ylab = "Probabilidad de supervivencia", xlab = "Tiempo")
legend("topright",c("Grupo A", "Grupo B", "Grupo C"),
       col = c("magenta","blue","green"), lty = 1)

# Comparativa de funciones

survdiff(s ~ datosc$GRUPO)
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Al menos una función de supervivencia difiere.
# Rechazamos Ho si p-valor < aplha
# 3e-05 < 0.05, por lo tanto R. Ho
# Existe evidencia estadística suficiente para decir, con 95% de
# confianza, que al menos una función de supervivencia difiere.

# Comparativa Grupo A vs Grupo B
survdiff(s[datosc$GRUPO!="C"] ~ datosc$GRUPO[datosc$GRUPO!="C"])
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.
# Rechazamos Ho si p-valor < aplha
# 0.02 < 0.05, por lo tanto R. Ho
# Existe evidencia estadística suficiente para decir, con 95% de
# confianza, que las funciones de supervivencia difieren.

# Comparativa Grupo A vs Grupo c
survdiff(s[datosc$GRUPO!="B"] ~ datosc$GRUPO[datosc$GRUPO!="B"])
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.
# Rechazamos Ho si p-valor < aplha
# 2e-06 < 0.05, por lo tanto R. Ho
# Existe evidencia estadística suficiente para decir, con 95% de
# confianza, que las funciones de supervivencia difieren.

# Comparativa Grupo B vs Grupo c
survdiff(s[datosc$GRUPO!="A"] ~ datosc$GRUPO[datosc$GRUPO!="A"])
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.
# Rechazamos Ho si p-valor < aplha
# 0.04 < 0.05, por lo tanto R. Ho
# Existe evidencia estadística suficiente para decir, con 95% de
# confianza, que las funciones de supervivencia difieren.

# Apoyados en la gráfica, el insecticida C es mejor.

# Objetivo 2. Del mejor, revisar cómo se desmepeña con cucarachas
# grandes (con mayor peso)

w <- datosc[101:150,]

# Considerando la mediana como corte
median(w$PESO)
# chico <- 6.401, grande >= 6.401

# Copiamos el vector para trabajar sobre la copia
vp <- w$PESO
vp[] <- "chica"
vp[w$PESO >= 6.401] <- "grande"

km.peso <- survfit(s[datosc$GRUPO == "C"] ~ vp)
summary(km.peso)
plot(km.peso, col = c("purple", "orange3"),
     ylab = "Probabilidad de supervivencia", xlab = "Peso",
     main = "Supervivencia por tamaño de cucarachas")
legend("topright",c("Chicas", "Grandes"),
       col = c("purple", "orange3"), lty = 1)

# Comparativa de funciones

survdiff(s[datosc$GRUPO == "C"] ~ vp)
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.
# Rechazamos Ho si p-valor < aplha
# 0.9 < 0.05, por lo tanto No R. Ho
# Existe evidencia estadística suficiente para decir, con 95% de
# confianza, que las funciones de supervivencia son iguales.

# Objetivo 3. ¿Sobreviven más las cucarachas con mayor peso con 
# cualquier insecticida?

pp <- datosc$PESO
pp[] <- "chico"
pp[datosc$PESO >= median(datosc$PESO)] <- "grande"
km.pp <- survfit(s ~ pp)
plot(km.pp, col = c("purple", "orange3"), xlab = "Tiempo",
     ylab = "Probabilidad de supervivencia",
     main = "Supervivencia general de las cucarachas")
legend("topright",c("Chicas", "Grandes"),
       col = c("purple", "orange3"), lty = 1)

# Comparativa de funciones

survdiff(s ~ pp)
# Hipótesis
# Ho: Las funciones de supervivencia son iguales.
# Ha: Las funciones de supervivencia difieren.
# Rechazamos Ho si p-valor < aplha
# 0.3 < 0.05, por lo tanto No R. Ho
# Existe evidencia estadística suficiente para decir, con 95% de
# confianza, que las funciones de supervivencia son iguales.

# Considerando grandes aquellas mayores a peso 6.316. Este valor
# se puede buscar en internet para saber a partir de qué peso una
# Cucaracha se condiera grande.


# Gráfica 4 en 1
par(mfrow = c(1,4))
plot(km.g, col = c("magenta", "blue", "green"), main = "Comparativa
     de Grupos",
     ylab = "Probabilidad de supervivencia")
plot(km.g[1], col = "magenta", main = "Grupo A", 
     xlab = "                Tiempo", yaxt = "n", axes = TRUE)
axis(2, col.axis = "white")
plot(km.g[2], col = "blue", main = "Grupo B", 
     yaxt = "n", axes = TRUE)
axis(2, col.axis = "white")
plot(km.g[3], col = "green", main = "Grupo C", 
     yaxt = "n", axes = TRUE)
axis(2, col.axis = "white")
par(mfrow = c(1,1))