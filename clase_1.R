# Primer sesion clase Estadística Aplicada

a <- c(1,2,3,4)

length(a)     # longitud del vector a
a[3]          # extraer un elemento del vector a
class(a)      # tipo de dato del vector a
summary(a)    # resumen estadístico del vector a
min(a)        # valor minimo del vector a
max(a)        # valor maximo del vector a
mean(a)       # promedio del vector a
median(a)     # mediana del vector a
range(a)      # rangos entre los que está el vector a (o bien, sus límites)
help(mean)    # funcion para saber como usar la funcion tal, en ese caso mean
data()        # vemos la infromacion que tenemos en el paquete 'datasets'
data("women") # seleccionamos "women" de nuestro conjuto de datos
e1 <- women   # llamamos e1 a women
e1            # imprimimos la matriz e1 de los datos
help(women)   # vemos la información que nos brinda women
dim(e1)       # dimensiones de la matriz e1
e1[1,2]       # posición: renglon 1, columna 2 de la matriz e1
e1[,1]        # todos los renglones de la columna 1 de la matriz e1
e1[2,]        # la infromacion de item 2 de la matriz e1
e1[1:5,]      # los primeros 5 renglones de la matriz e1
e1$weight     # indicar la columna con $, nos trae todos los pesos de e1
mean(e1$weight)     # promedio de todos los pesos de la matriz e1
summary(e1$weight)  # resumen estadístico de todos los pesos de e1
#attach(e1)         # reconocer nombre de columna de e1 <-- EN CONSOLA

# Calculando el promedio con un ciclo

aux <- 0
for(i in 1:length(e1$weight)){
  aux <- aux + e1$weight[i]
}
aux # suma de pesos acumulados

rep(0,15) # funcion repetir, aquí se repite el cero 15 veces
d <- rep(0,15)

for(i in 1:length(e1$weight)){
  d[i] <- sum(e1$weight[1:i])
}
d # pesos acumulados desglosados

# Histogramas

# Los histogramas solo son para una variable
hist(e1$weight, main = "Histograma del peso de mujeres de 30-39 años",
     xlab = "Peso en libras", ylab = "Frecuencia")

# Plot

plot(e1$height, e1$weight, main = "Gráfica de dispersión altura vs peso de
     mujeres de 30-39 años", xlab = "Altura", ylab = "Peso")

# Abrinedo un documento csv

setwd("C:/Users/DarioJaciel/Desktop/Zuky/Rstudio/Estadística apliacada")
# F es para contemplar los titulos como una fila mas
# T es para contemplar los titulos como titulos
e2 <- read.table("Tabla_ejemplo.csv", sep = ",", header = T)
dim(e2)                     # dimensiones de la matriz e2
names(e2)                   # nombres de las columnas de la matriz e2
summary(e2$sexo)            # resumen estadístico de todos los sexos de e2
e2[3,5] <- "F"              # cambiaos "M" por "F" en la matriz e2
e2[-4,]                     # eliminamos toda la fila 4 en la matriz e2
e2[4,5] <- "H"              # llenamos el dato faltante en la matriz e2
summary(e2)
str(e2)
mean(e2[e2$sexo == "F",6])  # promedio de las mujeres de la matriz e2
mean(e2[e2$sexo == "H",6])  # promedio de los hombres de la matriz e2

# Gráfica de pastel

edad <- factor(e2$edad)
levels(edad) <- c("20","21","22","23")
table(edad)
pie(table(edad), main = "Proporcion por edad")

e2[e2$sexo == "H",5] <- "M"
genero <- factor(e2$sexo)
levels(genero) <- c("Femenino", "Masculino")
table(genero)
pie(table(genero), main = "Proporcion por genero")