# Primera clase Supervivencia

# NOTA. 1 <- presencia del evento
#       0 <- ausencia del evento
#       El evento de interés se realiza en sólo una ocación.

library(survival)
# Surv(tiempo, evento)
Surv(15,1) # item con falla
Surv(18,0) # item censurado

v1 <- c(1,3,4,7,7,8,9,12) # vector de tiempos
v2 <- c(1,1,0,1,0,0,1,1)  # vector de eventos (fallas)
# ó
v2.nom <- c("muerte", "muerte", "censura", "muerte", "censura", "censura", "muerte", "muerte")

ejemplo <- Surv(v1,v2)
# Surv() nos dice que hubo falla en las posiciones 1, 2, 4, 7 y 8
# del vector ejemplo.

# Estadística descriptiva

sum(v2 == 1)            # Número de fallas
sum(v2.nom == "muerte") # Número de fallas
sum(v2 != 1)            # Número de censuras
sum(v2.nom != "muerte") # Número de censuras

sum(v2 == 1)/length(v2) # Proporción de fallas
mean(v1[v2 == 1])       # Promedio de los tiempos de fallas
mean(v1[v2 == 0])       # Promedio de los tiempos de censuras

# Paramétrica
# Estimador de Kaplan - Meier
# km.ejemplo <- survfit(ejemplo ~ 1, conf.int = 0.99)  # 
# km.ejemplo <- survfit(ejemplo ~ 1)                   # Con intervalos (líneas punteadas)
km.ejemplo <- survfit(ejemplo ~ 1, conf.type = "none") # Sin intervalos (líneas punteadas)
summary(km.ejemplo)
plot(km.ejemplo)

te <- c(2,5,13) # Vector tiempo específico
summary(km.ejemplo, te)