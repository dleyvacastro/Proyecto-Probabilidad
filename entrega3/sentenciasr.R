#Importo la base de datos
library(readxl)
#Cambiar el path a donde este guardado
databaseactualizada <- read_excel("Documents/Probabilidad_y_estadistica_1/Proyecto-Probabilidad/entrega3/databaseactualizada.xlsx")

# Listas de mediciones por trimestre guardadas como variables
inflacionCol = databaseactualizada$Ic
exportacionesUSA = databaseactualizada$Eu
pibUSA = databaseactualizada$Pu
importacionesUSA = databaseactualizada$Imu
pibCol = databaseactualizada$Pc
importacionesCol = databaseactualizada$Imc
exportacionesCol = databaseactualizada$Ec

n = length(inflacionCol)

# Distribuciones de los datos

# INFLACION DE COLOMBIA
MMinflacionCol = mean(inflacionCol)
VARinflacionCol = var(inflacionCol)
SDinflacionCol = sd(inflacionCol)
MINinflacionCol = min(inflacionCol)
MAXinflacionCol = max(inflacionCol)

#EXPORTACIONES DE COLOMBIA
MMexportacionesCol = mean(exportacionesCol)
VARexportacionesCol = var(exportacionesCol)
SDexportacionesCol = sd(exportacionesCol)
MINexportacionesCol = min(exportacionesCol)
MAXexportacionesCol = max(exportacionesCol)

#EXPORTACIONES DE ESTADOS UNIDOS
MMexportacionesUSA = mean(exportacionesUSA)
VARexportacionesUSA = var(exportacionesUSA)
SDexportacionesUSA = sd(exportacionesUSA)
MINexportacionesUSA = min(exportacionesUSA)
MAXexportacionesUSA = max(exportacionesUSA)

# IMPORTACIONES DE COLOMBIA
MMimportacionesCol = mean(importacionesCol)
VARimportacionesCol = var(importacionesCol)
SDimportacionesCol = sd(importacionesCol)
MINimportacionesCol = min(importacionesCol)
MAXimportacionesCol = max(importacionesCol)

# IMPORTACIONES DE ESTADOS UNIDOS
MMimportacionesUSA = mean(importacionesUSA)
VARimportacionesUSA = var(importacionesUSA)
SDimportacionesUSA = sd(importacionesUSA)
MINimportacionesUSA = min(importacionesUSA)
MAXimportacionesUSA = max(importacionesUSA)

# PIB DE COLOMBIA
MMpibCol = mean(pibCol)
VARpibCol = var(pibCol)
SDpibCol = sd(pibCol)
MINpibCol = min(pibCol)
MAXpibCol = max(pibCol)

# PIB DE ESTADOS UNIDOS
MMpibUSA = mean(pibUSA)
VARpibUSA = var(pibUSA)
SDpibUSA = sd(pibUSA)
MINpibUSA = min(pibUSA)
MAXpibUSA = max(pibUSA)

# Graficas, intervalos de confianza y pruebas de hipotesis de las variables, para los intervalos de confianza supondremos que las variables tienen distribución normal
alpha = 0.05
z_alphasup = qnorm(alpha, lower.tail = FALSE)
z_alphainf = qnorm(alpha, lower.tail = TRUE)
z_alpha2 = qnorm(alpha/2, lower.tail = FALSE)

# INFLACION DE COLOMBIA

# Histograma
hist(inflacionCol, xlab = "Porcentaje", ylab = "Frecuencia", ylim = c(0,40), main = "Inflación de Colombia por trimestre")
# Diagrama de caja
boxplot(inflacionCol, horizontal = TRUE, xlab = "Porcentaje", main = "Inflación de Colombia por trimestre")
# Intervalo de confianza del 95% para la media de la inflacion de colombia por trimestre
SEinflacionCol = SDinflacionCol/sqrt(n)
LSinflacionCol = MMinflacionCol + z_alpha2*SEinflacionCol
LIinflacionCol = MMinflacionCol - z_alpha2*SEinflacionCol
#Prueba de hipotesis para determinar si el nivel de inflacion esta por encima del 4% con un nivel de significancia de 0.05
H0inflacionCol = 4
ZinflacionCol = (MMinflacionCol-H0inflacionCol)/SEinflacionCol
#Nuestro estadistico de prueba esta fuera de nuestra region de rechazo, por lo que aceptamos H_0
#Error beta
HAinflacionCol = 3
KinflacionCol = 4 - z_alphasup*SEinflacionCol
BetainflacionCol = pnorm((KinflacionCol - HAinflacionCol)/SEinflacionCol, lower.tail = FALSE)

# EXPORTACIONES COL 

# Histograma
hist(exportacionesCol, xlab = "Valor en millones de pesos", ylab = "Frecuencia", ylim = c(0,20), main = "Exportaciones de Colombia por trimestre")
# Diagrama de caja
boxplot(exportacionesCol, horizontal = TRUE, xlab = "Valor en millones de pesos", main = "Exportaciones de Colombia por trimestre")
# Intervalo de confianza del 95% para la media del valor de las exportaciones de Colombia por trimestre
SEexportacionesCol = SDexportacionesCol/sqrt(n)
LSexportacionesCol = MMexportacionesCol + z_alpha2*SEexportacionesCol
LIexportacionesCol = MMexportacionesCol - z_alpha2*SEexportacionesCol

# EXPORTACIONES DE ESTADOS UNIDOS

# Histograma
hist(exportacionesUSA, xlab = "Valor en millones de dolares", ylab = "Frecuencia", ylim = c(0,20), main = "Exportaciones de Estados Unidos por trimestre")
# Diagrama de caja
boxplot(exportacionesUSA, horizontal = TRUE, xlab = "Valor en millones de dolares", main = "Exportaciones de Estados Unidos por trimestre")
# Intervalo de confianza del 95% para la media del valor de las exportaciones de Estados Unidos por trimestre
SEexportacionesUSA = SDexportacionesUSA/sqrt(n)
LSexportacionesUSA = MMexportacionesUSA + z_alpha2*SEexportacionesUSA
LIexportacionesUSA = MMexportacionesUSA - z_alpha2*SEexportacionesUSA

# IMPORTACIONES COLOMBIA

# Histograma
hist(importacionesCol, xlab = "Valor en millones de pesos", ylab = "Frecuencia", ylim = c(0,20), main = "Importaciones de Colombia por trimestre")
# Diagrama de caja
boxplot(importacionesCol, horizontal = TRUE, xlab = "Valor en millones de pesos", main = "Importaciones de Colombia por trimestre")
# Intervalo de confianza del 95% para la media del valor de las importaciones de Colombia por trimestre
SEimportacionesCol = SDimportacionesCol/sqrt(n)
LSimportacionesCol = MMimportacionesCol + z_alpha2*SEimportacionesCol
LIimportacionesCol = MMimportacionesCol - z_alpha2*SEimportacionesCol

# IMPORTACIONES USA

# Histograma
hist(importacionesUSA, xlab = "Valor en millones de dolares", ylab = "Frecuencia", ylim = c(0,20), main = "Importaciones de Estados Unidos por trimestre")
# Diagrama de caja
boxplot(importacionesUSA, horizontal = TRUE, xlab = "Valor en millones de dolares", main = "Importaciones de Estados Unidos por trimestre")
#Intervalo de confianza del 95% para la media del PIB de Estados Unidos por trimestre
SEimportacionesUSA = SDimportacionesUSA/sqrt(n)
LSimportacionesUSA = MMimportacionesUSA + z_alpha2*SEimportacionesUSA
LIimportacionesUSA = MMimportacionesUSA - z_alpha2*SEimportacionesUSA

# PIB COLOMBIA

# Histograma
hist(pibCol, xlab = "Valor en millones de pesos", ylab = "Frecuencia", ylim = c(0,20), main = "PIB de Colombia por trimestre")
# Diagrama de caja
boxplot(pibCol, horizontal = TRUE, xlab = "Valor en millones de pesos", main = "PIB de Colombia por trimestre")
# Intervalo de confianza del 95% para la media del PIB de Colombia por trimestre
SEpibCol = SDpibCol/sqrt(n)
LSpibCol = MMpibCol + z_alpha2*SEpibCol
LIpibCol = MMpibCol - z_alpha2*SEpibCol

# PIB USA

# Histograma
hist(pibUSA, xlab = "Valor en millones de dolares", ylab = "Frecuencia", ylim = c(0,20), main = "PIB de Estados Unidos por trimestre")
# Diagrama de caja
boxplot(pibUSA, horizontal = TRUE, xlab = "Valor en millones de dolares", main = "PIB de Estados Unidos por trimestre")
#Intervalo de confianza del 95% para la media del PIB de Estados Unidos por trimestre
SEpibUSA = SDpibUSA/sqrt(n)
LSpibUSA = MMpibUSA + z_alpha2*SEpibUSA
LIpibUSA = MMpibUSA - z_alpha2*SEpibUSA

#Modelos y diagramas de puntos
#inflacion de colombia vs importaciones de colombia
Ic_Imc = lm(inflacionCol~importacionesCol)
summary(Ic_Imc)
plot(importacionesCol, inflacionCol, xlab = "Importaciones Colombia", ylab = "Inflacion Colombia")

#inflacion de colombia vs exportaciones de colombia
Ic_Ec = lm(inflacionCol[0:29]~exportacionesCol[0:29])
summary(Ic_Ec)
plot(exportacionesCol[0:29], inflacionCol[0:29], xlab = "Exortaciones Colombia", ylab = "Inflacion Colombia")

Ic_Ec = lm(inflacionCol[29:63]~exportacionesCol[29:63])
summary(Ic_Ec)
plot(exportacionesCol[29:63], inflacionCol[29:63], xlab = "Exortaciones Colombia", ylab = "Inflacion Colombia")

# Exportaciones de colombia vs importaciones de estados unidos
Ex_Imu1 = lm(exportacionesCol[0:29]~importacionesUSA[0:29])
summary(Ex_Imu1)
plot(importacionesUSA[0:29], exportacionesCol[0:29], xlab = "Importaciones Estados Unidos en millones de dolares", ylab = "Exportaciones Colombia en millones de pesos", xlim = c(500, 2500), ylim = c(20000, 35000), main = "Entre 2005 y 2012") 

# Exportaciones de colombia con importaciones de estados unidos
Ex_Imu2 = lm(exportacionesCol[29:63]~importacionesUSA[29:63])
summary(Ex_Imu2)
plot(importacionesUSA[29:63], exportacionesCol[29:63],xlab = "Importaciones Estados Unidos en millones de dolares", ylab = "Exportaciones Colombia en millones de pesos", xlim = c(500, 2500), ylim = c(20000, 35000), main = "Entre 2012 y 2020") 

# Importaciones de colombia con exportaciones de estados unidos
Imc_Eu1 = lm(importacionesCol[0:29]~exportacionesUSA[0:29])
summary(Imc_Eu1)
plot(exportacionesUSA[0:29], importacionesCol[0:29], xlab = "Exportaciones Estados Unidos en millones de dolares", ylab = "Importaciones Colombia en millones de pesos", xlim = c(400,2000), ylim = c(15000, 55000), main = "Entre 2005 y 2012")

# Importaciones de colombia con exportaciones de estados unidos
Imc_Eu2 = lm(importacionesCol[29:63]~exportacionesUSA[29:63])
summary(Imc_Eu2)
plot(exportacionesUSA[29:63], importacionesCol[29:63], xlab = "Exportaciones Estados Unidos en millones de dolares", ylab = "Importaciones Colombia en millones de pesos", xlim = c(400,2000), ylim = c(15000, 55000), main = "Entre 2012 y 2020") 

# PIB de colombia con PIB de estados unidos
Pc_Pu1 = lm(pibCol[0:29]~pibUSA[0:29])
summary(Pc_Pu1)
plot(pibUSA[0:29], pibCol[0:29], xlab = "PIB Estados Unidos en millones de dolares", ylab = "PIB Colombia en millones de pesos", main = "Entre 2005 y 2012") 

# Scatterplot de PIB de colombia con PIB de estados unidos
Pc_Pu2 = lm(pibCol[29:63]~pibUSA[29:63])
summary(Pc_Pu2)
plot(pibUSA[29:63], pibCol[29:63], xlab = "PIB Estados Unidos en millones de dolares", ylab = "PIB Colombia en millones de pesos", main = "Entre 2012 y 2005") 