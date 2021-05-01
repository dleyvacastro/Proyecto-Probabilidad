# Histogramas

  # Inflacion
  hist(databaseactualizada$Ic, xlab = "Porcentaje", ylab = "Frecuencia", ylim = c(0,40), main = "Inflación de Colombia por trimestre")
  
  # Exportaciones USA
  hist(databaseactualizada$Eu, xlab = "Valor en millones de dolares", ylab = "Frecuencia", ylim = c(0,20), main = "Exportaciones de Estados Unidos por trimestre")
  
  # PIB USA
  hist(databaseactualizada$Pu, xlab = "Valor en millones de dolares", ylab = "Frecuencia", ylim = c(0,20), main = "PIB de Estados Unidos por trimestre")
  
  # Importaciones USA
  hist(databaseactualizada$Imu, xlab = "Valor en millones de dolares", ylab = "Frecuencia", ylim = c(0,20), main = "Importaciones de Estados Unidos por trimestre")
  
  # Desempleo COL
  hist(databaseactualizada$Pc, xlab = "Valor en millones de pesos", ylab = "Frecuencia", ylim = c(0,20), main = "PIB de Colombia por trimestre")
  
  # PIB COL
  hist(databaseactualizada$Pc, xlab = "Valor en millones de pesos", ylab = "Frecuencia", ylim = c(0,20), main = "PIB de Colombia por trimestre")
  
  # Importaciones COL
  hist(databaseactualizada$Imc, xlab = "Valor en millones de pesos", ylab = "Frecuencia", ylim = c(0,20), main = "Importaciones de Colombia por trimestre")
  
  # Exportaciones COL 
  hist(databaseactualizada$Ec, xlab = "Valor en millones de pesos", ylab = "Frecuencia", ylim = c(0,20), main = "Exportaciones de Colombia por trimestre")

# DIAGRAMAS DE CAJA
  # Exportaciones COL
  boxplot(databaseactualizada$Ec, horizontal = TRUE, xlab = "Valor en millones de pesos", main = "Exportaciones de Colombia por trimestre")
  
  # Importaciones COL
  boxplot(databaseactualizada$Imc, horizontal = TRUE, xlab = "Valor en millones de pesos", main = "Importaciones de Colombia por trimestre")
  
  # PIB COL
  boxplot(databaseactualizada$Pc, horizontal = TRUE, xlab = "Valor en millones de pesos", main = "PIB de Colombia por trimestre")
  
  # Desempleo COL
  boxplot(databaseactualizada$Dc, horizontal = TRUE, xlab = "Porcentaje", main = "Desempleo de la población activa de Colombia por trimestre")
  
  # Importaciones USA 
  boxplot(databaseactualizada$Imu, horizontal = TRUE, xlab = "Valor en millones de dolares", main = "Importaciones de Estados Unidos por trimestre")
  
  # Exportaciones USA
  boxplot(databaseactualizada$Eu, horizontal = TRUE, xlab = "Valor en millones de dolares", main = "Exportaciones de Estados Unidos por trimestre")
  
  # PIB USA
  boxplot(databaseactualizada$Pu, horizontal = TRUE, xlab = "Valor en millones de dolares", main = "PIB de Estados Unidos por trimestre")
  
  # Inflación USA 
  boxplot(databaseactualizada$Ic, horizontal = TRUE, xlab = "Porcentaje", main = "Inflación de Colombia por trimestre")
  
