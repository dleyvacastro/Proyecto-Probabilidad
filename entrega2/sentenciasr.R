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
  
  # Inflación Colombia
  boxplot(databaseactualizada$Ic, horizontal = TRUE, xlab = "Porcentaje", main = "Inflación de Colombia por trimestre")
  
  # Scatterplot de inflacion de colombia con importaciones de colombia
  plot(databaseactualizada$Imc, databaseactualizada$Ic, xlab = "Importaciones Colombia", ylab = "Inflacion Colombia")

  # Scatterplot de inflacion de colombia con exportaciones de colombia
  plot(databaseactualizada$Ec, databaseactualizada$Ic, xlab = "Exportaciones Colombia", ylab = "Inflacion Colombia")
  
  # Scatterplot de exportaciones de colombia con importaciones de estados unidos
  plot(databaseactualizada$Imu[0:29], databaseactualizada$Ec[0:29], xlab = "Importaciones Estados Unidos en millones de dolares", ylab = "Exportaciones Colombia en millones de pesos", xlim = c(500, 2500), ylim = c(20000, 35000), main = "Entre 2005 y 2012") 
  
  # Scatterplot de exportaciones de colombia con importaciones de estados unidos
  plot( databaseactualizada$Imu[29:63], databaseactualizada$Ec[29:63],xlab = "Importaciones Estados Unidos en millones de dolares", ylab = "Exportaciones Colombia en millones de pesos", xlim = c(500, 2500), ylim = c(20000, 35000), main = "Entre 2012 y 2020") 
  
  # Scatterplot de importaciones de colombia con exportaciones de estados unidos
  plot(databaseactualizada$Eu[0:29], databaseactualizada$Imc[0:29], xlab = "Exportaciones Estados Unidos en millones de dolares", ylab = "Importaciones Colombia en millones de pesos", xlim = c(400,2000), ylim = c(15000, 55000), main = "Entre 2005 y 2012")
  
  # Scatterplot de importaciones de colombia con exportaciones de estados unidos
  plot(databaseactualizada$Eu[29:63], databaseactualizada$Imc[29:63], xlab = "Exportaciones Estados Unidos en millones de dolares", ylab = "Importaciones Colombia en millones de pesos", xlim = c(400,2000), ylim = c(15000, 55000), main = "Entre 2012 y 2020") 
  
  # Scatterplot de PIB de colombia con PIB de estados unidos
  plot(databaseactualizada$Pu[0:29], databaseactualizada$Pc[0:29], xlab = "PIB Estados Unidos en millones de dolares", ylab = "PIB Colombia en millones de pesos", main = "Entre 2005 y 2012") 
  
  # Scatterplot de PIB de colombia con PIB de estados unidos
  plot(databaseactualizada$Pu[29:63], databaseactualizada$Pc[29:63], xlab = "PIB Estados Unidos en millones de dolares", ylab = "PIB Colombia en millones de pesos", main = "Entre 2012 y 2005") 
  