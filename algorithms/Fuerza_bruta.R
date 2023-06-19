###############LIBRERIAS##################
library(tictoc)
library("Metrics")

#y = c(17,21,19,23,18,16,20,18,22,20,15,22)
# Suavizamiento Exponencial
pow<- function(y,salto) {
  alpha=salto
  S_TABLE = c() # Creamos el vector vacio para CME
  alpha1=alpha
  tic()
  start_time <- Sys.time()
  while (alpha<1) # No puede ser mayor a 1
  {
    sua=HoltWinters(y,alpha,beta = FALSE,gamma=FALSE) # Metodo de Holtwinters 
    s1=sua$fitted[,1] # Valores de suavizamiento
    MSE = mse(y[c(2:length(y))],s1)
    RMSE = rmse(y[c(2:length(y))],s1)  
    MAE = mae(y[c(2:length(y))],s1)
    MAPE = mape(y[c(2:length(y))],s1)
    S_TABLE <- rbind(S_TABLE,c(alpha,MSE,RMSE,MAE,MAPE)) # Vector de CME
    result <- S_TABLE[S_TABLE[,2] == min(S_TABLE[,2])]
    alpha=alpha+alpha1 
  }
  colnames(S_TABLE) = c("alpha", "MSE", "RMSE", "MAE", "MAPE")
  print(S_TABLE)
  ventana = result[1]
  MSE = result[2]
  RMSE = result[3]
  MAE = result[4]
  MAPE = result[5]
  print("Valores Optimos")
  print(paste("Alpha: ", ventana)) 
  print(paste("MSE: ", MSE))
  print(paste("RMSE: ", RMSE))
  print(paste("MAE: ", MAE))
  print(paste("MAPE: ", MAPE))
  return(S_TABLE)
  end_time <- Sys.time()
  toc()
  end_time - start_time
}
