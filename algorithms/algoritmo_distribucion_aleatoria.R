###############LIBRERIAS#################
library(tictoc)
library("Metrics")

alalt <- function(y, n) {
  x <- abs(rnorm(n, mean = 0.5, sd = 0.7071068))
  print("Valores aleatoreos generados: ")
  print(x)
  # Vector de los valores validos.
  alt <- c()
  # Ciclo que limpia los datos bajo la condicion de 0< x < 1 para ser validos
  for (i in 1:length(x)) {
    if (x[i] < 1) {
      alt <- rbind(alt,c(x[i]))
    }
  }
  print("Valores aleatoreos utilizables: ")
  print(alt)
  ### Suavizamiento Exponencial
  S_TABLE = c() # Creamos el vector vacio para CME
  tic()
  start_time <- Sys.time()
  for (i in alt) {
    a = i # Constante de Suavizado
    suave = HoltWinters(y, a, beta = FALSE, gamma=FALSE) # Genera el pronostico
    y_est = suave$fitted[,1] # Extrae el pronostico
    MSE = mse(y[c(2:length(y))],y_est)
    RMSE = rmse(y[c(2:length(y))],y_est)  
    MAE = mae(y[c(2:length(y))],y_est)
    MAPE = mape(y[c(2:length(y))],y_est)
    S_TABLE <- rbind(S_TABLE,c(a,MSE,RMSE,MAE,MAPE)) # Vector de CME
    result <- S_TABLE[S_TABLE[,2] == min(S_TABLE[,2])]
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
  end_time <- Sys.time()
  toc()
  end_time - start_time
}

#alalt(y,10)