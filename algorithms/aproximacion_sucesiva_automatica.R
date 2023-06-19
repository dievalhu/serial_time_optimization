out <- matrix(NA, 10, ncol=2)

aproximacion_sucesiva <- function(y, tol) {
  
  tic()
  
  #######SI TOLERANCIA ES 1
  if(tol==1){
    
    #Variable de incremento de posicion en la matriz
    val=0
    #Valor de incremento de alpha
    alpha=0
    
    #Bucle de alfa con variacion desde 0.1 hasta 1 
    
    while (alpha<=0.9)
    {
      val = val+1
      alpha=alpha+0.1 #valor de la constante de suavizamiento en aumento
      sua = HoltWinters(y, alpha, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
      s1 = sua$fitted[,1] #valores del suavizamiento exponencial
      err_pr_sua = y[2:(length(y))]-s1 #error del pronostico con suavizamiento
      err_pr_sua2 = err_pr_sua^2 #error de pronostico con suavizamiento al cuadrado
      CME_s = sum(err_pr_sua2)/length(s1) #cuadrado medio debido al error con suavizamiento
      out[val,] <- c(alpha,CME_s)
      
    }
    
    print('Matriz de resultados:')
    print(out)
    min_cme=min(out[,2])
    #Valor minimo de CME
    print(min_cme)
    #Index del valor minimo
    index_min=which.min(out[,2])
    alpha=out[index_min,1]
    #Valor de alfa usado para encontrar el minimo CME
    print(alpha)
  }
  
  if(tol>1){
    
    ######SI TOLERANCIA ES >1
    #Variable de incremento de posicion en la matriz
    val=0
    #Valor de incremento de alpha
    alpha=0
    #Bucle de alfa con variacion desde 0.1 hasta 1 
    while (alpha<=0.9)
    {
      val = val+1
      alpha=alpha+0.1 #valor de la constante de suavizamiento en aumento
      sua = HoltWinters(y, alpha, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
      s1 = sua$fitted[,1] #valores del suavizamiento exponencial
      err_pr_sua = y[2:(length(y))]-s1 #error del pronostico con suavizamiento
      err_pr_sua2 = err_pr_sua^2 #error de pronostico con suavizamiento al cuadrado
      CME_s = sum(err_pr_sua2)/length(s1) #cuadrado medio debido al error con suavizamiento
      out[val,] <- c(alpha,CME_s)
      
    }

    for(i in 2:tol){
      var=10^(-i)
      #Vector de rango
      rango=c()
      #Valor minimo de CME
      min_cme=min(out[,2])
      #Index del valor minimo
      index_min=which.min(out[,2])
      index_min
      if(index_min==1){
        downvalue=min_cme
      }else{
        downvalue=out[index_min-1,2]
      }

      if(index_min==10){
        upvalue=min_cme
      }else{
        upvalue=out[index_min+1,2]
        
      }
      
      #Elegir el rango
      if(index_min==1){
        rango[1]=out[index_min,1]
        rango[2]=out[index_min+1,1]
      }else if(index_min<10&&downvalue>upvalue){
        rango[1]=out[index_min,1]
        rango[2]=out[index_min+1,1]
      }else if(index_min<10&&downvalue<upvalue){
        rango[1]=out[index_min,1]
        rango[2]=out[index_min-1,1]
      }else if(index_min==10){
        rango[1]=out[index_min,1]
        rango[2]=out[index_min-1,1]
      }
      
      rango=sort(rango)
      
      val=0
      #Valor de incremento de alpha
      alpha=rango[1]
      limite=rango[2]
      
      for (j in 1:10)
      {
        val = val+1
        alpha=alpha+var #valor de la constante de suavizamiento en aumento
        sua = HoltWinters(y, alpha, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
        s1 = sua$fitted[,1] #valores del suavizamiento exponencial
        err_pr_sua = y[2:(length(y))]-s1 #error del pronostico con suavizamiento
        err_pr_sua2 = err_pr_sua^2 #error de pronostico con suavizamiento al cuadrado
        CME_s = sum(err_pr_sua2)/length(s1) #cuadrado medio debido al error con suavizamiento
        print('LIMITE:')
        print(limite)
        print('ALFA:')
        print(alpha)
        out[val,] <- c(alpha,CME_s)
        bool=alpha<limite
      }
      
      #ITERACION
      print('Iteracion:')
      print(i)
      print('Matriz de resultados:')
      print(out)
      min_cme=min(out[,2])
      #Valor minimo de CME
      print('Valor minimo CME')
      print(min_cme)
      #Index del valor minimo
      index_min=which.min(out[,2])
      alpha=out[index_min,1]
      #Valor de alfa usado para encontrar el minimo CME
      print('Alfa de CME')
      print(alpha)
    }

  }
  MSE = mse(y[c(2:length(y))],s1)
  RMSE = rmse(y[c(2:length(y))],s1)
  MAE = mae(y[c(2:length(y))],s1)
  tmp=y[c(2:length(y))]
  MAPE = mape(y[c(2:length(y))],s1)
  toc()
  print("MSE:")
  print(MSE)
  print("RMSE:")
  print(RMSE)
  print("MAE:")
  print(MAE)
  print("MAPE:")
  print(MAPE)
  print(paste("Tiempo de Ejecucion:"))
  print("ms")
}

#aproximacion_sucesiva(y, 3)