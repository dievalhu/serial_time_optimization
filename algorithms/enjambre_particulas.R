############LIBRERIAS##############
library(Metrics)
library(readxl)
library(tictoc)

##############FUNCION CREAR ENJAMBRE######################
crear_enjambre <- function(n_particulas,
                           n_variables,
                           limites_inf = NULL,
                           limites_sup = NULL,
                           verbose     = TRUE) {
  
  enjambre        <- vector(length = 2, mode = "list")
  names(enjambre) <- c("particulas", "mejor_particula")
  
  enjambre[["particulas"]]        <- vector(length = n_particulas, mode = "list")
  names(enjambre[["particulas"]]) <- paste0("particula_", 1:n_particulas)

  enjambre[["mejor_particula"]]   <- list()
  
  for (i in 1:n_particulas) {
    enjambre[["particulas"]][[i]] <- crear_particula(
      n_variables = n_variables,
      limites_inf = limites_inf,
      limites_sup = limites_sup,
      verbose     = verbose
    )
  }
  
  if (verbose) {
    print("Enjambre creado")
    print("---------------")
    print(paste("Número de partículas =", n_particulas))
    cat("\n")
  }
  
  return(enjambre)
}

##############FUNCION CREAR PARTICULA######################
crear_particula <- function(n_variables,
                            limites_inf = NULL,
                            limites_sup = NULL,
                            verbose     = TRUE) {
  
  v_cmec <-c()
  i=1
  # Comprobaciones
  if (!is.null(limites_inf) & (length(limites_inf) != n_variables)) {
    stop(paste(
      "limites_inf debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere límite, emplear NA.",
      "Ejemplo: limites_inf = c(10, NA, 10)"
    ))
  } else if (!is.null(limites_sup) & length(limites_sup) != n_variables) {
    stop(paste(
      "limites_sup debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere límite, emplear NA.",
      "Ejemplo: limites_sup = c(10, NA, 10)"
    ))
  } else if (is.null(limites_sup) | is.null(limites_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los límites dentro de los",
      "cuales debe buscarse la solución de cada variable.",
      "Por defecto se emplea [-10^3, 10^3]."
    ))
  } else if (any(c(is.na(limites_sup), is.na(limites_inf)))) {
    warning(paste(
      "Los límites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."
    ))
    cat("\n")
  }
  
  if (is.null(limites_inf)) {
    limites_inf <- rep(x = -10^3, times = n_variables)
  }

  if (is.null(limites_sup)) {
    limites_sup <- rep(x = 10^3, times = n_variables)
  }

  if (!is.null(limites_inf)) {
    limites_inf[is.na(limites_inf)] <- -10^3
  }
  if (!is.null(limites_sup)) {
    limites_sup[is.na(limites_sup)] <- 10^3
  }
  
  # Se crea una lista que representa la partícula.
  particula        <- vector(length = 5, mode = "list")
  names(particula) <- c("posicion", "valor", "velocidad", "mejor_valor",
                        "mejor_posicion")
  
  posicion <- rep(NA, times = n_variables)
  for (i in 1:n_variables) {

    posicion[i] <- runif(n = 1, min = limites_inf[i], max = limites_sup[i])
  }
  particula[["posicion"]] <- posicion

  velocidad <- rep(0, times = n_variables)
  particula[["velocidad"]] <- velocidad
  
  if (verbose) {
    print("Nueva partícula creada")
    print("----------------------")
    print(paste(
      "Posición:",
      paste(particula[["posicion"]], collapse = ", ")
    ))
    cat("\n")
  }
  return(particula)
}

##############FUNCION EVALUAR ENJAMBRE#####################
evaluar_enjambre <- function(enjambre,
                             funcion_objetivo,
                             optimizacion,
                             verbose = TRUE) {
  
  enjambre[["particulas"]] <- purrr::map(
    .x               = enjambre[["particulas"]],
    .f               = evaluar_particula,
    funcion_objetivo = funcion_objetivo,
    optimizacion     = optimizacion,
    verbose          = verbose
  )
  
  mejor_particula <- enjambre[["particulas"]][[1]]
  
  for (i in seq_along(enjambre[["particulas"]])) {
    if (optimizacion == "minimizar") {
      if (enjambre[["particulas"]][[i]][["valor"]] < mejor_particula[["valor"]]) {
        mejor_particula <- enjambre[["particulas"]][[i]]
      }
    } else {
      if (enjambre[["particulas"]][[i]][["valor"]] > mejor_particula[["valor"]]) {
        mejor_particula <- enjambre[["particulas"]][[i]]
      }
    }
  }
  
  enjambre[["mejor_particula"]] <- mejor_particula
  
  if (verbose) {
    print("Enjambre evaluado")
    print("-----------------")
    print(paste(
      "Mejor alpha encontrado:",
      paste(mejor_particula[["posicion"]], collapse = ", ")
    ))
    print(paste(
      "Menor error encontrado:",
      mejor_particula[["valor"]]
    ))
    cat("\n")
  }
  
  return(enjambre)
}

##############FUNCION EVALUAR PARTICULA####################
evaluar_particula <- function(particula,
                              funcion_objetivo,
                              optimizacion,
                              verbose = TRUE) {
  
  if (!optimizacion %in% c("maximizar", "minimizar")) {
    stop("El argumento optimizacion debe ser: maximizar o minimizar")
  }

  valor <- do.call(
    funcion_objetivo,
    args = as.list(particula[["posicion"]])
  )

  particula[["valor"]] <- valor[4]

  if (is.null(particula[["mejor_valor"]])) {
    particula[["mejor_valor"]]    <- particula[["valor"]]
    particula[["mejor_posicion"]] <- particula[["posicion"]]
  } else{
    if (optimizacion == "minimizar") {
      if (particula[["valor"]] < particula[["mejor_valor"]]) {
        particula[["mejor_valor"]]    <- particula[["valor"]]
        particula[["mejor_posicion"]] <- particula[["posicion"]]
      }
    } else {
      if (particula[["valor"]] > particula[["mejor_valor"]]) {
        particula[["mejor_valor"]]    <- particula[["valor"]]
        particula[["mejor_posicion"]] <- particula[["posicion"]]
      }
    } 
  }
  
  if (verbose) {
    print(paste("La partícula ha sido evaluada"))
    print("-----------------------------")
    print(paste("CME:", valor[1]))
    print(paste("RMSE:", valor[2]))
    print(paste("MAE:", valor[3]))
    print(paste("MASE:", valor[4]))
    cat("\n")
  }
  
  #plot(args,y)
  
  return(particula)
  
}

enjambre_particulas <- function(y){
  funcion <- function(x1){
    sua = HoltWinters(y, x1, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
    s1 = sua$fitted[,1] #valores del suavizamiento exponencial
    err_pr_sua = y[2:(length(y))]-s1 #error del pronostico con suavizamiento
    err_pr_sua2 = err_pr_sua^2 
    CME_s = sum(err_pr_sua2)/length(s1)
    RMSE = rmse(y[2:length(y)], s1)
    MAE = mae(y[2:length(y)], s1)
    MASE = mase(y[2:length(y)], s1, step_size = 1)
    a = c(CME_s, RMSE, MAE, MASE)
    return(a)
  }
  alphas <- c()
  tic()
  for (i in 1:10){
    enjambre1 <- crear_enjambre(
      n_particulas = 100,
      n_variables  = 1,
      limites_inf  = c(0),
      limites_sup  = c(1),
      verbose      = TRUE
    )
    
    enjambre1 <- evaluar_enjambre(
      enjambre         = enjambre1,
      funcion_objetivo = funcion,
      optimizacion     = "minimizar",
      verbose          = TRUE
    )
    alphas <- rbind(alphas, enjambre1$mejor_particula$mejor_posicion)
  }
  toc()
  colnames(alphas) <- c("alpha")
  alphas = sort(alphas)
  cat("Alpha óptimo final: ", mean(alphas))
  medidas = funcion(mean(alphas))
  result = cbind(mean(alphas), medidas[1], medidas[2], medidas[3], medidas[4])
  colnames(result) = c("Alfa", "MSE", "RMSE", "MAE", "MAPE")
  return(result)
}