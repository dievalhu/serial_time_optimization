###############LIBRERIAS####################
library(readr)
library(readxl)
library(nortest)
library(Metrics)
library(tictoc)
library(GA)

##############FUNCIÓN CREAR POBLACIÓN#############33
crear_poblacion <- function(n_poblacion, n_variables, limite_inf = NULL,
                            limite_sup = NULL, verbose = TRUE) {
  if (!is.null(limite_inf) & (length(limite_inf) != n_variables)) {
    stop(paste(
      "limite_inf debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere limite, emplear NA.",
      "Ejemplo: lim_sup = c(10, NA, 10)"
    ))
  } else if (!is.null(limite_sup) & length(limite_sup) != n_variables) {
    stop(paste(
      "limite_sup debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere limite, emplear NA.",
      "Ejemplo: lim_sup = c(10, NA, 10)"
    ))
  } else if (is.null(limite_sup) | is.null(limite_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los limites dentro de los",
      "cuales debe buscarse la solucion de cada variable.",
      "Por defecto se emplea [-10^3, 10^3]."
    ))
  } else if (any(any(is.na(limite_sup)), any(is.na(limite_inf)))) {
    warning(paste(
      "Los limites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."
    ))
    cat("\n")
  }
  
  if (is.null(limite_inf)) {
    limite_inf <- rep(x = -10^3, times = n_variables)
  }
  
  if (is.null(limite_sup)) {
    limite_sup <- rep(x = 10^3, times = n_variables)
  }
  
  if (!is.null(limite_inf)) {
    limite_inf[is.na(limite_inf)] <- -10^3
  }
  
  if (!is.null(limite_sup)) {
    limite_sup[is.na(limite_sup)] <- 10^3
  }
  
  poblacion <- matrix(data = NA, nrow = n_poblacion, ncol = n_variables)
  
  for (i in 1:n_poblacion) {
    individuo <- rep(NA, times = n_variables)
    
    for (j in 1:n_variables) {
      individuo[j] <- runif(n = 1, min = limite_inf[j], max = limite_sup[j])
    }
    poblacion[i, ] <- individuo
  }
  
  attr(poblacion, 'fecha_creacion')    <- Sys.time()
  attr(poblacion, 'numero_individuos') <- n_poblacion
  attr(poblacion, "class") <- c("matrix", "poblacion")
  
  if (verbose) {
    cat("Poblacion inicial creada", "\n")
    cat("------------------------", "\n")
    cat("Fecha creacion:", as.character(Sys.time()), "\n")
    cat("Numero de individuos =", n_poblacion, "\n")
    cat("Limites inferiores de cada variable =", paste(limite_inf, collapse = ", "), "\n")
    cat("Limites superiores de cada variable =", paste(limite_sup, collapse = ", "), "\n")
    cat("\n")
  }
  
  return(poblacion)
}

##########FUNCIÓN CALCULAR FITNESS INDIVIDUO#############
calcular_fitness_individuo <- function(individuo, funcion_objetivo, optimizacion,
                                       verbose = TRUE, ...) {
  if (length(individuo) != length(names(formals(funcion_objetivo)))) {
    stop(paste("Los individuos deben tener tantos valores como argumentos tiene",
               "la funcion objetivo."))
  }
  
  if (optimizacion == "maximizar") {
    fitness <- do.call(funcion_objetivo, args = as.list(individuo))
  } else if (optimizacion == "minimizar") {
    fitness <- -(do.call(funcion_objetivo, args = as.list(individuo)))
  } else {
    stop("El argumento optimizacion debe ser maximizar o minimizar.")
  }
  
  if (verbose) {
    cat("El individuo ha sido evaluado", "\n")
    cat("-----------------------------", "\n")
    cat("Optimizacion =", optimizacion, "\n")
    cat("Individuo    =", paste(individuo, collapse = " "), "\n")
    cat("Fitness      =", fitness, "\n")
    cat("\n")
  }
  
  return(fitness)
}

##################FUNCIÓN CALCULAR FITNESS POBLACIÓN##################
calcular_fitness_poblacion <- function(poblacion, funcion_objetivo, optimizacion,
                                       verbose = TRUE, ...) {
  fitness_poblacion <- rep(NA, times = nrow(poblacion))
  
  for (i in 1:nrow(poblacion)) {
    individuo <- poblacion[i, ]
    
    fitness_individuo <- calcular_fitness_individuo(
      individuo        = individuo,
      funcion_objetivo = funcion_objetivo,
      optimizacion     = optimizacion,
      verbose          = verbose
    )
    fitness_poblacion[i] <- fitness_individuo
  }
  
  indice_mejor_individuo <- which.max(fitness_poblacion)
  
  if (optimizacion == "maximizar") {
    valor_funcion <- fitness_poblacion[indice_mejor_individuo]
  } else {
    valor_funcion <- -1*fitness_poblacion[indice_mejor_individuo]
  }
  
  if (verbose) {
    cat("------------------", "\n")
    cat("Poblacion evaluada", "\n")
    cat("------------------", "\n")
    cat("Optimizacion              =", optimizacion, "\n")
    cat("Mejor fitness encontrado  =", fitness_poblacion[indice_mejor_individuo], "\n")
    cat("Mejor solucion encontrada =",
        paste(poblacion[indice_mejor_individuo,], collapse = " "), "\n")
    cat("Valor funcion objetivo    =", valor_funcion, "\n")
    cat("\n")
  }
  
  return(fitness_poblacion)
}

###################FUNCIÓN SELECCIONAR INDIVIDUO################
seleccionar_individuo <- function(vector_fitness, metodo_seleccion = "tournament",
                                  verbose = FALSE) {
  if (!metodo_seleccion %in% c("ruleta", "rank", "tournament")) {
    stop("El método de selección debe de ser ruleta, rank o tournament.")
  }
  if (metodo_seleccion == "ruleta") {
    probabilidad_seleccion <- (vector_fitness) / sum(vector_fitness)
    
    ind_seleccionado <- sample(
      x    = 1:length(vector_fitness),
      size = 1,
      prob = probabilidad_seleccion
    )
  } else if (metodo_seleccion == "rank") {
    probabilidad_seleccion <- 1 / rank(-vector_fitness)
    
    ind_seleccionado <- sample(
      x    = 1:length(vector_fitness),
      size = 1,
      prob = probabilidad_seleccion
    )
  } else if (metodo_seleccion == "tournament") {
    
    ind_candidatos_a <- sample(x = 1:length(vector_fitness), size = 2)
    ind_candidatos_b <- sample(x = 1:length(vector_fitness), size = 2)
    
    ind_ganador_a <- ifelse(
      vector_fitness[ind_candidatos_a[1]] > vector_fitness[ind_candidatos_a[2]],
      ind_candidatos_a[1],
      ind_candidatos_a[2]
    )
    ind_ganador_b <- ifelse(
      vector_fitness[ind_candidatos_b[1]] > vector_fitness[ind_candidatos_b[2]],
      ind_candidatos_b[1],
      ind_candidatos_b[2]
    )
    
    ind_seleccionado <- ifelse(
      vector_fitness[ind_ganador_a] > vector_fitness[ind_ganador_b],
      ind_ganador_a,
      ind_ganador_b
    )
  }
  
  if (verbose) {
    cat("----------------------", "\n")
    cat("Individuo seleccionado", "\n")
    cat("----------------------", "\n")
    cat("Método seleccion    =", metodo_seleccion, "\n")
    cat("Indice seleccionado =", ind_seleccionado, "\n")
  }
  
  return(ind_seleccionado)
}

##################FUNCIÓN CRUZAR INDIVIDUOS#################
cruzar_individuos <- function(parental_1,
                              parental_2,
                              metodo_cruce = "uniforme",
                              verbose = TRUE) {
  if (length(parental_1) != length(parental_2)) {
    stop(paste0(
      "La longitud de los dos vectores que representan a los ",
      "individuos debe ser la misma."
    ))
  }
  if (!(metodo_cruce %in% c("uniforme", "punto_simple"))) {
    stop("El método de cruzamiento debe ser: uniforme o punto_simple.")
  }
  
  descendencia <- rep(NA, times = length(parental_1))
  
  if (metodo_cruce == "uniforme") {
    herencia_parent_1 <- sample(
      x       = c(TRUE, FALSE),
      size    = length(parental_1),
      replace = TRUE
    )
    herencia_parent_2 <- !herencia_parent_1
    
    descendencia[herencia_parent_1] <- parental_1[herencia_parent_1]
    descendencia[herencia_parent_2] <- parental_2[herencia_parent_2]
  } else {
    punto_cruce <- sample(
      x    = 2:length(parental_1),
      size = 1
    )
    descendencia <- c(
      parental_1[1:(punto_cruce - 1)],
      parental_2[punto_cruce:length(parental_1)]
    )
  }

  if (verbose) {
    cat("---------------", "\n")
    cat("Cruce realizado", "\n")
    cat("---------------", "\n")
    cat("Método =", metodo_cruce, "\n")
    cat("Parental 1 = ", "\n")
    cat(parental_1, "\n")
    cat("Parental 2 = ", "\n")
    cat(parental_2, "\n")
    cat("Descendencia = ", "\n")
    cat(descendencia, "\n")
    cat("\n")
  }
  return(descendencia)
}

##################FUNCION MUTAR INDIVIDUOS#################
mutar_individuo <- function(individuo, limite_inf, limite_sup,
                            prob_mut = 0.01, distribucion = "uniforme",
                            media_distribucion = 1, sd_distribucion = 1,
                            min_distribucion = -1, max_distribucion = 1,
                            verbose = TRUE) {
  if (!(distribucion %in% c("normal", "uniforme", "aleatoria"))) {
    stop("El argumento distribución debe ser: normal, uniforme o aleatoria.")
  }
  
  posiciones_mutadas <- runif(n = length(individuo), min = 0, max = 1) < prob_mut
  
  if (distribucion == "normal" | distribucion == "uniforme") {
    if (distribucion == "normal") {
      factor_mut <- rnorm(
        n = sum(posiciones_mutadas),
        mean = media_distribucion,
        sd = sd_distribucion
      )
    }
    if (distribucion == "uniforme") {
      factor_mut <- runif(
        n = sum(posiciones_mutadas),
        min = min_distribucion,
        max = max_distribucion
      )
    }
    
    individuo[posiciones_mutadas] <- individuo[posiciones_mutadas] + factor_mut
    
    for (i in which(posiciones_mutadas)) {
      if (individuo[i] < limite_inf[i]) {
        individuo[i] <- limite_inf[i]
      }
      if (individuo[i] > limite_sup[i]) {
        individuo[i] <- limite_sup[i]
      }
    }
  } else if (distribucion == "aleatoria") {
    for (i in which(posiciones_mutadas)) {
      individuo[i] <- runif(n = 1, min = limite_inf[i], max = limite_sup[i])
    }
  }

  if (verbose) {
    cat("-----------------", "\n")
    cat("Individuo mutado", "\n")
    cat("-----------------", "\n")
    cat("Probabilidad =", prob_mut, "\n")
    cat("Individuo    = ", individuo, "\n")
    cat("\n")
  }
  
  return(individuo)
}

##############FUNCION ALT AGENT#################
optimizar_ga <- function(
    funcion_objetivo,
    n_variables,
    optimizacion,
    limite_inf         = NULL,
    limite_sup         = NULL,
    n_poblacion        = 20,
    n_generaciones     = 50,
    elitismo           = 0.1,
    prob_mut           = 0.01,
    distribucion       = "uniforme",
    media_distribucion = 1,
    sd_distribucion    = 1,
    min_distribucion   = -1,
    max_distribucion   = 1,
    metodo_seleccion   = "tournament",
    metodo_cruce       = "uniforme",
    parada_temprana    = FALSE,
    rondas_parada      = NULL,
    tolerancia_parada  = NULL,
    verbose            = 1,
    y,
    ...) {
  start_time <- Sys.time()
  if (isTRUE(parada_temprana) &
      (is.null(rondas_parada) | is.null(tolerancia_parada)) ) {
    stop(paste(
      "Para activar la parada temprana es necesario indicar un valor",
      "de rondas_parada y de tolerancia_parada."
    ))
  }
  
  if (is.null(limite_sup) | is.null(limite_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los límites dentro de los",
      "cuales debe buscarse la solución de cada variable.",
      "Por defecto se emplea: [-10^3, 10^3]."
    ))
  }
  
  if (any(
    is.null(limite_sup), is.null(limite_inf), any(is.na(limite_sup)),
    any(is.na(limite_inf))
  )) {
    warning(paste(
      "Los límites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."
    ))
    cat("\n")
  }
  
  if (is.null(limite_inf)) {
    limite_inf <- rep(x = -10^3, times = n_variables)
  }
  
  if (is.null(limite_sup)) {
    limite_sup <- rep(x = 10^3, times = n_variables)
  }
  
  if (!is.null(limite_inf)) {
    limite_inf[is.na(limite_inf)] <- -10^3
  }
  
  if (!is.null(limite_sup)) {
    limite_sup[is.na(limite_sup)] <- 10^3
  }
  
  poblaciones          <- vector(mode = "list", length = n_generaciones)
  resultados_fitness   <- vector(mode = "list", length = n_generaciones)
  resultados_individuo <- vector(mode = "list", length = n_generaciones)
  diferencia_abs       <- vector(mode = "list", length = n_generaciones)
  
  for (i in 1:n_generaciones) {
    if (verbose %in% c(1,2)) {
      cat("-------------------", "\n")
      cat("Generación:", paste0(i, "\\", n_generaciones), "\n")
      cat("-------------------", "\n")
    }
    
    if (i == 1) {
      poblacion <- crear_poblacion(
        n_poblacion = n_poblacion,
        n_variables = n_variables,
        limite_inf  = limite_inf,
        limite_sup  = limite_sup,
        verbose     = verbose %in% c(2)
      )
    }
    
    fitness_ind_poblacion <- calcular_fitness_poblacion(
      poblacion        = poblacion,
      funcion_objetivo = funcion_objetivo,
      optimizacion     = optimizacion,
      verbose          = verbose %in% c(2)
    )
    
    poblaciones[[i]]          <- poblacion
    fitness_mejor_individuo   <- max(fitness_ind_poblacion)
    mejor_individuo           <- poblacion[which.max(fitness_ind_poblacion), ]
    resultados_fitness[[i]]   <- fitness_mejor_individuo
    resultados_individuo[[i]] <- mejor_individuo
    
    if (i > 1) {
      diferencia_abs[[i]] <- abs(resultados_fitness[[i - 1]] - resultados_fitness[[i]])
    }

    nueva_poblacion <- matrix(
      data = NA,
      nrow = nrow(poblacion),
      ncol = ncol(poblacion)
    )
    
    if (elitismo > 0) {
      n_elitismo         <- ceiling(nrow(poblacion) * elitismo)
      posicion_n_mejores <- order(fitness_ind_poblacion, decreasing = TRUE)
      posicion_n_mejores <- posicion_n_mejores[1:n_elitismo]
      nueva_poblacion[1:n_elitismo, ] <- poblacion[posicion_n_mejores, ]
    } else {
      n_elitismo <- 0
    }
    
    for (j in (n_elitismo + 1):nrow(nueva_poblacion)) {
   
      indice_parental_1 <- seleccionar_individuo(
        vector_fitness   = fitness_ind_poblacion,
        metodo_seleccion = metodo_seleccion,
        verbose          = verbose %in% c(2)
      )
      indice_parental_2 <- seleccionar_individuo(
        vector_fitness   = fitness_ind_poblacion,
        metodo_seleccion = metodo_seleccion,
        verbose          = verbose %in% c(2)
      )
      parental_1 <- poblacion[indice_parental_1, ]
      parental_2 <- poblacion[indice_parental_2, ]
      
      descendencia <- cruzar_individuos(
        parental_1   = parental_1,
        parental_2   = parental_2,
        metodo_cruce = metodo_cruce,
        verbose      = verbose %in% c(2)
      )
      descendencia <- mutar_individuo(
        individuo    = descendencia,
        prob_mut     = prob_mut,
        limite_inf   = limite_inf,
        limite_sup   = limite_sup,
        distribucion = distribucion,
        media_distribucion = media_distribucion,
        sd_distribucion    = sd_distribucion,
        min_distribucion   = min_distribucion,
        max_distribucion   = max_distribucion,
        verbose            = verbose %in% c(2)
      )
      
      nueva_poblacion[j, ] <- descendencia
    }
    poblacion <- nueva_poblacion
    
    if (parada_temprana && (i > rondas_parada)) {
      ultimos_n <- tail(unlist(diferencia_abs), n = rondas_parada)
      if (all(ultimos_n < tolerancia_parada)) {
        cat(
          "Algoritmo detenido en la generacion", i,
          "por falta cambio mínimo de", tolerancia_parada,
          "durante", rondas_parada,
          "generaciones consecutivas.",
          "\n"
        )
        break()
      }
    }
  }
  indice_mejor_individuo_global <- which.max(unlist(resultados_fitness))
  mejor_fitness_global   <- resultados_fitness[[indice_mejor_individuo_global]]
  mejor_individuo_global <- resultados_individuo[[indice_mejor_individuo_global]]
 
  if (optimizacion == "maximizar") {
    mejor_valor_global <- mejor_fitness_global
  } else {
    mejor_valor_global <- -1*mejor_fitness_global
  }
  
  yopt <- function(y,x){
    sua = HoltWinters(y, alpha=x, beta=FALSE, gamma=FALSE)
    s1 = sua$fitted[,1]
  }
  
  y_p <- yopt(y,mejor_individuo_global)
  m1<-mae(y[2:(length(y))],y_p)
  m2<-rmse(y[2:(length(y))],y_p)
  m3<-mape(y[2:(length(y))],y_p)
  
  resultados_fitness <- unlist(resultados_fitness)
  diferencia_abs     <- c(NA, unlist(diferencia_abs))
  
  resultados_individuo <- resultados_individuo[!sapply(resultados_individuo, is.null)]
  poblaciones          <- poblaciones[!sapply(poblaciones, is.null)]
  
  variables <- sapply(
    X = resultados_individuo,
    FUN = function(x) {
      paste(x, collapse = ", ")
    }
  )
  
  df_resultados <- data.frame(
    generacion        = seq_along(resultados_fitness),
    fitness           = resultados_fitness,
    predictores       = variables,
    diferencia_abs    = diferencia_abs
  )
  
  resultados <- list(
    mejor_individuo_global = mejor_individuo_global,
    mejor_valor_global     = mejor_valor_global,
    mejor_fitness_por_generacion   = resultados_fitness,
    mejor_individuo_por_generacion = resultados_individuo,
    diferencia_abs         = diferencia_abs,
    df_resultados          = df_resultados,
    poblaciones            = poblaciones,
    funcion_objetivo       = funcion_objetivo
  )
  
  end_time <- Sys.time()
  
  attr(resultados, "class") <- "optimizacion_ga"
  attr(resultados, 'fecha_creacion')        <- end_time
  attr(resultados, 'duracion_optimizacion') <- paste(
    difftime(end_time, start_time, "secs"),
    "secs"
  )
  attr(resultados, 'optimizacion')          <- optimizacion
  attr(resultados, 'lim_inf')               <- limite_inf
  attr(resultados, 'lim_sup')               <- limite_sup
  attr(resultados, 'n_poblacion')           <- n_poblacion
  attr(resultados, 'generaciones')          <- i 
  attr(resultados, 'valor_variables')       <- mejor_individuo_global
  attr(resultados, 'mejor_fitness')         <- mejor_fitness_global 
  attr(resultados, 'optimo_encontrado')     <- mejor_valor_global 
  attr(resultados, 'n_poblacion')           <- n_poblacion 
  attr(resultados, 'elitismo')              <- elitismo
  attr(resultados, 'prob_mut')              <- prob_mut
  attr(resultados, 'metodo_seleccion')      <- metodo_seleccion
  attr(resultados, 'metodo_cruce')          <- metodo_cruce
  attr(resultados, 'parada_temprana')       <- parada_temprana
  attr(resultados, 'rondas_parada')         <- rondas_parada
  attr(resultados, 'tolerancia_parada')     <- tolerancia_parada
  
  if (verbose %in% c(1,2)) {
    cat("-----------------------", "\n")
    cat("Optimización finalizada", "\n")
    cat("-----------------------", "\n")
    cat("Duración selección  = ")
    print(difftime(end_time, start_time))
    cat("Número generaciones =", i, "\n")
    cat("Límite inferior     =", paste(limite_inf, collapse = ", "), "\n")
    cat("Límite superior     =", paste(limite_sup, collapse = ", "), "\n")
    cat("Optimización        =", optimizacion,"\n")
    cat("Valor alpha óptimo     =", mejor_individuo_global, "\n")
    cat("MSE    =", mejor_valor_global,"\n")
    cat("RMSE     =", m2, "\n")
    cat("MAE     =", m1, "\n")
    cat("MAPE     =", m3, "\n")
    cat("\n")
  }
  return(resultados)
}

############ALGORITMO GENÉTICO################
algoritmo_genetico <- function(y){
  funcion <- function(x1){
    x1 = NULL
    sua = HoltWinters(y, alpha=x1, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
    s1 = sua$fitted[,1] #valores del suavizamiento exponencial
    err_pr_sua = y[2:(length(y))]-s1 #error del pronostico con suavizamiento
    err_pr_sua2 = err_pr_sua^2 
    CME_s = sum(err_pr_sua2)/length(s1)
  }
  alphas <- c()
  tic()
  for (i in 1:10){
    resultados_ga <- optimizar_ga(
      funcion_objetivo = funcion,
      n_variables      = 1,
      optimizacion     = "minimizar",
      limite_inf       = 0,
      limite_sup       = 1,
      n_poblacion      = 30,
      n_generaciones   = 500,
      elitismo         = 0.01,
      prob_mut         = 0.1,
      distribucion     = "normal",
      min_distribucion = -1,
      max_distribucion = 1,
      metodo_seleccion = "tournament",
      metodo_cruce     = "uniforme",
      parada_temprana  = TRUE,
      rondas_parada    = 10,
      tolerancia_parada = 10^-2,
      verbose          = 1,
      y = y
    )
    alphas <- rbind(alphas, resultados_ga["mejor_individuo_global"])
  }
  toc()
  colnames(alphas) <- c("alpha")
  alphas = sort(unlist(alphas))
  return(mean(alphas))
}