# funciones.R 

#' Construye una tabla de vida abreviada
#' 
#' @param x Vector de edades (inicio de cada intervalo)
#' @param n Vector de amplitudes de los intervalos (para el último grupo, NA)
#' @param mx Vector de tasas específicas de mortalidad
#' @param sex Sexo ("m" para hombres, "f" para mujeres) - usado para los coeficientes ax
#' @param radix Número de sobrevivientes al nacer (default 100000)
#' @return Data frame con la tabla de vida completa
#'
lt_abr <- function(x, n, mx, sex = "m", radix = 100000) {
  
  # Validaciones básicas
  if (length(x) != length(mx)) stop("x y mx deben tener la misma longitud")
  if (length(n) != length(mx)) stop("n y mx deben tener la misma longitud")
  
  # 1. Calcular ax (años vividos en promedio por los que mueren en el intervalo)
  # Usando método de Coale-Demeny
  ax <- numeric(length(mx))
  
  for (i in seq_along(mx)) {
    if (is.na(n[i])) {
      # Último grupo abierto: ax = 1/mx (suponiendo mortalidad constante)
      ax[i] <- 1 / mx[i]
    } else if (n[i] == 1) {
      # Edad 0: usar valores específicos por sexo
      m0 <- mx[i]
      if (sex == "m") {
        # Hombres
        if (m0 >= 0.107) {
          ax[i] <- 0.330
        } else {
          ax[i] <- 0.045 + 2.684 * m0
        }
      } else {
        # Mujeres
        if (m0 >= 0.107) {
          ax[i] <- 0.350
        } else {
          ax[i] <- 0.053 + 2.800 * m0
        }
      }
    } else if (n[i] == 4 && x[i] == 1) {
      # Grupo 1-4 años
      m0 <- mx[1]  # Usar m0 del primer grupo
      if (sex == "m") {
        # Hombres
        if (m0 >= 0.107) {
          ax[i] <- 1.352
        } else {
          ax[i] <- 1.651 - 2.816 * m0
        }
      } else {
        # Mujeres
        if (m0 >= 0.107) {
          ax[i] <- 1.361
        } else {
          ax[i] <- 1.522 - 1.518 * m0
        }
      }
    } else {
      # Resto de grupos: asumir ax = n/2
      ax[i] <- n[i] / 2
    }
  }
  
  # 2. Calcular qx (probabilidad de morir en el intervalo)
  qx <- numeric(length(mx))
  for (i in seq_along(mx)) {
    if (is.na(n[i])) {
      # Último grupo abierto: qx = 1
      qx[i] <- 1
    } else {
      qx[i] <- (n[i] * mx[i]) / (1 + (n[i] - ax[i]) * mx[i])
      # Asegurar que qx no sea mayor que 1
      if (qx[i] > 1) qx[i] <- 1
    }
  }
  
  # 3. Calcular lx (sobrevivientes al inicio del intervalo)
  lx <- numeric(length(mx))
  lx[1] <- radix
  for (i in 1:(length(mx) - 1)) {
    lx[i + 1] <- lx[i] * (1 - qx[i])
  }
  
  # 4. Calcular dx (defunciones en el intervalo)
  dx <- lx * qx
  
  # 5. Calcular Lx (años persona vividos en el intervalo)
  Lx <- numeric(length(mx))
  for (i in seq_along(mx)) {
    if (is.na(n[i])) {
      # Último grupo abierto
      Lx[i] <- lx[i] / mx[i]
    } else {
      Lx[i] <- n[i] * lx[i + 1] + ax[i] * dx[i]
      # En caso de que lx[i+1] no esté definido para el último grupo
      if (i == length(mx) && is.na(lx[i + 1])) {
        Lx[i] <- ax[i] * dx[i]
      }
    }
  }
  
  # 6. Calcular Tx (total de años persona por vivir a partir de edad x)
  Tx <- numeric(length(mx))
  Tx[length(mx)] <- Lx[length(mx)]
  for (i in (length(mx) - 1):1) {
    Tx[i] <- Tx[i + 1] + Lx[i]
  }
  
  # 7. Calcular ex (esperanza de vida a la edad x)
  ex <- Tx / lx
  
  # 8. Crear data frame resultado
  resultado <- data.frame(
    x = x,
    n = n,
    mx = round(mx, 6),
    qx = round(qx, 6),
    ax = round(ax, 2),
    lx = round(lx, 0),
    dx = round(dx, 0),
    Lx = round(Lx, 0),
    Tx = round(Tx, 0),
    ex = round(ex, 2)
  )
  
  return(resultado)
}

#  Suavizamiento por media móvil
#' @param mx Vector de tasas específicas de mortalidad
#' @param ventana Número de edades a promediar (3 o 5, debe ser impar)
#' @return Vector suavizado (los extremos se conservan sin cambio)
#'
suavizar_mx <- function(mx, ventana = 3) {
  if (ventana %% 2 == 0) {
    stop("La ventana debe ser impar (3, 5, 7...)")
  }
  
  n <- length(mx)
  mitad <- floor(ventana / 2)
  mx_suav <- mx  # Copia inicial
  
  for (i in (mitad + 1):(n - mitad)) {
    idx <- (i - mitad):(i + mitad)
    mx_suav[i] <- mean(mx[idx], na.rm = TRUE)
  }
  
  return(mx_suav)
}

#' Aplica suavizamiento secuencial 
#'
#' @param mx Vector de tasas
#' @param edad_max_ventana3 Hasta qué edad usar ventana 3 (default 60)
#' @return Vector suavizado combinado
#'
suavizar_completo <- function(mx, edad_max_ventana3 = 60) {
  n <- length(mx)
  edades <- 0:(n-1)
  
  # Ventana 3 para edades jóvenes y adultas (0-60)
  mx_suav3 <- suavizar_mx(mx, ventana = 3)
  
  # Ventana 5 para edades avanzadas (>60)
  mx_suav5 <- suavizar_mx(mx, ventana = 5)
  
  # Combinar
  mx_combinado <- mx_suav3
  mx_combinado[edades > edad_max_ventana3] <- mx_suav5[edades > edad_max_ventana3]
  
  return(mx_combinado)
}

#  Descomposición de diferencias de e0 
#' 
#' Método de Arriaga 
#' 
#' @param lx1 Vector de sobrevivientes del año 1
#' @param lx2 Vector de sobrevivientes del año 2
#' @param Lx1 Vector de años-persona vividos del año 1
#' @param Lx2 Vector de años-persona vividos del año 2
#' @param edades Vector de edades (inicio de cada intervalo)
#' @param n Vector de amplitudes de los intervalos
#' @return Data frame con efectos por edad
#'
descomponer_e0 <- function(lx1, lx2, Lx1, Lx2, edades, n) {
  
  l0 <- lx1[1]  # Radix (usualmente 100,000)
  
  # Efecto directo + indirecto por grupo de edad
  efecto <- numeric(length(edades))
  
  for (i in 1:(length(edades) - 1)) {
    # Término 1: Efecto directo
    term1 <- (lx1[i] / l0) * (
      (Lx2[i] / lx2[i]) - (Lx1[i] / lx1[i])
    )
    
    # Término 2: Efecto indirecto 
    term2 <- (Lx2[i+1] / l0) * (
      (lx1[i] / lx2[i]) - (lx1[i+1] / lx2[i+1])
    )
    
    efecto[i] <- term1 + term2
  }
  
  # Último grupo abierto
  i <- length(edades)
  term1 <- (lx1[i] / l0) * (
    (Lx2[i] / lx2[i]) - (Lx1[i] / lx1[i])
  )
  efecto[i] <- term1
  
  resultado <- data.frame(
    edad_inicio = edades,
    amplitud = n,
    efecto = efecto,
    efecto_acumulado = cumsum(efecto)
  )
  
  return(resultado)
}

#' Calcula la descomposición entre dos tablas de vida completas
#'
#' @param tabla1 Data frame con columnas: age, lx, Lx, ex
#' @param tabla2 Data frame con columnas: age, lx, Lx, ex
#' @param año1 Nombre del año 1 (para etiquetas)
#' @param año2 Nombre del año 2
#' @return Data frame con efectos por edad
#'
descomponer_tablas <- function(tabla1, tabla2, año1, año2, sexo) {
  
  # Ordenar por edad
  tabla1 <- tabla1[order(tabla1$age), ]
  tabla2 <- tabla2[order(tabla2$age), ]
  
  # Verificar que las edades coinciden
  if (!all(tabla1$age == tabla2$age)) {
    warning("Las edades no coinciden exactamente entre tablas, usando intersección")
    edades_comunes <- intersect(tabla1$age, tabla2$age)
    tabla1 <- tabla1[tabla1$age %in% edades_comunes, ]
    tabla2 <- tabla2[tabla2$age %in% edades_comunes, ]
  }
  
  # Calcular amplitudes
  edades <- tabla1$age
  n <- c(diff(edades), 5)
  n[length(n)] <- NA
  
  descomp <- descomponer_e0(
    lx1 = tabla1$lx,
    lx2 = tabla2$lx,
    Lx1 = tabla1$Lx,
    Lx2 = tabla2$Lx,
    edades = edades,
    n = n
  )
  
  descomp$sexo <- sexo
  descomp$periodo <- paste0(año1, "-", año2)
  descomp$diferencia_total <- tabla2$ex[1] - tabla1$ex[1]
  
  return(descomp)
}

#  APV (Años Potenciales de Vida Perdidos)

#' Calcula (APV)
#' 
#' @param defunciones Vector de defunciones por edad
#' @param esperanza_vida_referencia Esperanza de vida a cada edad (e_x de tabla estándar)
#' @param edad_limite Límite superior (ej. 75 años)
#' @return APV total y por edad
#'
calcular_apv <- function(defunciones, esperanza_vida_referencia, edad_limite = 75) {
  n <- length(defunciones)
  edades <- 0:(n-1)
  
  # Solo edades menores al límite
  idx <- edades < edad_limite
  
  apv_por_edad <- defunciones[idx] * esperanza_vida_referencia[idx]
  apv_total <- sum(apv_por_edad, na.rm = TRUE)
  
  resultado <- data.frame(
    edad = edades[idx],
    defunciones = defunciones[idx],
    esperanza_referencia = esperanza_vida_referencia[idx],
    apv = apv_por_edad[idx]
  )
  
  attr(resultado, "apv_total") <- apv_total
  return(resultado)
}

#' Estima defunciones desde mx y población expuesta
#'
#' @param mx Vector de tasas de mortalidad
#' @param poblacion Vector de población expuesta (AVP)
#' @return Vector de defunciones estimadas
#'
estimar_defunciones <- function(mx, poblacion) {
  return(mx * poblacion)
}

#' Construye esperanza de vida estándar (decrecimiento lineal)
#'
#' @param e0 Esperanza de vida al nacer (referencia)
#' @param edad_max Edad máxima (default 100)
#' @return Vector de e_x para cada edad
#'
construir_ex_estandar <- function(e0, edad_max = 100) {
  # Método simplificado: e_x decrece aproximadamente 1 año por cada año de edad
  ex <- pmax(e0 - 0:(edad_max-1), 0.5)
  return(ex)
}