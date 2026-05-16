# scripts/funciones.R

lt_abr <- function(x, n, mx, sex = "f", IMR = NA) {
  m <- length(x)
  ax <- n / 2
  
  # Coeficientes para a0 (Edad 0)
  if (sex == "m") { 
    ax[1] <- ifelse(mx[1] >= 0.107, 0.330, 0.045 + 2.684 * mx[1])
  } else {
    ax[1] <- ifelse(mx[1] >= 0.107, 0.350, 0.053 + 2.800 * mx[1])
  }
  
  # Coeficientes para a1 (Edad 1-4)
  if (sex == "m") { 
    ax[2] <- ifelse(mx[1] >= 0.107, 1.352, 1.651 - 2.816 * mx[1])
  } else {
    ax[2] <- ifelse(mx[1] >= 0.107, 1.361, 1.522 - 1.518 * mx[1])
  }
  
  # Probabilidad de morir qx
  qx <- (n * mx) / (1 + (n - ax) * mx)
  qx[m] <- 1 
  
  # Probabilidad de sobrevivir px
  px <- 1 - qx 
  
  # Sobrevivientes lx (Radix 100,000)
  lx <- 100000 * cumprod(c(1, px[-m]))
  
  # Muertes dx
  dx <- c(-diff(lx), lx[m])
  
  # Años vividos Lx
  Lx <- n * c(lx[-1], 0) + ax * dx
  Lx[m] <- lx[m] / mx[m]
  
  # Años vividos acumulados Tx
  Tx <- rev(cumsum(rev(Lx)))
  
  # Esperanza de vida ex
  ex <- Tx / lx
  
  return(data.frame(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex))
}

# Función para crecimiento exponencial
crecimiento_exp <- function(P0, Pt, t0, tt, t_deseada) {
  r <- log(Pt / P0) / (tt - t0)
  P_deseada <- P0 * exp(r * (t_deseada - t0))
  return(P_deseada)
}

cat("Funciones cargadas correctamente\n")
