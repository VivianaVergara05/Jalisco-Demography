# scripts/funciones.R
# ====================================================
# FUNCIONES PARA TABLAS DE VIDA
# ====================================================

# ----------------------------------------------------
# TABLA DE VIDA ABREVIADA (Método de Coale-Demeny)
# ----------------------------------------------------

lt_abr <- function(x, n, mx, sex = "f") {
  
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
  
  # 1. Probabilidad de morir qx
  qx <- (n * mx) / (1 + (n - ax) * mx)
  qx[m] <- 1
  
  # 2. Probabilidad de sobrevivir px
  px <- 1 - qx
  
  # 3. Sobrevivientes lx (con radix l0 = 100,000)
  lx <- rep(NA, m)
  lx[1] <- 100000
  
  for (i in 1:(m-1)) {
    if (!is.na(px[i])) {
      lx[i+1] <- lx[i] * px[i]
    } else {
      lx[i+1] <- 0
    }
  }
  
  # 4. Muertes dx
  dx <- rep(NA, m)
  for (i in 1:(m-1)) {
    dx[i] <- lx[i] - lx[i+1]
  }
  dx[m] <- lx[m]
  
  # 5. Años vividos Lx
  Lx <- rep(NA, m)
  for (i in 1:(m-1)) {
    Lx[i] <- n[i] * lx[i+1] + ax[i] * dx[i]
  }
  # Último grupo abierto
  Lx[m] <- lx[m] / mx[m]
  
  # 6. Años acumulados Tx
  Tx <- rev(cumsum(rev(Lx)))
  
  # 7. Esperanza de vida ex
  ex <- Tx / lx
  
  # Resultado
  resultado <- data.frame(
    x = x,
    n = n,
    mx = mx,
    ax = ax,
    qx = qx,
    px = px,
    lx = lx,
    dx = dx,
    Lx = Lx,
    Tx = Tx,
    ex = ex
  )
  
  return(resultado)
}

# ----------------------------------------------------
# CRECIMIENTO EXPONENCIAL
# ----------------------------------------------------

crecimiento_exp <- function(P0, Pt, t0, tt, t_deseada) {
  r <- log(Pt / P0) / (tt - t0)
  P_deseada <- P0 * exp(r * (t_deseada - t0))
  return(P_deseada)
}

cat("✅ Funciones cargadas correctamente\n")