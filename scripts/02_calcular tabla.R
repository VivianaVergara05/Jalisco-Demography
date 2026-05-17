# scripts/02_calcular_tablas.R

# CONSTRUCCIÓN DE TABLAS DE VIDA COMPLETAS

rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)

source("scripts/funciones.R")

# 1. CARGAR DATOS

cat("\n Cargando archivos...\n")

# AVP
avp_2010 <- read_excel("data/raw/AVP_2010.xlsx", sheet = "Hoja1")
avp_2019 <- read_excel("data/raw/AVP_2019.xlsx", sheet = "Hoja1")
avp_2021 <- read_excel("data/raw/AVP_2021.xlsx", sheet = "Hoja1")

# nMx
nmx_2010 <- read_excel("data/raw/nMx_2010.xlsx", sheet = "Hoja1")
nmx_2019 <- read_excel("data/raw/nMx_2019.xlsx", sheet = "Hoja1")
nmx_2021 <- read_excel("data/raw/nMx_2021.xlsx", sheet = "Hoja1")

cat("Archivos cargados\n")


# 2. FUNCIÓN PARA CONSTRUIR TABLA DE VIDA 

construir_tabla_vida <- function(avp_df, nmx_df, sexo, año) {
  
  # Extraer mx según sexo
  if (sexo == "Hombre") {
    mx_valores <- nmx_df$Hombre
  } else {
    mx_valores <- nmx_df$Mujer
  }
  
  nmx_2010_ref <- read_excel("data/raw/nMx_2010.xlsx", sheet = "Hoja1")
  if (sexo == "Hombre") {
    ref_2010 <- nmx_2010_ref$Hombre[!is.na(nmx_2010_ref$Hombre)][1]
  } else {
    ref_2010 <- nmx_2010_ref$Mujer[!is.na(nmx_2010_ref$Mujer)][1]
  }
  
  primer_valido <- mx_valores[!is.na(mx_valores)][1]
  
  # ---> CORRECCIÓN AQUÍ: Abrimos el IF para la corrección
  if (año != 2010 && !is.na(primer_valido) && primer_valido > 0) {
    factor_correccion <- ref_2010 / primer_valido
    mx_valores <- mx_valores * factor_correccion
    cat("  ", año, "-", sexo, ": Factor de corrección aplicado =", round(factor_correccion, 2), "\n")
  } # ---> CORRECCIÓN AQUÍ: Se cierra el IF aquí para que no se trague el resto de la función
  
  primer_mx_valido <- mx_valores[!is.na(mx_valores)][1]
  
  # Estimar mx para edades 0 y 1-4 usando relaciones demográficas típicas
  mx_0 <- primer_mx_valido * 100    # Mortalidad infantil ~100x la de 5-9 años
  mx_1_4 <- primer_mx_valido * 10   # Mortalidad 1-4 años ~10x la de 5-9 años
  
  cat("  ", año, "-", sexo, ": mx_0 =", formatC(mx_0, format = "e", digits = 3), 
      "| mx_5_9 =", formatC(primer_mx_valido, format = "e", digits = 3), "\n")
  
  # Crear vector completo de mx
  mx_completo <- c(mx_0, mx_1_4, mx_valores[3:length(mx_valores)])
  mx_clean <- mx_completo[!is.na(mx_completo)]
  
  x <- 1:length(mx_clean)
  n <- c(1, 4, rep(5, length(mx_clean) - 2))
  n[length(n)] <- NA
  
  edades_originales <- nmx_df$Edad[!is.na(nmx_df$Hombre)]
  edades_completas <- c("0", "1 a 4", edades_originales)
  edades_clean <- gsub("De ", "", edades_completas)
  edades_clean <- gsub(" años", "", edades_clean)
  edades_clean <- gsub(" y más", "+", edades_clean)
  
  lt <- lt_abr(x = x, n = n, mx = mx_clean, sex = ifelse(sexo == "Hombre", "m", "f"))
  
  lt$edad <- edades_clean
  lt$año <- año
  lt$sexo <- sexo
  
  return(lt)
}

# 3. CONSTRUIR TABLAS PARA CADA AÑO Y SEXO

cat("\n Construyendo tablas de vida...\n")

# 2010
lt_2010_h <- construir_tabla_vida(avp_2010, nmx_2010, "Hombre", 2010)
lt_2010_m <- construir_tabla_vida(avp_2010, nmx_2010, "Mujer", 2010)

# 2019
lt_2019_h <- construir_tabla_vida(avp_2019, nmx_2019, "Hombre", 2019)
lt_2019_m <- construir_tabla_vida(avp_2019, nmx_2019, "Mujer", 2019)

# 2021
lt_2021_h <- construir_tabla_vida(avp_2021, nmx_2021, "Hombre", 2021)
lt_2021_m <- construir_tabla_vida(avp_2021, nmx_2021, "Mujer", 2021)

cat("Tablas construidas\n")


# 4. VERIFICAR QUE lx NO SEA NA

cat("\n Verificando primeras filas de lt_2021_h:\n")
print(lt_2021_h[, c("x", "edad", "mx", "lx", "ex")] %>% head(10))


# 5. EXTRAER ESPERANZAS DE VIDA (e0)

tabla_e0 <- data.frame(
  Año = c(2010, 2010, 2019, 2019, 2021, 2021),
  Sexo = rep(c("Hombres", "Mujeres"), 3),
  e0 = round(c(lt_2010_h$ex[1], lt_2010_m$ex[1],
               lt_2019_h$ex[1], lt_2019_m$ex[1],
               lt_2021_h$ex[1], lt_2021_m$ex[1]), 2)
)

cat("\n ESPERANZAS DE VIDA AL NACER (e0):\n")
print(tabla_e0)


# 6. GUARDAR RESULTADOS

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

write.csv(tabla_e0, "output/tablas/esperanzas_vida.csv", row.names = FALSE)

# Modifica esto en tu script de R y vuelve a correrlo completo (Source)
saveRDS(
  list(
    lt_2010_h = lt_2010_h,  # <-- AGREGA ESTA LÍNEA
    lt_2010_m = lt_2010_m,  # <-- AGREGA ESTA LÍNEA
    lt_2019_h = lt_2019_h, 
    lt_2019_m = lt_2019_m,
    lt_2021_h = lt_2021_h, 
    lt_2021_m = lt_2021_m
  ), 
  file = "output/tablas/tablas_vida_completas.rds"
)

# Mensajes de control limpios
cat("\n Resultados guardados en output/tablas/\n")
cat("\n PROCESO COMPLETADO\n")