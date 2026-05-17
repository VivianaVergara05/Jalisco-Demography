# 02_calcular_tablas.R 

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

# 2. FUNCIÓN PARA LIMPIAR EDADES
limpiar_edades <- function(df) {
  df <- df %>%
    mutate(Edad = gsub("De ", "", Edad),
           Edad = gsub(" años", "", Edad),
           Edad = gsub(" y más", "+", Edad))
  return(df)
}

avp_2010 <- limpiar_edades(avp_2010)
avp_2019 <- limpiar_edades(avp_2019)
avp_2021 <- limpiar_edades(avp_2021)

nmx_2010 <- limpiar_edades(nmx_2010)
nmx_2019 <- limpiar_edades(nmx_2019)
nmx_2021 <- limpiar_edades(nmx_2021)

# 3. FUNCIÓN PARA CONSTRUIR TABLA DE VIDA
construir_tabla_vida <- function(avp_df, nmx_df, sexo, año) {
  
  # Extraer mx según sexo
  if (sexo == "Hombre") {
    mx_valores <- nmx_df$Hombre
  } else {
    mx_valores <- nmx_df$Mujer
  }
  

  if (sexo == "Hombre") {
    ref_2010 <- nmx_2010$Hombre[!is.na(nmx_2010$Hombre)][1]
  } else {
    ref_2010 <- nmx_2010$Mujer[!is.na(nmx_2010$Mujer)][1]
  }
  
  primer_valido <- mx_valores[!is.na(mx_valores)][1]
  
0
  if (año != 2010 && !is.na(primer_valido) && primer_valido > 0) {
    factor_correccion <- ref_2010 / primer_valido
    mx_valores <- mx_valores * factor_correccion
  }
  
  primer_mx_valido <- mx_valores[!is.na(mx_valores)][1]
  
  # Estimar mx para edades 0 y 1-4
  mx_0 <- primer_mx_valido * 100
  mx_1_4 <- primer_mx_valido * 10
  
  # Crear vector completo de mx
  mx_completo <- c(mx_0, mx_1_4, mx_valores[3:length(mx_valores)])
  mx_clean <- mx_completo[!is.na(mx_completo)]
  
  # Parámetros de la tabla de vida
  x <- 1:length(mx_clean)
  n <- c(1, 4, rep(5, length(mx_clean) - 2))
  n[length(n)] <- NA
  
  # Etiquetas de edades
  edades_originales <- nmx_df$Edad[!is.na(nmx_df$Hombre)]
  edades_completas <- c("0", "1 a 4", edades_originales)
  edades_clean <- gsub("De ", "", edades_completas)
  edades_clean <- gsub(" años", "", edades_clean)
  edades_clean <- gsub(" y más", "+", edades_clean)
  
  # Construir tabla de vida
  lt <- lt_abr(x = x, n = n, mx = mx_clean, sex = ifelse(sexo == "Hombre", "m", "f"))
  
  # Agregar columnas
  lt$edad <- edades_clean
  lt$age <- edades_clean  # Para compatibilidad con descomposición
  lt$año <- año
  lt$sexo <- sexo
  
  return(lt)
}

# 4. CONSTRUIR TABLAS POR SEPARADO
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



cat("\n")

cat("                     TABLAS DE VIDA - JALISCO 2010\n")


cat("\n------------------- HOMBRES 2010 -------------------\n")
print(lt_2010_h[, c("edad", "mx", "qx", "lx", "dx", "ex")] %>% 
        rename(Edad = edad, mx = mx, qx = qx, lx = lx, dx = dx, ex = ex))

cat("\n------------------- MUJERES 2010 -------------------\n")
print(lt_2010_m[, c("edad", "mx", "qx", "lx", "dx", "ex")] %>% 
        rename(Edad = edad, mx = mx, qx = qx, lx = lx, dx = dx, ex = ex))


cat("                     TABLAS DE VIDA - JALISCO 2019\n")

cat("\n------------------- HOMBRES 2019 -------------------\n")
print(lt_2019_h[, c("edad", "mx", "qx", "lx", "dx", "ex")] %>% 
        rename(Edad = edad, mx = mx, qx = qx, lx = lx, dx = dx, ex = ex))

cat("\n------------------- MUJERES 2019 -------------------\n")
print(lt_2019_m[, c("edad", "mx", "qx", "lx", "dx", "ex")] %>% 
        rename(Edad = edad, mx = mx, qx = qx, lx = lx, dx = dx, ex = ex))

cat("                     TABLAS DE VIDA - JALISCO 2021\n")


cat("\n------------------- HOMBRES 2021 -------------------\n")
print(lt_2021_h[, c("edad", "mx", "qx", "lx", "dx", "ex")] %>% 
        rename(Edad = edad, mx = mx, qx = qx, lx = lx, dx = dx, ex = ex))

cat("\n------------------- MUJERES 2021 -------------------\n")
print(lt_2021_m[, c("edad", "mx", "qx", "lx", "dx", "ex")] %>% 
        rename(Edad = edad, mx = mx, qx = qx, lx = lx, dx = dx, ex = ex))


# 6. ESPERANZAS DE VIDA AL NACER


tabla_e0 <- data.frame(
  Año = c(2010, 2010, 2019, 2019, 2021, 2021),
  Sexo = rep(c("Hombres", "Mujeres"), 3),
  e0 = round(c(lt_2010_h$ex[1], lt_2010_m$ex[1],
               lt_2019_h$ex[1], lt_2019_m$ex[1],
               lt_2021_h$ex[1], lt_2021_m$ex[1]), 2)
)

cat("                     ESPERANZAS DE VIDA AL NACER (e0)\n")
print(tabla_e0)


# 7. GUARDAR RESULTADOS

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

# Guardar tabla de esperanzas
write.csv(tabla_e0, "output/tablas/esperanzas_vida.csv", row.names = FALSE)

# Guardar tablas de vida individuales en archivos CSV separados
write.csv(lt_2010_h, "output/tablas/tabla_vida_2010_hombres.csv", row.names = FALSE)
write.csv(lt_2010_m, "output/tablas/tabla_vida_2010_mujeres.csv", row.names = FALSE)
write.csv(lt_2019_h, "output/tablas/tabla_vida_2019_hombres.csv", row.names = FALSE)
write.csv(lt_2019_m, "output/tablas/tabla_vida_2019_mujeres.csv", row.names = FALSE)
write.csv(lt_2021_h, "output/tablas/tabla_vida_2021_hombres.csv", row.names = FALSE)
write.csv(lt_2021_m, "output/tablas/tabla_vida_2021_mujeres.csv", row.names = FALSE)

# Guardar todas en un solo RDS (para descomposición y APV)
saveRDS(
  list(
    lt_2010_h = lt_2010_h,
    lt_2010_m = lt_2010_m,
    lt_2019_h = lt_2019_h,
    lt_2019_m = lt_2019_m,
    lt_2021_h = lt_2021_h,
    lt_2021_m = lt_2021_m
  ), 
  file = "output/tablas/tablas_vida_completas.rds"
)

cat("\n Resultados guardados en output/tablas/\n")
cat("   - tabla_vida_2010_hombres.csv\n")
cat("   - tabla_vida_2010_mujeres.csv\n")
cat("   - tabla_vida_2019_hombres.csv\n")
cat("   - tabla_vida_2019_mujeres.csv\n")
cat("   - tabla_vida_2021_hombres.csv\n")
cat("   - tabla_vida_2021_mujeres.csv\n")
cat("   - esperanzas_vida.csv\n")
cat("   - tablas_vida_completas.rds\n")

cat("\n PROCESO COMPLETADO\n")