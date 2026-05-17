# 04_descomposicion.R 

library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())
cat(" Entorno limpiado\n")

source("scripts/funciones.R")

# Cargar tablas de vida
cat("\n Cargando tablas de vida...\n")

if (file.exists("output/tablas/tablas_vida_completas.rds")) {
  tablas_lista <- readRDS("output/tablas/tablas_vida_completas.rds")
  cat("   Tablas de vida cargadas\n")
} else {
  stop(" No se encontró el archivo. Ejecuta primero 02_calcular_tablas.R")
}

# Convertir la lista a un solo data frame
tablas_vida <- bind_rows(
  tablas_lista$lt_2010_h %>% mutate(year = 2010, sex = "Hombres"),
  tablas_lista$lt_2010_m %>% mutate(year = 2010, sex = "Mujeres"),
  tablas_lista$lt_2019_h %>% mutate(year = 2019, sex = "Hombres"),
  tablas_lista$lt_2019_m %>% mutate(year = 2019, sex = "Mujeres"),
  tablas_lista$lt_2021_h %>% mutate(year = 2021, sex = "Hombres"),
  tablas_lista$lt_2021_m %>% mutate(year = 2021, sex = "Mujeres")
)

if ("edad" %in% names(tablas_vida) && !"age" %in% names(tablas_vida)) {
  tablas_vida <- tablas_vida %>% rename(age = edad)
}

# Verificar columnas necesarias
columnas_necesarias <- c("age", "lx", "Lx", "ex", "year", "sex")
for (col in columnas_necesarias) {
  if (!col %in% names(tablas_vida)) {
    stop(paste("Falta columna:", col))
  }
}

cat("\n Columnas verificadas\n")
print(head(tablas_vida[, c("age", "lx", "ex", "year", "sex")]))

