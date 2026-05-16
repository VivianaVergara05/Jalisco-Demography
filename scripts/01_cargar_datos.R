# scripts/01_cargar_datos.R
# ====================================================
# CARGA DE DATOS PROCESADOS (AVP y nMx)
# Jalisco 2010, 2019, 2021
# ====================================================

rm(list = ls())
cat("✅ Entorno limpiado\n")

# Cargar librerías
library(readxl)
library(dplyr)

# ====================================================
# 1. CARGAR AVP (Años Persona Vividos) por año
# ====================================================

cat("\n📥 Cargando archivos AVP (nNx)...\n")

avp_2010 <- read_excel("data/raw/AVP_2010.xlsx", sheet = "Hoja1")
avp_2019 <- read_excel("data/raw/AVP_2019.xlsx", sheet = "Hoja1")
avp_2021 <- read_excel("data/raw/AVP_2021.xlsx", sheet = "Hoja1")

# Limpiar nombres de edades
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

cat("✅ AVP 2010, 2019, 2021 cargados\n")

# ====================================================
# 2. CARGAR nMx (Tasas de mortalidad) por año
# ====================================================

cat("\n📥 Cargando archivos nMx...\n")

nmx_2010 <- read_excel("data/raw/nMx_2010.xlsx", sheet = "Hoja1")
nmx_2019 <- read_excel("data/raw/nMx_2019.xlsx", sheet = "Hoja1")
nmx_2021 <- read_excel("data/raw/nMx_2021.xlsx", sheet = "Hoja1")

nmx_2010 <- limpiar_edades(nmx_2010)
nmx_2019 <- limpiar_edades(nmx_2019)
nmx_2021 <- limpiar_edades(nmx_2021)

cat("✅ nMx 2010, 2019, 2021 cargados\n")

# ====================================================
# 3. VERIFICAR
# ====================================================

cat("\n📊 Resumen de datos cargados:\n")
cat("  - AVP 2010 Hombres: ", sum(avp_2010$Hombre, na.rm = TRUE), "\n")
cat("  - AVP 2010 Mujeres: ", sum(avp_2010$Mujer, na.rm = TRUE), "\n")
cat("  - nMx 2010 Hombres (primeros no NA): ", head(nmx_2010$Hombre[!is.na(nmx_2010$Hombre)], 3), "\n")

# ====================================================
# 4. GUARDAR EN RDS (corregido)
# ====================================================

cat("\n💾 Guardando datos en formato RDS...\n")

# Crear lista de forma segura
datos <- list()

datos$avp <- list()
datos$avp$"2010" <- avp_2010
datos$avp$"2019" <- avp_2019
datos$avp$"2021" <- avp_2021

datos$nmx <- list()
datos$nmx$"2010" <- nmx_2010
datos$nmx$"2019" <- nmx_2019
datos$nmx$"2021" <- nmx_2021

# Crear carpeta si no existe
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# Guardar
saveRDS(datos, "data/processed/datos_avp_nmx.rds")

cat("✅ Datos guardados en data/processed/datos_avp_nmx.rds\n")
cat("\n🎉 CARGA COMPLETADA\n")