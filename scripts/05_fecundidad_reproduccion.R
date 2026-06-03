# 04_fecundidad_reproduccion

library(readxl)
library(dplyr)

# Limpiar el entorno de trabajo
rm(list = ls())

# 1. CARGAR MICRODATOS DE NACIMIENTO (INEGI) Y TABLAS DE VIDA (RDS SCRIPT 02)
cat("Cargando archivos de datos locales...\n")

# Cargar nacimientos de Jalisco
df_nac_crudo <- read_excel("data/raw/nacimientos_2019_2010.xlsx")

# Homologar etiquetas oficiales del INEGI para los grupos de edad de la madre
etiquetas_inegi <- c("Total Ocurridos", "Menor de 15 años", "De 15 a 19 años", 
                     "De 20 a 24 años", "De 25 a 29 años", "De 30 a 34 años", 
                     "De 35 a 39 años", "De 40 a 44 años", "De 45 a 49 años", 
                     "De 50 y más años", "No especificado")

df_nacimientos <- data.frame(
  Grupo_Edad = etiquetas_inegi,
  Nac_2019   = as.numeric(df_nac_crudo[[2]]),
  Nac_2010   = as.numeric(df_nac_crudo[[3]])
)

# Cargar tablas de vida completas para extraer denominadores de exposición femeninos (lx)
tablas_vida <- readRDS("output/tablas/tablas_vida_completas.rds")

# 2. DEFINICIÓN DE LA FUNCIÓN MATEMÁTICA DE CÁLCULO 
calcular_medidas_sinteticas <- function(nac_vector, lx_vector, anio) {
  
  # Constantes Demográficas de la UNAM
  n <- 5            # Amplitud de los intervalos quinquenales de edad
  K <- 100 / 205    # Factor biológico de nacimientos femeninos (0.487805)
  radix <- 100000   # l0 estándar de la tabla de vida
  
  # Estructurar matriz de trabajo para el rango fértil (15 a 49 años exactos)
  df_tramo <- data.frame(
    Grupo = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
    B_x   = nac_vector,   # Nacimientos totales observados en el año
    l_x_F = lx_vector     # Sobrevivientes femeninas al punto medio (exposición)
  )
  
  # a) Tasa Específica de Fecundidad (TEF por mujer)
  df_tramo$tef <- df_tramo$B_x / df_tramo$l_x_F
  
  # b) Tasa Específica de Fecundidad Femenina (TEF_Fem por mujer)
  df_tramo$tef_fem <- df_tramo$tef * K
  
  # c) Probabilidad de Supervivencia Actuarial p(A) 
  df_tramo$p_A <- df_tramo$l_x_F / radix
  
  # d) Cálculos Sintéticos Globales (Sumatorias multiplicadas por la amplitud n)
  TGF_val <- n * sum(df_tramo$tef, na.rm = TRUE)
  TBR_val <- n * sum(df_tramo$tef_fem, na.rm = TRUE)
  TNR_val <- n * sum(df_tramo$tef_fem * df_tramo$p_A, na.rm = TRUE)
  
  cat(paste0("-> Indicadores consolidados para el año ", anio, " calculados con éxito.\n"))
  
  return(data.frame(
    Año = anio,
    TGF = round(TGF_val, 3),
    TBR = round(TBR_val, 3),
    TNR = round(TNR_val, 3)
  ))
}
# 3. EXTRACCIÓN DE VECTORES Y EJECUCIÓN DEL MODELO (JALISCO 2010 VS. 2019)

# Rango reproductivo estándar: renglones 3 al 9 de nuestras etiquetas asignadas
grupos_quinquenales <- c("De 15 a 19 años", "De 20 a 24 años", "De 25 a 29 años", 
                         "De 30 a 34 años", "De 35 a 39 años", "De 40 a 44 años", 
                         "De 45 a 49 años")

nac_2010_jal <- df_nacimientos$Nac_2010[df_nacimientos$Grupo_Edad %in% grupos_quinquenales]
nac_2019_jal <- df_nacimientos$Nac_2019[df_nacimientos$Grupo_Edad %in% grupos_quinquenales]

# Extraer lx femeninas de las tablas de vida guardadas
lx_2010_fem <- as.numeric(tablas_vida$lt_2010_m$lx[5:11])
lx_2019_fem <- as.numeric(tablas_vida$lt_2019_m$lx[5:11])

# Procesar ambos periodos a través de la función del modelo
res_2010 <- calcular_medidas_sinteticas(nac_2010_jal, lx_2010_fem, 2010)
res_2019 <- calcular_medidas_sinteticas(nac_2019_jal, lx_2019_fem, 2019)

# Unificar los resultados en la matriz final de indicadores oficiales
indicadores_jalisco <- bind_rows(res_2010, res_2019)

# 4. EXPORTAR RESULTADOS FÍSICOS DE MANERA AUTOMATIZADA
if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

write.csv(indicadores_jalisco, "output/tablas/indicadores_reproduccion_jalisco.csv", row.names = FALSE)
cat("\n=== SCRIPT 04 FINALIZADO: Archivo 'indicadores_reproduccion_jalisco.csv' guardado ===\n")

# Mostrar resultados finales en la consola para control de la usuaria
print(indicadores_jalisco)