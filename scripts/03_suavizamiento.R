# 03_suavizamiento.R - Suavizado de tasas de mortalidad

library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())
cat("Entorno limpiado\n")

# Cargar funciones
source("scripts/funciones.R")

# Cargar datos  01_cargar_datos.R
datos <- readRDS("data/processed/datos_avp_nmx.rds")

cat("\n Aplicando suavizamiento a las tasas nMx...\n")

# Función para procesar un año
procesar_anio_suav <- function(anio, nmx_df) {
  
  # Extraer edades y vectores mx
  edades <- nmx_df$Edad
  
  # Para hombres
  mx_h <- as.numeric(nmx_df$Hombre)
  mx_h_suav <- suavizar_completo(mx_h, edad_max_ventana3 = 60)
  
  # Para mujeres
  mx_m <- as.numeric(nmx_df$Mujer)
  mx_m_suav <- suavizar_completo(mx_m, edad_max_ventana3 = 60)
  
  # Crear data frame con resultados
  resultado <- data.frame(
    Edad = edades,
    mx_hombres_original = mx_h,
    mx_hombres_suav = mx_h_suav,
    mx_mujeres_original = mx_m,
    mx_mujeres_suav = mx_m_suav,
    año = anio,
    stringsAsFactors = FALSE
  )
  
  return(resultado)
}

# Aplicar a cada año
nmx_suav_list <- list()

for (anio in names(datos$nmx)) {
  nmx_suav_list[[anio]] <- procesar_anio_suav(anio, datos$nmx[[anio]])
  cat(" Año", anio, "procesado\n")
}

# Consolidar todos los años
nmx_suav_all <- bind_rows(nmx_suav_list)

# Mostrar resumen del suavizamiento
cat("\n Resumen del suavizamiento (primeras edades, hombres 2010):\n")
print(head(nmx_suav_all[nmx_suav_all$año == "2010", 
                        c("Edad", "mx_hombres_original", "mx_hombres_suav")]))

# GRÁFICAS DE DIAGNÓSTICO


cat("\n Generando gráficas de diagnóstico...\n")

# Crear directorio si no existe
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

# Convertir edades a numérico para graficar
nmx_suav_all$Edad_num <- as.numeric(gsub("\\+", "", nmx_suav_all$Edad))

# Gráfica 1: Comparación antes/después para hombres 2019
p1 <- ggplot(nmx_suav_all[nmx_suav_all$año == "2019", ], aes(x = Edad_num)) +
  geom_line(aes(y = mx_hombres_original, color = "Original", linetype = "Original"), size = 0.8) +
  geom_line(aes(y = mx_hombres_suav, color = "Suavizado", linetype = "Suavizado"), size = 1.2) +
  scale_y_log10() +
  labs(
    title = "Efecto del suavizamiento - Hombres Jalisco 2019",
    subtitle = "Método: media móvil (ventana 3 hasta 60 años, ventana 5 después)",
    x = "Edad",
    y = "mx (escala logarítmica)",
    color = "",
    linetype = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figuras/diagnostico_suavizamiento_hombres_2019.png", 
       plot = p1, width = 10, height = 6, dpi = 300)

# Gráfica 2: Comparación para mujeres 2019
p2 <- ggplot(nmx_suav_all[nmx_suav_all$año == "2019", ], aes(x = Edad_num)) +
  geom_line(aes(y = mx_mujeres_original, color = "Original", linetype = "Original"), size = 0.8) +
  geom_line(aes(y = mx_mujeres_suav, color = "Suavizado", linetype = "Suavizado"), size = 1.2) +
  scale_y_log10() +
  labs(
    title = "Efecto del suavizamiento - Mujeres Jalisco 2019",
    x = "Edad",
    y = "mx (escala logarítmica)",
    color = "",
    linetype = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figuras/diagnostico_suavizamiento_mujeres_2019.png", 
       plot = p2, width = 10, height = 6, dpi = 300)

# Gráfica 3: Comparación entre años (mx suavizada) - Hombres
p3 <- ggplot(nmx_suav_all, aes(x = Edad_num, y = mx_hombres_suav, color = año)) +
  geom_line(size = 1) +
  scale_y_log10() +
  labs(
    title = "Tasas de mortalidad suavizadas - Hombres Jalisco",
    subtitle = "Comparación 2010, 2019, 2021",
    x = "Edad",
    y = "mx (escala logarítmica)",
    color = "Año"
  ) +
  theme_minimal()

ggsave("output/figuras/mx_suavizadas_hombres_comparacion.png", 
       plot = p3, width = 10, height = 6, dpi = 300)

# Gráfica 4: Comparación entre años (mx suavizada) - Mujeres
p4 <- ggplot(nmx_suav_all, aes(x = Edad_num, y = mx_mujeres_suav, color = año)) +
  geom_line(size = 1) +
  scale_y_log10() +
  labs(
    title = "Tasas de mortalidad suavizadas - Mujeres Jalisco",
    subtitle = "Comparación 2010, 2019, 2021",
    x = "Edad",
    y = "mx (escala logarítmica)",
    color = "Año"
  ) +
  theme_minimal()

ggsave("output/figuras/mx_suavizadas_mujeres_comparacion.png", 
       plot = p4, width = 10, height = 6, dpi = 300)


# GUARDAR RESULTADOS

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# Guardar 
nmx_suav_hombres <- nmx_suav_all %>%
  select(Edad, año, mx = mx_hombres_suav) %>%
  mutate(sexo = "Hombres")

nmx_suav_mujeres <- nmx_suav_all %>%
  select(Edad, año, mx = mx_mujeres_suav) %>%
  mutate(sexo = "Mujeres")

nmx_suav_long <- bind_rows(nmx_suav_hombres, nmx_suav_mujeres)

write.csv(nmx_suav_all, "data/processed/nmx_suavizado_completo.csv", row.names = FALSE)
write.csv(nmx_suav_long, "data/processed/nmx_suavizado_long.csv", row.names = FALSE)
saveRDS(nmx_suav_all, "data/processed/nmx_suavizado.rds")
saveRDS(nmx_suav_long, "data/processed/nmx_suavizado_long.rds")

cat("\n Suavizamiento completado\n")
cat("   Archivos guardados:\n")
cat("   - data/processed/nmx_suavizado_completo.csv\n")
cat("   - data/processed/nmx_suavizado_long.csv\n")
cat("   - output/figuras/diagnostico_suavizamiento_*.png\n")