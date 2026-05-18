# 03_apv.R - Años Potenciales de Vida Perdidos 

library(dplyr)
library(ggplot2)

rm(list = ls())
cat(" Entorno limpiado\n")

source("scripts/funciones.R")

# 1. Cargar datos base procesados en el script 01
datos <- readRDS("data/processed/datos_avp_nmx.rds")

# 2. CARGADO DIRECTO
if (!is.null(datos$nmx)) {
  nmx_data <- datos$nmx
  cat(" Usando tasas de mortalidad (nMx) crudas oficiales del proyecto\n")
} else {
  stop("ERROR: No se encuentra la estructura de nmx dentro de datos_avp_nmx.rds. Verifica tu script 01.")
}

# 3. Cargar tablas de vida calculadas por el script 02 para obtener las e0 reales
tablas_vida <- readRDS("output/tablas/tablas_vida_completas.rds")

# Extraer las e0 de las tablas construidas
e0_reales <- data.frame(
  año = c(2010, 2010, 2019, 2019, 2021, 2021),
  sexo = c("Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres"),
  e0 = c(tablas_vida$lt_2010_h$ex[1],
         tablas_vida$lt_2010_m$ex[1],
         tablas_vida$lt_2019_h$ex[1],
         tablas_vida$lt_2019_m$ex[1],
         tablas_vida$lt_2021_h$ex[1],
         tablas_vida$lt_2021_m$ex[1])
)

cat("\n Esperanzas de vida reales calculadas (e0):\n")
print(e0_reales)

# 4. Función auxiliar para limpiar etiquetas de edad
limpiar_edad <- function(edad_str) {
  edad_str <- trimws(edad_str)
  ifelse(grepl("a", edad_str), 
         as.numeric(substr(edad_str, 1, 1)),
         as.numeric(gsub("\\+", "", edad_str)))
}

# 5. Función  para calcular APV acoplada al script 01
calcular_apv_para_sexo <- function(anio, sexo, avp_df, mx_df, e0_ref) {
  
  # Limpiar la estructura de edad en ambos conjuntos
  avp_df$edad_num <- sapply(avp_df$Edad, limpiar_edad)
  mx_df$edad_num <- sapply(mx_df$Edad, limpiar_edad)
  

  if (sexo == "Hombres") {
    avp_col <- "Hombre"
    mx_col <- "Hombre"
  } else {
    avp_col <- "Mujer"
    mx_col <- "Mujer"
  }
  

  merged <- merge(
    avp_df[, c("edad_num", avp_col)],
    mx_df[, c("edad_num", mx_col)],
    by = "edad_num",
    all = FALSE
  )
  
  colnames(merged) <- c("edad", "poblacion", "mx")
  merged <- merged[!is.na(merged$poblacion) & !is.na(merged$mx), ]
  
  if (nrow(merged) == 0) return(NULL)
  
  merged <- merged[order(merged$edad), ]
  
  # Estimar volumen teórico de defunciones
  merged$defunciones <- merged$poblacion * merged$mx
  
  # Esperanza de vida estándar respecto al Radix 
  merged$ex_estandar <- pmax(e0_ref - merged$edad, 0.5)
  
  merged_apv <- merged[merged$edad < 75, ]
  if (nrow(merged_apv) == 0) return(NULL)
  
  merged_apv$apv <- merged_apv$defunciones * merged_apv$ex_estandar
  
  resultado <- data.frame(
    edad = merged_apv$edad,
    poblacion = merged_apv$poblacion,
    mx = merged_apv$mx,
    defunciones = merged_apv$defunciones,
    ex_estandar = merged_apv$ex_estandar,
    apv = merged_apv$apv,
    año = anio,
    sexo = sexo
  )
  
  return(resultado)
}

# 6. Procesar de forma cíclica los periodos analizados
resultados_apv <- list()

for (anio in c("2010", "2019", "2021")) {
  cat("\n Procesando año", anio, "...\n")
  
  avp_df <- datos$avp[[anio]]
  mx_df <- nmx_data[[anio]] 
  if (is.null(mx_df) || nrow(mx_df) == 0) {
    cat("  No se localizaron registros de mx para el periodo:", anio, "\n")
    next
  }
  
  # Procesamiento Hombres
  e0_h <- e0_reales$e0[e0_reales$año == anio & e0_reales$sexo == "Hombres"]
  res_h <- calcular_apv_para_sexo(anio, "Hombres", avp_df, mx_df, e0_h)
  
  if (!is.null(res_h) && nrow(res_h) > 0) {
    resultados_apv[[paste0(anio, "_Hombres")]] <- res_h
    apv_total <- sum(res_h$apv)
    cat(sprintf("   Hombres: APV total = %s años perdidos acumulados\n", 
                format(round(apv_total), big.mark = ",")))
  }
  
  # Procesamiento Mujeres
  e0_m <- e0_reales$e0[e0_reales$año == anio & e0_reales$sexo == "Mujeres"]
  res_m <- calcular_apv_para_sexo(anio, "Mujeres", avp_df, mx_df, e0_m)
  
  if (!is.null(res_m) && nrow(res_m) > 0) {
    resultados_apv[[paste0(anio, "_Mujeres")]] <- res_m
    apv_total <- sum(res_m$apv)
    cat(sprintf("   Mujeres: APV total = %s años perdidos acumulados\n", 
                format(round(apv_total), big.mark = ",")))
  }
}

# 7. Consolidar e imprimir la matriz resumen de resultados
if (length(resultados_apv) > 0) {
  
  apv_df <- bind_rows(resultados_apv)
  
  apv_total_df <- apv_df %>%
    group_by(año, sexo) %>%
    summarise(
      apv_total = sum(apv, na.rm = TRUE),
      apv_total_miles = sum(apv, na.rm = TRUE) / 1000,
      apv_total_millones = sum(apv, na.rm = TRUE) / 1e6,
      .groups = "drop"
    )
  
  cat("\n     MATRIZ CONSOLIDADA DE APV TOTAL POR AÑO Y SEXO:\n")
  print(apv_total_df)
  
  # 8. Renderizar y respaldar las representaciones gráficas (.png)
  if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)
  if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)
  
  # Gráfica 4: Histograma agregado de barras comparativo (Leída por el Quarto)
  p4 <- ggplot(apv_total_df, aes(x = año, y = apv_total_millones, fill = sexo)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Años Potenciales de Vida Perdidos (APV) totales",
      subtitle = "Jalisco 2010-2021",
      x = "Año", y = "APV total (millones de años)", fill = "Sexo"
    ) +
    theme_minimal() + theme(legend.position = "bottom") +
    scale_fill_manual(values = c("Hombres" = "#2E86AB", "Mujeres" = "#A23B72"))
  
  ggsave("output/figuras/apv_total_barras.png", plot = p4, width = 8, height = 5, dpi = 300)
  cat("\n Gráfica de barras exportada con éxito: output/figuras/apv_total_barras.png\n")
  
  # 9. Guardar bases de datos de salida de texto comprimido (CSV)
  write.csv(apv_df, "output/tablas/apv_por_edad.csv", row.names = FALSE)
  write.csv(apv_total_df, "output/tablas/apv_total.csv", row.names = FALSE)
  
  cat("\n Archivos de datos generados y guardados en output/tablas/\n")
}

cat("\n PROCESO COMPLETADO EXITOSAMENTE\n")