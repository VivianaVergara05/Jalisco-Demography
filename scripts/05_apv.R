# 05_apv.R - Años Potenciales de Vida Perdidos

library(dplyr)
library(ggplot2)

rm(list = ls())
cat(" Entorno limpiado\n")

source("scripts/funciones.R")

# 1. Cargar datos
datos <- readRDS("data/processed/datos_avp_nmx.rds")

# 2. Cargar mx suavizadas
if (file.exists("data/processed/nmx_suavizado.rds")) {
  nmx_data <- readRDS("data/processed/nmx_suavizado.rds")
  cat(" Usando mx suavizadas\n")
} else {
  stop("No se encuentra nmx_suavizado.rds. Ejecuta primero 03_suavizamiento.R")
}

# 3. Cargar tablas de vida para obtener e0 reales
tablas_vida <- readRDS("output/tablas/tablas_vida_completas.rds")

# Extraer e0 reales
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

cat("\n Esperanzas de vida reales (e0):\n")
print(e0_reales)

# 4. Función para limpiar edades (CONVIERTE "1 a 4" en 1, etc.)
limpiar_edad <- function(edad_str) {
  # Casos especiales
  edad_str <- trimws(edad_str)
  
  # Para "1 a 4" -> devolver 1 (edad de inicio del grupo)
  ifelse(grepl("a", edad_str), 
         as.numeric(substr(edad_str, 1, 1)),
         as.numeric(gsub("\\+", "", edad_str)))
}

# 5. Función para calcular APV de un año y sexo
calcular_apv_para_sexo <- function(anio, sexo, avp_df, mx_df, e0_ref) {
  
  # Limpiar edades en ambos dataframes
  avp_df$edad_num <- sapply(avp_df$Edad, limpiar_edad)
  mx_df$edad_num <- sapply(mx_df$Edad, limpiar_edad)
  
  # Seleccionar columna correcta según sexo
  if (sexo == "Hombres") {
    avp_col <- "Hombre"
    mx_col <- "mx_hombres_suav"
  } else {
    avp_col <- "Mujer"
    mx_col <- "mx_mujeres_suav"
  }
  
  # Unir por edad
  merged <- merge(
    avp_df[, c("edad_num", avp_col)],
    mx_df[, c("edad_num", mx_col)],
    by = "edad_num",
    all = FALSE
  )
  
  colnames(merged) <- c("edad", "poblacion", "mx")
  
  # Limpiar NAs
  merged <- merged[!is.na(merged$poblacion) & !is.na(merged$mx), ]
  
  if (nrow(merged) == 0) {
    return(NULL)
  }
  
  # Ordenar por edad
  merged <- merged[order(merged$edad), ]
  
  # Mostrar primeras filas para depuración
  if (anio == "2010" && sexo == "Hombres") {
    cat("\n     [DEBUG] Primeras filas del merge:\n")
    print(head(merged))
  }
  
  # Estimar defunciones
  merged$defunciones <- merged$poblacion * merged$mx
  
  # Esperanza de vida estándar (decrecimiento lineal desde e0)
  merged$ex_estandar <- pmax(e0_ref - merged$edad, 0.5)
  
  # APV por edad (solo para edades < 75)
  merged_apv <- merged[merged$edad < 75, ]
  
  if (nrow(merged_apv) == 0) {
    return(NULL)
  }
  
  merged_apv$apv <- merged_apv$defunciones * merged_apv$ex_estandar
  
  # Resultado
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

# 6. Procesar todos los años y sexos
resultados_apv <- list()

for (anio in c("2010", "2019", "2021")) {
  cat("\n Procesando año", anio, "...\n")
  
  # Obtener datos
  avp_df <- datos$avp[[anio]]
  mx_df <- nmx_data[nmx_data$año == anio, ]
  
  # Verificar que mx_df no esté vacío
  if (nrow(mx_df) == 0) {
    cat(" ️ No hay datos de mx para", anio, "\n")
    next
  }
  
  # Hombres
  e0_h <- e0_reales$e0[e0_reales$año == anio & e0_reales$sexo == "Hombres"]
  res_h <- calcular_apv_para_sexo(anio, "Hombres", avp_df, mx_df, e0_h)
  
  if (!is.null(res_h) && nrow(res_h) > 0) {
    resultados_apv[[paste0(anio, "_Hombres")]] <- res_h
    apv_total <- sum(res_h$apv)
    cat(sprintf("   Hombres: APV total = %s años perdidos\n", 
                format(round(apv_total), big.mark = ",")))
  } else {
    cat("   Hombres: Sin datos válidos\n")
  }
  
  # Mujeres
  e0_m <- e0_reales$e0[e0_reales$año == anio & e0_reales$sexo == "Mujeres"]
  res_m <- calcular_apv_para_sexo(anio, "Mujeres", avp_df, mx_df, e0_m)
  
  if (!is.null(res_m) && nrow(res_m) > 0) {
    resultados_apv[[paste0(anio, "_Mujeres")]] <- res_m
    apv_total <- sum(res_m$apv)
    cat(sprintf("   Mujeres: APV total = %s años perdidos\n", 
                format(round(apv_total), big.mark = ",")))
  } else {
    cat("  Mujeres: Sin datos válidos\n")
  }
}

# 7. Consolidar resultados
if (length(resultados_apv) > 0) {
  
  apv_df <- bind_rows(resultados_apv)
  
  # APV total por año y sexo
  apv_total_df <- apv_df %>%
    group_by(año, sexo) %>%
    summarise(
      apv_total = sum(apv, na.rm = TRUE),
      apv_total_miles = sum(apv, na.rm = TRUE) / 1000,
      apv_total_millones = sum(apv, na.rm = TRUE) / 1e6,
      .groups = "drop"
    )
  

  cat("     APV TOTAL POR AÑO Y SEXO\n")
  print(apv_total_df)
  
  # 8. Gráficas
  if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)
  if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)
  
  # Gráfica 1: APV por edad (2021)
  apv_2021 <- apv_df[apv_df$año == "2021", ]
  if (nrow(apv_2021) > 0) {
    p1 <- ggplot(apv_2021, aes(x = edad, y = apv, color = sexo)) +
      geom_line(size = 1.2) +
      labs(
        title = "Años Potenciales de Vida Perdidos (APV) por edad - Jalisco 2021",
        subtitle = "Impacto de la pandemia de COVID-19",
        x = "Edad",
        y = "APV (años)",
        color = "Sexo"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggsave("output/figuras/apv_por_edad_2021.png", plot = p1, width = 10, height = 6, dpi = 300)
    cat("\n Gráfica guardada: output/figuras/apv_por_edad_2021.png\n")
  }
  
  # Gráfica 2: Comparación hombres por año
  apv_hombres <- apv_df[apv_df$sexo == "Hombres", ]
  if (nrow(apv_hombres) > 0) {
    p2 <- ggplot(apv_hombres, aes(x = edad, y = apv, color = año)) +
      geom_line(size = 1) +
      labs(
        title = "Años Potenciales de Vida Perdidos (APV) - Hombres Jalisco",
        subtitle = "Comparación 2010, 2019, 2021",
        x = "Edad",
        y = "APV (años)",
        color = "Año"
      ) +
      theme_minimal()
    
    ggsave("output/figuras/apv_hombres_comparacion.png", plot = p2, width = 10, height = 6, dpi = 300)
    cat(" Gráfica guardada: output/figuras/apv_hombres_comparacion.png\n")
  }
  
  # Gráfica 3: Comparación mujeres por año
  apv_mujeres <- apv_df[apv_df$sexo == "Mujeres", ]
  if (nrow(apv_mujeres) > 0) {
    p3 <- ggplot(apv_mujeres, aes(x = edad, y = apv, color = año)) +
      geom_line(size = 1) +
      labs(
        title = "Años Potenciales de Vida Perdidos (APV) - Mujeres Jalisco",
        subtitle = "Comparación 2010, 2019, 2021",
        x = "Edad",
        y = "APV (años)",
        color = "Año"
      ) +
      theme_minimal()
    
    ggsave("output/figuras/apv_mujeres_comparacion.png", plot = p3, width = 10, height = 6, dpi = 300)
    cat(" Gráfica guardada: output/figuras/apv_mujeres_comparacion.png\n")
  }
  
  # Gráfica 4: APV total (barras)
  p4 <- ggplot(apv_total_df, aes(x = año, y = apv_total_millones, fill = sexo)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Años Potenciales de Vida Perdidos (APV) totales",
      subtitle = "Jalisco 2010-2021",
      x = "Año",
      y = "APV total (millones de años)",
      fill = "Sexo"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("output/figuras/apv_total_barras.png", plot = p4, width = 8, height = 5, dpi = 300)
  cat(" Gráfica guardada: output/figuras/apv_total_barras.png\n")
  
  # 9. Guardar datos
  write.csv(apv_df, "output/tablas/apv_por_edad.csv", row.names = FALSE)
  write.csv(apv_total_df, "output/tablas/apv_total.csv", row.names = FALSE)
  
  cat("\n APV calculado correctamente\n")
  cat("   Archivos guardados:\n")
  cat("   - output/tablas/apv_por_edad.csv\n")
  cat("   - output/tablas/apv_total.csv\n")
  
} else {
  cat("\n No se pudo calcular APV. Ejecuta primero el diagnóstico.\n")
}

cat("\n PROCESO COMPLETADO\n")