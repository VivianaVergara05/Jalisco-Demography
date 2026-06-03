# 04_causa_eliminada.R

rm(list = ls())
cat("=== INICIO: Tabla de vida por causa eliminada (Homicidios) ===\n")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)

# Función para limpiar
limpiar_numeric <- function(x) {
  x <- as.character(x)
  x <- gsub(",", "", x)  # Eliminar comas
  x <- gsub(" ", "", x)   # Eliminar espacios
  x <- gsub("NA", "", x)  # Eliminar "NA" como string
  x <- ifelse(x == "", NA, x)
  return(as.numeric(x))
}

# Crear carpetas
if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)


archivo_causa <- "C:/Users/vivim/OneDrive/Desktop/Demografia Jalisco/Demography/Jalisco-Demography/Data/raw/LT_CausaEliminada.xlsx"

decrementos <- read_excel(archivo_causa, sheet = "Decrementos Múltiples", col_names = FALSE)

cat("Archivo cargado. Dimensiones:", dim(decrementos), "\n")


# 1. EXTRAER DATOS DE HOMBRES 

cat("\n1. Extrayendo datos de HOMBRES (filas 3-21)...\n")

hombres_raw <- decrementos[3:21, ]

hombres <- data.frame(
  Edad = c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
           "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
           "70-74", "75-79", "80-84", "85+"),
  edad_inicio = limpiar_numeric(hombres_raw[[1]]),
  n = limpiar_numeric(hombres_raw[[2]]),
  nDx = limpiar_numeric(hombres_raw[[3]]),
  nDxi = limpiar_numeric(hombres_raw[[4]]),
  lx = limpiar_numeric(hombres_raw[[6]]),
  nqx_con = limpiar_numeric(hombres_raw[[8]]),
  ex_con = limpiar_numeric(hombres_raw[[10]]),
  nqx_sin = limpiar_numeric(hombres_raw[[12]]),
  ex_sin = limpiar_numeric(hombres_raw[[18]]),
  diff = limpiar_numeric(hombres_raw[[19]])
)

# Limpiar NAs en edad_inicio
hombres <- hombres[!is.na(hombres$edad_inicio), ]

# Punto medio para gráficas
hombres$edad_plot <- c(0, 2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 
                       42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 72.5, 77.5, 82.5, 87.5)

cat("Hombres:", nrow(hombres), "grupos de edad\n")

# 2. EXTRAER DATOS DE MUJERES 

cat("\n2. Extrayendo datos de MUJERES (filas 26-44)...\n")

mujeres_raw <- decrementos[26:44, ]

mujeres <- data.frame(
  Edad = c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
           "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
           "70-74", "75-79", "80-84", "85+"),
  edad_inicio = limpiar_numeric(mujeres_raw[[1]]),
  n = limpiar_numeric(mujeres_raw[[2]]),
  nDx = limpiar_numeric(mujeres_raw[[3]]),
  nDxi = limpiar_numeric(mujeres_raw[[4]]),
  lx = limpiar_numeric(mujeres_raw[[6]]),
  nqx_con = limpiar_numeric(mujeres_raw[[8]]),
  ex_con = limpiar_numeric(mujeres_raw[[10]]),
  nqx_sin = limpiar_numeric(mujeres_raw[[12]]),
  ex_sin = limpiar_numeric(mujeres_raw[[18]]),
  diff = limpiar_numeric(mujeres_raw[[19]])
)

mujeres <- mujeres[!is.na(mujeres$edad_inicio), ]

mujeres$edad_plot <- c(0, 2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 
                       42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 72.5, 77.5, 82.5, 87.5)

cat("Mujeres:", nrow(mujeres), "grupos de edad\n")

# 3. VERIFICAR DATOS 

cat("\n=== VERIFICACIÓN DE DATOS ===\n")
cat("\n--- HOMBRES (nDx ahora debe tener valores) ---\n")
print(hombres[, c("Edad", "nDx", "nDxi", "nqx_con", "ex_con", "nqx_sin", "ex_sin", "diff")])

cat("\n--- MUJERES (nDx ahora debe tener valores) ---\n")
print(mujeres[, c("Edad", "nDx", "nDxi", "nqx_con", "ex_con", "nqx_sin", "ex_sin", "diff")])


# 4. TABLA FORMATEADA PARA EL INFORME

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== TABLA DE DECREMENTOS MÚLTIPLES - HOMBRES (Jalisco 2019) ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

tabla_hombres <- hombres %>%
  select(Edad, n, nDx, nDxi, lx, nqx_con, ex_con, nqx_sin, ex_sin, diff) %>%
  mutate(
    nqx_con = round(nqx_con, 6),
    nqx_sin = round(nqx_sin, 6),
    ex_con = round(ex_con, 2),
    ex_sin = round(ex_sin, 2),
    diff = round(diff, 4),
    nDx = round(nDx, 0),
    nDxi = round(nDxi, 0),
    lx = round(lx, 0)
  )

print(tabla_hombres)

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== TABLA DE DECREMENTOS MÚLTIPLES - MUJERES (Jalisco 2019) ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

tabla_mujeres <- mujeres %>%
  select(Edad, n, nDx, nDxi, lx, nqx_con, ex_con, nqx_sin, ex_sin, diff) %>%
  mutate(
    nqx_con = round(nqx_con, 6),
    nqx_sin = round(nqx_sin, 6),
    ex_con = round(ex_con, 2),
    ex_sin = round(ex_sin, 2),
    diff = round(diff, 4),
    nDx = round(nDx, 0),
    nDxi = round(nDxi, 0),
    lx = round(lx, 0)
  )

print(tabla_mujeres)

# 5. ESPERANZAS DE VIDA AL NACER

cat("\n\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== ESPERANZAS DE VIDA AL NACER (e₀) - Jalisco 2019 ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

e0_resultados <- data.frame(
  Sexo = c("Hombres", "Hombres", "Mujeres", "Mujeres"),
  Escenario = c("Con homicidios", "Sin homicidios", "Con homicidios", "Sin homicidios"),
  e0 = c(hombres$ex_con[1], hombres$ex_sin[1],
         mujeres$ex_con[1], mujeres$ex_sin[1]),
  Ganancia = c(0, hombres$diff[1], 0, mujeres$diff[1])
)

print(e0_resultados)
# 6. GUARDAR TABLAS

cat("\n=== GUARDANDO TABLAS ===\n")

write.csv(tabla_hombres, "output/tablas/tabla_hombres_causa_eliminada.csv", row.names = FALSE)
write.csv(tabla_mujeres, "output/tablas/tabla_mujeres_causa_eliminada.csv", row.names = FALSE)
write.csv(e0_resultados, "output/tablas/e0_causa_eliminada.csv", row.names = FALSE)

cat("  ✓ tabla_hombres_causa_eliminada.csv\n")
cat("  ✓ tabla_mujeres_causa_eliminada.csv\n")
cat("  ✓ e0_causa_eliminada.csv\n")

# 7. GRÁFICAS
cat("\n=== GENERANDO GRÁFICAS ===\n")

# Gráfica Hombres
hombres_plot <- hombres %>%
  select(edad_plot, Edad, nqx_con, nqx_sin) %>%
  pivot_longer(cols = c(nqx_con, nqx_sin),
               names_to = "Escenario",
               values_to = "qx") %>%
  mutate(Escenario = ifelse(Escenario == "nqx_con", "Con homicidios", "Sin homicidios"))

p_hombres <- ggplot(hombres_plot, aes(x = edad_plot, y = qx, color = Escenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_x_continuous(breaks = c(0, 2.5, seq(10, 90, 10)),
                     labels = c("0", "1-4", "10", "20", "30", "40", "50", "60", "70", "80", "85+")) +
  scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1),
                labels = c("0.001", "0.01", "0.1", "1")) +
  coord_cartesian(ylim = c(0.001, 1)) +
  labs(title = "HOMBRES: Probabilidad de morir (nqx) por edad",
       subtitle = "Jalisco 2019 - Comparación con y sin homicidios",
       x = "Edad", y = "Probabilidad de morir (escala logarítmica)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Con homicidios" = "#D6604D", "Sin homicidios" = "#2E86AB"))

ggsave("output/figuras/nqx_hombres_comparativo.png", plot = p_hombres, width = 10, height = 6, dpi = 300)
cat("  ✓ nqx_hombres_comparativo.png\n")

# Gráfica Mujeres
mujeres_plot <- mujeres %>%
  select(edad_plot, Edad, nqx_con, nqx_sin) %>%
  pivot_longer(cols = c(nqx_con, nqx_sin),
               names_to = "Escenario",
               values_to = "qx") %>%
  mutate(Escenario = ifelse(Escenario == "nqx_con", "Con homicidios", "Sin homicidios"))

p_mujeres <- ggplot(mujeres_plot, aes(x = edad_plot, y = qx, color = Escenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_x_continuous(breaks = c(0, 2.5, seq(10, 90, 10)),
                     labels = c("0", "1-4", "10", "20", "30", "40", "50", "60", "70", "80", "85+")) +
  scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1),
                labels = c("0.001", "0.01", "0.1", "1")) +
  coord_cartesian(ylim = c(0.001, 1)) +
  labs(title = "MUJERES: Probabilidad de morir (nqx) por edad",
       subtitle = "Jalisco 2019 - Comparación con y sin homicidios",
       x = "Edad", y = "Probabilidad de morir (escala logarítmica)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Con homicidios" = "#D6604D", "Sin homicidios" = "#2E86AB"))

ggsave("output/figuras/nqx_mujeres_comparativo.png", plot = p_mujeres, width = 10, height = 6, dpi = 300)
cat("  ✓ nqx_mujeres_comparativo.png\n")

# Gráfica comparativa por sexo
ambos_con <- bind_rows(
  hombres_plot %>% filter(Escenario == "Con homicidios") %>% mutate(Sexo = "Hombres"),
  mujeres_plot %>% filter(Escenario == "Con homicidios") %>% mutate(Sexo = "Mujeres")
)

p_comparativo <- ggplot(ambos_con, aes(x = edad_plot, y = qx, color = Sexo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_x_continuous(breaks = c(0, 2.5, seq(10, 90, 10)),
                     labels = c("0", "1-4", "10", "20", "30", "40", "50", "60", "70", "80", "85+")) +
  scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1),
                labels = c("0.001", "0.01", "0.1", "1")) +
  coord_cartesian(ylim = c(0.001, 1)) +
  labs(title = "COMPARACIÓN POR SEXO: Probabilidad de morir (nqx) - Con homicidios",
       subtitle = "Jalisco 2019",
       x = "Edad", y = "Probabilidad de morir (escala logarítmica)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Hombres" = "#D6604D", "Mujeres" = "#2E86AB"))

ggsave("output/figuras/nqx_comparativo_sexos.png", plot = p_comparativo, width = 10, height = 6, dpi = 300)
cat("  ✓ nqx_comparativo_sexos.png\n")

# Gráfica e0
p_e0 <- ggplot(e0_resultados, aes(x = Sexo, y = e0, fill = Escenario)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_text(aes(label = round(e0, 2)), 
            position = position_dodge(0.9), 
            vjust = -0.5, size = 4) +
  labs(title = "Esperanza de vida al nacer (e₀) con y sin homicidios",
       subtitle = "Jalisco, 2019",
       x = "Sexo", y = "Esperanza de vida (años)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values = c("Con homicidios" = "#D6604D", "Sin homicidios" = "#2E86AB"))

ggsave("output/figuras/e0_causa_eliminada.png", plot = p_e0, width = 8, height = 6, dpi = 300)
cat("  ✓ e0_causa_eliminada.png\n")

# Panel combinado
if (require(patchwork, quietly = TRUE)) {
  panel_combinado <- (p_hombres | p_mujeres) / (p_comparativo | p_e0) +
    plot_annotation(
      title = "Impacto de la eliminación de homicidios en la mortalidad",
      subtitle = "Jalisco, 2019",
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  ggsave("output/figuras/panel_causa_eliminada.png", plot = panel_combinado, width = 14, height = 12, dpi = 300)
  cat("  ✓ panel_causa_eliminada.png\n")
}

# 8. RESUMEN FINAL
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== RESUMEN DE RESULTADOS ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("ESPERANZAS DE VIDA AL NACER (e₀) - Jalisco 2019:\n")
cat("─────────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("  Hombres con homicidios:   %.2f años\n", hombres$ex_con[1]))
cat(sprintf("  Hombres sin homicidios:   %.2f años\n", hombres$ex_sin[1]))
cat(sprintf("  → Ganancia hombres:       +%.4f años\n", hombres$diff[1]))
cat("\n")
cat(sprintf("  Mujeres con homicidios:   %.2f años\n", mujeres$ex_con[1]))
cat(sprintf("  Mujeres sin homicidios:   %.2f años\n", mujeres$ex_sin[1]))
cat(sprintf("  → Ganancia mujeres:       +%.4f años\n", mujeres$diff[1]))

cat("\n\nIMPACTO EN EDADES JÓVENES (15-29 años) - Hombres:\n")
cat("─────────────────────────────────────────────────────────────────────────────\n")
hombres_joven <- hombres %>% filter(edad_inicio %in% c(15, 20, 25))
for(i in 1:nrow(hombres_joven)) {
  reduccion <- (hombres_joven$nqx_con[i] - hombres_joven$nqx_sin[i]) / hombres_joven$nqx_con[i] * 100
  cat(sprintf("  %s: nqx = %.6f → sin homicidios = %.6f (reducción del %.1f%%)\n", 
              hombres_joven$Edad[i],
              hombres_joven$nqx_con[i],
              hombres_joven$nqx_sin[i],
              reduccion))
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== SCRIPT COMPLETADO EXITOSAMENTE ===\n")