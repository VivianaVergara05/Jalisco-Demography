# scripts/06_fecundidad.R
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

rm(list = ls())
cat("\014")
cat("=== Análisis de Fecundidad  2019 ===\n")

ruta_excel <- "C:/Users/vivim/OneDrive/Desktop/Demografia Jalisco/Demography/Jalisco-Demography/Data/raw/TEF_Jalisco_Mexico_Alemania_2019.xlsx"

df_raw <- read_excel(ruta_excel)

# 1. Limpieza y reestructuración directa de los datos observados 2019
df_2019 <- df_raw %>%
  filter(!is.na(`Grupo de edad`)) %>%
  rename(age_group = `Grupo de edad`, 
         Jalisco = `Jalisco 2019`, 
         Mexico = `México 2019`, 
         Alemania = `Alemania 2019`) %>%
  select(age_group, Jalisco, Mexico, Alemania)

# 2. Convertir a formato largo 
df_grafica <- df_2019 %>%
  pivot_longer(cols = c(Jalisco, Mexico, Alemania), 
               names_to = "Region", 
               values_to = "TEF") %>%
  mutate(Region = case_when(
    Region == "Jalisco"  ~ "Jalisco (2019)",
    Region == "Mexico"   ~ "México (2019)",
    Region == "Alemania" ~ "Alemania (2019)"
  ))

if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)
if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

# Guardar la tabla limpia de 2019
write.csv(df_2019, "output/tablas/fecundidad_real_2019.csv", row.names = FALSE)

# GENERACIÓN DE LA GRÁFICA 2019
cat("Generando curvas específicas de fecundidad observada 2019...\n")

p_tef <- ggplot(df_grafica, aes(x = age_group, y = TEF, group = Region, color = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) + 

  scale_color_manual(values = c(
    "Jalisco (2019)"  = "#2E86AB", 
    "México (2019)"   = "#3A86C8", 
    "Alemania (2019)" = "#A23B72"
  )) +
  
  labs(
    title = "Curvas de las Tasas Específicas de Fecundidad (TEF) - 2019",
    subtitle = "Comparativo Estructural: Jalisco vs. México vs. Alemania",
    x = "Grupo de Edad de la Madre",
    y = "Nacimientos por cada 1,000 Mujeres",
    color = "Región"
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Guardar la imagen 
ggsave("output/figuras/fecundidad_tef_2019.png", plot = p_tef, width = 8, height = 5, dpi = 300)

cat("=== ¡GRÁFICA GENERADA CON ÉXITO SIN ERRORES DE TIPO! ===\n")