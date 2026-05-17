# Jalisco

Este repositorio alberga el proyecto de investigación demográfica y actuarial dedicado a la construcción de **tablas de vida abreviadas** para el estado de Jalisco, México. El objetivo principal es evaluar de manera cuantitativa la evolución de la mortalidad y la estructura de la supervivencia en la entidad a lo largo de una década, aislando el impacto de la estructura por edad para identificar brechas de género y el comportamiento de riesgos exógenos frente a coyunturas epidemiológicas.

El análisis incorpora de manera rigurosa técnicas actuariales como la corrección de la mortalidad en las primeras edades mediante el modelo de regiones de Coale-Demeny, el cálculo de las funciones biométricas fundamentales ($l_x$, $d_x$, $q_x$, $p_x$, $L_x$, $T_x$, $e_x$) y la medición del impacto de las muertes prematuras mediante los Años Potenciales de Vida Perdidos (APV).

---

## 📂 Proyecto

*Archivos del proyecto:*

<ul>
  <li>📄 <b>Jalisco-Demography.Rproj</b> <small><i>(Archivo de configuración del proyecto en RStudio)</i></small></li>
  <li>📄 <b>InformeJalisco.qmd</b> <small><i>(Documento maestro en formato Quarto para compilación dinámica)</i></small></li>
  <li>📄 <b>InformeJalisco.pdf</b> <small><i>(Reporte técnico de la entrega final generado en PDF)</i></small></li>
  <li>🖼️ <b>diagrama.jpeg</b> <small><i>(Diagrama de flujo conceptual del proceso demográfico)</i></small></li>
  <li>📄 <b>.gitignore</b> <small><i>(Filtro de exclusión para omitir archivos de caché temporales de Git)</i></small></li>
  
  <li>
    <details>
      <summary>📁 <b>data/</b></summary>
      <ul>
        <li>
          <details>
            <summary>📁 <b>raw/</b> <small><i>(Microdatos e información demográfica original en Excel)</i></small></summary>
            <ul>
              <li>📊 <code>AVP_2010.xlsx</code> <small><i>(Población expuesta / Años persona vividos 2010)</i></small></li>
              <li>📊 <code>AVP_2019.xlsx</code> <small><i>(Población expuesta / Años persona vividos 2019)</i></small></li>
              <li>📊 <code>AVP_2021.xlsx</code> <small><i>(Población expuesta / Años persona vividos 2021)</i></small></li>
              <li>📊 <code>nMx_2010.xlsx</code> <small><i>(Tasas específicas de mortalidad observadas 2010)</i></small></li>
              <li>📊 <code>nMx_2019.xlsx</code> <small><i>(Tasas específicas de mortalidad observadas 2019)</i></small></li>
              <li>📊 <code>nMx_2021.xlsx</code> <small><i>(Tasas específicas de mortalidad observadas 2021)</i></small></li>
            </ul>
          </details>
        </li>
      </ul>
    </details>
  </li>

  <li>
    <details>
      <summary>📁 <b>scripts/</b> <small><i>(Módulos de programación en R para el flujo de cálculo)</i></small></summary>
      <ul>
        <li>
          <details>
            <summary>🛠️ <code>funciones.R</code> <small><i>(Algoritmo base de la función actuarial lt_abr)</i></small></summary>
            <br>
            <details>
              <summary>👁️ <b>[Clic aquí] Desplegar código fuente de funciones.R</b></summary>
              <br>

```r
# Funciones fundamentales para tablas de vida abreviadas
# Autoras: Viviana Vergara & Claudia Avila

lt_abr <- function(edad, nMx, sexo = "M", inf_adj = TRUE) {
  n <- c(diff(edad), 999) 
  N <- length(edad)
  ax <- rep(0.5, N)
  
  # Ajuste de Coale-Demeny para primeras edades
  if(inf_adj) {
    if(sexo == "M") {
      if(nMx[1] >= 0.107) { ax[1] <- 0.33; ax[2] <- 1.352 } 
      else { ax[1] <- 0.045 + 2.684 * nMx[1]; ax[2] <- 1.651 - 2.816 * nMx[1] }
    } else {
      if(nMx[1] >= 0.107) { ax[1] <- 0.35; ax[2] <- 1.361 } 
      else { ax[1] <- 0.053 + 2.800 * nMx[1]; ax[2] <- 1.522 - 1.518 * nMx[1] }
    }
  }
  
  qx <- (n * nMx) / (1 + (n - ax) * nMx)
  qx[N] <- 1.0  # El último grupo de edad cierra con certeza de morir
  qx <- pmin(pmax(qx, 0), 1)
  
  px <- 1 - qx
  lx <- rep(0, N); lx[1] <- 100000
  for(i in 1:(N-1)) { lx[i+1] <- lx[i] * px[i] }
  
  dx <- lx * qx
  Lx <- n * (lx - dx) + (ax * n * dx)
  Lx[N] <- lx[N] / nMx[N]  # Grupo abierto superior
  
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  
  return(data.frame(edad, n, nMx, qx, px, lx, dx, Lx, Tx, ex))
}
```
</details>
      </details>
    </li>
    <li>
      <details>
        <summary>🛠️ <code>01_cargar_datos.R</code> <small><i>(Lectura, limpieza y homologación de grupos de edad)</i></small></summary>
      </details>
    </li>
    <li>
      <details>
        <summary>🛠️ <code>02_calcular_tablas_COMPLETO.R</code> <small><i>(Construcción formal de las tablas actuariales y e0)</i></small></summary>
        <br>
        <details>
          <summary>👁️ <b>[Clic aquí] Desplegar código fuente de 02_calcular_tablas_COMPLETO.R</b></summary>
          <br>
```r
source("scripts/funciones.R")
library(tidyverse)
library(readxl)

anios <- c(2010, 2019, 2021)
sexos <- c("Hombres", "Mujeres")
tablas_vida <- list()

for (a in anios) {
  for (s in sexos) {
    # Carga dinámica de tasas observadas por cohorte y periodo
    ruta_mx <- paste0("data/raw/nMx_", a, ".xlsx")
    datos <- read_excel(ruta_mx, sheet = s)
    
    # Ejecución de la función biométrica
    sex_code <- ifelse(s == "Hombres", "M", "F")
    tabla_res <- lt_abr(datos$edad, datos$nMx, sexo = sex_code)
    
    tabla_res$año <- a
    tabla_res$sexo <- s
    
    key <- paste0(s, "_", a)
    tablas_vida[[key]] <- tabla_res
  }
}

# Serialización y respaldo de seguridad biometrizada
saveRDS(tablas_vida, "output/tablas/tablas_vida_completas.rds")
print("¡Tablas calculadas y respaldadas exitosamente!")
```
</details>
      </details>
    </li>
    <li>🛠️ <code>03_suavizamiento.R</code> <small><i>(Atenuación de fluctuaciones aleatorias mediante medias móviles)</i></small></li>
    <li>🛠️ <code>04_descomposicion.R</code> <small><i>(Modelado de contribuciones netas por cohortes de edad)</i></small></li>
    <li>🛠️ <code>05_apv.R</code> <small><i>(Algoritmo para la cuantificación y estructura de los APV)</i></small></li>
  </ul>
</details>
🛠️ Metodología Actuarial IncorporadaEl proyecto implementa las metodologías estándar para el análisis de decrementos de una población:Transformación Biométrica: Estimación de las tasas específicas de mortalidad ($m_x = D_x / N_x$) y su posterior conversión a probabilidades de morir ($q_x$).Modelo de Coale-Demeny: Ajuste matemático y ponderación del promedio de años vividos en el primer año de vida ($a_0$) y el grupo de niñez ($a_1$) para corregir el comportamiento de los registros en las edades tempranas.Funciones Actuariales de Supervivencia: Simulación del descenso de una cohorte teórica cerrada a partir de un radix de $l_0 = 100,000$ sobrevivientes.Análisis de Pérdidas Prematuras: Cuantificación del volumen absoluto de los Años Potenciales de Vida Perdidos (APV), aislando el impacto de la mortalidad exógena en hombres jóvenes y el efecto del envejecimiento demográfico.
