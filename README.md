# Análisis Demográfico para el Estado de Jalisco 

<p align="center">
  <img src="mapa_jalisco.png" alt="Mapa Cultural y de Ubicación de Jalisco" width="750">
</p>

## 1. Introducción
Este repositorio contiene el desarrollo actuarial y demográfico para la construcción, análisis y depuración de tablas de vida completas para la población del estado de Jalisco, México, correspondientes a los años coyunturales **2010, 2019 y 2021**. 

A través de modelos de decremento simple y funciones de supervivencia, el proyecto evalúa la evolución de la estructura de mortalidad por edad y sexo, calculando las esperanzas de vida al nacer ($e_0$) reales de la entidad. Asimismo, se incorpora un módulo de análisis avanzado enfocado en la estimación de los **Años Potenciales de Vida Perdidos (APV)** para menores de 75 años, permitiendo cuantificar el impacto teórico de la mortalidad prematura en el estado y proporcionando un insumo técnico rigoroso para la planeación social, la salud pública y el diseño de modelos de pensiones o seguros a largo plazo.

---

## 2. Objetivo del Proyecto
Construir y analizar de manera comparativa las tablas de vida abreviadas para el estado de Jalisco correspondientes a los años 2010, 2019 y 2021 a partir de microdatos oficiales de población y defunciones. El propósito es evaluar la evolución de la longevidad en la entidad, cuantificar el impacto de las muertes prematuras mediante los Años Potenciales de Vida Perdidos (APV) e identificar las brechas de género derivadas de la sobremortalidad masculina y factores epidemiológicos coyunturales.

---

## 3. Autores
* **Viviana Montserrat Vergara Galindo** 
* **Claudia Areli Avila Melgarejo**

---

## 4. Estructura Global del Proyecto
A continuación se muestra el esquema estático de la organización de los directorios de este espacio de trabajo:

```text
Jalisco-Demography/
│
├── Jalisco-Demography.Rproj          # Archivo del proyecto RStudio
│
├── InformeJalisco.qmd                # Informe final (Quarto)
├── InformeJalisco.pdf                # PDF generado
│
├── diagrama.jpeg                     # Diagrama de flujo
│
├── data/
│   └── raw/
│       ├── AVP_2010.xlsx             # Años persona vividos 2010
│       ├── AVP_2019.xlsx             # Años persona vividos 2019
│       ├── AVP_2021.xlsx             # Años persona vividos 2021
│       ├── nMx_2010.xlsx             # Tasas mortalidad 2010
│       ├── nMx_2019.xlsx             # Tasas mortalidad 2019
│       └── nMx_2021.xlsx             # Tasas mortalidad 2021
│
├── scripts/
│   ├── funciones.R                   # Función lt_abr()
│   ├── 01_cargar_datos.R             # Carga AVP y nMx
│   └── 02_calcular_tablas_COMPLETO.R # Construye tablas de vida
│
├── output/
│   └── tablas/
│       ├── esperanzas_vida.csv       # Tabla de e0
│       └── tablas_vida_completas.rds # Tablas de vida completas
│
└── .gitignore
```
# 5. Explorador o del Proyecto



*Haz clic en las carpetas que tienen una flecha (▶) para expandir y explorar dinámicamente el árbol de archivos de este repositorio:*

<ul>
  <li>📄 <b>Jalisco-Demography.Rproj</b> <small><i>(Archivo de configuración del proyecto en RStudio)</i></small></li>
  <li>📄 <b>InformeJalisco.qmd</b> <small><i>(Documento maestro en formato Quarto para compilación dinámica del reporte)</i></small></li>
  <li>📄 <b>InformeJalisco.pdf</b> <small><i>(Reporte técnico de la entrega final generado en PDF)</i></small></li>
  <li>📄 <b>mapa_jalisco.png</b> <small><i>(Mapa de ubicación geográfica y elementos culturales del estado)</i></small></li>
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
        <li>
          <details>
            <summary>📁 <b>processed/</b> <small><i>(Estructuras de datos consolidadas)</i></small></summary>
            <ul>
              <li>📄 <code>datos_avp_nmx.rds</code> <small><i>(Matriz binaria con las estructuras base por año y sexo)</i></small></li>
            </ul>
          </details>
        </li>
      </ul>
    </details>
  </li>

  <li>
    <details>
      <summary>📁 <b>scripts/</b> <small><i>(Módulos de programación en R para el flujo actuarial)</i></small></summary>
      <ul>
        <li>🛠️ <code>funciones.R</code> <small><i>(Algoritmo base de la función actuarial lt_abr e interpolaciones)</i></small></li>
        <li>🛠️ <code>01_cargar_datos.R</code> <small><i>(Lectura, limpieza y homologación inicial de grupos de edad)</i></small></li>
        <li>🛠️ <code>02_calcular_tablas_COMPLETO.R</code> <small><i>(Construcción formal de las tablas actuariales y cálculo de e0)</i></small></li>
        <li>🛠️ <code>05_apv.R</code> <small><i>(Algoritmo actuarial para la cuantificación y estructura de los APV &lt; 75 años)</i></small></li>
      </ul>
    </details>
  </li>

  <li>
    <details>
      <summary>📁 <b>output/</b></summary>
      <ul>
        <li>
          <details>
            <summary>📁 <b>tablas/</b> <small><i>(Resultados y matrices de datos listas para reporte)</i></small></summary>
            <ul>
              <li>📋 <code>esperanzas_vida.csv</code> <small><i>(Matriz resumen de esperanzas de vida al nacer e0)</i></small></li>
              <li>📋 <code>apv_por_edad.csv</code> <small><i>(Desagregación de años perdidos por cohortes específicas)</i></small></li>
              <li>📋 <code>apv_total.csv</code> <small><i>(Matriz consolidada final con los totales de APV)</i></small></li>
              <li>📦 <code>tablas_vida_completas.rds</code> <small><i>(Estructura de listas con las funciones de vida serializadas)</i></small></li>
            </ul>
          </details>
        </li>
        <li>
          <details>
            <summary>📁 <b>figuras/</b> <small><i>(Gráficas y outputs visuales exportados)</i></small></summary>
            <ul>
              <li>📉 <code>apv_total_barras.png</code> <small><i>(Gráfica comparativa agregada de APV generada en ggplot2)</i></small></li>
            </ul>
          </details>
        </li>
      </ul>
    </details>
  </li>
</ul>

---

## 6. Requisitos del Entorno
Para reproducir los cálculos y renderizar el informe final, asegúrese de contar con **R (version >= 4.0.0)** y las siguientes librerías instaladas:

```r
install.packages(c("tidyverse", "readxl", "data.table", "knitr", "kableExtra", "ggplot2"))
