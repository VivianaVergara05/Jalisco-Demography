<div align="center">

# 📊 Análisis Demográfico y Actuarial para el Estado de Jalisco
### Modelación de Funciones de Supervivencia, Decrementos Múltiples y Dinámicas de Fecundidad

![R](https://img.shields.io/badge/R-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Quarto](https://img.shields.io/badge/Quarto-%234752B2.svg?style=for-the-badge&logo=quarto&logoColor=white)
![GitHub](https://img.shields.io/badge/GitHub-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)

<!-- Imagen de Portada Oficial -->
<img src="mapa_jalisco.png" width="85%" alt="Ubicación Geográfica de Jalisco en la República Mexicana" style="border-radius: 8px; margin: 20px 0; box-shadow: 0 4px 8px rgba(0,0,0,0.15);">

</div>

---

### 1. Introducción
Este repositorio contiene el desarrollo actuarial y demográfico enfocado en la construcción, análisis y depuración de tablas de vida completas y pautas de reproducción para la población del estado de Jalisco, México, correspondientes a los años coyunturales **2010, 2019 y 2021**.

A través de modelos de decremento simple, tablas de decrementos múltiples y funciones de supervivencia, el proyecto evalúa la evolución de la estructura de mortalidad por edad y sexo, calculando las esperanzas de vida al nacer ($e_0$) reales de la entidad y aislando el impacto de causas exógenas (Homicidios). Asimismo, se incorpora un módulo de análisis avanzado enfocado en la estimación de los **Años Potenciales de Vida Perdidos (APV)** y un estudio comparativo de las **Tasas Específicas de Fecundidad (TEF)** frente al promedio nacional y al referente internacional de Alemania para el año 2019. Esto proporciona un insumo técnico riguroso para la planeación social, la salud pública y el diseño de modelos de pensiones o seguros a largo plazo.

---

### 2. Objetivo del Proyecto
Construir y analizar de manera comparativa las tablas de vida abreviadas y los indicadores de reproducción para el estado de Jalisco correspondientes a los años 2010, 2019 y 2021 a partir de microdatos oficiales de población, defunciones y natalidad. El propósito es evaluar la evolución de la longevidad en la entidad, cuantificar el impacto de las muertes prematuras mediante los Años Potenciales de Vida Perdidos (APV), analizar el efecto de causas de muerte específicas mediante tablas de causa eliminada, e identificar las brechas de género y las transiciones demográficas derivadas del comportamiento de la estructura de fecundidad quinquenal.

---

### 3. Autores
* **Viviana Montserrat Vergara Galindo**
* **Claudia Areli Avila Melgarejo**

---

### 4. Estructura Global del Proyecto
A continuación se muestra el esquema estático de la organización de los directorios de este espacio de trabajo:

```text
Jalisco-Demography/
│
├── Jalisco-Demography.Rproj
├── InformeJalisco.qmd
├── InformeJalisco.pdf
├── mapa_jalisco.png
├── diagrama.jpeg
│
├── data/
│   └── raw/
│       ├── AVP_2010.xlsx
│       ├── AVP_2019.xlsx
│       ├── AVP_2021.xlsx
│       ├── nMx_2010.xlsx
│       ├── nMx_2019.xlsx
│       ├── nMx_2021.xlsx
│       └── TEF_Jalisco_Mexico_Alemania_2019.xlsx
│       └──Data Alemania
│       └──nacimientos_2019_2010
│
├── scripts/
│   ├── funciones.R
│   ├── 01_cargar_datos.R
│   ├── 02_calcular_tablas_COMPLETAS.R
│   ├── 04_causa_eliminada.R
│   ├── 05_fecundidad_reproduccion.R
│   └── 06_fecundidad.R
│
├── output/
│   ├── tablas/
│   │   ├── esperanzas_vida.csv
│   │   ├── tablas_vida_completas.csv
│   │   └── fecundidad_real_2019.csv
│   └── figuras/
│       ├── nqx_comparativo_sexos.png
│       └── fecundidad_tef_2019.png
│
└── .gitig
### 5. Explorador Dinámico del Proyecto

*Haz clic en las carpetas que tienen una flecha para expandir y explorar interactivamente el árbol de archivos de este repositorio:*

<ul>
  <li>📄 <b>Jalisco-Demography.Rproj</b> <i>(Archivo de configuración del proyecto en RStudio)</i></li>
  <li>📄 <b>Informe_Avila_Vergara.qmd</b> <i>(Documento maestro en formato Quarto para compilación dinámica del reporte)</i></li>
  <li>📄 <b>Informe_Avila_Vergara.pdf</b> <i>(Reporte técnico de la entrega final generado en PDF)</i></li>
  <li>📄 <b>jalisco.jpg</b> <i>(Ilustración de portada del informe)</i></li>
  <li>📄 <b>mapa_jalisco.png</b> <i>(Imagen de portada del repositorio en GitHub)</i></li>
  <li>📄 <b>diagrama.jpeg</b> <i>(Diagrama de flujo conceptual de la metodología actuarial)</i></li>
  <li>📄 <b>.gitignore</b> <i>(Filtro de exclusión para omitir archivos de caché temporales de Git)</i></li>
  
  <li>
    <details>
      <summary>📁 <b>data/</b> <i>(Hojas de cálculo de mortalidad y natalidad)</i></summary>
      <ul>
        <li>
          <details>
            <summary>📁 <b>raw/</b></summary>
            <ul>
              <li>📊 <code>AVP_2010.xlsx</code> | <code>AVP_2019.xlsx</code> | <code>AVP_2021.xlsx</code> <i>(Registros de defunciones prematuras)</i></li>
              <li>📊 <code>nMx_2010.xlsx</code> | <code>nMx_2019.xlsx</code> | <code>nMx_2021.xlsx</code> <i>(Tasas centrales de mortalidad de la entidad)</i></li>
              <li>📊 <code>TEF_Jalisco_Mexico_Alemania_2019.xlsx</code> <i>(Registros oficiales de fecundidad por área geográfica)</i></li>
            </ul>
          </details>
        </li>
      </ul>
    </details>
  </li>

  <li>
    <details>
      <summary>📁 <b>scripts/</b> <i>(Módulos de programación en R)</i></summary>
      <ul>
        <li>🛠️ <code>funciones.R</code> <i>(Funciones matemáticas base y método de Coale-Demeny)</i></li>
        <li>⚙️ <code>01_cargar_datos.R</code> <i>(Pipeline de extracción y homologación de bases)</i></li>
        <li>⚙️ <code>02_calcular_tablas_COMPLETAS.R</code> <i>(Algoritmo para las funciones actuariales completas)</i></li>
        <li>⚙️ <code>04_causa_eliminada.R</code> <i>(Modelo actuarial de decrementos múltiples para homicidios)</i></li>
        <li>⚙️ <code>05_fecundidad_reproduccion.R</code> <i>(Cálculo de indicadores clave: TGF, TBR y TNR)</i></li>
        <li>⚙️ <code>06_proyecciones_fecundidad.R</code> <i>(Procesamiento y estructuración para curvas comparativas de fecundidad 2019)</i></li>
      </ul>
    </details>
  </li>

  <li>
    <details>
      <summary>📁 <b>output/</b> <i>(Resultados consolidados por el flujo de programación)</i></summary>
      <ul>
        <li>
          <details>
            <summary>📁 <b>tablas/</b></summary>
            <ul>
              <li>📝 <code>esperanzas_vida.csv</code> <i>(Evolución histórica de e0)</i></li>
              <li>📝 <code>tablas_vida_completas.csv</code> <i>(Matrices finales de las funciones vitales)</i></li>
              <li>📝 <code>fecundidad_real_2019.csv</code> <i>(Tasas específicas de fecundidad por grupo quinquenal)</i></li>
            </ul>
          </details>
        </li>
        <li>
          <details>
            <summary>📁 <b>figuras/</b></summary>
            <ul>
              <li>🖼️ <code>nqx_comparativo_sexos.png</code> <i>(Gráfica de sobremortalidad masculina)</i></li>
              <li>🖼️ <code>fecundidad_tef_2019.png</code> <i>(Curvas de fecundidad Jalisco vs. México vs. Alemania)</i></li>
            </ul>
          </details>
        </li>
      </ul>
    </details>
  </li>
</ul>


6. Requisitos del Entorno
Para reproducir los cálculos y renderizar el informe final, asegúrese de contar con R (version >= 4.0.0) y las siguientes librerías instaladas:

```R
install.packages(c("tidyverse", "readxl", "knitr", "dplyr", "ggplot2", "tidyr"))
```



