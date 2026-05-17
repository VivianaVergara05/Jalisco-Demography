
# CARGAR LIBRERÍAS NECESARIAS

library(dplyr)
library(ggplot2)
library(readr)
library(knitr)
library(kableExtra)

# VERIFICAR QUE LAS LIBRERÍAS CARGARON

loaded <- (.packages())

cat("=== LIBRERÍAS CARGADAS ===\n")
cat("Paquetes:", paste(loaded, collapse = ", "), "\n\n")

if ("dplyr" %in% loaded) {
  cat("pddlyr versión:", as.character(packageVersion("dplyr")), "\n")
} else {
  cat("dplyr\n")
}

if ("ggplot2" %in% loaded) {
  cat(" ggplot2 versión:", as.character(packageVersion("ggplot2")), "\n")
} else {
  cat(" ggplot2\n")
}

if ("knitr" %in% loaded) {
  cat(" knitr versión:", as.character(packageVersion("knitr")), "\n")
} else {
  cat(" knitr\n")
}

if ("kableExtra" %in% loaded) {
  cat(" kableExtra versión:", as.character(packageVersion("kableExtra")), "\n")
} else {
  cat(" kableExtra\n")
}

if ("readxl" %in% loaded) {
  cat(" readxl versión:", as.character(packageVersion("readxl")), "\n")
} else {
  cat(" readxl - Cárgalo con: library(readxl)\n")
}

cat("\n Verificación completada\n")
