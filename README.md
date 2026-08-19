# AtlasMX
Visualizador de datos municipales de educación, vivienda, etc. en México.

*Colaboradores actuales*: Ingrid, Andrea, Rodrigo

## Índice

- [Objetivo](#objetivo)
- [Instalación](#instalación)
- [Uso](#uso)
- [Contenido](#contenido)

## Objetivo

## Instalación
### Requisitos
* R

### Pasos

1. Clonar el repositorio

```bash
git clone https://github.com/indra-mh/AtlasMX.git

cd AtlasMX/
```

2. Crear rama para aportes con inicial del colaborador X
```bash
git checkout -b aportes-X
```
3. Instalar dependencias
```r
install.packages(c(
  "shiny",
  "shinyWidgets",
  "DT",
  "bslib",
  "shinyjs",
  "tidyverse"
))
```
## Uso

Para ejecutar la aplicación, abrir R desde la carpeta raíz del proyecto y ejecutar:

```r
shiny::runApp()
```

La aplicación se abrirá en el navegador.

## Descripción de contenido
### Carpeta `data`
Contiene los archivos XLSX por año (desde 1990 a 2024) con información de cada municipio en México de los datos geográficos, de demografía, educación, condiciones laborales y vivienda.
Cada archivo tiene su propio descriptor de las variables en su interior.

Además contiene el archivo `info_estado.xlsx` que indica la clasificación por regiones centronorte, noroeste, sureste, noreste, occidente, suroeste, centrosur, oriente de cada estado de la república.

### `app.R`
Script de la aplicación realizada con shiny para mostrar diferentes filtrados de la información de Población y Vivienda en México.
