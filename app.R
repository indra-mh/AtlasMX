# Atlas MX [1990-2024] #

# Librerías
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(dplyr)
library(bslib)
library(stringr)
library(readxl)
library(shinyjs)

# Obtener las opciones que se mostrarán en el menú
estados <- read_csv("data/info_estado.csv", show_col_types = FALSE)

opcion_edo <- setNames(as.list(estados$CLAVE_ENT), str_to_title(estados$ENTIDAD)) # Entidades federativas
opcion_anio <- 2024:1990 # Años [1990-2024]

# Tema de la aplicación
tema <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#78C2AD",
  secondary = "#96A4A0",
  success = "#B0F2C2",
  info = "#B2E2F2",
  warning = "#FFDA9E",
  danger = "#F3969A",
  dark = "#333333",
  "navbar-bg" = "#333333"
) |> bs_add_rules(
  ".titulo{
     background-color: $primary !important;
     color: #FFFFFF;
  }
  .accordion-button,
  .accordion-body{
    background-color: #FAFCFC;
    color: #333333;
  }
  .dropdown-menu .text {
    white-space: normal !important;
    word-break: break-word;
  }"
)

# Barra de menú
sidebar <- sidebar(
  bg = "#FAFCFC",
  width = "280px",
  accordion(
    multiple = FALSE,
    accordion_panel("Visualización", id = "vista", icon = icon("location-dot"), # Menú de "Visualización"
                    radioButtons("nivel", label = tags$span("Mostrar", class = "text-body-secondary"), choices = list("Todo" = "T", "Por Municipio" = "M", "Por Entidad Federativa" = "E", "Por Región" = "R")),
                    conditionalPanel(
                      condition = "input.nivel == 'M'",
                      pickerInput(
                        inputId = "nivel1", label = "Entidades federativas", choices = opcion_edo, multiple = TRUE,
                        options = pickerOptions(
                          actionsBox = TRUE, liveSearch = TRUE, 
                          `none-selected-text` = "Elige una opción o más", 
                          `select-all-text` = "Seleccionar todo", 
                          `deselect-all-text` = "Deseleccionar todo"
                        )
                      )
                    )
    ),
    
    accordion_panel("Filtros", id = "filtros", icon = icon("filter"), # Menú de "Filtros"
                    selectInput(inputId = "anio", label = tags$span("Año", class = "text-body-secondary"), choices = c("Elige una opción" = "", opcion_anio)),
                    uiOutput("selectInfo1"),
                    uiOutput("selectInfo2")
    ),
    
    tags$br(),
    uiOutput("boton") # Botón de "Consultar"
  )
)

# Elemento UI
ui <- page_navbar(
  # Tema
  theme = tema,
  
  # Título
  title = "Atlas MX",
  header = shinyjs::useShinyjs(),
  
  # Pestaña de Datos
  nav_panel(
    title = "Consulta de datos",
    icon = icon("table", class = "fa-solid"),
    layout_sidebar(
      sidebar = sidebar,
      uiOutput("titulo"),
      uiOutput("panel"),
      uiOutput("tablas")
    )
  )
)

# Servidor
server = function(input, output){
  # Barra de menú:
  # Restablecer valores de la pestaña "Visualización"
  observeEvent(input$nivel, {
    if(input$nivel != "M") reset("nivel1")
  })
  
  # Mostrar opciones de la pestaña "Información"
  observeEvent(input$anio, {
    reset("selec")
  })
    
  output$selectInfo1 <- renderUI({
    req(input$anio)
    selectInput(inputId = "selec", label = tags$span("Seleccionar", class = "text-body-secondary"), choices = list("Elige una opción" = "", "Total" = "X", "Por tema" = "T", "Por variable" = "V"))
  })
    
  observeEvent(input$selec, {
    reset("tema")
    hide(id = "tema")
  })
  
  opciones <- list()
  output$selectInfo2 <- renderUI({
    req(input$selec)
    if (input$selec != "X") {
      tema_desc <- read_excel(paste0("data/",input$anio,".xlsx"), sheet = "Descriptor") %>% select("VARIABLE", "TEMA", "DESCRIPCIÓN")
      tema_desc <- tema_desc[-(1:7),]
      
      if(input$selec == "T"){
        opciones <<- unique(substr(tema_desc$VARIABLE, 1, 3))
        names(opciones) <<- unique(str_to_sentence(tema_desc$TEMA))
        lbl <- "Temas"
      } else if(input$selec == "V"){
        opciones <<- tema_desc$VARIABLE
        names(opciones) <<- str_to_sentence(tema_desc$DESCRIPCIÓN)
        lbl <- "Variables"
      }
      
      pickerInput(
        inputId = "tema", label = tags$span(lbl, class = "text-body-secondary"), choices = opciones, multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE, 
          `none-selected-text` = "Elige una opción o más", 
          `select-all-text` = "Seleccionar todo", 
          `deselect-all-text` = "Deseleccionar todo"
        )
      )
    }
  })
  
  # Validación previa a mostrar el botón
  output$boton <- renderUI({
    if(input$nivel == "M") req(input$nivel1)
    req(input$anio, input$selec)
    if(input$selec != "X") req(input$tema)
    
    column(12, align = "center", offset = 0, actionButton("enviar", "Consultar", width = "100", class="btn btn-primary"))
  })
  
  # Cuerpo:
  # Función para generar el título en el panel "Consulta de datos"
  generar_titulo <- function(i,j){
    titulo <- paste0("Datos de Población y Vivienda en México durante ", input$anio, ".")
    
    output$titulo <- renderUI({
      tags$div(
        class = "d-flex align-items-center",
        style = "gap: 10px;",
        tags$h3(titulo, style = "margin: 0; color: #78C2AD;"),
        actionButton(inputId = "btn_info", label = NULL, icon = icon("info-circle", style = "color: #78C2AD;"), 
                     class = "btn-sm btn-outline-light", `data-bs-toggle` = "offcanvas", `data-bs-target` = "#info_consulta")
      )
    })
    
    # Panel con información detallada de la consulta
    output$panel <- renderUI({
      tags$div(
        class = "offcanvas offcanvas-end text-bg-dark", 
        tabindex = "-1", 
        id = "info_consulta",
        tags$div(
          class = "offcanvas-header",
          tags$h4(class = "offcanvas-title", "Detalles de la consulta", style = "color: #78C2AD;"),
          tags$button(type = "button", class = "btn-close btn-close-white", `data-bs-dismiss` = "offcanvas")
        ),
        tags$div(
          class = "offcanvas-body", 
          card(
            class = "card text-white bg-dark border-primary mb-3",
            card_header(
              class = "card-header bg-primary text-dark",
              strong("Visualización")
            ),
            card_body(
              class = "card-body bg-dark",
              if(input$nivel == "M"){
                tagList(
                  p("Se muestran los datos de población y vivienda agrupados por municipio para las siguientes entidades federativas:"),
                  p(str_to_title(paste(i, collapse = ", ")))
                )
              } else if(input$nivel == "E"){
                p("Se muestran los datos de población y vivienda a nivel nacional agrupados por entidad federativa.")
              } else if(input$nivel == "R"){
                tagList(
                  p("Se muestran los datos de población y vivienda a nivel nacional agrupados por región."),
                  p("Centronorte: Aguascalientes, Guanajuato, Querétaro, San Luis Potosí, Zacatecas"),
                  p("Centrosur: Ciudad De México, México, Morelos"),
                  p("Noreste: Coahuila, Nuevo León, Tamaulipas"),
                  p("Noroeste: Baja California, Baja California Sur, Chihuahua, Durango, Sinaloa, Sonora"),
                  p("Occidente: Colima, Jalisco, Michoacán, Nayarit"),
                  p("Oriente: Hidalgo, Puebla, Tlaxcala, Veracruz"),
                  p("Sureste: Campeche, Quintana Roo, Tabasco, Yucatán"),
                  p("Suroeste: Chiapas, Guerrero, Oaxaca")
                )
              } else {
                p("Se muestran todos los datos de población y vivienda a nivel nacional agrupados por municipio.")
              }
            )
          ),
          card(
            class = "card text-white bg-dark border-success mb-3",
            card_header(
              class = "card-header bg-success text-dark",
              strong("Periodo")
            ),
            card_body(
              class = "card-body bg-dark",
              p("Se seleccionaron los datos registrados durante el año", input$anio)
            )
          ),
          card(
            class = "card text-white bg-dark border-light mb-3",
            card_header(
              class = "card-header bg-light text-dark",
              strong("Información")
            ),
            card_body(
              class = "card-body bg-dark",
              if(input$selec == "T"){
                tagList(
                  p("Se presentan los datos correspondientes a los siguientes temas:"),
                  p(paste(str_to_sentence(j), collapse = ", "))
                )
              } else if(input$selec == "V"){
                tagList(
                  p("Se presentan los siguientes datos:"),
                  p(paste(str_to_sentence(j), collapse = ", "))
                )
              } else {
                p("Se presenta toda la información disponible para el año seleccionado.")
              }
            )
          )
        )
      )
    })
    
    output$tablas <- renderUI({
      tabsetPanel(
        type = "pills",
        tabPanel("Datos", br(), dataTableOutput("tabla")),
        tabPanel("Descripción", br(), dataTableOutput("descr"))
      )
    })
  }
  
  # Configuración predeterminada para DataTables
  base_dt <- list(
    language = list(
      url = 'https://cdn.datatables.net/plug-ins/3.0.1/i18n/es-MX.json',
      paginate = list(
        previous = '<i class="fa-solid fa-angle-left"></i>',
        `next` = '<i class="fa-solid fa-angle-right"></i>',
        first = '<i class="fa-solid fa-angles-left"></i>',
        last = '<i class="fa-solid fa-angles-right"></i>'
      )
    ),
    pagingType = "full",
    searching = TRUE,
    escape = FALSE,
    pageLength = 10,
    scrollX = TRUE,
    fixedColumns = list(leftColumns = 2),
    lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'Todo')),
    dom = "<'row'<'col-sm-12'B>>
                 <'row'<'col-sm-6'l>
                 <'col-sm-6'f>>
                 <'row'<'col-sm-12'tr>>
                 <'row'<'col-sm-6'i>
                 <'col-sm-6'p>>",
    buttons = list(
      list(extend = 'copy'),
      list(extend = 'print'),
      list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Descargar'
      )
    )
  )
  
  # Función para generar las tablas
  generar_tabla <- function(datos, cols_dbl, nombres){
    datos <- datos %>% mutate(across(all_of(cols_dbl), \(x) round(x, digits = 2)))
    datos <- datos %>% mutate(across(everything(), ~ replace_na(as.character(.), "NA")))
    
    output$tabla <- renderDataTable({
      datatable(
        datos,
        colnames = nombres,
        extensions = c('Buttons', 'FixedColumns'),
        options = base_dt
      );
    })
  }
  
  generar_descr <- function(datos){
    output$descr <- renderDataTable({
      datatable(
        datos,
        extensions = c('Buttons', 'FixedColumns'),
        options = base_dt
      );
    })
  }
  
  # Se genera la consulta
  observeEvent(input$enviar, {
    # Auxiliares para el panel de información de consulta
    i <- c()
    j <- c()
    
    # Lectura del [Descriptor] del archivo de datos
    descriptor <- read_excel(paste0("data/",input$anio,".xlsx"), sheet = "Descriptor") %>% slice(-c(1, 2))
    
    # Obtener tipo de dato de las columnas
    tipos_cols <- list(
      chr = descriptor$VARIABLE[descriptor$TIPO == "CARACTER"],
      int = descriptor$VARIABLE[descriptor$TIPO == "ENTERO"],
      dbl  = descriptor$VARIABLE[descriptor$TIPO == "DECIMAL"]
    )
    
    descriptor <- descriptor %>% select(-c("AÑO","TIPO","CATEGORÍAS O DIVISIONES"))
    
    # Lectura del [Año] del archivo de datos
    datos_anio <- read_excel(paste0("data/",input$anio,".xlsx"), sheet = input$anio, na = 'NA') %>% select(-1,-2)
    
    datos_anio <- datos_anio %>% 
                  mutate(
                    across(all_of(tipos_cols$chr), as.character),
                    across(all_of(tipos_cols$int), as.integer),
                    across(all_of(tipos_cols$dbl), as.double)
                  )
    
    # Filtrar según lo que seleccione el usuario
    if(input$selec == "T"){ # Por tema
      datos_anio <- datos_anio %>% select(1:5, contains(input$tema))
    } else if(input$selec == "V"){ # Por variable
      datos_anio <- datos_anio %>% select(1:5, input$tema)
    } 
    
    # Forma de visualización
    subdatos <- datos_anio # Vista para Todo
    
    if(input$nivel == "M"){ # Vista por Municipio
      subdatos <- subdatos %>% filter(CLAVE_ENT %in% as.integer(input$nivel1))
      i <- unique(subdatos$ENTIDAD)
    } else if(input$nivel == "E"){ # Vista por Entidad Federativa
      subdatos <- subdatos %>% select(-c("CLAVE_GEO","CLAVE_MUN","MUNICIPIO"))
      
      enteros <- intersect(names(subdatos),tipos_cols$int)
      enteros <- enteros[enteros != "CLAVE_ENT"]
      decimales <- intersect(names(subdatos),tipos_cols$dbl)
      
      if("AUR03" %in% names(subdatos)){
        grado <- factor(subdatos$AUR03, levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))
        subdatos$AUR03 <- as.numeric(grado)
        decimales <- c(decimales, "AUR03")
      }
      
      subdatos <- subdatos %>% group_by(CLAVE_ENT) %>%
        summarise(
          across("ENTIDAD", first),
          across(all_of(enteros), \(x) sum(x, na.rm = TRUE)),
          across(all_of(decimales), \(x) mean(x, na.rm = TRUE))
        )
      
      if("AUR03" %in% names(subdatos)){
        subdatos$AUR03 <- levels(grado)[round(subdatos$AUR03)]
      }
      
      subdatos <- subdatos %>% select(-1)
    } else if(input$nivel == "R"){ # Vista por Región
      reg <- estados %>% select("CLAVE_ENT","REGION")
      subdatos <- left_join(subdatos, reg, by = "CLAVE_ENT") %>% relocate(last_col(), .before = 1)
      subdatos <- subdatos %>% select(-c("CLAVE_GEO","CLAVE_ENT","CLAVE_MUN","MUNICIPIO"))
      
      enteros <- intersect(names(subdatos),tipos_cols$int)
      decimales <- intersect(names(subdatos),tipos_cols$dbl)
      
      if("AUR03" %in% names(subdatos)){
        grado <- factor(subdatos$AUR03, levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))
        subdatos$AUR03 <- as.numeric(grado)
        decimales <- c(decimales, "AUR03")
      }
      
      subdatos <- subdatos %>% group_by(REGION) %>%
        summarise(
          across(all_of(enteros), \(x) sum(x, na.rm = TRUE)),
          across(all_of(decimales), \(x) mean(x, na.rm = TRUE))
        )
      
      if("AUR03" %in% names(subdatos)){
        subdatos$AUR03 <- levels(grado)[round(subdatos$AUR03)]
      }
      
      nr <- data.frame(
        TEMA = "DATOS GEOGRÁFICOS",
        VARIABLE = "REGION",
        DESCRIPCIÓN = "NOMBRE DE LA REGIÓN",
        NEMÓNICO = "NOM_REG",
        FUENTE = ""
      )
      descriptor <- rbind(nr, descriptor)
    }

    mne <- descriptor[, c("VARIABLE","NEMÓNICO")]
    mne <- mne %>% filter(VARIABLE %in% names(subdatos))
    cols_dbl <- intersect(names(subdatos),tipos_cols$dbl)
    descriptor <- descriptor %>% filter(NEMÓNICO %in% mne[["NEMÓNICO"]])
    
    if(input$selec == "T") j <- unique(descriptor$TEMA)[-1]
    if(input$selec == "V"){
      j <- unique(descriptor$DESCRIPCIÓN)
      j <- tail(j, length(input$tema))
    }
    
    descriptor <- descriptor %>% select(-"VARIABLE") %>% relocate("NEMÓNICO", .before = "TEMA")
    colnames(descriptor)[1] <- "NOMBRE"
    
    # [Consulta de datos]
    # Título
    generar_titulo(i,j)
    
    # DataTables
    generar_tabla(subdatos, cols_dbl, mne[["NEMÓNICO"]])
    generar_descr(descriptor)
  })
}

# Ejecución de la aplicación
shinyApp(ui, server)