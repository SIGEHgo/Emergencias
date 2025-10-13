# app.R
#DBI::dbDisconnect(buig)
#source("../../../Reutilizables/Postgres_BUIG/conexion_buig.R")

library(shiny)
library(bslib)
library(leaflet) 
library(dplyr)
library(sf) 

CAPA_CONFIG <- list(
  # [tipo_geom, group_name, nombre_buig]
  
  ## Grupo 1: Desagregación geográfica
  'g1_c1' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="limite_municipal",cols=c('Name',"Nombre","")),  # Municipios
  'g1_c2' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="poblacion_scince_2020ageb"),  # AGEB
  'g1_c3' = list(tipo_geom="", group="Desagregación geográfica", nombre_buig=""),  # Localidad Urbana
  'g1_c4' = list(tipo_geom="", group="Desagregación geográfica", nombre_buig=""),  # Localidad Rural
  'g1_c5' = list(tipo_geom="", group="Desagregación geográfica", nombre_buig=""),  # Regiones
  
  ## Grupo 2: Capas de Salud
  'g2_c1' = list(tipo_geom="POINT", group="Capas de Salud", nombre_buig=''), # Centros de Salud
  'g2_c2' = list(tipo_geom="POINT", group="Capas de Salud", nombre_buig=''),  # Hospital General
  'g2_c3' = list(tipo_geom="", group="Capas de Salud", nombre_buig=""),  # Hospital regional
  
  ## Grupo 3: Recursos Hídricos y Cuencas
  'g3_c1' = list(tipo_geom="", group="Recursos Hídricos y Cuencas", nombre_buig=""),  # Canales
  'g3_c2' = list(tipo_geom="", group="Recursos Hídricos y Cuencas", nombre_buig=""),  # Pozos
  'g3_c3' = list(tipo_geom="", group="Recursos Hídricos y Cuencas", nombre_buig=""), # Ríos
  'g3_c4' = list(tipo_geom="", group="Recursos Hídricos y Cuencas", nombre_buig=""),  # Manantiales
  'g3_c5' = list(tipo_geom="", group="Recursos Hídricos y Cuencas", nombre_buig=""),  # Cuerpos de Agua
  
  ## Grupo 4: Zonificación de Vulnerabilidad
  # g4_c5 simula un error de carga para demostrar el manejo de excepciones.
  'g4_c1' = list(tipo_geom="", group="Zonificación de Vulnerabilidad", nombre_buig=""),  # Caida de bloques
  'g4_c2' = list(tipo_geom="", group="Zonificación de Vulnerabilidad", nombre_buig=""),  # Suceptibles a deslizamiento
  'g4_c3' = list(tipo_geom="", group="Zonificación de Vulnerabilidad", nombre_buig=""),  # Hundimiento
  'g4_c4' = list(tipo_geom="", group="Zonificación de Vulnerabilidad", nombre_buig=""),  # Fallas geológicas
  'g4_c5' = list(tipo_geom="POLYGON", group="Zonificación de Vulnerabilidad", nombre_buig=""), # Zonas de Inundación
  
  ## Grupo 5: Infraestructura Vial
  'g5_c1' = list(tipo_geom="", group="Infraestructura Vial", nombre_buig=""),  # Estructuras Viales
  'g5_c2' = list(tipo_geom="", group="Infraestructura Vial", nombre_buig=""),  # Puentes
  'g5_c3' = list(tipo_geom="LINESTRING", group="Infraestructura Vial", nombre_buig=""), # Carreteras Federales
  'g5_c4' = list(tipo_geom="", group="Infraestructura Vial", nombre_buig=""),  # Carreteras Estatales
  'g5_c5' = list(tipo_geom="", group="Infraestructura Vial", nombre_buig=""),  # Carreteras Municipales
  'g5_c6' = list(tipo_geom="", group="Infraestructura Vial", nombre_buig=""),  # Caminos Rurales
  
  ## Grupo 6: Otra Infraestructura
  'g6_c1' = list(tipo_geom="", group="Otra Infraestructura", nombre_buig=""),  # Estaciones eléctricas
  'g6_c2' = list(tipo_geom="", group="Otra Infraestructura", nombre_buig=""),  # Drenes pluviales
  'g6_c3' = list(tipo_geom="", group="Otra Infraestructura", nombre_buig=""),  # Estructuras de Elevación
  
  ## Grupo 7: Puntos de Reunión y Centros de Acopio
  'g7_c1' = list(tipo_geom="", group="Puntos de Reunión y Centros de Acopio", nombre_buig=""),  # Centros de Acopio
  'g7_c2' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="albergues"), # Albergues
  'g7_c3' = list(tipo_geom="", group="Puntos de Reunión y Centros de Acopio", nombre_buig=""),  # Comedores Comunitarios
  'g7_c4' = list(tipo_geom="", group="Puntos de Reunión y Centros de Acopio", nombre_buig=""),  # Escuelas
  'g7_c5' = list(tipo_geom="", group="Puntos de Reunión y Centros de Acopio", nombre_buig=""),  # Universidades Estatales
  'g7_c6' = list(tipo_geom="", group="Puntos de Reunión y Centros de Acopio", nombre_buig="")  # Centros de Atención Infantil Comunitaria (CAIC)
)

load_layer_data <- function(layer_id) {
  return(st_read(buig, CAPA_CONFIG[[layer_id]]$nombre_buig))
}

ui <- page_sidebar(
  title = "Explorador de Capas Geográficas de Emergencia",
  sidebar = sidebar(
    title = "Control de Capas",
    width = 350, 
    open = "open", 
    accordion(
      id = "acordeon_capas",
      multiple = TRUE, 
      
      accordion_panel(title = "Desagregación geográfica", icon = icon("map"), 
                      checkboxGroupInput(inputId = "capa_base_g1", label = "Selecciona elementos:", 
                                         choiceNames = c("Municipios","AGEB","Localidad Urbana","Localidad Rural","Regiones"),
                                         choiceValues = paste0("g1_c", 1:5))),
      accordion_panel(title = "Salud", icon = icon("hospital"), 
                      checkboxGroupInput(inputId = "capa_dem_g2", label = "Selecciona elementos:", 
                                         choiceNames = c("Centros de Salud","Hospital General","Hospital regional"),
                                         choiceValues = paste0("g2_c", 1:3))),
      accordion_panel(title = "Recursos Hídricos y Cuencas", icon = icon("water"),
                      checkboxGroupInput(inputId = "capa_hid_g3", label = "Selecciona elementos:", 
                                         choiceNames = c("Canales","Pozos","Ríos","Manantiales","Cuerpos de Agua"),
                                         choiceValues = paste0("g3_c", 1:5))),
      accordion_panel(title = "Zonificación de Vulnerabilidad", icon = icon("triangle-exclamation"), 
                      checkboxGroupInput(inputId = "capa_hid_g4", label = "Selecciona elementos:", 
                                         choiceNames = c("Caida de bloques","Suceptibles a deslizamiento","Hundimiento","Fallas geológicas","Zonas de Inundación"),
                                         choiceValues = paste0("g4_c", 1:5))),
      accordion_panel(title = "Infraestructura Vial", icon = icon("road"),
                      checkboxGroupInput(inputId = "capa_vial_g5", label = "Selecciona elementos:", 
                                         choiceNames = c("Estructuras Viales","Puentes","Carreteras Federales", "Carreteras Estatales", "Carreteras Municipales", "Caminos Rurales"),
                                         choiceValues = paste0("g5_c", 1:6))),
      accordion_panel(title = "Otra Infraestructura", icon = icon("building"),
                      checkboxGroupInput(inputId = "capa_infra_g6", label = "Selecciona elementos:", 
                                         choiceNames = c("Estaciones eléctricas","Drenes pluviales","Estructuras de Elevación"),
                                         choiceValues = paste0("g6_c", 1:3))),
      accordion_panel(title = "Puntos de Reunión y Centros de Acopio", icon = icon("handshake"),
                      checkboxGroupInput(inputId = "capa_poi_g7", label = "Selecciona elementos:", 
                                         choiceNames = c("Centros de Acopio","Albergues","Comedores Comunitarios","Escuelas","Universidades Estatales","Centros de Atención Infantil Comunitaria (CAIC)"),
                                         choiceValues = paste0("g7_c", 1:6)))
    )
  ),
  card(
    full_screen = TRUE, 
    card_header("Visualización Geográfica"),
    leafletOutput("mapa_principal", height = "80vh") 
  )
)


server <- function(input, output, session) {
  
  # Mapeo de Layer Key (ej. 'g1_c1') al Input ID (ej. 'capa_base_g1')
  INPUT_TO_KEY_MAP <- list(
    capa_base_g1 = paste0("g1_c", 1:5),
    capa_dem_g2 = paste0("g2_c", 1:3),
    capa_hid_g3 = paste0("g3_c", 1:5),
    capa_hid_g4 = paste0("g4_c", 1:5),
    capa_vial_g5 = paste0("g5_c", 1:6),
    capa_infra_g6 = paste0("g6_c", 1:3),
    capa_poi_g7 = paste0("g7_c", 1:6)
  )
  
  # Función para encontrar el ID del input padre a partir del layer_key
  find_parent_input_id <- function(layer_key) {
    for (id in names(INPUT_TO_KEY_MAP)) {
      if (layer_key %in% INPUT_TO_KEY_MAP[[id]]) {
        return(id)
      }
    }
    return(NULL)
  }
  
  # Almacena el estado de las capas activas del ciclo anterior
  v <- reactiveValues(selected = character(0))
  
  #Inicialización del Mapa Base
  output$mapa_principal <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(lng = -98, lat = 20, zoom = 6) |>  
      addLayersControl(
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  selected_layers_vector <- reactive({
    c(input$capa_base_g1, input$capa_dem_g2, input$capa_hid_g3, 
      input$capa_hid_g4, input$capa_vial_g5, input$capa_infra_g6, 
      input$capa_poi_g7) %>% unique()
  })
  
  observe({
    
    print("Una llamada")##Quiero ver si puedo hacer solo una llamada activando varios checkboxes
    
    current_layers <- selected_layers_vector()
    previous_layers <- v$selected
    
    proxy <- leafletProxy("mapa_principal", session)
    
    layers_to_add <- setdiff(current_layers, previous_layers)
    layers_to_remove <- setdiff(previous_layers, current_layers)
    
    for (layer_key in layers_to_remove) {
      proxy %>% clearGroup(group = layer_key)
    }
    successful_layers <- setdiff(previous_layers, layers_to_remove)##Aquí no debería haber problema
    
    for (layer_key in layers_to_add) {
      
      config <- CAPA_CONFIG[[layer_key]]
      if (is.null(config)) next 
      tryCatch({
        data_sf <- load_layer_data(layer_key)
        geom_type <- as.character(unique(st_geometry_type(data_sf)))[1]
        if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {###Estos se pueden pasar a una función auxiliar
          proxy |> 
            addPolygons(
              data = data_sf,
              fillColor = "#66A3D2", 
              color = "white",
              weight = 1,
              fillOpacity = 0.4,
              group = layer_key
            )
        } else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
          proxy |> 
            addPolylines(
              data = data_sf,
              color = "black", 
              weight = 3,
              opacity = 0.8,
              group = layer_key
            )
        } else if (geom_type %in% c("POINT", "MULTIPOINT")) {
          proxy |> 
            addCircleMarkers(
              data = data_sf,
              radius = 6,
              color = "red", 
              fillOpacity = 0.9,
              group = layer_key
            )
        }
        
        # Si todo fue exitoso, añadimos la clave a las capas activas
        successful_layers <- c(successful_layers, layer_key)
        
      }, error = function(e) {##Si algo falla, que desmarque las checkboxes
        print(paste("ERROR:", e$message))
          showNotification(
          ui = HTML(paste("<strong>Error de Carga:</strong> No se pudo cargar la capa <b>", config$nombre_buig, "</b>.", 
                          "Por favor, verifique la conexión o el nombre de la tabla. ")),
          type = "error",
          duration = 2
        )
        parent_input_id <- find_parent_input_id(layer_key)
        if (!is.null(parent_input_id)) {
          
          # Obtenemos la lista actual de seleccionados en ese grupo y filtramos la capa fallida
          current_selection_in_group <- input[[parent_input_id]]
          new_selection <- current_selection_in_group[current_selection_in_group != layer_key] 
          
          # Actualizamos el input para desmarcar el que falló
          updateCheckboxGroupInput(session, parent_input_id, selected = new_selection)
        }
        
      })
    }
    
    v$selected <- successful_layers %>% unique()
    
  }) # Fin del observe
}

shinyApp(ui, server)

