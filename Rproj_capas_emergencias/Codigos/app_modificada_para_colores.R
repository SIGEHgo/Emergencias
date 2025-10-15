# app.R
#DBI::dbDisconnect(buig)
source("../../../Reutilizables/Postgres_BUIG/conexion_buig.R")

library(shiny)
library(bslib)
library(leaflet) 
library(dplyr)
library(sf) 
library(shinybusy)
library(colourpicker)
CAPA_CONFIG <- list(
  # [tipo_geom, group_name, nombre_buig]
  
  ## Grupo 1: Desagregación geográfica
  'g1_c1' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="limite_municipal", cols=c("cvegeo", "nomgeo", "the_geom"), data = NULL),  # Municipios
  'g1_c2' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="poblacion_scince_2020ageb", cols=c("cve_ent", "cve_mun", "cve_loc", "cve_ageb", "pob1", "geom"), data = NULL),  # AGEB
  'g1_c3' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="poblacion_scince_2020localidad_urbana", cols=c("nomgeo", "cabecera", "cve_mun", "cve_loc", "pob1", "geom"), data = NULL),  # Localidad Urbana
  'g1_c4' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="poblacion_scince_2020localidad rural", cols=c("nomgeo", "nom_ent", "pob1", "geom"), data = NULL),  # Localidad Rural
  'g1_c5' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="regionalizacion", cols=c("region","the_geom"), data = NULL),  # Regiones
  
  ## Grupo 2: Capas de Salud
  'g2_c1' = list(tipo_geom="POINT", group="Salud", nombre_buig='13salud', cols=c("Name", "Nombre", ""), data = NULL), # Centros de Salud
  'g2_c2' = list(tipo_geom="POLYGON", group="Salud", nombre_buig="hospitales_hgo", cols=c("name", "area", "geom"), data = NULL),  # Hospital General
  'g2_c3' = list(tipo_geom="POLYGON", group="Salud", nombre_buig="hospitales_hgo", cols=c("name", "area", "geom"), data = NULL),  # Hospital regional
  
  ## Grupo 3: Recursos Hídricos y Cuencas
  'g3_c1' = list(tipo_geom="LINESTRING", group="Recursos Hídricos y Cuencas", nombre_buig="canales", cols=c("identifica", "condicion", "geom"), data = NULL),  # Canales
  'g3_c2' = list(tipo_geom="POINT", group="Recursos Hídricos y Cuencas", nombre_buig="pozos_con_nivel_piezometrico", cols=c("nom_pozo", "geom"), data = NULL),  # Pozos
  'g3_c3' = list(tipo_geom="LINESTRING", group="Recursos Hídricos y Cuencas", nombre_buig="rios", cols=c("nombre", "condicion", "st_length_", "geom"), data = NULL), # Ríos
  'g3_c4' = list(tipo_geom="POINT", group="Recursos Hídricos y Cuencas", nombre_buig="manantiales_50_inegi", cols=c("nom_man", "geom"), data = NULL),  # Manantiales
  'g3_c5' = list(tipo_geom="POLYGON", group="Recursos Hídricos y Cuencas", nombre_buig="cuerpos_de_agua", cols=c("condicion", "shape_leng", "shape_area","geom"), data = NULL),  # Cuerpos de Agua
  
  ## Grupo 4: Zonificación de Vulnerabilidad
  # g4_c5 simula un error de carga para demostrar el manejo de excepciones.
  'g4_c1' = list(tipo_geom="POLYGON", group="Zonificación de Vulnerabilidad", nombre_buig="peligro_por_caida_de_bloques", cols=c("tipo","geom"), data = NULL),  # Caida de bloques
  'g4_c2' = list(tipo_geom="POLYGON", group="Zonificación de Vulnerabilidad", nombre_buig="zonificacion_de_vulnerabilidad_y_areas_susceptibles_a_deslizami", cols=c("tipo", "geom"), data = NULL),  # Suceptibles a deslizamiento
  'g4_c3' = list(tipo_geom="POLYGON", group="Zonificación de Vulnerabilidad", nombre_buig="zonificacion_de_vulnerabilidad_y_areas_susceptibles_a_hundimien", cols=c("peligro", "geom"), data = NULL),  # Hundimiento
  'g4_c4' = list(tipo_geom="LINESTRING", group="Zonificación de Vulnerabilidad", nombre_buig="estructuras_geologicas", cols=c("entidad", "geom"), data = NULL),  # Fallas geológicas
  'g4_c5' = list(tipo_geom="POLYGON", group="Zonificación de Vulnerabilidad", nombre_buig="", cols=c("Name", "Nombre", ""), data = NULL), # Zonas de Inundación
  
  ## Grupo 5: Infraestructura Vial
  'g5_c1' = list(tipo_geom="POINT", group="Infraestructura Vial", nombre_buig="estructuras_viales", cols=c("tipo", "nombre", "altura", "ancho", "geom"), data = NULL),  # Estructuras Viales
  'g5_c2' = list(tipo_geom="LINESTRING", group="Infraestructura Vial", nombre_buig="red_carretera_sipdus", cols=c("administra", "nombre", "cond_pav", "recubri", "carriles", "circula", "velocidad","geom"), data = NULL,custom_filter='Federal'), # Carreteras Federales
  'g5_c3' = list(tipo_geom="LINESTRING", group="Infraestructura Vial", nombre_buig="red_carretera_sipdus", cols=c("administra", "nombre", "cond_pav", "recubri", "carriles", "circula", "velocidad","geom"), data = NULL,custom_filter='Estatal'),  # Carreteras Estatales
  'g5_c4' = list(tipo_geom="LINESTRING", group="Infraestructura Vial", nombre_buig="red_carretera_sipdus", cols=c("administra", "nombre", "cond_pav", "recubri", "carriles", "circula", "velocidad","geom"), data = NULL,custom_filter='Municipal'),  # Carreteras Municipales
  
  ## Grupo 6: Otra Infraestructura
  'g6_c1' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="subestacion_electrica", cols=c("geografico", "nombre", "condicion",'geom'), data = NULL),  # Estaciones eléctricas
  'g6_c2' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="oficinas_nacionales", cols=c("nom_estab", "municipio","localidad", "geom"), data = NULL),  # Oficina
  'g6_c3' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="oficinas_estatales", cols=c("nom_estab", "geom"), data = NULL),  # Oficina
  'g6_c4' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="oficinas_municipales", cols=c("nom_estab", "municipio","localidad","geom"), data = NULL),  # Oficina
  'g6_c5' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="oficinas_regionales", cols=c("nom_estab", "municipio","localidad", "geom"), data = NULL),  # Oficina
  
  ## Grupo 7: Puntos de Reunión y Centros de Acopio
  'g7_c1' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="centros_de_acopio", cols=c("nom_estab", "tipo", "geom"), data = NULL),  # Centros de Acopio
  'g7_c2' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="albergues", cols=c("nom_mun", "colonia", "nombre", "registr", "anio", "ambito","geom"), data = NULL), # Albergues
  'g7_c3' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="comedores_comunitarios_en_operacion", cols=c("tipo_educa", "nivel_educ", "nombre_d_4", "mun", "loc", "nom_plante", "nom_comedo", "geom"), data = NULL),  # Comedores Comunitarios
  'g7_c4' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="escuelas_seph_2025", cols=c("nom_centro", "nom_turno", "tipo", "nivel_educ", "servicio_e", "nom_mun", "nom_loc", "colonia","geom"), data = NULL),  # Escuelas
  'g7_c5' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="universidades_estatales", cols=c("name", "unv_inst", "geom"), data = NULL),  # Universidades Estatales
  'g7_c6' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="caics_2024", cols=c("name", "popupinfo", "geom"), data = NULL)  # Centros de Atención Infantil Comunitaria (CAIC)
)

load_layer_data = function(buig=buig,nombre_buig, columnas_interes,custom_filter='') {
  datos = dplyr::tbl(buig, nombre_buig) |> 
    dplyr::select(dplyr::all_of(columnas_interes)) 
  if(custom_filter!=''){
    datos=datos |>
      dplyr::filter(administra==custom_filter)
  }
  datos=datos |> dplyr::collect() |> 
    dplyr::mutate(geom = sf::st_as_sfc(geom, EWKB = T)) 
  
  coordenadas = sf::st_coordinates(datos$geom[1])[1, 1]
  
  if (coordenadas > 30) {
    datos = datos |> sf::st_as_sf(crs = 32614) |> sf::st_transform(crs = 4326) |>  sf::st_zm()
  } else {
    datos = datos |> sf::st_as_sf(crs = 4326)  |>  sf::st_zm()
  }
  return(datos)
}
#load_layer_data(buig = buig,nombre_buig = "red_carretera_sipdus",columnas_interes =c("administra", "nombre", "cond_pav", "recubri", "carriles", "circula", "velocidad","geom") ,custom_filter = "Federal")
#El de municipal no jala porque la columna de geometría se llama the_geom.#Revisar el nombre, si es the_geom, renombrar y continuar

config_modal <- function(layer_key, layer_name, initial_color) {
  modalDialog(
    title = paste("Configuración de", layer_name),
    size = "s", # Modal pequeño
    
    #Selector de Color
    colourInput(
      inputId = paste0("modal_", layer_key, "_col"),
      label = "Color de la Capa:",
      value = initial_color,
      palette = "square",
      closeOnClick = TRUE
    ),
    
    # Slider de Tamaño/Radio ###Pendiente. Deberá aplicarse a las fronteras de los poligonos o a los anchos de las lineas o a los radios de los circulos
    sliderInput(
      inputId = paste0("modal_", layer_key, "_size"),
      label = "Tamaño/Radio:",
      min = 1, max = 15, value = 6, step = 1
    ),
    
    footer = tagList(
      modalButton("Cancelar"),
      actionButton(paste0("modal_", layer_key, "_save"), "Guardar Cambios", class = "btn-primary")
    )
  )
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
                      checkboxGroupInput(inputId = "capa_base_g1", label = NULL, 
                                         choiceNames = NULL, choiceValues = NULL),##Vacio
                      
                      
                          # Municipio
                          div(class = "d-flex justify-content-between align-items-center mb-2", 
                              checkboxInput("g1_c1_chk", "Municipios", value = FALSE),
                              actionButton("g1_c1_btn", label = NULL, icon = icon("gear"), 
                                           class = "btn-xs btn-default", 
                                           style = "padding: 5px; height: 30px;")
                          ),
                          # AGEB
                          div(class = "d-flex justify-content-between align-items-center mb-2",
                              checkboxInput("g1_c2_chk", "AGEB", value = FALSE),
                              actionButton("g1_c2_btn", label = NULL, icon = icon("gear"), 
                                           class = "btn-xs btn-default", 
                                           style = "padding: 5px; height: 30px;")
                          ),
                          # Localidad Urbana
                          div(class = "d-flex justify-content-between align-items-center mb-2",
                              checkboxInput("g1_c3_chk", "Localidad Urbana", value = FALSE),
                              actionButton("g1_c3_btn", label = NULL, icon = icon("gear"), 
                                           class = "btn-xs btn-default", 
                                           style = "padding: 5px; height: 30px;")
                          )#,... Los otros
                      
      ),
      accordion_panel(title = "Salud", icon = icon("hospital"), 
                      checkboxGroupInput(inputId = "capa_dem_g2", label = NULL, 
                                         choiceNames = NULL, choiceValues = NULL),
                      
                      div(class = "custom-control-capa",
                          div(class = "d-flex justify-content-between align-items-center mb-2", 
                              checkboxInput("g2_c1_chk", "Centros de Salud", value = FALSE),
                              actionButton("g2_c1_btn", label = NULL, icon = icon("gear"), 
                                           class = "btn-xs btn-default", 
                                           style = "padding: 5px; height: 30px;")
                          ),
                          div(class = "d-flex justify-content-between align-items-center mb-2",
                              checkboxInput("g2_c2_chk", "Hospital General", value = FALSE),
                              actionButton("g2_c2_btn", label = NULL, icon = icon("gear"), 
                                           class = "btn-xs btn-default", 
                                           style = "padding: 5px; height: 30px;")
                          ),
                          div(class = "d-flex justify-content-between align-items-center mb-2",
                              checkboxInput("g2_c3_chk", "Hospital regional", value = FALSE),
                              actionButton("g2_c3_btn", label = NULL, icon = icon("gear"), 
                                           class = "btn-xs btn-default", 
                                           style = "padding: 5px; height: 30px;")
                          )
                      )
      ),###Debería ser parecido para las demás. 
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
                                         choiceNames = c("Puentes y Túneles","Carreteras Federales", "Carreteras Estatales", "Carreteras Municipales"),
                                         choiceValues = paste0("g5_c", 1:4))),
      accordion_panel(title = "Otra Infraestructura", icon = icon("building"),
                      checkboxGroupInput(inputId = "capa_infra_g6", label = "Selecciona elementos:", 
                                         choiceNames = c("Estaciones eléctricas","Oficinas Nacionales","Oficinas Estatales","Oficinas Municipales","Oficinas Regionales"),
                                         choiceValues = paste0("g6_c", 1:5))),
      accordion_panel(title = "Puntos de Reunión y Centros de Acopio", icon = icon("handshake"),
                      checkboxGroupInput(inputId = "capa_poi_g7", label = "Selecciona elementos:", 
                                         choiceNames = c("Centros de Acopio","Albergues","Comedores Comunitarios","Escuelas","Universidades Estatales","Centros de Atención Infantil Comunitaria (CAIC)"),
                                         choiceValues = paste0("g7_c", 1:6)))
    )
  ),
  card(
    full_screen = TRUE, 
    card_header("Visualización Geográfica"),
    leafletOutput("mapa_principal", height = "80vh"),
    add_busy_spinner(spin = "fading-circle",position = "bottom-right",margins = c("5vh","5vw"))
  )
)


server <- function(input, output, session) {
  source("../../../Reutilizables/Postgres_BUIG/conexion_buig.R")
  
  session$onSessionEnded(function() {
    if (DBI::dbIsValid(buig)) {
      DBI::dbDisconnect(buig)
      message("Conexión a PostgreSQL cerrada exitosamente.")
    }
  })
  
  rv_config <- reactiveValues(CAPA_CONFIG_DATA = CAPA_CONFIG)
  
  rv_map_params <- reactiveValues(#Color defualt y nombre de capa
    g1_c1 = list(color = "black", size = 6, name = "Municipios"),
    g1_c2 = list(color = "black", size = 6, name = "AGEB"),
    g1_c3 = list(color = "black", size = 6, name = "Localidad Urbana"),
    
    g2_c1 = list(color = "red", size = 6, name = "Centros de Salud"),
    g2_c2 = list(color = "red", size = 6, name = "Hospital General"),
    g2_c3 = list(color = "red", size = 6, name = "Hospital regional")
    # Las demás
  )
  
  
  v <- reactiveValues(selected = character(0))
  
  output$mapa_principal <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(lng = -98, lat = 20, zoom = 6) |>  
      addLayersControl(
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  # Vector de capas seleccionadas
  selected_layers_vector <- reactive({
    selected_g1 <- c("g1_c1", "g1_c2", "g1_c3")[c(input$g1_c1_chk, input$g1_c2_chk, input$g1_c3_chk)]
    selected_g2 <- c("g2_c1", "g2_c2", "g2_c3")[c(input$g2_c1_chk, input$g2_c2_chk, input$g2_c3_chk)]
    c(selected_g1, selected_g2, input$capa_hid_g3, 
      input$capa_hid_g4, input$capa_vial_g5, input$capa_infra_g6, 
      input$capa_poi_g7) %>% unique()
  })
  
 
  #Ejemplo con g1_c2
  observeEvent(input$g1_c2_btn, {
    current_config <- isolate(rv_map_params$g1_c2)
    showModal(config_modal("g1_c2", current_config$name, current_config$color))
    #Inputs
    updateColourInput(session, "modal_g1_c2_col", value = current_config$color)
    updateSliderInput(session, "modal_g1_c2_size", value = current_config$size)
  })
  
  #Igual g1_c2
  observeEvent(input$modal_g1_c2_save, {
    rv_map_params$g1_c2$color <- input$modal_g1_c2_col
    rv_map_params$g1_c2$size <- input$modal_g1_c2_size
    removeModal()
  })
  ###Y sería uno para cada capa... 
  
  observe({
    
    print("Una llamada")
    current_layers <- selected_layers_vector()
    previous_layers <- v$selected
    
    proxy <- leafletProxy("mapa_principal", session)
    
    layers_to_add <- setdiff(current_layers, previous_layers)
    layers_to_remove <- setdiff(previous_layers, current_layers)
    
    for (layer_key in layers_to_remove) {
      proxy %>% clearGroup(group = layer_key)
    }
    successful_layers <- setdiff(previous_layers, layers_to_remove)
    
    for (layer_key in c(layers_to_add, successful_layers)) {
      config <- rv_config$CAPA_CONFIG_DATA[[layer_key]] 
      if (is.null(config)) next 
      layer_params <- rv_map_params[[layer_key]] 
      
      if (is.null(layer_params)) {
        # Configuración por defecto
        layer_color <- "#66A3D2" 
        layer_size <- 6
      } else {
        #Configuración para capas modales (g1, g2)
        layer_color <- layer_params$color
        layer_size <- layer_params$size 
      }
      
      # Si la capa ya existe y el color/tamaño cambió, la borramos y la volvemos a dibujar
      is_new_layer <- layer_key %in% layers_to_add
      if (layer_key %in% successful_layers && !is_new_layer) {
        proxy %>% clearGroup(group = layer_key)##Podria eficientarse para no tener que dibujar pero hay que ver las opciones de leaflet
      }
      
      #
      if (is_new_layer || layer_key %in% successful_layers) { 
        
        
        
        tryCatch({
          if (rv_config$CAPA_CONFIG_DATA[[layer_key]]$data |> is.null()) {
            data_sf <- load_layer_data(buig = buig,
                                       nombre_buig =  config$nombre_buig,
                                       columnas_interes = config$cols,
                                       custom_filter = ifelse(config$custom_filter |> is.null(),'',config$custom_filter))
            rv_config$CAPA_CONFIG_DATA[[layer_key]]$data <- data_sf
          } else {
            data_sf <- rv_config$CAPA_CONFIG_DATA[[layer_key]]$data 
          }
          
          geom_type <- as.character(unique(st_geometry_type(data_sf)))[1]
          
          if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
            proxy |> addPolygons(data = data_sf, fillColor = layer_color, color = "white", weight = 1, fillOpacity = 0.4, group = layer_key)
          } else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
            proxy |> addPolylines(data = data_sf, color = layer_color, weight = layer_size, opacity = 0.8, group = layer_key)
          } else if (geom_type %in% c("POINT", "MULTIPOINT")) {
            # Usamos layer_size como el radio (radius) para CircleMarkers
            proxy |> addCircleMarkers(data = data_sf, radius = layer_size, color = layer_color, fillOpacity = 0.9, group = layer_key)
          }
          
          successful_layers <- c(successful_layers, layer_key)
          
        }, error = function(e) {
          print(paste("ERROR:", e$message))
          showNotification(
            ui = HTML(paste("<strong>Error de Carga:</strong> No se pudo cargar la capa <b>", config$nombre_buig, "</b>.", 
                            "Por favor, verifique la conexión o el nombre de la tabla. ")),
            type = "error",
            duration = 2
          )
          parent_input_id <- find_parent_input_id(layer_key)
          if (!is.null(parent_input_id)) {
            current_selection_in_group <- input[[parent_input_id]]
            new_selection <- current_selection_in_group[current_selection_in_group != layer_key] 
            updateCheckboxGroupInput(session, parent_input_id, selected = new_selection)
          }
          
        })
      }
    }
    
    v$selected <- successful_layers %>% unique()
    
  }) # Fin del observe
}
shinyApp(ui, server)

