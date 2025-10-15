# app.R
#DBI::dbDisconnect(buig)
#source("../../../Reutilizables/Postgres_BUIG/conexion_buig.R")

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
  'g1_c1' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="limite_municipal_simple", cols=c("cvegeo", "nomgeo", "the_geom"), data = NULL,color = "black", size = 6, name = "Municipios"),  # Municipios
  'g1_c2' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="agebs_simple", cols=c("cve_ent", "cve_mun", "cve_loc", "cve_ageb", "pob1", "geom"), data = NULL,color = "black", size = 6, name = "AGEB"),  # AGEB
  'g1_c3' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="loc_urb_simple", cols=c("nomgeo", "cabecera", "cve_mun", "cve_loc", "pob1", "geom"), data = NULL,color = "black", size = 6, name = "Localidad Urbana"),  # Localidad Urbana
  'g1_c4' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="loc_rur_simple", cols=c("nomgeo", "nom_ent", "pob1", "geom"), data = NULL),  # Localidad Rural
  'g1_c5' = list(tipo_geom="POLYGON", group="Desagregación geográfica", nombre_buig="regiones_simple", cols=c("region", "the_geom"), data = NULL),  # Regiones
  
  ## Grupo 2: Capas de Salud
  'g2_c1' = list(tipo_geom="POINT", group="Infraestructura de Salud", nombre_buig='13salud', cols=c("nombre", "unidad", "admin","geom"), data = NULL), # Centros de Salud
  'g2_c2' = list(tipo_geom="POLYGON", group="Infraestructura de Salud", nombre_buig="hospitales_hgo_gral", cols=c("name", "area", "geom"), data = NULL),  # Hospital General
  'g2_c3' = list(tipo_geom="POLYGON", group="Infraestructura de Salud", nombre_buig="hospitales_hgo_reg", cols=c("name", "area", "geom"), data = NULL),  # Hospital regional
  
  ## Grupo 3: Recursos Hídricos
  'g3_c1' = list(tipo_geom="LINESTRING", group="Recursos Hídricos", nombre_buig="canales", cols=c("identifica", "condicion", "geom"), data = NULL),  # Canales
  'g3_c2' = list(tipo_geom="POINT", group="Recursos Hídricos", nombre_buig="Pozos_de_Estado_de_Hidalgo", cols=c("nombre","tipo", "geom"), data = NULL),  # Pozos
  'g3_c3' = list(tipo_geom="LINESTRING", group="Recursos Hídricos", nombre_buig="rios", cols=c("nombre", "condicion", "st_length_", "geom"), data = NULL), # Ríos
  'g3_c4' = list(tipo_geom="POINT", group="Recursos Hídricos", nombre_buig="manantiales_50_inegi", cols=c("nom_man", "geom"), data = NULL),  # Manantiales
  'g3_c5' = list(tipo_geom="POLYGON", group="Recursos Hídricos", nombre_buig="cuerpos_de_agua", cols=c("condicion", "shape_leng", "shape_area","geom"), data = NULL),  # Cuerpos de Agua
  'g3_c6' = list(tipo_geom="POINT", group="Recursos Hídricos", nombre_buig="Estructuras_elevadas", cols=c("geografico","tipo","geom"), data = NULL),  # Oficina
  
  ## Grupo 4: Zonificación de Vulnerabilidad
  # g4_c5 simula un error de carga para demostrar el manejo de excepciones.
  'g4_c1' = list(tipo_geom="POLYGON", group="Zonificación de Vulnerabilidad", nombre_buig="peligro_por_caida_de_bloques", cols=c("tipo","geom"), data = NULL),  # Caida de bloques
  'g4_c2' = list(tipo_geom="POLYGON", group="Zonificación de Vulnerabilidad", nombre_buig="zonificacion_de_vulnerabilidad_y_areas_susceptibles_a_deslizami", cols=c("tipo", "geom"), data = NULL),  # Suceptibles a deslizamiento
  'g4_c3' = list(tipo_geom="POLYGON", group="Zonificación de Vulnerabilidad", nombre_buig="zonificacion_de_vulnerabilidad_y_areas_susceptibles_a_hundimien", cols=c("peligro", "geom"), data = NULL),  # Hundimiento
  'g4_c4' = list(tipo_geom="LINESTRING", group="Zonificación de Vulnerabilidad", nombre_buig="estructuras_geologicas", cols=c("entidad", "geom"), data = NULL),  # Fallas geológicas
  'g4_c5' = list(tipo_geom="POLYGON", group="Zonificación de Vulnerabilidad", nombre_buig="inundacion", cols=c("objectid", "geom"), data = NULL), # Zonas de Inundación
  'g4_c6' = list(tipo_geom="POINT", group="Zonificación de Vulnerabilidad", nombre_buig="PRIORIDAD_ESCUELAS", cols=c('claveseph','Nombre_d_1','LOCALIDAD','MUNICIPIO','total', "geom"), data = NULL), # Escuelas en riesgo
  
  ## Grupo 5: Infraestructura Vial
  'g5_c1' = list(tipo_geom="POINT", group="Infraestructura Vial", nombre_buig="puentes_hgo", cols=c("tipo", "nombre", "altura", "ancho", "geom"), data = NULL),  # Estructuras Viales
  'g5_c2' = list(tipo_geom="LINESTRING", group="Infraestructura Vial", nombre_buig="red_carretera_sipdus_federal", cols=c("administra", "nombre", "cond_pav", "recubri", "carriles", "circula", "velocidad","geom"), data = NULL,custom_filter=c("administra",'Federal')), # Carreteras Federales
  'g5_c3' = list(tipo_geom="LINESTRING", group="Infraestructura Vial", nombre_buig="red_carretera_sipdus_estatal", cols=c("administra", "nombre", "cond_pav", "recubri", "carriles", "circula", "velocidad","geom"), data = NULL,custom_filter='Estatal'),  # Carreteras Estatales
  'g5_c4' = list(tipo_geom="LINESTRING", group="Infraestructura Vial", nombre_buig="red_carretera_sipdus_municipal", cols=c("administra", "nombre", "cond_pav", "recubri", "carriles", "circula", "velocidad","geom"), data = NULL,custom_filter='Municipal'),  # Carreteras Municipales

  ## Grupo 6: Otra Infraestructura
  'g6_c1' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="subestacion_electrica", cols=c("geografico", "nombre", "condicion",'geom'), data = NULL),  # Estaciones eléctricas
  'g6_c2' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="oficinas_nacionales", cols=c("nom_estab", "municipio","localidad", "geom"), data = NULL),  # Oficina
  'g6_c3' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="oficinas_estatales", cols=c("nom_estab", "geom"), data = NULL),  # Oficina
  'g6_c4' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="oficinas_municipales", cols=c("nom_estab", "municipio","localidad","geom"), data = NULL),  # Oficina
  'g6_c5' = list(tipo_geom="POINT", group="Otra Infraestructura", nombre_buig="oficinas_regionales", cols=c("nom_estab", "municipio","localidad", "geom"), data = NULL),  # Oficina
  
  ## Grupo 7: Puntos de Reunión y Centros de Acopio
  'g7_c1' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="centros_de_acopio", cols=c("nom_estab", "tipo", "geom"), data = NULL),  # Centros de Acopio
  'g7_c2' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="albergues_y_refugios", cols=c("nom_mun", "colonia", "nombre", "registr", "anio", "ambito","geom"), data = NULL), # Albergues
  'g7_c3' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="comedores_comunitarios_en_operacion", cols=c("tipo_educa", "nivel_educ", "nombre_d_4", "mun", "loc", "nom_plante", "nom_comedo", "geom"), data = NULL),  # Comedores Comunitarios
  'g7_c4' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="escuelas_seph_2025", cols=c("nom_centro", "nom_turno", "tipo", "nivel_educ", "servicio_e", "nom_mun", "nom_loc", "colonia","geom"), data = NULL),  # Escuelas
  'g7_c5' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="universidades_estatales", cols=c("name", "unv_inst", "geom"), data = NULL),  # Universidades Estatales
  'g7_c6' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="caics_2024", cols=c("name", "popupinfo", "geom"), data = NULL) , # Centros de Atención Infantil Comunitaria (CAIC)
  'g7_c7' = list(tipo_geom="POINT", group="Puntos de Reunión y Centros de Acopio", nombre_buig="tiendas_diconsa", cols=c("municipio", "localidad", 'direccion',"geom"), data = NULL)  # tiendas_diconsa
)
CAPA_CONFIG[['g5_c1']]$custom_filter

load_layer_data =function(buig, nombre_buig = "Estructuras_elevadas", columnas_interes = c("geografico", "tipo", "geom"), 
                          columna_filtrar = "", custom_filter = "") {
  
  datos = dplyr::tbl(buig, nombre_buig) |> dplyr::select(all_of(columnas_interes))
  if("the_geom"%in%columnas_interes){
    datos=datos |> dplyr::rename(geom=the_geom)
  }
  
  if (columna_filtrar != "" && custom_filter != "") {
    #datos = datos |> dplyr::filter(!!dplyr::sym(columna_filtrar) == custom_filter)
    datos = datos |> dplyr::filter(grepl(custom_filter, !!dplyr::sym(columna_filtrar)))
  }
  
  datos = datos |> dplyr::collect() |> dplyr::mutate(geom = sf::st_as_sfc(geom, EWKB = TRUE))
  
  coordenadas = sf::st_coordinates(datos$geom[1])[1,1]
  if (coordenadas > 30) {
    datos = datos |> sf::st_as_sf(crs = 32614) |> sf::st_transform(crs = 4326) |> sf::st_zm()
  } else {
    datos = datos |> sf::st_as_sf(crs = 4326) |> sf::st_zm()
  }
  
  return(datos)
}
#load_layer_data(buig = buig,nombre_buig = "red_carretera_sipdus",columnas_interes =c("administra", "nombre", "cond_pav", "recubri", "carriles", "circula", "velocidad","geom") ,custom_filter = "Federal")
layer_control_item <- function(layer_key, label_name) {
  # El ID para la casilla de verificación será "gX_cY_chk"
  checkbox_id <- paste0(layer_key, "_chk")
  # El ID para el botón de acción será "gX_cY_btn"
  button_id <- paste0(layer_key, "_btn")
  
  div(class = "d-flex justify-content-between align-items-center mb-2", 
      # Casilla de verificación
      checkboxInput(checkbox_id, label_name, value = FALSE),
      # Botón de configuración
      actionButton(button_id, label = NULL, icon = icon("gear"), 
                   class = "btn-xs btn-default", 
                   style = "padding: 5px; height: 30px;")
  )
}
config_modal <- function(layer_key, layer_name, initial_color) {
  modalDialog(
    title = paste("Configuración de", layer_name),
    size = "s", # Modal pequeño
    
    #Selector de Color
    colourpicker::colourInput(
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
popup_general = function(datos_sf = datos_sf) {
  
  columnas = names(datos_sf)
  # Filtramos columnas de geometría
  columnas = columnas[columnas != "geom"] 
  columnas = columnas[columnas != "geometry"] 
  
  if (length(columnas) == 0) {
    return(rep(htmltools::HTML("<b>Sin datos disponibles</b>"), nrow(datos_sf)))
  }
  
  popup = apply(
    # 🚨 CORRECCIÓN CLAVE: Usar drop = FALSE para mantener la estructura de data frame
    sf::st_drop_geometry(datos_sf)[, columnas, drop = FALSE], 
    1, # Aplicar por fila
    function(fila) {
      # Nota: 'fila' será un vector, incluso si solo hay una columna
      campos =  paste0("<b>", columnas, ":</b> ", fila, collapse = "<br>")
      return(htmltools::HTML(campos))
    }
  )
  return(popup)
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
      
      # G1: Desagregación geográfica
      accordion_panel(title = "Desagregación geográfica", icon = icon("map"), 
                      h5("Selecciona elementos:"),
                      
                      layer_control_item("g1_c1", "Municipios"),
                      layer_control_item("g1_c2", "AGEB"),
                      layer_control_item("g1_c3", "Localidad Urbana"),
                      layer_control_item("g1_c4", "Localidad Rural"),
                      layer_control_item("g1_c5", "Regiones")
      ),
      
      # G2: Infraestructura de Salud
      accordion_panel(title = "Infraestructura de Salud", icon = icon("hospital"), 
                      h5("Selecciona elementos:"),
                      layer_control_item("g2_c1", "Centros de Salud"),
                      layer_control_item("g2_c2", "Hospital General"),
                      layer_control_item("g2_c3", "Hospital Regional")
      ),
      
      # G3: Recursos Hídricos y Cuencas
      accordion_panel(title = "Recursos Hídricos", icon = icon("water"),
                      h5("Selecciona elementos:"),
                      layer_control_item("g3_c1", "Canales"),
                      layer_control_item("g3_c2", "Pozos"),
                      layer_control_item("g3_c3", "Ríos"),
                      layer_control_item("g3_c4", "Manantiales"),
                      layer_control_item("g3_c5", "Cuerpos de Agua"),
                      layer_control_item("g3_c6", "Estructuras Elevadas"),
      ),
      
      # G4: Zonificación de Vulnerabilidad
      accordion_panel(title = "Zonificación de Vulnerabilidad", icon = icon("triangle-exclamation"), 
                      h5("Selecciona elementos:"),
                      layer_control_item("g4_c1", "Caída de bloques"),
                      layer_control_item("g4_c2", "Susceptibles a deslizamiento"),
                      layer_control_item("g4_c3", "Hundimiento"),
                      layer_control_item("g4_c4", "Fallas geológicas"),
                      layer_control_item("g4_c5", "Zonas de Inundación"),
                      layer_control_item("g4_c6", "Escuelas prioritarias por riesgos")
      ),
      
      # G5: Infraestructura Vial
      accordion_panel(title = "Infraestructura Vial", icon = icon("road"),
                      h5("Selecciona elementos:"),
                      layer_control_item("g5_c1", "Puentes y Túneles"),
                      layer_control_item("g5_c2", "Carreteras Federales"),
                      layer_control_item("g5_c3", "Carreteras Estatales"),
                      layer_control_item("g5_c4", "Carreteras Municipales")
      ),
      
      # G6: Otra Infraestructura
      accordion_panel(title = "Otra Infraestructura", icon = icon("building"),
                      h5("Selecciona elementos:"),
                      layer_control_item("g6_c1", "Estaciones eléctricas"),
                      layer_control_item("g6_c2", "Oficinas Nacionales"),
                      layer_control_item("g6_c3", "Oficinas Estatales"),
                      layer_control_item("g6_c4", "Oficinas Municipales"),
                      layer_control_item("g6_c5", "Oficinas Regionales")
      ),
      
      # G7: Puntos de Reunión y Centros de Acopio
      accordion_panel(title = "Puntos de Reunión y Centros de Acopio", icon = icon("handshake"),
                      h5("Selecciona elementos:"),
                      layer_control_item("g7_c1", "Centros de Acopio"),
                      layer_control_item("g7_c2", "Albergues"),
                      layer_control_item("g7_c3", "Comedores Comunitarios"),
                      layer_control_item("g7_c4", "Escuelas"),
                      layer_control_item("g7_c5", "Universidades Estatales"),
                      layer_control_item("g7_c6", "Centros de Atención Infantil Comunitaria (CAIC)"),
                      layer_control_item("g7_c7", "Tiendas Diconsa")
      )
    )
  ),
  card(
    full_screen = TRUE, 
    card_header("Visualización Geográfica"),
    leafletOutput("mapa_principal", height = "80vh"),
    add_busy_spinner(spin = "fading-circle",position = "bottom-right",margins = c("5vh","5vw"))
  )
)
#source("Scripts/db_con.R")

server <- function(input, output, session) {
  #source("../../../Reutilizables/Postgres_BUIG/conexion_buig.R")
  
  # 2. Cerrar la conexión cuando la sesión termina
  session$onSessionEnded(function() {
    if (DBI::dbIsValid(buig)) {
      DBI::dbDisconnect(buig)
      message("Conexión a PostgreSQL cerrada exitosamente.")
    }
  })
  rv_config <- reactiveValues(CAPA_CONFIG_DATA = CAPA_CONFIG)

  INPUT_TO_KEY_MAP <- list(
    capa_base_g1 = paste0("g1_c", 1:5),
    capa_dem_g2 = paste0("g2_c", 1:3),
    capa_hid_g3 = paste0("g3_c", 1:5),
    capa_hid_g4 = paste0("g4_c", 1:5),
    capa_vial_g5 = paste0("g5_c", 1:4),
    capa_infra_g6 = paste0("g6_c", 1:6),
    capa_poi_g7 = paste0("g7_c", 1:7)
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
  ALL_LAYER_KEYS <- names(CAPA_CONFIG) 
  
  selected_layers_vector <- reactive({
    active_layers <- character(0)
    
    # ITERAMOS SOBRE TODOS LOS CHECKBOXES INDIVIDUALES (gX_cY_chk)
    for (layer_key in ALL_LAYER_KEYS) {
      checkbox_id <- paste0(layer_key, "_chk")
      
      # input[[checkbox_id]] es cómo se accede dinámicamente
      if (isTRUE(input[[checkbox_id]])) {
        active_layers <- c(active_layers, layer_key)
      }
    }
    return(active_layers)
  })
  # Almacena el estado de las capas activas del ciclo anterior
  v <- reactiveValues(selected = character(0))
  
  # Inicialización del Mapa Base
  output$mapa_principal <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(lng = -98, lat = 20, zoom = 6) |>  
      addLayersControl(
        options = layersControlOptions(collapsed = FALSE)
      ) #|> addDrawToolbar()
  })
  
  # Vector de capas seleccionadas 
  # selected_layers_vector <- reactive({
  #   c(input$capa_base_g1, input$capa_dem_g2, input$capa_hid_g3, 
  #     input$capa_hid_g4, input$capa_vial_g5, input$capa_infra_g6, 
  #     input$capa_poi_g7)
  # })
  
  
  # observeEvent(input$mapa_principal_draw_new_feature, {##Pendiente hacer algo con el dibujo.
  #   print("El usuario hizo un dibujito")
  #   print(input$mapa_principal_draw_new_feature)
  #   print(class(input$mapa_principal_draw_new_feature))
  # })
  rv_map_params <- rv_config
  ALL_BUTTON_KEYS <- names(CAPA_CONFIG)  # Lista de todas las capas
  ALL_BUTTON_INPUTS <- paste0(ALL_BUTTON_KEYS, "_btn") # Lista de IDs de input
  ALL_SAVE_BUTTONS=paste0("modal_",ALL_BUTTON_KEYS,"_save")
  #Igual g1_c2
  observeEvent(input$modal_g1_c2_save, {
    rv_map_params$g1_c2$color <- input$modal_g1_c2_col
    rv_map_params$g1_c2$size <- input$modal_g1_c2_size
    removeModal()
  })
  rv_click_state <- reactiveValues()
  rv_save_state <- reactiveValues()
  
  # Inicializar con 0s
  for (btn_id in ALL_BUTTON_INPUTS) {
    rv_click_state[[btn_id]] <- 0 
  }
  for (btn_id in ALL_SAVE_BUTTONS) {
    rv_save_state[[btn_id]] <- 0 
  }
  observeEvent({
    # Escuchar a todos los botones dinámicamente
    sapply(ALL_BUTTON_INPUTS, function(id) input[[id]]) 
  }, {
    culprit_key <- NULL
    
    # Iterar sobre todos los botones para encontrar el que aumentó su contador
    for (layer_key in ALL_BUTTON_KEYS) {
      btn_id <- paste0(layer_key, "_btn")
      
      # El valor actual del contador de clics
      current_click_count <- input[[btn_id]]
      
      # El valor anterior del contador de clics
      previous_click_count <- isolate(rv_click_state[[btn_id]])
      
      # Si el contador aumentó en uno, este es el botón "culpable"
      if (current_click_count > previous_click_count) {
        culprit_key <- layer_key
        
        # 🚨 Actualizar el estado anterior para el próximo ciclo
        rv_click_state[[btn_id]] <- current_click_count 
        break # Encontramos al culpable, salimos del bucle
      }
    }
    
    # 3. Lógica principal con la clave identificada
    if (!is.null(culprit_key)) {
      current_config <- isolate(rv_map_params[[culprit_key]])
      
      # Usar la clave dinámica aquí
      showModal(config_modal(culprit_key, current_config$name, current_config$color))
      
      # Actualizar los inputs del modal dinámicamente
      colourpicker::updateColourInput(session, paste0("modal_", culprit_key, "_col"), value = current_config$color)
      updateSliderInput(session, paste0("modal_", culprit_key, "_size"), value = current_config$size)
      
    }
  }, ignoreInit = TRUE)
  observeEvent({
    # Escucha a todos los botones Save
    sapply(ALL_SAVE_BUTTONS, function(id) input[[id]])  
  }, {
    culprit_key <- NULL
    
    # 1. Identificar qué botón Save fue presionado
    for (layer_key in ALL_BUTTON_KEYS) {
      
      # 🚨 CORRECCIÓN 1: Definir el ID correcto del botón de Guardar
      save_btn_id <- paste0("modal_", layer_key, "_save") 
      
      # 🚨 CORRECCIÓN 2: Usar ifelse para manejar el valor NULL inicial
      # Si input[[ID]] es NULL, asumimos que el contador es 0 (no se ha presionado).
      current_save_count <- ifelse(is.null(input[[save_btn_id]]), 
                                   0, 
                                   input[[save_btn_id]])
      
      previous_save_count <- isolate(rv_save_state[[save_btn_id]])
      
      # Ahora la comparación funciona, ya que current_save_count es 0, no NULL.
      if (current_save_count > previous_save_count) { 
        culprit_key <- layer_key
        
        # Actualizamos el estado anterior con el valor que DISPARÓ el evento.
        rv_save_state[[save_btn_id]] <- current_save_count
        break
      }
    }
    
    # 2. Ejecutar la lógica de guardar si se encontró al culpable
    if (!is.null(culprit_key)) {
      modal_col_id <- paste0("modal_", culprit_key, "_col")
      modal_size_id <- paste0("modal_", culprit_key, "_size")
      
      # Es seguro acceder a estos inputs ya que el modal estaba abierto 
      # (y por lo tanto estos inputs deben tener valores)
      rv_map_params[[culprit_key]]$color <- input[[modal_col_id]]
      rv_map_params[[culprit_key]]$size <- input[[modal_size_id]]
      
      removeModal()
      
      ##Y actualizamos el leafletproxy. No hay que usar más informacion, solo borramos la capa, y la agregamos con el nuevo color
      #culprit_key
      group=find_parent_input_id(culprit_key)
      #Removegroup
      proxy = leafletProxy("mapa_principal", session)
      proxy |>  clearGroup(group = culprit_key)
      #Agregamos grupo
      data_para_agregar=rv_config$CAPA_CONFIG_DATA[[culprit_key]]$data
      geom_type <- as.character(unique(st_geometry_type(data_para_agregar)))[1]

      if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
        proxy |> addPolygons(data = data_para_agregar, fillColor =input[[modal_col_id]], color = "black",weight=input[[modal_size_id]], fillOpacity = 0.4, group = layer_key,popup=popup_general(data_para_agregar) |> as.character())
      } else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
        proxy |> addPolylines(data = data_para_agregar, color = input[[modal_col_id]],weight=input[[modal_size_id]], opacity = 0.8, group = layer_key,popup=popup_general(data_para_agregar)|> as.character())
      } else if (geom_type %in% c("POINT", "MULTIPOINT")) {
        proxy |> addCircleMarkers(data = data_para_agregar, radius = input[[modal_size_id]], color = input[[modal_col_id]], fillOpacity = 0.9, group = layer_key,popup=popup_general(data_para_agregar)|> as.character()) 
      }
    }
  }, ignoreInit = TRUE)
  
  
  
  
  observe({
    current_layers <- selected_layers_vector()
    previous_layers <- v$selected
    
    proxy = leafletProxy("mapa_principal", session)
    
    layers_to_add = setdiff(current_layers, previous_layers)
    layers_to_remove = setdiff(previous_layers, current_layers)
    print(layers_to_remove)
    for (layer_key in layers_to_remove) {
      proxy |>  clearGroup(group = layer_key)
    }
    successful_layers = setdiff(previous_layers, layers_to_remove)
    
    for (layer_key in layers_to_add) {
      #print(rv_config$CAPA_CONFIG_DATA[[layer_key]])
      config <- rv_config$CAPA_CONFIG_DATA[[layer_key]] 
      if (is.null(config)) next 
      tryCatch({
        #load_layer_data(layer_key)
        if (rv_config$CAPA_CONFIG_DATA[[layer_key]]$data |> is.null()) {
          print("Se lee desde el buig")
          print(config$nombre_buig)
          data_sf <-st_read(paste0("Inputs/",config$nombre_buig,".geojson")) |> dplyr::filter(!st_is_empty(geometry)) #load_layer_data(buig = buig,
                      #               nombre_buig =  config$nombre_buig,
                       #              columnas_interes = config$cols,
                        #             custom_filter = ifelse(config$custom_filter |> is.null(),'',config$custom_filter))###Aqui se ve a cambiar por la función custom de dplyr.
          rv_config$CAPA_CONFIG_DATA[[layer_key]]$data <- data_sf
          
        } else {
          print("Se lee desde local ")
          Sys.sleep(1)
          data_sf <- rv_config$CAPA_CONFIG_DATA[[layer_key]]$data 
        }
        
        geom_type <- as.character(unique(st_geometry_type(data_sf)))[1]
        

        if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
          proxy |> addPolygons(data = data_sf, fillColor = "#66A3D2", color = "black", weight = 1, fillOpacity = 0.4, group = layer_key,popup=popup_general(data_sf)|> as.character())
        } else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
          proxy |> addPolylines(data = data_sf, color = "black", weight = 3, opacity = 0.8, group = layer_key,popup=popup_general(data_sf)|> as.character())
        } else if (geom_type %in% c("POINT", "MULTIPOINT")) {
          proxy |> addCircleMarkers(data = data_sf, radius = 6, color = "red", fillOpacity = 0.9, group = layer_key,popup=popup_general(data_sf)|> as.character())
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
    
    v$selected <- successful_layers %>% unique()
    
  }) # Fin del observe
}

shinyApp(ui, server)

