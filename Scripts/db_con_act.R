

# # Obtener las variables de entorno para la conexión
db_host <- Sys.getenv("db_host")
db_user <- Sys.getenv("db_user")
db_pass <- Sys.getenv("db_pass")
db_port <- as.numeric(Sys.getenv("db_port")) # Asegúrate de convertir el puerto a numérico
db_name <- Sys.getenv("db_name")

library(pool)
library(dbplyr)
library(shiny)
library(dplyr)





buig <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = db_name,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_pass
)

onStop(function() {
  print("Desconexion del servidor")
  pool::poolClose(buig)
  print(buig)
})


#pool::dbExecute(con, "set search_path to descancito;")


Lista_BUIG = pool::dbListTables(buig) |> as.list()
lista_interes = c(
  "afectaciones_metztitlan_puntos",
  "afectaciones_metztitlan_lineas",
  "limite_municipal_simple",
  "agebs_simple",
  "loc_urb_simple",
  "loc_rur_simple",
  "regiones_simple",
  "13salud",
  "hospitales_hgo_gral",
  "hospitales_hgo_reg",
  "canales",
  "Pozos_de_Estado_de_Hidalgo",
  "rios",
  "manantiales_50_inegi",
  "cuerpos_de_agua",
  "Estructuras_elevadas",
  "peligro_por_caida_de_bloques",
  "zonificacion_de_vulnerabilidad_y_areas_susceptibles_a_deslizami",
  "zonificacion_de_vulnerabilidad_y_areas_susceptibles_a_hundimien",
  "estructuras_geologicas",
  "inundacion",
  "PRIORIDAD_ESCUELAS",
  "puentes_hgo",
  "red_carretera_sipdus_federal",
  "red_carretera_sipdus_estatal",
  "red_carretera_sipdus_municipal",
  "subestacion_electrica",
  "oficinas_nacionales",
  "oficinas_estatales",
  "oficinas_municipales",
  "oficinas_regionales",
  "centros_de_acopio",
  "albergues_y_refugios",
  "comedores_comunitarios_en_operacion",
  "escuelas_seph_2025",
  "universidades_estatales",
  "caics_2024",
  "tiendas_diconsa"
)

Lista_BUIG = Lista_BUIG[Lista_BUIG %in% lista_interes]



