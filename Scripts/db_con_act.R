

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
lista_interes = lapply(CAPA_CONFIG, function(x) x[["nombre_buig"]]) |>  unlist() |>  as.character()
Lista_BUIG = Lista_BUIG[Lista_BUIG %in% lista_interes]



