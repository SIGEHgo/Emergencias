
# Obtener las variables de entorno para la conexión
# db_host <- Sys.getenv("DB_HOST")
# db_user <- Sys.getenv("DB_USER")
# db_pass <- Sys.getenv("DB_PASS")
# db_port <- as.numeric(Sys.getenv("DB_PORT")) # Asegúrate de convertir el puerto a numérico
# db_name <- Sys.getenv("DB_NAME")

##Codigo para conectarnos a sql usando dplyr
library(DBI)
library(RPostgres)
buig <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname=db_name,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_pass
)
#DBI::dbExecute(buig, "SET search_path TO emergencia;")


Lista_BUIG=DBI::dbListTables(buig) |> as.list()

library(sf)

print("Ejemplo de uso: ")
print('st_read(buig,  "limite_municipal")')
print("Cuando te quieras salir puedes usar")
print("DBI::dbDisconnect(buig)")