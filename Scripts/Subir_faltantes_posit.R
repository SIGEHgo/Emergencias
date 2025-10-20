faltantes = lista_interes[!lista_interes %in% Lista_BUIG]

archivos = list.files(path = "../Inputs/", pattern = "\\.geojson", all.files = T, full.names = T, recursive = T)
nombres = basename(archivos) |>  gsub(pattern = ".geojson", replacement = "") |>  stringr::str_squish()


archivos = archivos[which(nombres %in% faltantes) ]
nombres = nombres[which(nombres %in% faltantes)]


for (i in seq_along(nombres)) {
  datos = sf::read_sf(archivos[i])
  sf::st_write(
    obj = datos,
    dsn = buig,
    layer = DBI::Id(schema = "public", table = nombres[i]),
    append = FALSE  # FALSE = crea una tabla nueva
  )
  cat("Se acaba de finalizar:", nombres[i], "\n")
}
