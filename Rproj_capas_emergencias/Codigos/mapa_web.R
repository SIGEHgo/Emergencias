"../../../Reutilizables/Postgres_BUIG/conexion_buig.R" |> source()



library(sf)
library(leaflet)
library(leafem)
library(leaflegend)
library(leaflet.extras)
library(leaflet.extras2)

municipios=st_read(buig,"limite_municipal")
municipios=municipios |> dplyr::select(nomgeo,the_geom) |> st_simplify(preserveTopology = T,dTolerance = 200)
# municipios |> st_simplify(preserveTopology = T,dTolerance = 350) |> st_write("../Nueva carpeta/municipios_350.geojson",driver = "GeoJSON")
# municipios |> st_simplify(preserveTopology = T,dTolerance = 350) |> st_geometry() |> plot()

escuelas=read_sf("../Inputs/ESCUELAS SEPH 2025.shp")
escuelas

leaflet() |> 
  addTiles() |> 
  addPolygons(data=municipios,color = "white",stroke = 1,fillColor = "lightgray",fillOpacity = 0.5 ,group = "Municipios") |> 

  addLayersControl(overlayGroups = )
  