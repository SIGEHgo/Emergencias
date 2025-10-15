# vialidades=st_read(buig,"13vialidades")
# carreteras=st_read(buig,"13carreteras")
red_carr_sipdus=st_read(buig,"red_carretera_sipdus")
estr_viales=st_read(buig,"estructuras_viales")##Se utiliza tal cual


red_carr_sipdus |> dplyr::select(tipo_vial,cond_pav,recubri,carriles,estatus,condicion,peaje,administra,circula,velocidad,longitud) |> 
  st_drop_geometry() |> lapply(table)

z=dplyr::tbl(buig,"red_carretera_sipdus")
z |> dplyr::filter(administra=='Federal') |> dplyr::collect() |> st_as_sf()|>  st_geometry() |> plot()


hospitales=st_read(buig,)

hhgo= st_read("../Inputs/hospitales_hgo.geojson")
hhgo |> dplyr::filter(grepl(pattern = "Regional",name)) |> st_zm() |> st_write("../Inputs/hospitales_hgo_reg.geojson",driver = "GeoJSON")
hhgo |> dplyr::filter(!grepl(pattern = "Regional",name))|> st_zm()  |> st_write("../Inputs/hospitales_hgo_gral.geojson",driver = "GeoJSON")



hhgo= st_read("../Inputs/red_carretera_sipdus.geojson")
hhgo |> dplyr::filter(administra=='Federal') |> st_write("../Inputs/red_carretera_sipdus_federal.geojson",driver = "GeoJSON")
hhgo |> dplyr::filter(administra=='Estatal') |> st_write("../Inputs/red_carretera_sipdus_estatal.geojson",driver = "GeoJSON")
hhgo |> dplyr::filter(administra=='Municipal') |> st_write("../Inputs/red_carretera_sipdus_municipal.geojson",driver = "GeoJSON")
hhgo |> dplyr::filter(administra=='Federal') |> st_write("../Inputs/red_carretera_sipdus_fed.geojson",driver = "GeoJSON")

library(sf)
puentes=st_read("../Inputs/puentes_hgo.geojson")



##Del buig, pozos_estado_de_hidalgo
st_read(buig,"Pozos_de_Estado_de_Hidalgo") |> dplyr::select(nombre,tipo) |> st_write("../Inputs/Pozos_de_Estado_de_Hidalgo.geojson",driver='GeoJSON')
pozos_con_nivel_piezometrico
#reemplazar pozos











##Escuelas de prioridad 
escuelas_en_riesgo=st_read(buig,Lista_BUIG[[45]])
escuelas_en_riesgo |> 
  dplyr::filter(total>mean(total)) |> dplyr::select(claveseph,Nombre_d_1,LOCALIDAD,MUNICIPIO,total,geom) |> 
  dplyr::rename(riesgo_prioridad=total) |> st_zm() |> st_write("../Inputs/PRIORIDAD_ESCUELAS.geojson",driver = "GeoJSON")
