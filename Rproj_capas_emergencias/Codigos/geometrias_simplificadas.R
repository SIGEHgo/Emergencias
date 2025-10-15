"../../../Reutilizables/Postgres_BUIG/conexion_buig.R" |> source()



library(sf)
library(leaflet)
library(leafem)
library(leaflegend)
library(leaflet.extras)
library(leaflet.extras2)
#Municipios
municipios=st_read(buig,"limite_municipal")
municipios=municipios |> dplyr::select(cvegeo,nomgeo,the_geom) |> st_simplify(preserveTopology = T,dTolerance = 200)
##Le ponemos info demográfica. 
municipios=merge(x=municipios,y=st_read(buig, "poblacion_scince_2020municipal") |> dplyr::select(cvegeo,pob1,geom) |> st_drop_geometry(),
      by='cvegeo',all.x=T) |> 
  merge(st_read(buig, "vivienda_scince_2020municipal") |> dplyr::select(cvegeo,viv1,geom) |> st_drop_geometry(),by='cvegeo',all.x=)

municipios |> st_write("../Inputs/limite_municipal_simple.geojson",driver = "GeoJSON")
#AGEBs
agebs_or=st_read(buig,"poblacion_scince_2020ageb")
agebs_simple=agebs_or |> dplyr::select(cvegeo,pob1,geom) |> st_simplify(preserveTopology = T,dTolerance = 20)
agebs=agebs_simple |> 
  dplyr::filter(st_geometry_type(geom)=='GEOMETRYCOLLECTION')
agebs=agebs |>st_drop_geometry() |> merge(agebs_or |> dplyr::select(cvegeo,geom),by='cvegeo',all.x=T)
agebs_simple=agebs_simple |> 
  dplyr::filter(st_geometry_type(geom)!='GEOMETRYCOLLECTION')
agebs_simple=rbind(agebs_simple,agebs |> st_as_sf())

#agebs |> st_simplify(preserveTopology = T,dTolerance = 100)|> st_write("../Inputs/agebs_100.geojson",driver='GeoJSON')

#leaflet() |> addTiles() |> addPolygons(data=agebs_simple )

#agebs_or |> dplyr::select(cvegeo,geom) |> st_write("../Inputs/agebs_or_or.geojson",driver='GeoJSON')
#agebs |> st_simplify(preserveTopology = T,dTolerance = 100)


##Le ponemos info demográfica. 


agebs_simple=merge(x=agebs_simple,y=st_read(buig, "vivienda_scince_2020ageb") |> dplyr::select(cvegeo,viv1,geom) |> st_drop_geometry(),
                 by='cvegeo',all.x=T)

agebs_simple |> st_write("../Inputs/agebs_simple.geojson",driver='GeoJSON')


#Localidades
loc_pob_rur=st_read(buig,"poblacion_scince_2020localidad rural") |> 
  dplyr::select(cvegeo,nomgeo,cve_mun,pob1,geom) |> 
  dplyr::mutate(ambito='Rural') |> 
  dplyr::relocate(ambito,.before = geom) |> 
  merge(municipios |> dplyr::select(cvegeo,nomgeo) |> 
          dplyr::rename(nom_mun=nomgeo) |> 
          dplyr::mutate(cvegeo=substr(cvegeo,3,5))|> 
          st_drop_geometry(),
        by.x='cve_mun',by.y='cvegeo') 

loc_pob_urb=st_read(buig,"poblacion_scince_2020localidad_urbana")|> 
  dplyr::select(cvegeo,nomgeo,cve_mun,pob1,ambito,geom) |> 
  merge(municipios |> dplyr::select(cvegeo,nomgeo) |> 
          dplyr::rename(nom_mun=nomgeo) |> 
          dplyr::mutate(cvegeo=substr(cvegeo,3,5))|> 
          st_drop_geometry(),
        by.x='cve_mun',by.y='cvegeo') 

loc_viv_rur=st_read(buig,"vivienda_scince_2020localidadrural") |> 
  dplyr::select(cvegeo,nomgeo,cve_mun,viv1,geom) |> 
  merge(municipios |> dplyr::select(cvegeo,nomgeo) |> 
          dplyr::rename(nom_mun=nomgeo) |> 
          dplyr::mutate(cvegeo=substr(cvegeo,3,5))|> 
          st_drop_geometry(),
        by.x='cve_mun',by.y='cvegeo') |> 
  dplyr::mutate(ambito='Rural') |> 
  dplyr::relocate(ambito,.before = geom)

loc_viv_urb=st_read(buig,"vivienda_scince_2020localidadurbana") |> 
  dplyr::select(cvegeo,nomgeo,cve_mun,viv1,ambito,geom) |> 
  merge(municipios |> dplyr::select(cvegeo,nomgeo) |> 
          dplyr::rename(nom_mun=nomgeo) |> 
          dplyr::mutate(cvegeo=substr(cvegeo,3,5))|> 
          st_drop_geometry(),
        by.x='cve_mun',by.y='cvegeo') 


loc_urb=merge(loc_pob_urb |> dplyr::select(cvegeo,pob1) |> st_drop_geometry(),loc_viv_urb,by='cvegeo')
loc_rur=merge(loc_pob_rur |> dplyr::select(cvegeo,pob1) |> st_drop_geometry(),loc_viv_rur,by='cvegeo')
leaflet() |> addTiles() |> addPolygons(data=loc_urb |> st_as_sf()|> st_simplify(preserveTopology = T,dTolerance = 20))
#loc_urb |> st_write("../Inputs/loc_urb_or.geojson",driver='GeoJSON')
loc_urb |> st_as_sf() |> st_simplify(preserveTopology = T,dTolerance = 20) |> st_write("../Inputs/loc_urb_simple.geojson",driver='GeoJSON')
loc_rur |> st_as_sf() |> st_write("../Inputs/loc_rur_simple.geojson",driver='GeoJSON')
#loc_rur |> st_as_sf() |> st_write("../Inputs/loc_rur.geojson",driver='GeoJSON')

#Regiones

"../../../Reutilizables/regiones/Regiones_FINAL.shp" |> read_sf()->regiones
regiones$geometry |> plot()
regiones |> st_write("../Inputs/regiones_or.geojson",driver = "GeoJSON")
regiones |> st_as_sf() |> st_transform(st_crs("EPSG:4326")) |> st_simplify(preserveTopology = T,dTolerance = 200) |> st_write("../Inputs/regiones_simple.geojson",driver = "GeoJSON")

leaflet() |> addTiles() |> addPolygons(data=regiones |> st_as_sf()|> st_transform(st_crs("EPSG:4326")))
leaflet() |> addTiles() |> addPolygons(data=regiones |> st_as_sf() |> st_transform(st_crs("EPSG:4326"))|> st_simplify(preserveTopology = T,dTolerance = 200))



#puentes
st_read(buig,"estructuras_viales") |> st_join(y = regiones |> st_transform(st_crs("EPSG:4326")) ,join = st_intersects) |> dplyr::filter(!is.na(region)) |> 
  dplyr::select(tipo,nombre,altura,ancho,geom) |> st_as_sf() |> st_write("../Inputs/puentes_hgo.geojson",driver='GeoJSON')
