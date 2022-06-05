# Primero, definimos nuestro sf, el cual corresponde a las zonas urbanas de Chile

Urban <- sf::st_read(file.choose())

# Filtramos los datos de las comunas que no ocuparemos
filterurban <- dplyr::select(Urban, -COMUNA, -Shape_Leng, -Shape_Area, -GEOCODIGO, -PROVINCIA)

# Utilizaremos la distancia desde la municipalidad de Santa B?rbara hasta la laguna La Mula

SantaBarbara <- filterurban[filterurban$URBANO == "SANTA BÁRBARA",]
SantaBarbara <- na.omit(SantaBarbara)

plot(SantaBarbara$geometry)

# Ahora, cargamos la informaci?n de los cuerpos de agua de Chile

LagosChile <- sf::st_read(file.choose())

# Sacamos solamente las columnas que nos interesan

LagosChileFilter <- dplyr::select(LagosChile, nombre, tipo, geometry)

LagunaLaMula <- LagosChileFilter[LagosChileFilter$nombre=="Laguna La Mula",]
LagunaLaMula <- na.omit(LagunaLaMula)

plot(LagunaLaMula$geometry)

# Para el c?lculo de la accesibilidad, ocuparemos la librer?a OSRM
#install.packages('osrm')
library(osrm)
options(osrm.server = 'https://routing.openstreetmap.de/')
coord <- data.frame(lugar = c('Municipalidad de Santa B?rbara', 'Laguna La mUla'), lon = c(-72.0214, -71.3667), lat = c(-37.6706, -37.8833))
coord <- sf::st_as_sf(x = coord, coords = c('lon', 'lat'), crs = sf::st_crs(4326))

rutacar <- osrm::osrmRoute(src = coord[1,], dst = coord[2,], returnclass = 'sf', osrm.profile ='car')
rutapie <- osrm::osrmRoute(src = coord[1,], dst = coord[2,], returnclass = 'sf', osrm.profile = 'foot')


rutacar

# De esta manera podemos encontrar la duraci?n en minutos y la distancia en kil?metros

rutapie
# Y as? podemos encontrar la duraci?n a pie en minutos y distancia en kil?metros

library(mapview)
mapview::mapview(rutacar) + mapview(rutapie, color = 'red')

library(mapview)
mapview::mapview(SantaBarbara$geometry, col.regions = 'green') + mapview(LagunaLaMula$geometry) + mapview::mapview(rutacar, color = 'red') + mapview::mapview(rutapie)


# Ahora, veremos el google earth engine

pacman::p_load(raster, mapview, sf, rgdal)
#install.packages('rgee')

# Cargamos las librer?as de Google Drive y Google Earth Engine
library(googledrive)
library(rgee)
#ee_reattach() # reattach ee as a reserve word
# Initialize just Earth Engine
#ee_Initialize() 
#ee_Initialize(user = 'b.juanalvarez@gmail.com') # Use the argument email is not mandatory, but it's helpful to change of EE user.
# Initialize Earth Engine and GD
#ee_Initialize(user = 'b.juanalvarez@gmail.com', drive = TRUE)
# Initialize Earth Engine and GCS
#ee_Initialize(user = 'b.juanalvarez@gmail.com', gcs = TRUE)
# Initialize Earth Engine, GD and GCS
#ee_Initialize(user = 'b.juanalvarez@gmail.com', drive = TRUE, gcs = TRUE)


ee_Initialize('bastianjuan', drive = TRUE) 

# Seleccionamos el satélite LANDSAT 8, filtrando la data para el a?o 2020 de la Laguna La mula
coll <-ee$ImageCollection('LANDSAT/LC08/C01/T1_TOA')$
  filterDate('2019-04-01', '2020-06-30')$
  filterBounds(ee$Geometry$Point(-71.369629,-37.894510))$
  filterMetadata('CLOUD_COVER', 'less_than',10)

# Obtenemos la fecha exacta de la imagen captada por el sat?lite
ee_get_date_ic(coll)
L8 <- 'LANDSAT/LC08/C01/T1_TOA/LC08_233086_20200330' %>%
  ee$Image() %>%
  ee$Image$select(c('B6', 'B5', 'B4', 'B3'))

# Ploteamos el mapa de GGE
Map$centerObject(L8)
Map$addLayer(L8)

library(geojson)
roi <- 
  c(-71.369629, -37.894510) %>%  # Acceso a la Laguna La Mula.
  sf::st_point(dim = "XYZ") %>% 
  sf::st_buffer(dist = 0.2) %>% 
  sf_as_ee()

Map$centerObject(roi, zoom = 15)
Map$addLayer(roi)

getNDWI <- function(image) {
  image$normalizedDifference(c("B3", "B5"))
}

# Ahora visualizamos las capas por separado para poder analizar el cuerpo de agua
ndwi <- getNDWI(L8)
library(cptcity)
Map$centerObject(roi, zoom = 15)
Map$addLayer(ndwi, visParams = list(palette = cpt("ds9_rainbow", 10)))

# De esta manera, hemos terminado la visualización


