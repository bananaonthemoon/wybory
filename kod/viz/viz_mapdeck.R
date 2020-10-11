library(mapdeck)
library(tidyverse)
library(sf)
library(st)
library(tmaptools)


# WOJEWODZTWA -------------------------------------------------------------


key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

mapdeck(token = key)

prez_woj = "https://raw.github.com/bananaonthemoon/wybory/master/dane/powiaty.gpkg" %>%
  read_sf() %>%
  st_transform(crs = 4326)

map_woj = st_centroid(prez_woj)

mapdeck(token = key, style = mapdeck_style('light'),pitch = 45 ) %>%
  add_hexagon(
    data = map_woj,
    radius = 5000,
    elevation ="t1_Frekwencja",
    elevation_scale = 10
  )
