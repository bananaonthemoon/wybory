library(mapdeck)
library(tidyverse)
library(sf)
library(st)
library(tmaptools)
library(colourvalues)

key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

mapdeck(token = key)

paleta_red = colour_values(1:5, palette = "reds")

prez_woj = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_powiaty.geojson" %>%
  read_sf() %>%
  st_transform(crs = 4326)

prez_obreby = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_gminy.geojson"

map_woj = st_centroid(prez_obreby)
prez_obreby
mapdeck(token = key, style = mapdeck_style('dark'),pitch = 45 ) %>%
  add_hexagon(
    data = map_woj,
    elevation ="t1_Frekwencja",
    elevation_scale = 10
  )
str(map_woj)

map_woj = st_centroid(prez_woj)
map_woj


library(reprex)

reprex(venue = "html")

prez_woj$A = prez_woj$t1_Frekwencja ^2
mapdeck(token = key, style = mapdeck_style('dark'),pitch = 45 ) %>%
  add_polygon(
    data = prez_woj,
    elevation ="A",
    elevation_scale = 10,
    fill_colour = "t1_Frekwencja"
  )
