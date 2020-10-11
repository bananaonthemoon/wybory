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