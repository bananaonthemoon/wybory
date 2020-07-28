library(mapdeck)
library(tidyverse)
library(sf)
library(st)
library(lwgeom)

key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

mapdeck(token = key)

prez_woj = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_woj.gpkg" %>%
  read_sf() %>%
  st_transform(crs = 4326)

url <- 'https://raw.githubusercontent.com/bananaonthemoon/wybory/master/dane/pobrane/tura1/wyniki_gl_na_kand_po_wojewodztwach_proc_utf8.csv'
dane_wybory <- read.csv2(url, encoding = "UTF-8")

mapdeck(token = key, style = mapdeck_style('dark')) %>%
  add_grid(
    data = prez_woj,
    layer_id = "grid_layer"
  )

tinytex::install_tinytex()
