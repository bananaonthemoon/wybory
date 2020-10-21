library(mapdeck)
library(tidyverse)
library(sf)
library(st)
library(tmaptools)
library(colourvalues)

#wizualizacja hex

# add_hexagon() -------------------------------------------------------------


# WOJEWODZTWA


key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

prez_woj = "https://raw.github.com/bananaonthemoon/wybory/master/dane/wojewodztwa.gpkg" %>%
  read_sf() %>%
  st_transform(crs = 4326)

hex_woj = st_centroid(prez_woj)
hex_woj$frek = prez_woj$t1_Frekwencja ^2 #nie ma sensu tutej używać

mapdeck(token = key, style = mapdeck_style('light'),pitch = 45 ) %>%
  add_hexagon(
    data = hex_woj,
    radius = 25000,
    elevation ="t1_Frekwencja",
    elevation_scale = 100,
    fill_colour = "t1_Frekwencja",
    palette = "oranges",
    colour_range = colour_values(1:6, palette = "orrd"), #kolor nie działa?
    legend = TRUE #value to same 111111
  )



# POWIATY


key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

prez_pow = "https://raw.github.com/bananaonthemoon/wybory/master/dane/powiaty.gpkg" %>%
  read_sf() %>%
  st_transform(crs = 4326)

hex_woj = st_centroid(prez_pow)
hex_woj$frek = prez_pow$t1_Frekwencja ^2 #nie ma sensu tutej używać

mapdeck(token = key, style = mapdeck_style('light'),pitch = 45 ) %>%
  add_hexagon(
    data = prez_pow,
    radius = 25000,
    elevation ="t1_Frekwencja",
    elevation_scale = 100,
    fill_colour = "t1_Frekwencja",
    palette = "oranges",
    colour_range = colour_values(1:6, palette = "orrd"), #kolor nie działa?
    legend = TRUE #value to same 111111
  )



# WIELKOPOLSKA


key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

prez_wlkp = "https://raw.github.com/bananaonthemoon/wybory/master/dane/wielkopolska.gpkg" %>%
  read_sf() %>%
  st_transform(crs = 4326)

hex_woj = st_centroid(prez_wlkp)
hex_woj$frek = prez_wlkp$t1_Frekwencja ^2 #nie ma sensu tutej używać

mapdeck(token = key, style = mapdeck_style('light'),pitch = 45 ) %>%
  add_hexagon(
    data = prez_wlkp,
    radius = 25000,
    elevation ="t1_Frekwencja",
    elevation_scale = 100,
    fill_colour = "t1_Frekwencja",
    palette = "oranges",
    colour_range = colour_values(1:6, palette = "orrd"), #kolor nie działa?
    legend = TRUE #value to same 111111
  )




#wizualizacja poly

# add_polygon() -----------------------------------------------------------


# WOJEWODZTWA 


key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

poly_woj = "https://raw.github.com/bananaonthemoon/wybory/master/dane/wojewodztwa.gpkg" %>%
  read_sf() %>%
  st_transform(crs = 4326)


poly_woj$frek = prez_woj$t1_Frekwencja ^2 #też nie idzie

mapdeck(token = key, style = mapdeck_style('light'),pitch = 45 ) %>%
  add_polygon(
    data = poly_woj,
    fill_colour = "t1_Frekwencja",
    elevation ="frek",
    elevation_scale = 100,
    palette = "oranges"
  )



# POWIATY 


key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

poly_pow = "https://raw.github.com/bananaonthemoon/wybory/master/powiaty.gpkg" %>%
  read_sf() %>%
  st_transform(crs = 4326)


poly_pow$frek = poly_pow$t1_Frekwencja ^2 #też nie idzie

mapdeck(token = key, style = mapdeck_style('light'),pitch = 45 ) %>%
  add_polygon(
    data = poly_pow,
    fill_colour = "t1_Frekwencja",
    elevation ="frek",
    elevation_scale = 100,
    palette = "oranges"
  )



# WIELKOPOLSKA 


key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

poly_wlkp = "https://raw.github.com/bananaonthemoon/wybory/master/wielkopolska.gpkg" %>%
  read_sf() %>%
  st_transform(crs = 4326)


poly_wlkp$frek = poly_wlkp$t1_Frekwencja ^2 #też nie idzie

mapdeck(token = key, style = mapdeck_style('light'),pitch = 45 ) %>%
  add_polygon(
    data = poly_wlkp,
    fill_colour = "t1_Frekwencja",
    elevation ="frek",
    elevation_scale = 100,
    palette = "oranges"
  )
