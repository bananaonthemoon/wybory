library(cartogram)
library(tmap)
library(tmaptools)
library(tidyverse)
library(sf)

prez = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_powiaty.gpkg"

prez_woj = read_sf(prez, stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180)

download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_map_subunits.zip", "dane/temp/ne_10m_admin_0_map_subunits.zip")
unzip("dane/temp/ne_10m_admin_0_map_subunits.zip", exdir = "dane/pobrane")

sasiad = c("Germany", "Czechia", "Slovakia", "Ukraine", "Belarus", "Lithuania", "Kalinigrad", "Austria")

bord = read_sf("dane/pobrane/ne_10m_admin_0_map_subunits.shp") %>%
  filter(SUBUNIT %in% sasiad) %>%
  select(NAME, POSTAL) %>%
  st_transform(crs = 2180)

granica = read_sf("dane/pobrane/ne_10m_admin_0_map_subunits.shp") %>%
  filter(SUBUNIT == "Poland") %>%
  select(NAME, POSTAL) %>%
  st_transform(crs = 2180)

# Continuous Area Cartogram
prez_woj_cont = cartogram_cont(prez_woj, "f1.duda", 10)

# Non-contiguous Area Cartogram
prez_woj_ncont = cartogram_ncont(prez_woj, "f1.duda")

# Non-overlapping Circles Cartogram
prez_woj_dorling = cartogram_dorling(prez_woj, "f1.duda")


# Plots

tmaptools::palette_explorer()

# Bounding box dla Polski
bb = bb(prez_woj, ext = 1.05)

paleta_duda = get_brewer_pal("YlOrRd", n = 5, contrast = c(0.15, 0.7))

# Continuous Area Cartogram
map_1 = tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(bord) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(prez_woj_cont) +
  tm_polygons("f1.duda", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_duda) +
tm_scale_bar(position = c("left", "bottom")) +
tm_layout(bg.color = "lightcyan2")

map_2 = tm_shape(granica, bbox = bb) +
tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(bord) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(prez_woj_ncont) +
  tm_polygons("f1.duda", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_duda) +
tm_scale_bar(position = c("left", "bottom")) +
tm_layout(bg.color = "lightcyan2")

#map_3 = 
tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(bord) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(prez_woj_dorling) +
  tm_polygons("f1.duda", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_duda) +
tm_scale_bar(position = c("left", "bottom")) +
tm_layout(bg.color = "lightcyan2")

ml = tm_shape(prez_woj_dorling) + tm_polygons("f1.duda", style = "jenks") +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

#tmap_arrange(map_1, map_2, map_3, ml, nrow = 1)
