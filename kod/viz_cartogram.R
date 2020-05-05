library(cartogram)
library(tmap)
library(tmaptools)
library(tidyverse)
library(sf)



prez = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_powiaty_demo.gpkg"

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


# t1.duda -----------------------------------------------------------------


# Continuous Area Cartogram
prez_woj_cont_duda = cartogram_cont(prez_woj, "t1.duda", 10)

# Non-contiguous Area Cartogram
prez_woj_ncont_duda = cartogram_ncont(prez_woj, "t1.duda")

# Non-overlapping Circles Cartogram
prez_woj_dorling_duda = cartogram_dorling(prez_woj, "t1.duda")


# Plots

tmaptools::palette_explorer()

# Bounding box dla Polski
bb = bb(prez_woj, ext = 1.05)

paleta_duda = get_brewer_pal("YlOrRd", n = 5, contrast = c(0.15, 0.7))

# Continuous Area Cartogram
map_1_duda = tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(bord) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(prez_woj_cont) +
  tm_polygons("t1.duda", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_duda) +
tm_scale_bar(position = c("left", "bottom")) +
tm_layout(bg.color = "lightcyan2")

map_2_duda = tm_shape(granica, bbox = bb) +
tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(bord) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(prez_woj_ncont) +
  tm_polygons("t1.duda", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_duda) +
tm_scale_bar(position = c("left", "bottom")) +
tm_layout(bg.color = "lightcyan2")

map_3_duda = tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(bord) +
  tm_polygons(border.col = "white", col = "gray85") +
tm_shape(prez_woj_dorling) +
  tm_polygons("t1.duda", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_duda) +
tm_scale_bar(position = c("left", "bottom")) +
tm_layout(bg.color = "lightcyan2")

ml_duda = tm_shape(prez_woj_dorling) + tm_polygons("t1.duda", style = "jenks") +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

#tmap_arrange(map_1_duda, map_2_duda, map_3_duda, ml_duda, nrow = 1)


# f1 ----------------------------------------------------------------


# Continuous Area Cartogram
prez_woj_cont_f1 = cartogram_cont(prez_woj, "f1", 50, threshold = 0.1)

# Non-contiguous Area Cartogram
prez_woj_ncont_f1 = cartogram_ncont(prez_woj, "f1")

# Non-overlapping Circles Cartogram
prez_woj_dorling_f1 = cartogram_dorling(prez_woj, "f1")


# Plots pojedyncze (z granicami innych krajów) --------------------------------------------------------


tmaptools::palette_explorer()

# Bounding box dla Polski
bb = bb(prez_woj, ext = 1.05)

paleta_f1 = get_brewer_pal("YlOrRd", n = 5, contrast = c(0.15, 0.7))

# Continuous Area Cartogram
map_1_f1 = tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(bord) +
  tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(prez_woj_cont_f1) +
  tm_polygons("f1", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_f1) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(bg.color = "lightcyan2")

map_2_f1 = tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(bord) +
  tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(prez_woj_ncont_f1) +
  tm_polygons("f1", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_f1) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(bg.color = "lightcyan2")

map_3_f1 = tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(bord) +
  tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(prez_woj_dorling_f1) +
  tm_polygons("f1", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_f1) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(bg.color = "lightcyan2")

ml_f1 = tm_shape(prez_woj_dorling_f1) + tm_polygons("f1", style = "jenks") +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

#może porównać frek1 z frek2
#tmap_arrange(map_1_f1, map_2_f1, map_3_f1, ml_f1, nrow = 1)


# Plots razem -------------------------------------------------------------


m1 = tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(prez_woj_cont_f1) +
  tm_polygons("f1", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_f1, legend.show = FALSE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

m2 = tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(prez_woj_ncont_f1) +
  tm_polygons("f1", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_f1, legend.show = FALSE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

m3 = tm_shape(granica, bbox = bb) +
  tm_polygons(border.col = "white", col = "gray85") +
  tm_shape(prez_woj_dorling_f1) +
  tm_polygons("f1", style = "jenks", border.col = "White", border.alpha = 0.1, palette = paleta_f1, legend.show = FALSE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

ml = tm_shape(prez_woj_dorling_f1) + tm_polygons("f1", style = "jenks") +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

tmap_arrange(m1, m2, m3, ml, nrow = 1)

#tutaj wyraźnie jest coś nie tak z kólkami