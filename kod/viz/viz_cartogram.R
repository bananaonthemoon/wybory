---
title: "rmark_cartogram"
author: "Maria_Król"
date: "7 05 2020"
output: html_document
---
  
#Pakiet cartogram

library(cartogram)
library(tmap)
library(maptools)
library(tidyverse)
library(sf)
library(tmaptools)

# WOJEWODZTWA -------------------------------------------------------------

# Dane

prez_w = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_woj.gpkg"
prez_woj = read_sf(prez_w, stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180)

# Obliczenia

# Continuous Area Cartogram
prez_woj_cont_duda = cartogram_cont(prez_woj, "t1.duda", 8)
prez_woj_cont_frek = cartogram_cont(prez_woj, "f1", 50, threshold = 0.1)

# Non-contiguous Area Cartogram
prez_woj_ncont_duda = cartogram_ncont(prez_woj, "t1.duda")
prez_woj_ncont_frek = cartogram_ncont(prez_woj, "f1")

# Non-overlapping Circles Cartogram
prez_woj_dorling_duda = cartogram_dorling(prez_woj, "t1.duda")
prez_woj_dorling_frek = cartogram_dorling(prez_woj, "f1")

# Palety kolorów dla zmiennych

paleta_red = get_brewer_pal("Reds")
paleta_green = get_brewer_pal("Greens")

# Plot CAC

cont_woj_duda = tm_shape(prez_woj_cont_duda) + 
  tm_polygons("t1.duda", style = "jenks", palette = paleta_red, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

cont_woj_frek = tm_shape(prez_woj_cont_frek) + 
  tm_polygons("f1", style = "jenks", palette = paleta_green, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# Plot NCAC

ncont_woj_duda = tm_shape(prez_woj) + 
  tm_borders() + 
  tm_shape(prez_woj_ncont_duda) + 
  tm_polygons("t1.duda", style = "jenks", palette = paleta_red, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

ncont_woj_frek = tm_shape(prez_woj) + 
  tm_borders() + 
  tm_shape(prez_woj_ncont_frek) + 
  tm_polygons("f1", style = "jenks", palette = paleta_green, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# Plot NCC

dorling_woj_duda = tm_shape(prez_woj) + 
  tm_borders() + 
  tm_shape(prez_woj_dorling_duda) + 
  tm_polygons("t1.duda", style = "jenks", palette = paleta_red, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

dorling_woj_frek = tm_shape(prez_woj) + 
  tm_borders() + 
  tm_shape(prez_woj_dorling_frek) + 
  tm_polygons("f1", style = "jenks", palette = paleta_green, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# Layout

layout_woj_duda = tm_shape(prez_woj_dorling_duda) + 
  tm_polygons("t1.duda", style = "jenks", palette = paleta_red) +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

layout_woj_frek = tm_shape(prez_woj_dorling_frek) + 
  tm_polygons("f1", style = "jenks", palette = paleta_green) +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

tmap_arrange(cont_woj_duda, ncont_woj_duda, dorling_woj_duda, layout_woj_duda, cont_woj_frek, ncont_woj_frek, dorling_woj_frek, layout_woj_frek, nrow = 2)


# POWIATY -----------------------------------------------------------------


# Dane

prez_p = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_pow.gpkg"
prez_pow = read_sf(prez_p, stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180)

# Continuous Area Cartogram
prez_pow_cont_duda = cartogram_cont(prez_pow, "t1.duda", 8)
prez_pow_cont_frek = cartogram_cont(prez_pow, "f1", 8)

# Non-contiguous Area Cartogram
prez_pow_ncont_duda = cartogram_ncont(prez_pow, "t1.duda")
prez_pow_ncont_frek = cartogram_ncont(prez_pow, "f1")

# Non-overlapping Circles Cartogram
prez_pow_dorling_duda = cartogram_dorling(prez_pow, "t1.duda")
prez_pow_dorling_frek = cartogram_dorling(prez_pow, "f1")

paleta_red = get_brewer_pal("Reds")
paleta_green = get_brewer_pal("Greens")

cont_pow_duda = tm_shape(prez_pow_cont_duda) + 
  tm_polygons("t1.duda", style = "jenks", palette = paleta_red, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

cont_pow_frek = tm_shape(prez_pow_cont_frek) + 
  tm_polygons("f1", style = "jenks", palette = paleta_green, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

ncont_pow_duda = tm_shape(prez_pow) + 
  tm_borders() + 
  tm_shape(prez_pow_ncont_duda) + 
  tm_polygons("t1.duda", style = "jenks", palette = paleta_red, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

ncont_pow_frek = tm_shape(prez_pow) + 
  tm_borders() + 
  tm_shape(prez_pow_ncont_frek) + 
  tm_polygons("f1", style = "jenks", palette = paleta_green, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

dorling_pow_duda = tm_shape(prez_pow) + 
  tm_borders() + 
  tm_shape(prez_pow_dorling_duda) + 
  tm_polygons("t1.duda", style = "jenks", palette = paleta_red, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

dorling_pow_frek = tm_shape(prez_pow) + 
  tm_borders() + 
  tm_shape(prez_pow_dorling_frek) + 
  tm_polygons("f1", style = "jenks", palette = paleta_green, legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

layout_pow_duda = tm_shape(prez_pow_dorling_duda) + 
  tm_polygons("t1.duda", style = "jenks", palette = paleta_red) +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

layout_pow_frek = tm_shape(prez_pow_dorling_frek) + 
  tm_polygons("f1", style = "jenks", palette = paleta_green) +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

tmap_arrange(cont_pow_duda, ncont_pow_duda, dorling_pow_duda, layout_pow_duda, cont_pow_frek, ncont_pow_frek, dorling_pow_frek, layout_pow_frek, nrow = 2)

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

