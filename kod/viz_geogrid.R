library(geogrid)
library(sf)
library(tidyverse)
library(tmap)

prez = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_woj.gpkg"
prez_woj = read_sf(prez, stringsAsFactors=FALSE) 
prez_woj$Nazwa4 <- substr(prez_woj$Nazwa, 1, 4)


# rawplot -----------------------------------------------------------------


rawplot <- tm_shape(prez_woj) + 
  tm_polygons("f1.duda", palette = "viridis") +
  tm_text("Nazwa4")


# hexplot -----------------------------------------------------------------


par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = prez_woj, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex <- calculate_grid(shape = prez_woj, grid_type = "hexagonal", seed = 2)
resulthex <- assign_polygons(prez_woj, new_cells_hex)

hexplot <- tm_shape(resulthex) + 
  tm_polygons("t1_Andrzej.Sebastian.Duda", palette = "viridis") +
  tm_text("Nazwa4")


# regplot -----------------------------------------------------------------

par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = prez_woj, grid_type = "regular", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_reg <- calculate_grid(shape = prez_woj, grid_type = "regular", seed = 2)
resultreg <- assign_polygons(prez_woj, new_cells_reg)

regplot <- tm_shape(resultreg) + 
  tm_polygons("f1.duda", palette = "viridis") +
  tm_text("Nazwa4")

tmap_arrange(rawplot, hexplot, regplot, nrow = 3)

