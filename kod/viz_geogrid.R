library(geogrid)
library(sf)
library(tidyverse)
library(tmap)

#download.file("https://www.gis-support.pl/downloads/Wojewodztwa.zip", "Wojewodztwa.zip")
#unzip("Wojewodztwa.zip")
granice = read_sf("Wojew˘dztwa.shp", stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180) %>%
  st_combine() 

prez_woj = read_sf("prez_woj.gpkg", stringsAsFactors=FALSE) 
prez_woj$nazwa4 <- substr(prez_woj$Nazwa, 1, 4)

rawplot <- tm_shape(prez_woj) + 
  tm_polygons("X1_frekw", palette = "viridis") +
  tm_text("nazwa4")
rawplot



# hex ---------------------------------------------------------------------


par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = prez_woj, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex <- calculate_grid(shape = prez_woj, grid_type = "hexagonal", seed = 2)
resulthex <- assign_polygons(prez_woj, new_cells_hex)


# reg ---------------------------------------------------------------------

par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = prez_woj, grid_type = "regular", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_reg <- calculate_grid(shape = prez_woj, grid_type = "regular", seed = 2)
resultreg <- assign_polygons(prez_woj, new_cells_reg)


# laczenie wizualizacji ---------------------------------------------------

hexplot <- tm_shape(resulthex) + 
  tm_polygons("X1_frekw", palette = "viridis") +
  tm_text("nazwa4")

regplot <- tm_shape(resultreg) + 
  tm_polygons("X1_frekw", palette = "viridis") +
  tm_text("nazwa4")

tmap_arrange(rawplot, hexplot, regplot, nrow = 3)
