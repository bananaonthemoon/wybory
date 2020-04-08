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

rawplot <- tm_shape(prez_woj) + 
  tm_polygons("t1_Andrzej.Sebastian.Duda", palette = "viridis") +
  tm_text("NAZWA")
rawplot

par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = prez_woj, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex <- calculate_grid(shape = prez_woj, grid_type = "hexagonal", seed = 2)
resulthex <- assign_polygons(prez_woj, new_cells_hex)

hexplot <- tm_shape(resulthex) + 
  tm_polygons("HECTARES", palette = "viridis") +
  tm_text("SNAME")

tmap_arrange(rawplot, hexplot, regplot, nrow = 3)
