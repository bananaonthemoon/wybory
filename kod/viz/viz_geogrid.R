library(geogrid)
library(sf)
library(tidyverse)
library(tmap)

#wizualizacja na przykładzie województw

# WOJEWODZTWA -------------------------------------------------------------

wojewodztwa = "https://raw.github.com/bananaonthemoon/wybory/master/dane/wojewodztwa.gpkg"
woj = read_sf(wojewodztwa, stringsAsFactors=FALSE) 
woj$Nazwa4 = substr(woj$JPT_NAZWA_, 1, 4)


# rawplot
rawplot = tm_shape(woj) + 
  tm_polygons("f1.duda", palette = "viridis") +
  tm_text("Nazwa4")


# hexplot
par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells = calculate_grid(shape = woj, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex = calculate_grid(shape = woj, grid_type = "hexagonal", seed = 1)
resulthex <- assign_polygons(woj, new_cells_hex)

hexplot = tm_shape(resulthex) + 
  tm_polygons("t1_Andrzej.Sebastian.Duda", palette = "viridis") +
  tm_text("Nazwa4")


# regplot
par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = woj, grid_type = "regular", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_reg = calculate_grid(shape = woj, grid_type = "regular", seed = 2)
resultreg <- assign_polygons(woj, new_cells_reg)

regplot = tm_shape(resultreg) + 
  tm_polygons("f1.duda", palette = "viridis") +
  tm_text("Nazwa4")

# plot
tmap_arrange(rawplot, hexplot, regplot, nrow = 3)


#czy dodawać też powiaty i wielkopolskę?
# POWIATY -----------------------------------------------------------------


powiaty = "https://raw.github.com/bananaonthemoon/wybory/master/dane/powiaty.gpkg"
pow = read_sf(prez, stringsAsFactors=FALSE) 
pow$Nazwa4 = substr(pow$Nazwa, 1, 4)


# rawplot
rawplot = tm_shape(pow) + 
  tm_polygons("f1.duda", palette = "viridis") +
  tm_text("Nazwa4")


# hexplot
par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells = calculate_grid(shape = pow, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex = calculate_grid(shape = pow, grid_type = "hexagonal", seed = 2)
resulthex = assign_polygons(pow, new_cells_hex)

hexplot = tm_shape(resulthex) + 
  tm_polygons("t1_Andrzej.Sebastian.Duda", palette = "viridis") +
  tm_text("Nazwa4")


# regplot
par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells = calculate_grid(shape = pow, grid_type = "regular", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_reg = calculate_grid(shape = pow, grid_type = "regular", seed = 2)
resultreg <- assign_polygons(pow, new_cells_reg)

regplot = tm_shape(resultreg) + 
  tm_polygons("f1.duda", palette = "viridis") +
  tm_text("Nazwa4")

# plot
tmap_arrange(rawplot, hexplot, regplot, nrow = 3)

