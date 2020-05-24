library(rayshader)
library(geogrid)
library(dplyr)
library(sf)


prez_w = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_woj.gpkg"
prez_woj = read_sf(prez_w, stringsAsFactors=FALSE)
xy = st_coordinates(prez_woj)
extractCoords <- function(prez_woj)
{
  results <- list()
  for(i in 1:length(prez_woj@polygons[[1]]@Polygons))
  {
    results[[i]] <- prez_woj@polygons[[1]]@Polygons[[i]]@coords
  }
  results <- Reduce(rbind, results)
  results
}



set.seed(32)

par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells = calculate_grid(shape = prez_woj, learning_rate = 0.4, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

new_cells_hex_woj = calculate_grid(shape = prez_woj, grid_type = "hexagonal", seed = 2)

resulthex_woj = assign_polygons(prez_woj, new_cells_hex_woj)



#Lines
pp = ggplot(prez_woj, aes(x = x, y = y)) +
  geom_hex(bins = 20, size = 0.5, color = "black") +
  scale_fill_viridis_c(option = "C")

par(mfrow = c(1, 2))
plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))
render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)
