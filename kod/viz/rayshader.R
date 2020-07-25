library(rayshader)
library(geogrid)
library(dplyr)
library(sf)
library(ggplot2)


prez_w = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_woj.gpkg"
prez_woj = read_sf(prez_w, stringsAsFactors=FALSE)
xy = st_coordinates(prez_woj)



xy2 = stat_sf_coordinates(mapping = aes(), data = prez_woj)

#Lines
pp = ggplot(prez_woj, aes(x = x, y = y)) +
  geom_hex(bins = 20, size = 0.5, color = "black") +
  scale_fill_viridis_c(option = "C")
plot(pp)

par(mfrow = c(1, 2))
plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))
render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)