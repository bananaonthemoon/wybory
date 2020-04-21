library(cartogram)
library(tmap)
library(maptools)
library(tidyverse)
library(sf)

prez = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_woj.gpkg"
prez_woj = read_sf(prez, stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180)

# Continuous Area Cartogram
prez_woj_cont <- cartogram_cont(prez_woj, "f1.duda", 3)

# Non-contiguous Area Cartogram
prez_woj_ncont <- cartogram_ncont(prez_woj, "f1.duda")

# Non-overlapping Circles Cartogram
prez_woj_dorling <- cartogram_dorling(prez_woj, "f1.duda")

# Plots
m1 <- tm_shape(prez_woj_cont) + tm_polygons("f1.duda", style = "jenks", legend.show = FALSE) +
  tm_layout(frame = FALSE)

m2 <- tm_shape(prez_woj) + tm_borders() + 
  tm_shape(prez_woj_ncont) + tm_polygons("f1.duda", style = "jenks", legend.show = FALSE) +
  tm_layout(frame = FALSE)

m3 <- tm_shape(prez_woj) + tm_borders() + 
  tm_shape(prez_woj_dorling) + tm_polygons("f1.duda", style = "jenks", legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

ml <- tm_shape(prez_woj_dorling) + tm_polygons("f1.duda", style = "jenks") +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

tmap_arrange(m1, m2, m3, ml, nrow = 1)
