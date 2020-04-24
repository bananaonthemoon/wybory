library(cartogram)
library(tmap)
library(tmaptools)
library(tidyverse)
library(sf)

prez = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_powiaty.gpkg"

prez_woj = read_sf(prez, stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180)

#granice = read_sf(prez, stringsAsFactors=FALSE) %>%
#  st_transform(crs = 2180) %>%
#  st_union() 

download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_map_subunits.zip", "dane/temp/ne_10m_admin_0_map_subunits.zip")
unzip("dane/temp/ne_10m_admin_0_map_subunits.zip", exdir = "dane/pobrane")

sasiad = c("Germany", "Czechia", "Slovakia", "Ukraine", "Belarus", "Lithuania", "Kalinigrad", "Poland")

bord = read_sf("dane/pobrane/ne_10m_admin_0_map_subunits.shp") %>%
  filter(SUBUNIT %in% sasiad) %>%
  select(NAME, POSTAL) %>%
  st_transform(crs = 2180)

# Continuous Area Cartogram
prez_woj_cont = cartogram_cont(prez_woj, "f1.duda", 10)

# Non-contiguous Area Cartogram
prez_woj_ncont = cartogram_ncont(prez_woj, "f1.duda")

# Non-overlapping Circles Cartogram
prez_woj_dorling = cartogram_dorling(prez_woj, "f1.duda")

#xlim = c(min(prez_woj$X), max(prez_woj$X)), ylim= c(min(prez_woj$Y), max(prez_woj$Y)))

# Plots
tm_shape(bord) + 
  tm_borders() +
tm_shape(prez_woj_cont) +
  tm_polygons("f1.duda", style = "jenks", legend.show = FALSE) +

last_plot()

+
  tm_shape(prez_woj_cont) + tm_polygons("f1.duda", style = "jenks", legend.show = FALSE) +
  tm_layout(frame = FALSE)
plot(m1)

m2 = tm_shape(prez_woj) + tm_borders() + 
  tm_shape(prez_woj_ncont) + tm_polygons("f1.duda", style = "jenks", legend.show = FALSE) +
  tm_layout(frame = FALSE)

m3 = tm_shape(prez_woj) + tm_borders() + 
  tm_shape(prez_woj_dorling) + tm_polygons("f1.duda", style = "jenks", legend.show = FALSE) +
  tm_layout(frame = FALSE, legend.outside = TRUE)

ml = tm_shape(prez_woj_dorling) + tm_polygons("f1.duda", style = "jenks") +
  tm_layout(frame = FALSE, legend.only = TRUE, legend.position = c("center", "center"))

tmap_arrange(m1, m2, m3, ml, nrow = 1)
