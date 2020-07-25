library(sf)
library(rayshader)
library(tidyverse)
library(RColorBrewer)

prez_w = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_woj_demo.gpkg"
prez_woj = read_sf(prez_w, stringsAsFactors=FALSE)

paleta_blue = display.brewer.pal(5, "Blues")
               
theme_set(theme_bw())

gg_prez_woj = ggplot(prez_woj) +
  geom_sf(aes(fill = t1.komo), color = "white", size = 0.4) +
  scale_colour_brewer(palette = "Blues", direction = -1) +
  ggtitle("Rayshader")

plot(gg_prez_woj)

plot_gg(gg_prez_woj, shadow_intensity = 0.2, fov = 70)
render_depth(focallength=100,focus=0.72)
