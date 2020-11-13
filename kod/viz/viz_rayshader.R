library(sf)
library(rayshader)
library(tidyverse)
library(RColorBrewer)
library(classInt)

prez_w = "https://raw.github.com/bananaonthemoon/wybory/master/dane/wojewodztwa.gpkg"
prez_woj = read_sf(prez_w, stringsAsFactors=FALSE)


#coś takiego zrobiłam, ale nie wiem czy nie znaleźć czegoś innego?
frek1_klasy <- classIntervals(prez_woj$t1_Frekwencja, n = 5, style = "jenks")
prez_woj = prez_woj %>% mutate(frek1 = cut(t1_Frekwencja, frek1_klasy$brks, include.lowest = T))
               
theme_set(theme_bw())

gg_prez_woj = ggplot(prez_woj) +
  geom_sf(aes(fill = t1_Frekwencja), color = "white", size = 0.4) +
  scale_fill_fermenter("Frekwencja [%]", palette = "Greens", direction = 1) + #ferment
  ggtitle("Frekwencja w pierwszej turze \nwyborów prezydenckich w 2020 r.")

plot(gg_prez_woj)

plot_gg(gg_prez_woj, width = 5, height = 4, scale = 100, multicore = TRUE, shadow_intensity = 0.5, preview = TRUE)
render_depth(focallength=100,focus=0.72)
