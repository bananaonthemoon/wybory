library(sf)
library(tidyverse)
library(linemap)

prez = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_woj.gpkg"
prez_woj = read_sf(prez, stringsAsFactors=FALSE) 

download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_map_subunits.zip", "dane/temp/ne_10m_admin_0_map_subunits.zip")
unzip("dane/temp/ne_10m_admin_0_map_subunits.zip", exdir = "dane/pobrane")

sasiad = c("Germany", "Czechia", "Austria", "Slovakia", "Ukraine", "Belarus", "Lithuania", "Kalinigrad", "Poland")

border = read_sf("dane/pobrane/ne_10m_admin_0_map_subunits.shp") %>%
  filter(SUBUNIT %in% sasiad) %>%
  select(NAME, POSTAL) %>%
  st_transform(crs = 2180)


# LINEMAP -----------------------------------------------------------------

# wizualizacja 1 ------------------------------------------------------------


#dopasowanie wartości
grid = getgrid(x = prez_woj, cellsize = 8000, var = "f1.duda")
tlo = par(mar=c(0,0,0,0), bg = "ivory2")
plot(st_geometry(border), col="ivory1", border = NA,
     xlim = c(min(grid$X), max(grid$X)), ylim= c(min(grid$Y), max(grid$Y)))
linemap(x = grid, var = "f1.duda", k = 200, threshold = 0.00005,
        col = "ivory1", border = "ivory4", lwd = 0.6, add = TRUE)
par(tlo)


# wizualizacja 2 ------------------------------------------------------------

opar <- par(mar = c(0,0,0,0))
plot(st_geometry(granice), col="lightblue3", border = NA, bg = "lightblue2",
     xlim = c(min(grid$X), max(grid$X)), ylim= c(min(grid$Y), max(grid$Y)))
linemap(x = grid, var = "f1.duda", k = 5000, threshold = 0.000005,
        col = "lightblue3", border = "white", lwd = 0.6,
        add = TRUE)
par(opar)
