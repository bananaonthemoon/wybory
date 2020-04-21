library(sf)
library(tidyverse)
library(linemap)

#download.file("https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-2016-20m.shp.zip", "ref-countries-2016-20m.shp.zip")
#unzip("ref-countries-2016-20m.shp.zip")
#unzip("CNTR_RG_20M_2016_4326.shp.zip")
#europa = read_sf("CNTR_RG_20M_2016_4326.shp", stringsAsFactors=FALSE) %>%
st_transform(crs = 2180) %>%
  drop_na() 
#próbowane st_cast, st_sf

download.file("https://www.gis-support.pl/downloads/Wojewodztwa.zip", "Wojewodztwa.zip")
unzip("Wojewodztwa.zip")
granice = read_sf("dane/Wojew˘dztwa.shp", stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180) %>%
  st_combine() 

prez = "https://raw.github.com/bananaonthemoon/wybory/master/dane/prez_woj.gpkg"
prez_woj = read_sf(prez, stringsAsFactors=FALSE) 


# LINEMAP -----------------------------------------------------------------

# wizualizacja 1 ------------------------------------------------------------


#tak na oko
#możnaby było zrobić coś
#dla europa nie działa, ale dla granice tak
grid = getgrid(x = prez_woj, cellsize = 6000, var = "f1.duda")
tlo = par(mar=c(0,0,0,0), bg = "ivory2")
plot(st_geometry(granice), col="ivory1", border = NA,
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
