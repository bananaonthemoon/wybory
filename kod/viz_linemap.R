
# dodanie danych ----------------------------------------------------------


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

#download.file("https://www.gis-support.pl/downloads/Wojewodztwa.zip", "Wojewodztwa.zip")
#unzip("Wojewodztwa.zip")
granice = read_sf("Wojew˘dztwa.shp", stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180) %>%
  st_combine() 

pres_gmin = read_sf("pres_gmin.gpkg", stringsAsFactors=FALSE) 


# LINEMAP -----------------------------------------------------------------


# wizualizacja 1 ------------------------------------------------------------


#tak na oko
#możnaby było zrobić coś
#dla europa nie działa, ale dla granice tak
grid = getgrid(x = pres_gmin, cellsize = 15000, var = "X1_frekw")
tlo = par(mar=c(0,0,0,0), bg = "ivory2")
plot(st_geometry(granice), col="ivory1", border = NA,
     xlim = c(min(grid$X), max(grid$X)), ylim= c(min(grid$Y), max(grid$Y)))
linemap(x = grid, var = "X1_frekw", k = 50, threshold = 0.5,
        col = "ivory1", border = "ivory4", lwd = 1, add = TRUE)
par(tlo)


# wizualizacja 2 ------------------------------------------------------------



plot(st_geometry(europa), col="lightblue3", border = NA, bg = "lightblue2",
     xlim = c(min(grid$X), max(grid$X)), ylim= c(min(grid$Y), max(grid$Y)))
#Błąd w poleceniu 'UseMethod("st_geometry")':
#niestosowalna metoda dla 'st_geometry' zastosowana do obiektu klasy "c('tbl_df', 'tbl', 'data.frame')"
linemap(x = grid, var = "X1_frekw", k = 500, threshold = 0.5,
        col = "lightblue3", border = "white", lwd = 0.8,
        add = TRUE)
par(opar)

